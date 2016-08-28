//===- interp.go - llgo-based Go interpreter ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This package contains functions and types for interpreting Go programs,
// based on llgo and the LLVM JIT.
//
//===----------------------------------------------------------------------===//

package interp

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"path/filepath"
	"runtime/debug"
	"strconv"
	"sync"
	"unsafe"

	"github.com/axw/interputil"

	"llvm.org/llgo/driver"
	"llvm.org/llgo/irgen"
	"llvm.org/llgo/third_party/gotools/go/types"
	"llvm.org/llvm/bindings/go/llvm"
)

// /* Force exporting __morestack if it's available, so that it is
//    available to the engine when linking with libLLVM.so. */
//
// void *__morestack __attribute__((weak));
import "C"

var initLLVMOnce sync.Once

func initLLVM() {
	llvm.LinkInMCJIT()
	llvm.InitializeNativeTarget()
	llvm.InitializeNativeAsmPrinter()
}

// Config contains configuration for an Interpreter.
type Config struct {
	// ImportPaths contains import paths to search for llgo-compiled
	// packages. This should include the libgo-llgo runtime, which is
	// typically found in <llvm-install-prefix>/lib/go/llgo-<llvm-version>.
	ImportPaths []string
}

// Validate validates the configuration.
func (cfg Config) Validate() error {
	return nil
}

// Interpreter encapsulates a Go interpreter session.
//
// It is currently only valid to create a single Interpreter per host process.
type Interpreter struct {
	engine  llvm.ExecutionEngine
	copts   irgen.CompilerOptions
	imports []*types.Package
	scope   map[string]types.Object
	modules map[string]llvm.Module
	pkgmap  map[string]*types.Package
	pkgnum  int
}

// NewInterpreter returns a new Interpreter with the provided
// configuration.
func NewInterpreter(cfg Config) (*Interpreter, error) {
	if err := cfg.Validate(); err != nil {
		return nil, err
	}
	initLLVMOnce.Do(initLLVM)

	// Create the compiler options, and package importer.
	pkgmap := make(map[string]*types.Package)
	copts := irgen.CompilerOptions{
		TargetTriple:  llvm.DefaultTargetTriple(),
		ImportPaths:   cfg.ImportPaths,
		GenerateDebug: true,
		Packages:      pkgmap,
	}
	if err := copts.MakeImporter(); err != nil {
		return nil, err
	}
	origImporter := copts.Importer
	copts.Importer = func(pkgmap map[string]*types.Package, pkgpath string) (*types.Package, error) {
		if pkg, ok := pkgmap[pkgpath]; ok && pkg.Complete() {
			return pkg, nil
		}
		return origImporter(pkgmap, pkgpath)
	}

	return &Interpreter{
		copts:   copts,
		pkgmap:  pkgmap,
		scope:   make(map[string]types.Object),
		modules: make(map[string]llvm.Module),
	}, nil
}

// Dispose disposes of any resources created by the interpreter.
func (in *Interpreter) Dispose() {
	if in.engine.C != nil {
		in.engine.Dispose()
		in.engine = llvm.ExecutionEngine{}
	}
}

// Interpret takes a Buffer and interprets the declaration, statement, or
// expression within it. The Buffer must be ready, and non-empty.
func (in *Interpreter) Interpret(buf *interputil.Buffer) ([]interface{}, error) {
	if !buf.Ready() {
		panic("buffer is not ready")
	}

	tokens := buf.Tokens()
	switch tokens[0] {
	case token.COMMENT:
		// Comments are ignored for now. We may later want to attach
		// them to `import "C"`.
		return nil, nil

	case token.IMPORT:
		fset := token.NewFileSet()
		imports, err := interputil.ParseImports(fset, buf)
		if err != nil {
			return nil, fmt.Errorf("error parsing imports: %v", err)
		}
		for _, spec := range imports {
			// TODO(axw) honour spec.Name
			pkgpath, err := strconv.Unquote(spec.Path.Value)
			if err != nil {
				return nil, err
			}
			if _, err := in.Import(pkgpath); err != nil {
				return nil, err
			}
		}
		return nil, nil

	case token.CONST, token.VAR:
		fset := token.NewFileSet()
		spec, err := interputil.ParseValueSpec(fset, buf)
		if err != nil {
			return nil, fmt.Errorf("error parsing value spec: %v", err)
		}
		return nil, in.interpretDecl(buf, spec.Names...)

	case token.TYPE:
		fset := token.NewFileSet()
		spec, err := interputil.ParseTypeSpec(fset, buf)
		if err != nil {
			return nil, fmt.Errorf("error parsing type: %v", err)
		}
		return nil, in.interpretDecl(buf, spec.Name)

	case token.FUNC:
		// Note that we don't support defining methods in the
		// interpreter. If the second token is not an identifier,
		// we could be dealing with either a method or a function
		// literal; we assume it's a function literal and leave
		// it to the parser to flag the error.
		if len(tokens) > 1 && tokens[1] == token.IDENT {
			fset := token.NewFileSet()
			decl, err := interputil.ParseFuncDecl(fset, buf)
			if err != nil {
				return nil, fmt.Errorf("error parsing function: %v", err)
			}
			return nil, in.interpretDecl(buf, decl.Name)
		}
		fallthrough

	default:
		fset := token.NewFileSet()
		stmt, err := interputil.ParseStmt(fset, buf)
		if err != nil {
			return nil, fmt.Errorf("error parsing statement: %v", err)
		}
		if _, ok := stmt.(*ast.ExprStmt); ok {
			return in.interpretExpr(buf)
		}
		return nil, in.interpretStmt(fset, buf, stmt)
	}
}

// Program takes a Buffer and returns a program that embeds the buffer's
// declaration, statement, or expression within it, and the position in
// the output at which the input begins. The Buffer must be ready, and
// non-empty.
//
// The input buffer may be incomplete, so that Program may be used for
// code completion.
func (in *Interpreter) Program(buf *interputil.Buffer) (string, int, error) {
	if !buf.Ready() {
		panic("buffer is not ready")
	}

	code := in.preparePackageCode()
	begin := code.Len()

	tokens := buf.Tokens()
	switch tokens[0] {
	case token.COMMENT:
		code.WriteString(buf.String())
		return code.String(), begin, nil

	case token.IMPORT:
		fset := token.NewFileSet()
		imports, err := interputil.ParseImports(fset, buf)
		if err != nil {
			return "", -1, fmt.Errorf("error parsing imports: %v", err)
		}
		for _, spec := range imports {
			fmt.Fprint(code, "import ")
			if spec.Name != nil {
				fmt.Fprintf(code, "%s ", spec.Name.Name)
			}
			fmt.Fprintln(code, spec.Path.Value)
		}
		return code.String(), begin, nil
	}

	// For all token types handled below, the code may refer to existing
	// in-scope objects. Declare them first.
	for name, obj := range in.scope {
		declType := "var"
		if _, ok := obj.(*types.Const); ok {
			declType = "const"
		}
		fmt.Fprintf(code, "%s %s %s\n", declType, name, obj.Type())
	}
	begin = code.Len()

	switch tokens[0] {
	case token.TYPE:
		code.WriteString(buf.String())
		code.WriteRune('\n')
		return code.String(), begin, nil

	case token.FUNC:
		// Note that we don't support defining methods in the
		// interpreter. If the second token is not an identifier,
		// we could be dealing with either a method or a function
		// literal; we assume it's a function literal and leave
		// it to the parser to flag the error.
		if len(tokens) > 1 && tokens[1] == token.IDENT {
			code.WriteString(buf.String())
			code.WriteRune('\n')
			return code.String(), begin, nil
		}
		fallthrough

	default:
		fmt.Fprintln(code, "func init() {")
		begin = code.Len()
		code.WriteString(buf.String())
		code.WriteString("\n}\n")
		return code.String(), begin, nil
	}
}

// Import directs the interpreter to import the package with the specified
// path, and returns it. If the package is found in the import path, it
// will be loaded and returned. Otherwise, the package will be JIT-compiled
// from source.
//
// TODO(axw) this should accept a package name, which may be "".
func (in *Interpreter) Import(pkgpath string) (*types.Package, error) {
	pkg, err := in.importPackage(pkgpath)
	if err != nil {
		return nil, err
	}
	in.imports = append(in.imports, pkg)
	return pkg, nil
}

func (in *Interpreter) importPackage(pkgpath string) (*types.Package, error) {
	pkg, err := in.copts.Importer(in.pkgmap, pkgpath)
	if err == nil {
		return pkg, nil
	}

	buildpkg, err := build.Import(pkgpath, ".", 0)
	if err != nil {
		return nil, err
	}
	if len(buildpkg.CgoFiles) != 0 {
		return nil, fmt.Errorf("%s: cannot load cgo package", pkgpath)
	}

	for _, imp := range buildpkg.Imports {
		if _, err := in.importPackage(imp); err != nil {
			return nil, err
		}
	}

	inputs := make([]string, len(buildpkg.GoFiles))
	for i, file := range buildpkg.GoFiles {
		inputs[i] = filepath.Join(buildpkg.Dir, file)
	}

	fset := token.NewFileSet()
	files, err := driver.ParseFiles(fset, inputs)
	if err != nil {
		return nil, err
	}
	return in.loadSourcePackage(fset, files, pkgpath, in.copts)
}

func (in *Interpreter) loadSourcePackageFromCode(pkgcode, pkgpath string, copts irgen.CompilerOptions) (*types.Package, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "<input>", pkgcode, parser.DeclarationErrors|parser.ParseComments)
	if err != nil {
		return nil, err
	}
	files := []*ast.File{file}
	return in.loadSourcePackage(fset, files, pkgpath, copts)
}

func (in *Interpreter) loadSourcePackage(fset *token.FileSet, files []*ast.File, pkgpath string, copts irgen.CompilerOptions) (_ *types.Package, resultErr error) {
	compiler, err := irgen.NewCompiler(copts)
	if err != nil {
		return nil, err
	}

	module, err := compiler.Compile(fset, files, pkgpath)
	if err != nil {
		return nil, err
	}
	in.modules[pkgpath] = module.Module

	if in.engine.C != nil {
		in.engine.AddModule(module.Module)
	} else {
		options := llvm.NewMCJITCompilerOptions()
		in.engine, err = llvm.NewMCJITCompiler(module.Module, options)
		if err != nil {
			return nil, err
		}
	}

	var importFunc func()
	importAddress := in.getPackageSymbol(pkgpath, ".import$descriptor")
	*(*unsafe.Pointer)(unsafe.Pointer(&importFunc)) = importAddress

	defer func() {
		p := recover()
		if p != nil {
			resultErr = fmt.Errorf("panic: %v\n%v", p, string(debug.Stack()))
		}
	}()
	importFunc()
	in.pkgmap[pkgpath] = module.Package

	return module.Package, nil
}

func (in *Interpreter) getPackageSymbol(pkgpath, name string) unsafe.Pointer {
	symbolName := irgen.ManglePackagePath(pkgpath) + "." + name
	global := in.modules[pkgpath].NamedGlobal(symbolName)
	if global.IsNil() {
		return nil
	}
	return in.engine.PointerToGlobal(global)
}

func (in *Interpreter) augmentPackageScope(pkg *types.Package) {
	for _, obj := range in.scope {
		pkg.Scope().Insert(obj)
	}
}

// interpretDecl interprets one of the following:
//     function declaration, type spec, (const or var) value spec.
// The declaration will be added to the interpreter scope.
func (in *Interpreter) interpretDecl(buf *interputil.Buffer, names ...*ast.Ident) error {
	pkg, code := in.preparePackage()
	code.WriteString(buf.String())
	code.WriteRune('\n')
	pkg, err := in.finalizePackage(pkg, code)
	if err != nil {
		return err
	}
	for _, name := range names {
		in.scope[name.Name] = pkg.Scope().Lookup(name.Name)
	}
	return nil
}

// interpretExpr interprets an expression, and returns the results (if any)
// as a slice of interface{}.
func (in *Interpreter) interpretExpr(buf *interputil.Buffer) ([]interface{}, error) {
	pkg, code := in.preparePackage()
	n, err := in.countExprValues(buf, pkg)
	if err != nil {
		return nil, err
	}
	if n == 0 {
		// This is an expression with no results.
		fmt.Fprintf(code, "func init() {\n\t%s\n}\n", buf.String())
		_, err := in.finalizePackage(pkg, code)
		return nil, err
	}

	// Generate code that looks like:
	//    func __llgoiExpr() []interface{} {
	//        __llgoiV0, __llgoiV1 := expr
	//        return []interface{}{__llgoiV0, __llgoiV1}
	//    }
	code.WriteString("func __llgoiExpr() []interface{} {\n\t")
	for i := 0; i < n; i++ {
		if i != 0 {
			code.WriteString(", ")
		}
		fmt.Fprintf(code, "__llgoiV%d", i)
	}
	fmt.Fprintf(code, " := %s\n", buf.String())
	code.WriteString("\treturn []interface{}{")
	for i := 0; i < n; i++ {
		if i != 0 {
			code.WriteString(", ")
		}
		fmt.Fprintf(code, "__llgoiV%d", i)
	}
	code.WriteString("}\n}\n")

	pkg, err = in.finalizePackage(pkg, code)
	if err != nil {
		return nil, err
	}
	var resultsFunc func() []interface{}
	llgoiResultsAddress := in.getPackageSymbol(pkg.Path(), "__llgoiExpr$descriptor")
	*(*unsafe.Pointer)(unsafe.Pointer(&resultsFunc)) = llgoiResultsAddress
	return resultsFunc(), nil
}

// interpretStmt interprets a statement. If it is an assignment statement,
// then the defined identifiers are added to the interpreter scope.
func (in *Interpreter) interpretStmt(fset *token.FileSet, buf *interputil.Buffer, stmt ast.Stmt) error {
	if assignStmt, ok := stmt.(*ast.AssignStmt); ok && assignStmt.Tok == token.DEFINE {
		return in.interpretAssignStmt(fset, buf, assignStmt)
	}
	pkg, code := in.preparePackage()
	fmt.Fprintf(code, "func init() {\n\t%s\n}\n", buf.String())
	_, err := in.finalizePackage(pkg, code)
	return err
}

// interpretAssignStmt interprets an assignment statement, adding the defined
// identifiers to the interpreter scope.
func (in *Interpreter) interpretAssignStmt(fset *token.FileSet, buf *interputil.Buffer, stmt *ast.AssignStmt) error {
	pkg, code := in.preparePackage()

	// Some of the names may have been defined in a previous
	// interpretStmt or interpretDecl call. We'll need to
	// check for those, and define a temporary variable to
	// assign to.
	type tempAssignment struct {
		name, temp string
	}
	var temps []tempAssignment
	var defined []string
	code.WriteString("var ")
	for i, lhs := range stmt.Lhs {
		if i != 0 {
			code.WriteString(", ")
		}
		name := lhs.(*ast.Ident).Name
		if _, ok := in.scope[name]; ok {
			temp := fmt.Sprintf("__llgoiV%d", i)
			temps = append(temps, tempAssignment{name, temp})
			code.WriteString(temp)
		} else {
			if name != "_" {
				defined = append(defined, name)
			}
			code.WriteString(name)
		}
	}
	// TODO(axw) update interputil to return nodes with tokens
	// having offsets relative to the start of "buf".
	stmtPosition := fset.Position(stmt.Pos())
	beginPosition := fset.Position(stmt.Rhs[0].Pos())
	endPosition := fset.Position(stmt.End())
	beginOffset := beginPosition.Offset - stmtPosition.Offset
	endOffset := endPosition.Offset - stmtPosition.Offset
	code.WriteString(" = ")
	code.WriteString(buf.String()[beginOffset:endOffset])
	code.WriteRune('\n')

	if len(temps) > 0 {
		code.WriteString("func init() {\n")
		for _, temp := range temps {
			fmt.Fprintf(code, "\t%s = %s\n", temp.name, temp.temp)
		}
		code.WriteString("}\n")
	}
	pkg, err := in.finalizePackage(pkg, code)
	if err != nil {
		return err
	}
	for _, name := range defined {
		in.scope[name] = pkg.Scope().Lookup(name)
	}
	return nil
}

// countExprValues returns the number of values returned by the expression
// contained within the specified buffer.
func (in *Interpreter) countExprValues(buf *interputil.Buffer, pkg *types.Package) (int, error) {
	scope := pkg.Scope()
	for _, imppkg := range in.imports {
		name := importAlias(imppkg)
		if name == "" {
			name = imppkg.Name()
		}
		obj := types.NewPkgName(token.NoPos, pkg, name, imppkg)
		scope.Insert(obj)
	}
	var tv types.TypeAndValue
	tv, err := types.Eval(buf.String(), pkg, scope)
	if err != nil {
		return -1, fmt.Errorf("evaluating expression: %v", err)
	}
	if tv.IsVoid() {
		return 0, nil
	}
	if tuple, ok := tv.Type.(*types.Tuple); ok {
		return tuple.Len(), nil
	}
	return 1, nil
}

func (in *Interpreter) preparePackage() (*types.Package, *bytes.Buffer) {
	pkgpath := fmt.Sprintf("input%05d", in.pkgnum)
	in.pkgnum++

	pkg := types.NewPackage(pkgpath, "llgoi")
	in.augmentPackageScope(pkg)

	return pkg, in.preparePackageCode()
}

func (in *Interpreter) preparePackageCode() *bytes.Buffer {
	var code bytes.Buffer
	fmt.Fprintln(&code, "package llgoi")

	for _, pkg := range in.imports {
		alias := importAlias(pkg)
		fmt.Fprintf(&code, "import %s %q\n", alias, pkg.Path())
	}

	return &code
}

func (in *Interpreter) finalizePackage(pkg *types.Package, code *bytes.Buffer) (*types.Package, error) {
	copts := in.copts
	copts.PackageCreated = in.augmentPackageScope
	copts.DisableUnusedImportCheck = true
	pkg, err := in.loadSourcePackageFromCode(code.String(), pkg.Path(), copts)
	if err != nil {
		return nil, err
	}
	return pkg, nil
}

// importAlias returns an alias to use for a package import,
// or the empty string of the import does not need to be
// aliased.
func importAlias(pkg *types.Package) string {
	if pkg.Name() == "llgoi" {
		// We call all llgoi-compiled packages
		// "llgoi", so we must alias them by their
		// unique package paths.
		return pkg.Path()
	}
	return ""
}
