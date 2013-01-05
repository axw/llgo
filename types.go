// Copyright 2012 Andrew Wilkins.
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file.

package llgo

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/types"
)

// XXX the below function is a clone of the one from llgo/types.
// this is used to check whether a "function call" is in fact a
// type conversion.

// isType checks if an expression is a type.
func isType(x ast.Expr) bool {
	switch t := x.(type) {
	case *ast.Ident:
		return t.Obj != nil && t.Obj.Kind == ast.Typ
	case *ast.ParenExpr:
		return isType(t.X)
	case *ast.SelectorExpr:
		// qualified identifier
		if ident, ok := t.X.(*ast.Ident); ok {
			if obj := ident.Obj; obj != nil {
				if obj.Kind != ast.Pkg {
					return false
				}
				pkgscope := obj.Data.(*ast.Scope)
				obj := pkgscope.Lookup(t.Sel.Name)
				return obj != nil && obj.Kind == ast.Typ
			}
		}
		return false
	case *ast.StarExpr:
		return isType(t.X)
	case *ast.ArrayType,
		*ast.StructType,
		*ast.FuncType,
		*ast.InterfaceType,
		*ast.MapType,
		*ast.ChanType:
		return true
	}
	return false
}

func underlyingType(typ types.Type) types.Type {
	if typ, ok := typ.(*types.NamedType); ok {
		return typ.Underlying
	}
	return typ
}

func derefType(typ types.Type) types.Type {
	if typ, ok := underlyingType(typ).(*types.Pointer); ok {
		return typ.Base
	}
	return typ
}

func (c *compiler) convertUntyped(from ast.Expr, to interface{}) bool {
	frominfo := c.types.expr[from]
	if isUntyped(frominfo.Type) {
		switch to := to.(type) {
		case types.Type:
			frominfo.Type = to
			c.types.expr[from] = frominfo
		case *ast.Ident:
			frominfo.Type = to.Obj.Type.(types.Type)
			c.types.expr[from] = frominfo
		case ast.Expr:
			toinfo := c.types.expr[to]
			frominfo.Type = toinfo.Type
			c.types.expr[from] = frominfo
		default:
			panic(fmt.Errorf("unexpected type: %T", to))
		}
		return true
	}
	return false
}

func (c *compiler) exportBuiltinRuntimeTypes() {
	kinds := [...]types.BasicKind{
		types.Uint,
		types.Uint8,
		types.Uint16,
		types.Uint32,
		types.Uint64,
		types.Int,
		types.Int8,
		types.Int16,
		types.Int32,
		types.Int64,
		types.Float32,
		types.Float64,
		types.Complex64,
		types.Complex128,
		types.Bool,
		types.Uintptr,
		types.UnsafePointer,
		types.String,
	}
	for _, kind := range kinds {
		typ := types.Typ[kind]
		c.types.ToRuntime(typ)
	}

	// error
	errorObj := types.Universe.Lookup("error")
	c.types.ToRuntime(errorObj.Type.(types.Type))
}

func fieldIndex(s *types.Struct, name string) int {
	for i, f := range s.Fields {
		if f.Name == name {
			return i
		}
	}
	return -1
}

type TypeStringer struct {
	// pkgmap maps AST objects to the package they
	// were declared within.
	pkgmap map[*ast.Object]string
}

func NewTypeStringer(pkgmap map[*ast.Object]string) *TypeStringer {
	return &TypeStringer{pkgmap: pkgmap}
}

// typeString returns a string representation for typ.
// below code based on go/types' typeString and friends.
func (ts *TypeStringer) TypeString(typ types.Type) string {
	var buf bytes.Buffer
	ts.writeType(&buf, typ)
	return buf.String()
}

func (ts *TypeStringer) writeParams(buf *bytes.Buffer, params []*types.Var, isVariadic bool) {
	buf.WriteByte('(')
	for i, par := range params {
		if i > 0 {
			buf.WriteString(", ")
		}
		if isVariadic && i == len(params)-1 {
			buf.WriteString("...")
		}
		ts.writeType(buf, par.Type)
	}
	buf.WriteByte(')')
}

func (ts *TypeStringer) writeSignature(buf *bytes.Buffer, sig *types.Signature) {
	if sig.Recv != nil {
		ts.writeType(buf, sig.Recv.Type)
		buf.WriteByte(' ')
	}

	ts.writeParams(buf, sig.Params, sig.IsVariadic)
	if len(sig.Results) == 0 {
		// no result
		return
	}

	buf.WriteByte(' ')
	if len(sig.Results) == 1 {
		// single unnamed result
		ts.writeType(buf, sig.Results[0].Type)
		return
	}

	// multiple or named result(s)
	ts.writeParams(buf, sig.Results, false)
}

func (ts *TypeStringer) writeType(buf *bytes.Buffer, typ types.Type) {
	switch t := typ.(type) {
	case nil:
		buf.WriteString("<nil>")

	case *types.Basic:
		// De-alias.
		t = types.Typ[t.Kind]
		buf.WriteString(t.Name)

	case *types.Array:
		fmt.Fprintf(buf, "[%d]", t.Len)
		ts.writeType(buf, t.Elt)

	case *types.Slice:
		buf.WriteString("[]")
		ts.writeType(buf, t.Elt)

	case *types.Struct:
		buf.WriteString("struct{")
		for i, f := range t.Fields {
			if i > 0 {
				buf.WriteString("; ")
			}
			if !f.IsAnonymous {
				buf.WriteString(f.Name)
				buf.WriteByte(' ')
			}
			ts.writeType(buf, f.Type)
			if f.Tag != "" {
				fmt.Fprintf(buf, " %q", f.Tag)
			}
		}
		buf.WriteByte('}')

	case *types.Pointer:
		buf.WriteByte('*')
		ts.writeType(buf, t.Base)

	case *types.Result:
		ts.writeParams(buf, t.Values, false)

	case *types.Signature:
		buf.WriteString("func")
		ts.writeSignature(buf, t)

	case *types.Interface:
		buf.WriteString("interface{")
		for i, m := range t.Methods {
			if i > 0 {
				buf.WriteString("; ")
			}
			buf.WriteString(m.Name)
			ts.writeSignature(buf, m.Type)
		}
		buf.WriteByte('}')

	case *types.Map:
		buf.WriteString("map[")
		ts.writeType(buf, t.Key)
		buf.WriteByte(']')
		ts.writeType(buf, t.Elt)

	case *types.Chan:
		var s string
		switch t.Dir {
		case ast.SEND:
			s = "chan<- "
		case ast.RECV:
			s = "<-chan "
		default:
			s = "chan "
		}
		buf.WriteString(s)
		ts.writeType(buf, t.Elt)

	case *types.NamedType:
		if pkgname, ok := ts.pkgmap[t.Obj]; ok {
			buf.WriteString(pkgname)
			buf.WriteByte('.')
		}
		buf.WriteString(t.Obj.Name)

	default:
		fmt.Fprintf(buf, "<type %T>", t)
	}
}
