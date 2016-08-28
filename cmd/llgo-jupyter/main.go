//===- main.go - llgo-based Go REPL ------0--------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is llgo-jupyter, a Jupyter kernel based on llgo and the LLVM JIT.
//
//===----------------------------------------------------------------------===//

package main

import (
	"bytes"
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"llvm.org/llgo/interp"
	"llvm.org/llvm/bindings/go/llvm"

	"github.com/axw/go-jupyter"
)

// /* Force exporting __morestack if it's available, so that it is
//    available to the engine when linking with libLLVM.so. */
//
// void *__morestack __attribute__((weak));
import "C"

func getInstPrefix() (string, error) {
	path, err := exec.LookPath(os.Args[0])
	if err != nil {
		return "", err
	}

	path, err = filepath.EvalSymlinks(path)
	if err != nil {
		return "", err
	}

	prefix := filepath.Join(path, "..", "..")
	return prefix, nil
}

func llvmVersion() string {
	return strings.Replace(llvm.Version, "svn", "", 1)
}

// printResult prints a value that was the result of an expression evaluated
// by the interpreter.
func printResult(w io.Writer, v interface{}) {
	// TODO the result should be formatted in Go syntax, without
	// package qualifiers for types defined within the interpreter.
	fmt.Fprintf(w, "%+v", v)
}

// formatHistory reformats the provided Go source by collapsing all lines
// and adding semicolons where required, suitable for adding to line history.
func formatHistory(input string) string {
	var buf bytes.Buffer
	var s scanner.Scanner
	fset := token.NewFileSet()
	file := fset.AddFile("", fset.Base(), len(input))
	s.Init(file, []byte(input), nil, 0)
	pos, tok, lit := s.Scan()
	for tok != token.EOF {
		if int(pos)-1 > buf.Len() {
			n := int(pos) - 1 - buf.Len()
			buf.WriteString(strings.Repeat(" ", n))
		}
		var semicolon bool
		if tok == token.SEMICOLON {
			semicolon = true
		} else if lit != "" {
			buf.WriteString(lit)
		} else {
			buf.WriteString(tok.String())
		}
		pos, tok, lit = s.Scan()
		if semicolon {
			switch tok {
			case token.RBRACE, token.RPAREN, token.EOF:
			default:
				buf.WriteRune(';')
			}
		}
	}
	return buf.String()
}

func main() {
	llvmInstallPrefix, err := getInstPrefix()
	if err != nil {
		panic(err)
	}
	connInfo, err := jupyter.ReadConnectionFile(os.Args[1])
	if err != nil {
		log.Fatalf("reading connection file: %v", err)
	}
	k := &llgoKernel{interpConfig: interp.Config{
		ImportPaths: []string{
			filepath.Join(llvmInstallPrefix, "lib", "go", "llgo-"+llvmVersion()),
		},
	}}
	if err := k.Init(); err != nil {
		panic(err)
	}
	if err := jupyter.RunKernel(k, connInfo); err != nil {
		log.Fatalf("running kernel: %v", err)
	}
}
