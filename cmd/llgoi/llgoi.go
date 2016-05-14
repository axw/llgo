//===- llgoi.go - llgo-based Go REPL --------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is llgoi, a Go REPL based on llgo and the LLVM JIT.
//
//===----------------------------------------------------------------------===//

package main

import (
	"bytes"
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/axw/interputil"

	"llvm.org/llgo/interp"
	"llvm.org/llgo/third_party/liner"
	"llvm.org/llvm/bindings/go/llvm"
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
	prefix, err := getInstPrefix()
	if err != nil {
		panic(err)
	}
	in, err := interp.NewInterpreter(interp.Config{
		ImportPaths: []string{
			filepath.Join(prefix, "lib", "go", "llgo-"+llvmVersion()),
		},
	})
	if err != nil {
		panic(err)
	}
	defer in.Dispose()

	var buf interputil.Buffer
	linerState := liner.NewLiner()
	if liner.TerminalSupported() {
		defer fmt.Println()
	}

	for {
		prompt := "(llgo) "
		if buf.Len() > 0 {
			prompt = strings.Repeat(" ", len(prompt))
		}
		line, err := linerState.Prompt(prompt)
		if err == io.EOF {
			break
		} else if err != nil {
			panic(err)
		}
		if line == "" {
			continue
		}
		if _, err := buf.WriteString(line + "\n"); err != nil {
			panic(err)
		}
		if !buf.Ready() {
			continue
		}

		//fmt.Fprintf(os.Stderr, "%q\n", buf.String())
		results, err := in.Interpret(&buf)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
		}
		for _, result := range results {
			printResult(os.Stdout, result)
			fmt.Println()
		}
		history := formatHistory(buf.String())
		linerState.AppendHistory(history)
		buf.Reset()
	}
}
