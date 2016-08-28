//===- kernel.go - Jupyter kernel implementation for llgo  ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Jupyter kernel interface on top of the llgo
// interpreter.
//
//===----------------------------------------------------------------------===//

package main

import (
	"fmt"
	"runtime"

	"llvm.org/llgo/interp"
	"llvm.org/llgo/irgen"

	"github.com/axw/go-jupyter"
	"github.com/axw/interputil"
	"github.com/axw/interputil/gocode"
)

const (
	llgoJupyterVersion = "0.1.0"
)

type llgoKernel struct {
	interpConfig interp.Config
	buf          interputil.Buffer
	in           *interp.Interpreter
}

func (k *llgoKernel) Info() jupyter.KernelInfo {
	llgoJupyterBanner := fmt.Sprintf(`
%v

llgo-jupyter %v
`[1:], runtime.Version(), llgoJupyterVersion)

	return jupyter.KernelInfo{
		Implementation:        "llgo-jupyter",
		ImplementationVersion: llgoJupyterVersion,
		LanguageInfo: jupyter.LanguageInfo{
			Name:          "go",
			Version:       irgen.GoVersion(),
			MimeType:      "text/x-go",
			FileExtension: ".go",
		},
		Banner: llgoJupyterBanner,
		HelpLinks: []jupyter.HelpLink{{
			Text: "llgo-dev mailing list",
			URL:  "https://groups.google.com/forum/#!forum/llgo-dev",
		}, {
			Text: "LLVM website",
			URL:  "http://llvm.org",
		}},
	}
}

func (k *llgoKernel) Init() error {
	in, err := interp.NewInterpreter(k.interpConfig)
	if err != nil {
		return err
	}
	k.in = in
	return nil
}

func (k *llgoKernel) Shutdown(restart bool) error {
	k.in.Dispose()
	return nil
}

func (k *llgoKernel) Execute(code string, options jupyter.ExecuteOptions) ([]interface{}, error) {
	var allResults []interface{}
	for len(code) > 0 {
		switch n, err := k.buf.WriteString(code); err {
		case nil, interputil.ErrMultipleLines:
			code = code[n:]
		default:
			return nil, err
		}
		if k.buf.Ready() {
			results, err := k.in.Interpret(&k.buf)
			if err != nil {
				return nil, err
			}
			if err := k.buf.Reset(); err != nil {
				return nil, err
			}
			allResults = append(allResults, results...)
		}
	}
	return allResults, nil
}

func (k *llgoKernel) Complete(code string, cursorPos int) (*jupyter.CompletionResult, error) {
	completer := gocode.DefaultCompleter
	if !completer.Available() {
		// TODO(axw) log a message to inform the user that there
		// is no completion available, and how they can go about
		// enabling it (i.e. by installing gocode).
		return &jupyter.CompletionResult{}, nil
	}

	var buf interputil.Buffer
	if _, err := buf.WriteString(k.buf.String()); err != nil {
		return nil, err
	}
	switch _, err := buf.WriteString(code); err {
	case nil:
	case interputil.ErrMultipleLines:
		// TODO(axw) if the cursorPos is after the consumed code,
		// reset the buffer and feed until we're at the cursor.
		return nil, err
	default:
		return nil, err
	}

	programSource, offset, err := k.in.Program(&buf)
	if err != nil {
		return nil, err
	}
	result, err := completer.Query([]byte(programSource), offset+cursorPos)
	if err != nil {
		return nil, err
	}
	matches := make([]string, len(result.Candidates))
	for i, candidate := range result.Candidates {
		matches[i] = candidate.Name
	}
	return &jupyter.CompletionResult{
		Matches:     matches,
		CursorStart: cursorPos - result.Cursor,
		CursorEnd:   cursorPos,
	}, nil
}

func (k *llgoKernel) Completeness(code string) jupyter.Completeness {
	var buf interputil.Buffer
	for len(code) > 0 {
		if buf.Ready() {
			buf.Reset()
		}
		switch n, err := buf.WriteString(code); err {
		case nil, interputil.ErrMultipleLines:
			code = code[n:]
		default:
			return jupyter.InvalidCode
		}
	}
	if buf.Ready() {
		return jupyter.Complete
	}
	return jupyter.Incomplete
}
