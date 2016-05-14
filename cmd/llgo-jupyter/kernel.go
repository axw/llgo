package main

import (
	"bufio"
	"bytes"

	"github.com/axw/go-jupyter"
)

const (
	llgoJupyterVersion = "0.1.0"
)

type llgoKernel struct {
	in interp
}

func (k *llgoKernel) Info() jupyter.KernelInfo {
	return jupyter.KernelInfo{
		Implementation:        "llgo-jupyter",
		ImplementationVersion: llgoJupyterVersion,
		LanguageInfo: jupyter.LanguageInfo{
			Name:          "go",
			Version:       "1.4.2",
			MimeType:      "text/x-go",
			FileExtension: ".go",
		},
		//Banner:                llgoJupyterBanner,
		//HelpLinks:             llgoJupyterHelpLinks,
	}
}

func (k *llgoKernel) Init() error {
	if err := k.in.init(); err != nil {
		return err
	}
	return nil
}

func (k *llgoKernel) Shutdown(restart bool) error {
	k.in.dispose()
	return nil
}

func (k *llgoKernel) Execute(code string, options jupyter.ExecuteOptions) ([]interface{}, error) {
	var results []interface{}
	scanner := bufio.NewScanner(bytes.NewReader([]byte(code)))
	for scanner.Scan() {
		lineResults, err := k.in.readLine(scanner.Text() + "\n")
		if err != nil {
			return nil, err
		}
		results = append(results, lineResults...)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return results, nil
}

func (k *llgoKernel) Complete(code string, cursorPos int) (*jupyter.CompletionResult, error) {
	return &jupyter.CompletionResult{}, nil
}
