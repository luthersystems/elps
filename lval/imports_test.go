// Copyright © 2026 The ELPS authors

package lval_test

import (
	"go/parser"
	"go/token"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// TestImportsNoInterpreter: the package must build where package lisp
// cannot be linked, so its only import from this module is elpstag, and it
// does not use encoding/json.
func TestImportsNoInterpreter(t *testing.T) {
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, name := range files {
		if strings.HasSuffix(name, "_test.go") {
			continue
		}
		f, err := parser.ParseFile(token.NewFileSet(), name, nil, parser.ImportsOnly)
		if err != nil {
			t.Fatal(err)
		}
		for _, imp := range f.Imports {
			path, _ := strconv.Unquote(imp.Path.Value)
			if path == "encoding/json" || strings.Contains(path, ".") && path != "github.com/luthersystems/elps/elpstag" {
				t.Errorf("%s imports %s", name, path)
			}
		}
	}
}
