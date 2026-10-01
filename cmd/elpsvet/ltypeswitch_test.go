// Copyright © 2026 The ELPS authors

package main

import (
	"path/filepath"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestLTypeSwitchAnalyzer(t *testing.T) {
	analysistest.Run(t, filepath.Join(analysistest.TestData(), "ltypeswitch"), lTypeSwitchAnalyzer,
		lispPkgPath, libjsonPkgPath)
}

func TestLTypeSwitchScope(t *testing.T) {
	for _, tc := range []struct {
		pkg, file string
		want      bool
	}{
		{lispPkgPath, "/tmp/lisp/shape.go", true},
		{lispPkgPath, "loader.go", true},
		{lispPkgPath, "detach.go", true},
		{lispPkgPath, "copier.go", true},
		{lispPkgPath, "lisp.go", true},
		{lispPkgPath, "builtins.go", false},
		{libjsonPkgPath, "encode.go", true},
		{libjsonPkgPath, "tag.go", true},
		{libjsonPkgPath, "canonize.go", true},
		{libjsonPkgPath, "untag.go", true},
		{"other", "shape.go", false},
	} {
		if _, got := lTypeSwitchFuncs(tc.pkg, tc.file); got != tc.want {
			t.Errorf("scope(%q, %q) = %t, want %t", tc.pkg, tc.file, got, tc.want)
		}
	}
	if funcs, _ := lTypeSwitchFuncs(lispPkgPath, "lisp.go"); len(funcs) != 2 {
		t.Errorf("lisp.go scope = %v, want equalShallow and equalIter only", funcs)
	}
}
