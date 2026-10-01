// Copyright © 2026 The ELPS authors

package main

import (
	"path/filepath"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestLTypeSwitchAnalyzer(t *testing.T) {
	analysistest.Run(t, filepath.Join(analysistest.TestData(), "ltypeswitch"), lTypeSwitchAnalyzer,
		lispPkgPath, "github.com/luthersystems/elps/internal/valwalk/nested")
}

func TestLTypeSwitchScope(t *testing.T) {
	for _, tc := range []struct {
		pkg, file string
		want      bool
	}{
		{lispPkgPath, "/tmp/lisp/shape.go", true},
		{lispPkgPath, "loader.go", true},
		{lispPkgPath, "detach.go", true},
		{lispPkgPath, "copier.go", false},
		{lispPkgPath, "lisp.go", false},
		{"other", "shape.go", false},
		{"github.com/luthersystems/elps/internal/valwalk", "walk.go", true},
		{"github.com/luthersystems/elps/internal/valwalk/nested", "walk.go", true},
		{"github.com/luthersystems/elps/internal/valwalker", "walk.go", false},
	} {
		if got := inLTypeSwitchScope(tc.pkg, tc.file); got != tc.want {
			t.Errorf("scope(%q, %q) = %t, want %t", tc.pkg, tc.file, got, tc.want)
		}
	}
}
