// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

// Issue #628: an opaque, sealed cache handle is not custody of mutable ASTs.
// The sibling raw value and foreign lookalike must still produce diagnostics.
func TestOwnershipAnalyzer(t *testing.T) {
	analysistest.Run(t, filepath.Join(analysistest.TestData(), "ownership"), analyzer, "consumer")
}

// Any change to the audited shape or public surface requires another review;
// merely retaining the name CachedSource must not suppress a new exposure.
func TestOwnershipCachedSourceBoundaryDrift(t *testing.T) {
	path := filepath.Join(analysistest.TestData(), "ownership", "src", filepath.FromSlash(lispPkgPath), "cache.go")
	source, err := os.ReadFile(path) // #nosec G304 -- fixed repository fixture path; no input controls the path.
	if err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		name, from, to string
	}{
		{"exported-field", "key", "KeyField"},
		{"extra-private-state", "key", "state *LVal; key"},
		{"program-state", "exprs []*LVal", "exprs []*LVal; extra *LVal"},
		{"different-program", "prog Program", "prog struct { exprs []*LVal }"},
		{"mutator", "// extra methods", "func (*CachedSource) Reset() {}"},
		{"scalar-setter", "Len() int", "Len(n int) int"},
		{"raw-accessor", "// extra methods", "func (*CachedSource) AST() *LVal { return nil }"},
		{"changed-result", "Len() int", "Len() int64"},
		{"missing-method", "String() string", "description() string"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			changed := strings.Replace(string(source), tc.from, tc.to, 1)
			if changed == string(source) {
				t.Fatal("mutation did not change fixture")
			}
			fset := token.NewFileSet()
			file, err := parser.ParseFile(fset, "cache.go", changed, 0)
			if err != nil {
				t.Fatal(err)
			}
			var conf types.Config
			pkg, err := conf.Check(lispPkgPath, fset, []*ast.File{file}, nil)
			if err != nil {
				t.Fatal(err)
			}
			if !containsLVal(types.NewPointer(pkg.Scope().Lookup("CachedSource").Type()), make(map[types.Type]bool)) {
				t.Fatal("changed cache contract silently received the opaque exemption")
			}
		})
	}
}
