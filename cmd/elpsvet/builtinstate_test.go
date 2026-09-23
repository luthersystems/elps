// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestBuiltinStateAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), builtinStateAnalyzer, "builtinstate")
}

func TestJustifiedSharedAllow(t *testing.T) {
	for text, want := range map[string]bool{
		"//elpsvet:allow-shared guarded by the mutex":  true,
		"//elpsvet:allow-shared too short":             false,
		"//elpsvet:allow-shared":                       false,
		"//elpsvet:allow-sharedx guarded by the mutex": false,
		"//elpsvet:allow guarded by the mutex":         false,
		"//elpsvet:allow-native guarded by the mutex":  false,
	} {
		if got := justifiedSharedAllow(text); got != want {
			t.Errorf("justifiedSharedAllow(%q) = %v, want %v", text, got, want)
		}
	}
	// The ownership rule must not read allow-shared as its own marker.
	if commentAllowsOwnership("//elpsvet:allow-shared guarded by the mutex") {
		t.Error("ownership marker matched allow-shared")
	}
}

func commentAllowsOwnership(text string) bool {
	return allowed(&ast.CommentGroup{List: []*ast.Comment{{Text: text}}})
}
