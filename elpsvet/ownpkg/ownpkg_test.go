// Copyright © 2026 The ELPS authors

package ownpkg_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsvet/ownpkg"
	"golang.org/x/tools/go/analysis/analysistest"
)

func TestAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), ownpkg.Analyzer, "libcase", "github.com/luthersystems/elps/lisp")
}

func TestJustified(t *testing.T) {
	for text, want := range map[string]bool{
		"//elpsvet:allow-ownpkg the library's own package":  true,
		"//elpsvet:allow-ownpkg too short":                  false,
		"//elpsvet:allow-ownpkg":                            false,
		"//elpsvet:allow-ownpkgx the library's own package": false,
		"//elpsvet:allow the library's own package":         false,
		"//elpsvet:allow-native the library's own package":  false,
		"/* elpsvet:allow-ownpkg in a block comment too */": true,
	} {
		if got := ownpkg.Justified(text); got != want {
			t.Errorf("Justified(%q) = %v, want %v", text, got, want)
		}
	}
}
