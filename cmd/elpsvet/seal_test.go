// Copyright © 2026 The ELPS authors

package main

import (
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

// TestSealAnalyzer runs the rule over both sides of the package boundary.
// Package "seal" is the out-of-package position (libelpspath, analysis), where
// `sealed` is unexported and IsSealed()-plus-copy is the only fix; the stub
// lisp package is the in-package position, where propagating the flag is the
// kernel's idiom.  Both must be covered: the two shipped defects were one of
// each.
func TestSealAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), sealAnalyzer,
		"seal", "github.com/luthersystems/elps/lisp")
}
