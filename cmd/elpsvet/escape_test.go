// Copyright © 2026 The ELPS authors

package main

import (
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

// TestEscapeAnalyzer runs the escape-aliasing rule over two fixture
// packages: the lisp stub carries the in-package retro-catch shapes (the
// pre-fix ErrorCondition/ErrorConditionf and ErrorAssociate bodies,
// mirrored verbatim from history), and esc carries the cross-package
// shapes (SetSource, return-escape, registry stores, the cleansers and
// the annotation).
func TestEscapeAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), escapeAnalyzer,
		"github.com/luthersystems/elps/lisp", "esc")
}
