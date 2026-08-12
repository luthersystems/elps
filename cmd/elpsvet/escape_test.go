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
//
// esc IMPORTS the lisp stub, so this also exercises the freshLocation fact
// across a package boundary.  analysistest checks facts as strictly as it
// checks diagnostics: a `// want Name:"freshLocation"` on a declaration
// asserts the fact was recorded, and a declaration WITHOUT one asserts it
// was not.  Every function the fixtures expect to earn no summary — the
// by-reference accessors, the one-leaking-return accessor, the recursion
// cycle, the bare-return named result, the loop-rebound local — is pinned
// by that absence.
func TestEscapeAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), escapeAnalyzer,
		"github.com/luthersystems/elps/lisp", "esc")
}
