// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestUsePackageHidesLisp(t *testing.T) {
	diags := lintCheck(t, AnalyzerUsePackageHidesLisp, `(in-package 'phy)
(use-package 's)
(set 'x 1)
(set! x 2)
(defun f (v) (if (not v) (list 1) (sorted-map)))`)
	assertDiagOnLine(t, diags, 2, "hides lisp names: error, list, not, sorted-map, when")
	assertDiagOnLine(t, diags, 5, "not here is s:not")
	assertDiagOnLine(t, diags, 5, "list here is s:list")
	assertDiagOnLine(t, diags, 5, "sorted-map here is s:sorted-map")
	assert.Len(t, diags, 4, "set! is not hidden: s exports set-field!")

	// Qualified calls, quoted data and quasiquote templates are not calls.
	assertNoDiags(t, lintCheck(t, AnalyzerUsePackageHidesLisp, `(in-package 'phy)
(use-package 's) ; nolint:use-package-hides-lisp
(lisp:not true)
(set 'q '(not x))
(set 'r (quasiquote (not (unquote q))))`))
	// A package that hides nothing is fine, and so is a call before the
	// use-package or in another package.
	assertNoDiags(t, lintCheck(t, AnalyzerUsePackageHidesLisp, `(in-package 'a)
(not true)
(use-package 'math)
(in-package 'b)
(not true)`))
	// in-package takes docstrings, use-package takes several packages, and
	// a progn at top level is still top level.
	diags = lintCheck(t, AnalyzerUsePackageHidesLisp, `(in-package 'p "doc")
(progn (use-package 'math 's))
(in-package 'q "doc")
(not 1)
(in-package 'p)
(not 1)`)
	assert.Len(t, diags, 2)
	assertDiagOnLine(t, diags, 2, "use-package 's hides")
	assertDiagOnLine(t, diags, 6, "not here is s:not")
	// A package named by a variable is not known; testing re-exports lisp's
	// own test, which hides nothing.
	assertNoDiags(t, lintCheck(t, AnalyzerUsePackageHidesLisp, `(use-package pkg)
(use-package 'testing)
(not 1)
(test "t" 1)`))
	// A file that defines the name itself is left to builtin-shadowing.
	diags = lintCheck(t, AnalyzerUsePackageHidesLisp, `(use-package 's)
(defun when (a b) a)
(when 1 2)`)
	assert.Len(t, diags, 1)
	assertDiagOnLine(t, diags, 1, "hides lisp names")
}
