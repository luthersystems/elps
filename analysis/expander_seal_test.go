// Copyright © 2026 The ELPS authors

package analysis

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestExpandMacroDoesNotAliasSealedForm pins the seal-laundering fix in
// EnvMacroExpander.ExpandMacro, found by elpsvet's elpsseal rule.
//
// ExpandMacro used to build the macro's argument list as
// lisp.SExpr(form.Cells[1:]) — a fresh, UNSEALED header over the parse tree's
// own backing array. Macro arguments are not evaluated, so that array reaches
// the macro's &rest binding directly, and any in-place mutator the macro body
// calls (stable-sort here; append! and the other kernel mutators are the same
// shape) writes straight through it into the program the analyzer is reading.
// The copy-on-write guards in lisp/seal.go cannot help: they check the
// header's sealed flag, and the point of the defect is that the new header
// does not have one.
//
// lisp/builtins.go macroExpandArgs closes exactly this hole on the
// (macroexpand '(m 1 2 3)) path, and says so in its doc comment. The
// analyzer's expander was the other call path that reached a macro without
// copying — same defect, different package, invisible to everything until the
// rule looked at both.
//
// Red-proofed against the pre-fix expander: the assertion below reports the
// literal's cells reordered to [1 2 3] where the source says (3 1 2).
func TestExpandMacroDoesNotAliasSealedForm(t *testing.T) {
	env := newTestEnv(t)
	evalSource(t, env, `
(defmacro sort-my-args (&rest body)
  (stable-sort < body)
  (quasiquote 0))`)

	// Parse the call form the way the analyzer does — rdparser seals every
	// completed top-level expression, so this is genuinely shared-parse data.
	src := "(sort-my-args 3 1 2)"
	s := token.NewScanner("seal_test.lisp", strings.NewReader(src))
	exprs, err := rdparser.New(s).ParseProgram()
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	form := exprs[0]
	require.True(t, form.IsSealed(), "precondition: parser output must be sealed")
	require.Len(t, form.Cells, 4)

	before := []int{form.Cells[1].Int, form.Cells[2].Int, form.Cells[3].Int}
	require.Equal(t, []int{3, 1, 2}, before, "precondition: source order")

	expander := &EnvMacroExpander{Env: env}
	_ = expander.ExpandMacro(form, lisp.DefaultUserPackage)

	after := []int{form.Cells[1].Int, form.Cells[2].Int, form.Cells[3].Int}
	assert.Equal(t, before, after,
		"macro expansion rewrote the SEALED program literal in place —"+
			" the analyzer's own parse tree was corrupted by expanding a macro over it")
}
