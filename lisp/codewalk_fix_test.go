// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// dag returns (f x x) doubled n times over one shared node per level.
func dag(n int) *lisp.LVal {
	x := lisp.Symbol("a")
	for range n {
		x = lisp.SExpr([]*lisp.LVal{lisp.Symbol("f"), x, x})
	}
	return x
}

// Code whose structure is shared (a DAG) is walked once per node, not once
// per path: 40 doublings would otherwise be 2^40 visits.
func TestMacroExpandAllSharedStructureIsLinear(t *testing.T) {
	for _, head := range []string{"quasiquote", "progn"} {
		env := newCowTestEnv(t)
		start := time.Now()
		form := lisp.SExpr([]*lisp.LVal{lisp.Symbol(head), dag(40)})
		r := env.MacroExpandAll(form)
		require.NotEqual(t, lisp.LError, r.Type, "%v", r)
		assert.Less(t, time.Since(start), 5*time.Second, head)
	}
}

// Walking charges steps, so a step budget bounds macroexpand-all.
func TestMacroExpandAllChargesSteps(t *testing.T) {
	env := newCowTestEnv(t)
	env.Runtime.SetStepBudget(10)
	form := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), dag(3), dag(3), dag(3), dag(3), dag(3)})
	for i := range form.Cells[1:] {
		form.Cells[i+1] = dag(4) // distinct nodes: nothing to share
	}
	require.NotEqual(t, lisp.LError, env.Put(lisp.Symbol("big"), form).Type)
	r := env.Eval(parseCached(t, "(macroexpand-all big)")[0])
	require.Equal(t, lisp.LError, r.Type)
	assert.Contains(t, r.String(), "step")
}
