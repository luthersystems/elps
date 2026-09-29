// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"
	"time"

	"github.com/luthersystems/elps/elpstest"

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

func TestMacroExpandAllMacroletScope(t *testing.T) {
	tests := elpstest.TestSuite{
		{"macrolet lexical scope", elpstest.TestSequence{
			{`(set 'x 99)`, "99", ""},
			// At run time m sees the local x (1).  Expanding ahead of time
			// cannot know it, and must not substitute the global 99.
			{`(let ((x 1)) (macrolet ((m () x)) (m)))`, "1", ""},
			{`(ignore-errors (macroexpand-all '(let ((x 1)) (macrolet ((m () x)) (m)))))`, "()", ""},
			// A local macro may use an enclosing one.
			{`(macroexpand-all '(macrolet ((a () 1)) (macrolet ((b () (a))) (b))))`,
				`'(macrolet ((a () 1)) (macrolet ((b () 1)) 1))`, ""},
			// Siblings do not see each other: b's (a) is the global call.
			{`(defmacro a () 7)`, "()", ""},
			{`(macroexpand-all '(macrolet ((a () 1) (b () (a))) (b)))`,
				`'(macrolet ((a () 1) (b () 7)) 7)`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// Expanding a macro inside an expr pattern must not change the arity expr
// inferred from it.
func TestMacroExpandAllExprKeepsArity(t *testing.T) {
	tests := elpstest.TestSuite{
		{"expr arity", elpstest.TestSequence{
			{`(defmacro first-of (a b) (quasiquote (list (unquote a))))`, "()", ""},
			{`(funcall (expr (first-of %1 %2)) 1 2)`, "'(1)", ""},
			{`(macroexpand-all '(expr (first-of %1 %2)))`, "'(lisp:lambda (%1 %2) (list %1))", ""},
			{`(funcall (eval (macroexpand-all '(expr (first-of %1 %2)))) 1 2)`, "'(1)", ""},
			// Unchanged patterns stay expr.
			{`(macroexpand-all '(expr (+ % 1)))`, "'(expr (+ % 1))", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

type hostOp struct{}

func (hostOp) Name() string                               { return "hop" }
func (hostOp) Formals() *lisp.LVal                        { return lisp.Formals(lisp.VarArgSymbol, "args") }
func (hostOp) Eval(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return lisp.Nil() }

// A special operator a host package registers has no known shape: the
// walker must not treat its arguments as code, whatever it is named.
func TestMacroExpandAllHostSpecialOp(t *testing.T) {
	env := newCowTestEnv(t)
	evalOK := func(src string) *lisp.LVal {
		t.Helper()
		var r *lisp.LVal
		for _, e := range parseCached(t, src) {
			r = env.Eval(e)
			require.NotEqual(t, lisp.LError, r.Type, "%v", r)
		}
		return r
	}
	evalOK(`(defmacro m (x) (quasiquote (list (unquote x))))`)
	env.Runtime.Registry.DefinePackage("host")
	require.NotEqual(t, lisp.LError, env.InPackage(lisp.Symbol("host")).Type)
	env.AddSpecialOps(true, hostOp{})
	require.NotEqual(t, lisp.LError, env.InPackage(lisp.Symbol(lisp.DefaultUserPackage)).Type)
	evalOK(`(use-package 'host)`)
	assert.Equal(t, "'(hop (m 1))", evalOK(`(macroexpand-all '(hop (m 1)))`).String())
	assert.Equal(t, "'(host:hop (m 1))", evalOK(`(macroexpand-all '(host:hop (m 1)))`).String())
}

// A qualified symbol resolves in its package, never in a lexical scope, as
// LEnv.Get resolves it: a local binding spelled the same way does not
// shadow it.
func TestCodeWalkerQualifiedHeadIgnoresLocals(t *testing.T) {
	form := parseCached(t, `(let ((user:m 1)) (user:m 2))`)[0]
	var bound []bool
	w := &lisp.CodeWalker{
		Expand1: func(f *lisp.LVal) (*lisp.LVal, bool) {
			if f.Cells[0].Str != "user:m" {
				return nil, false
			}
			return lisp.SExpr([]*lisp.LVal{lisp.Symbol("list"), f.Cells[1]}), true
		},
		Visit: func(n *lisp.WalkNode) bool {
			if n.Event == lisp.WalkRef && n.Node.Str == "user:m" {
				bound = append(bound, n.Bound)
			}
			return true
		},
	}
	assert.Equal(t, "(let ((user:m 1)) (list 2))", w.Walk(form).String())
	assert.Empty(t, bound)
}
