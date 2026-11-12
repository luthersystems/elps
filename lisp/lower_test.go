// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// macroexpand-all :lower true expands get-default as the macro of elps
// v1.96.0 did, in code positions only, and the result evaluates as
// get-default does.
func TestMacroExpandAllLower(t *testing.T) {
	const gd = `(lisp:let ((map@1@1 m) (key@1@2 "k")) (lisp:if (lisp:if (lisp:nil? map@1@1) lisp:false (lisp:key? map@1@1 key@1@2)) (lisp:get map@1@1 key@1@2) 0))`
	tests := elpstest.TestSuite{
		{"lower get-default", elpstest.TestSequence{
			{`(macroexpand-all '(get-default m "k" 0) :lower true)`, `'` + gd, ""},
			{`(macroexpand-all '(lisp:get-default m "k" 0) :lower true)`, `'` + gd, ""},
		}},
		{"no lowering without :lower", elpstest.TestSequence{
			{`(macroexpand-all '(get-default m "k" 0))`, `'(get-default m "k" 0)`, ""},
			{`(macroexpand-all '(get-default m "k" 0) :lower false)`, `'(get-default m "k" 0)`, ""},
		}},
		{"quoted data is not lowered", elpstest.TestSequence{
			{`(macroexpand-all '(list (quote (get-default m "k" 0))) :lower true)`, `'(list (quote (get-default m "k" 0)))`, ""},
		}},
		{"a local function shadows get-default", elpstest.TestSequence{
			{`(macroexpand-all '(flet ([get-default (a b c) a]) (get-default 1 2 3)) :lower true)`, `'(flet ('(get-default (a b c) a)) (get-default 1 2 3))`, ""},
		}},
		{"wrong argument count", elpstest.TestSequence{
			{`(macroexpand-all '(get-default m "k") :lower true)`, "test:1:1: lisp:get-default: invalid number of arguments: 2", ""},
		}},
		{"lowered code evaluates as get-default", elpstest.TestSequence{
			{`(set 'm (sorted-map "k" 1))`, `(sorted-map "k" 1)`, ""},
			{`(eval (macroexpand-all '(list (get-default m "k" 0) (get-default m "z" 2) (get-default () "k" 3) (get-default m "z" (get-default m "k" 4))) :lower true))`, `'(1 2 3 1)`, ""},
			{`(list (get-default m "k" 0) (get-default m "z" 2) (get-default () "k" 3) (get-default m "z" (get-default m "k" 4)))`, `'(1 2 3 1)`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// HasLowering takes the name with or without the lisp: prefix.
func TestHasLowering(t *testing.T) {
	require.True(t, lisp.HasLowering("get-default"))
	require.True(t, lisp.HasLowering("lisp:get-default"))
	require.False(t, lisp.HasLowering("if"))
	require.False(t, lisp.HasLowering("other:get-default"))
}

// An error in lowered code points at the get-default call, as the old
// macro's expansion did, and not at an argument or an unknown position.
func TestLoweredErrorLocation(t *testing.T) {
	env := newGenSymsEnv(t)
	got := env.LoadString("t.lisp", "(set 'x 5)\n(eval (macroexpand-all '(get-default x \"k\" 0) :lower true))")
	require.Equal(t, lisp.LError, got.Type)
	// Column 24 is the get-default form inside the quote.
	src, ok := got.Source()
	require.True(t, ok)
	require.Equal(t, "t.lisp:2:24", src.String())
	for _, f := range got.CallStack().Frames {
		require.NotNil(t, f.Source, "frame %s has no location", f.Name)
	}
}

// With KeepGoing, a get-default the lowering refuses (wrong argument count)
// stays the special operator, as it is without Lower, and is not walked as
// a call to a function named get-default.
func TestLowerKeepGoingKeepsMalformedOperator(t *testing.T) {
	form := lisp.SExpr([]*lisp.LVal{lisp.Symbol("get-default"), lisp.Symbol("m"), lisp.String("k")})
	for _, lower := range []bool{false, true} {
		var ops []string
		w := &lisp.CodeWalker{KeepGoing: true, Lower: lower, Visit: func(n *lisp.WalkNode) bool {
			if n.Event == lisp.WalkForm && n.Node == form {
				ops = append(ops, n.Op)
			}
			return true
		}}
		out := w.Walk(form)
		require.False(t, out.IsError(), "lower=%v: %v", lower, out)
		require.Equal(t, []string{"get-default"}, ops, "lower=%v", lower)
	}
}
