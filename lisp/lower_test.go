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

// Each lowering names a special operator of package lisp.
func TestLoweringsNameSpecialOps(t *testing.T) {
	ops := map[string]bool{}
	for _, op := range lisp.DefaultSpecialOps() {
		ops[op.Name()] = true
	}
	require.True(t, lisp.HasLowering("get-default"))
	require.False(t, lisp.HasLowering("if"))
	for _, name := range []string{"get-default"} {
		require.True(t, ops[name], "%s has a lowering but is not a special operator", name)
	}
}
