// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// secondOf returns its second argument.  It keeps no list, so it may carry
// the DropsArgList mark.
func secondOf(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return args.Cells[1]
}

// callAllocs returns the allocations of one evaluation of (head args...) in
// a warm VM, after it checks the result.
func callAllocs(t *testing.T, env *lisp.LEnv, want string, head string, args ...*lisp.LVal) float64 {
	t.Helper()
	call := lisp.SExpr(append([]*lisp.LVal{lisp.Symbol(head)}, args...))
	require.Equal(t, want, env.Eval(call).String(), head)
	return testing.AllocsPerRun(200, func() { env.Eval(call) })
}

// A builtin or special operator registered through DropsArgList reuses its
// list by every registration route, which saves the one allocation of the
// list.  The same function without the mark allocates it.
func TestDropsArgListReusesTheList(t *testing.T) {
	marked := func(name string) lisp.LBuiltinDef {
		return lisp.DropsArgList(elpsutil.Function(name, lisp.Formals("a", "b"), secondOf))
	}
	env := newCallSemanticsEnv(t)
	env.AddBuiltins(false, marked("add-marked"), elpsutil.Function("unmarked", lisp.Formals("a", "b"), secondOf))
	require.True(t, env.BindBuiltins(lisp.BindOpts{}, marked("bind-marked")).IsNil())
	reg := env.RegisterBuiltins(marked("reg-marked"))
	require.NotEqual(t, lisp.LError, env.PutGlobal(lisp.Symbol("reg-marked"), reg[0]).Type)
	env.AddSpecialOps(false, marked("op-marked"), elpsutil.Function("op-unmarked", lisp.Formals("a", "b"), secondOf))

	one, two := lisp.Int(1), lisp.Int(2)
	unmarked := callAllocs(t, env, "2", "unmarked", one, two)
	for _, name := range []string{"add-marked", "bind-marked", "reg-marked"} {
		assert.InDelta(t, unmarked-1, callAllocs(t, env, "2", name, one, two), 0, "%s did not reuse its list", name)
	}
	opUnmarked := callAllocs(t, env, "2", "op-unmarked", one, two)
	assert.InDelta(t, opUnmarked-1, callAllocs(t, env, "2", "op-marked", one, two), 0, "op-marked did not reuse its list")
}

// The mark changes no result: values, errors, arity errors and nested calls
// read the same with and without it.
func TestDropsArgListKeepsResults(t *testing.T) {
	env := newCallSemanticsEnv(t)
	env.AddBuiltins(false,
		lisp.DropsArgList(elpsutil.Function("m", lisp.Formals("a", "b"), secondOf)),
		elpsutil.Function("u", lisp.Formals("a", "b"), secondOf))
	for _, src := range []string{
		"(m 1 (m 2 3))",
		"(m (m 1 2) (m 3 (m 4 5)))",
		"(m 1)",
		"(m 1 (error 'boom \"x\"))",
		"(let ((x 0)) (dotimes (i 50) (set! x (m x i))) x)",
		"(map 'list (lambda (x) (m x (m x x))) '(1 2 3))",
	} {
		env.Runtime.ResetSteps()
		got := env.LoadString("t.lisp", src)
		gotSteps := env.Runtime.Steps()
		env.Runtime.ResetSteps()
		want := env.LoadString("t.lisp", strings.ReplaceAll(src, "(m ", "(u "))
		assert.Equal(t, want.Type, got.Type, src)
		assert.Equal(t, strings.ReplaceAll(want.String(), " u: ", " m: "), got.String(), src)
		assert.Equal(t, env.Runtime.Steps(), gotSteps, "steps of %s", src)
	}
}

// DropsArgList and FreeKeywords compose in either order: the keyword
// literals stay free, the list is reused and the docstring reaches the
// function value.
func TestDropsArgListComposesWithFreeKeywords(t *testing.T) {
	for _, order := range []string{"drops-of-free", "free-of-drops"} {
		t.Run(order, func(t *testing.T) {
			def := elpsutil.FunctionDoc("fk", lisp.Formals("x", lisp.KeyArgSymbol, "a", "b"), keyEcho, "free and dropped")
			var wrapped lisp.LBuiltinDef
			if order == "drops-of-free" {
				wrapped = lisp.DropsArgList(lisp.FreeKeywords(def))
			} else {
				wrapped = lisp.FreeKeywords(lisp.DropsArgList(def))
			}
			env := keywordStepsEnv(t)
			env.AddBuiltins(false, wrapped, elpsutil.Function("uk", lisp.Formals("x", lisp.KeyArgSymbol, "a", "b"), keyEcho))
			assert.Equal(t, measureSteps(env, "(uk 1 :a 2 :b 3)")-2, measureSteps(env, "(fk 1 :a 2 :b 3)"), "the keyword literals were charged")

			args := []*lisp.LVal{lisp.Int(1), lisp.Symbol(":a"), lisp.Int(2), lisp.Symbol(":b"), lisp.Int(3)}
			unmarked := callAllocs(t, env, "'(1 2 3)", "uk", args...)
			assert.InDelta(t, unmarked-1, callAllocs(t, env, "'(1 2 3)", "fk", args...), 0, "the list was not reused")

			fk := env.LoadString("t.lisp", "fk")
			assert.Equal(t, "free and dropped", fk.Docstring())
		})
	}
}
