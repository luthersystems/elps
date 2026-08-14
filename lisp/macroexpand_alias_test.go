// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// destructiveMacroSrc defines a macro whose body mutates its &rest list in
// place.  stable-sort is the clearest of the kernel's destructive builtins --
// seqCells hands it list.Cells and sort.Stable reorders that array -- but sort
// and append! have the same shape, so this stands in for all of them.
//
// The macro returns a constant so that nothing about the RESULT can mask a
// side effect on the argument list.  That is the whole point: the expansion
// always looked fine, which is why this went unnoticed.
const destructiveMacroSrc = `
(defmacro sort-my-args (&rest body)
  (stable-sort < body)
  (quasiquote 0))`

// TestMacroExpandDoesNotAliasCallerForm pins the lisp-builtin half of elps#396.
//
// builtinMacroExpand and builtinMacroExpand1 both built the macro's argument
// list as SExpr(form.Cells[1:]).  SExpr does not copy -- it wraps the slice it
// is handed -- so that header carried the backing array of `form`, and `form`
// is the builtin's FIRST ARGUMENT, already evaluated.  When lisp code writes
// (macroexpand '(m 3 1 2)) or stores the form in a variable first, that value
// is the caller's own quoted literal: a node of the running program's parse
// tree, not a scratch copy.
//
// Macro arguments are not evaluated, so the borrowed array reached the macro's
// parameters untouched -- LEnv.bindFormalNext binds a variadic parameter to
// QExpr(args.Rest()) and argParser.Rest returns p.args[p.i:], another window
// onto the same storage -- and any in-place mutator in the macro body wrote
// straight back through into the caller's form.
//
// The assertions are on the CALLER'S FORM, never on the expansion's result.
//
// This needs no library and no analysis machinery; it is reachable from four
// lines of ordinary lisp.
func TestMacroExpandDoesNotAliasCallerForm(t *testing.T) {
	tests := elpstest.TestSuite{
		{"macroexpand does not rewrite the caller's form", elpstest.TestSequence{
			{destructiveMacroSrc, "()", ""},
			{"(set 'form '(sort-my-args 3 1 2))", "'(sort-my-args 3 1 2)", ""},
			{"(macroexpand form)", "0", ""},
			// Before the fix this read '(sort-my-args 1 2 3): asking what a
			// form expands to rewrote the form.
			{"form", "'(sort-my-args 3 1 2)", ""},
		}},
		{"macroexpand-1 does not rewrite the caller's form", elpstest.TestSequence{
			{destructiveMacroSrc, "()", ""},
			{"(set 'form '(sort-my-args 3 1 2))", "'(sort-my-args 3 1 2)", ""},
			{"(macroexpand-1 form)", "0", ""},
			{"form", "'(sort-my-args 3 1 2)", ""},
		}},
		// The invariant the fix is actually pinned to, stated as a
		// comparison rather than as a hardcoded "unchanged".
		//
		// LEnv.evalSExprCells is the runtime's normal route to a macro, and on
		// its IsSpecialFun branch it copies the caller's cells into a fresh
		// array before binding them.  So RUNNING the form was already
		// harmless, and only INSPECTING it was destructive -- the passive
		// operation was strictly worse than the active one.  Comparing the two
		// keeps this honest if macro-argument semantics are ever revised:
		// whatever eval does to a form, macroexpand must do no more.
		{"macroexpand perturbs a form no more than eval does", elpstest.TestSequence{
			{destructiveMacroSrc, "()", ""},
			{"(set 'evaled '(sort-my-args 3 1 2))", "'(sort-my-args 3 1 2)", ""},
			{"(set 'expanded '(sort-my-args 3 1 2))", "'(sort-my-args 3 1 2)", ""},
			{"(eval evaled)", "0", ""},
			{"(macroexpand expanded)", "0", ""},
			{"(equal? evaled expanded)", "true", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// TestMacroExpandDoesNotAliasParsedForm is the same defect observed on genuine
// parse-tree storage reached through the Go API, so the cells under test are
// what the reader produced rather than a hand-built slice that happens to have
// spare capacity.
//
// It also measures the eval/macroexpand asymmetry directly: the same form is
// handed to the `eval` builtin in one env and to `macroexpand` in another, and
// the two source trees are required to come out identical.
func TestMacroExpandDoesNotAliasParsedForm(t *testing.T) {
	// newEnv returns an initialized env with a reader attached, plus the
	// destructive macro already defined.
	newEnv := func(t *testing.T) *lisp.LEnv {
		t.Helper()
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
		lerr := env.LoadString("macro.lisp", destructiveMacroSrc)
		require.NoError(t, lisp.GoError(lerr))
		return env
	}

	// readForm parses one quoted form and returns the parse-tree node.
	readForm := func(t *testing.T, env *lisp.LEnv) *lisp.LVal {
		t.Helper()
		form := env.LoadString("form.lisp", `'(sort-my-args 3 1 2)`)
		require.NoError(t, lisp.GoError(form))
		require.Equal(t, lisp.LSExpr, form.Type)
		return form
	}

	// callBuiltin invokes a global builtin by name with a single argument.
	callBuiltin := func(t *testing.T, env *lisp.LEnv, name string, arg *lisp.LVal) *lisp.LVal {
		t.Helper()
		fun := env.GetGlobal(lisp.Symbol(name))
		require.NoError(t, lisp.GoError(fun))
		r := env.FunCall(fun, lisp.SExpr([]*lisp.LVal{arg}))
		require.NoError(t, lisp.GoError(r))
		return r
	}

	for _, name := range []string{"macroexpand", "macroexpand-1"} {
		t.Run(name, func(t *testing.T) {
			env := newEnv(t)
			form := readForm(t, env)
			before := form.String()
			callBuiltin(t, env, name, form)
			assert.Equal(t, before, form.String(),
				"(%s form) rewrote the caller's form in place", name)
		})
	}

	t.Run("parity with eval", func(t *testing.T) {
		evalEnv := newEnv(t)
		evaled := readForm(t, evalEnv)
		callBuiltin(t, evalEnv, "eval", evaled)

		expandEnv := newEnv(t)
		expanded := readForm(t, expandEnv)
		callBuiltin(t, expandEnv, "macroexpand", expanded)

		assert.Equal(t, evaled.String(), expanded.String(),
			"macroexpand must perturb a form no more than eval does")
	})
}
