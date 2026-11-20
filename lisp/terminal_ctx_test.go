// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type terminalCtxKey struct{}

// TestTerminalMarkEvaluationContext drives a terminal mark through each frame
// that evaluates one -- specialOpCall (if, let), funCallFrame (a builtin
// function) and macroCall (a Go macro) -- and checks the evaluation context
// the terminal expression sees and the one each environment has afterwards.
//
// Why: callBuiltin used to evaluate the mark itself, under its deferred
// restore of env's context.  It now returns the mark, and each of those
// three callers evaluates it with beginTerminal and endTerminal, so that
// call's and callBuiltin's frames are not on the Go stack under the
// expression.  A caller that skips beginTerminal or endTerminal would show
// a stale context here.
func TestTerminalMarkEvaluationContext(t *testing.T) {
	env := initSafetyTestEnv(t)
	var seen []any
	env.AddBuiltins(true, &langBuiltin{
		name:    "probe-ctx",
		formals: Formals(),
		fun: func(env *LEnv, _ *LVal) *LVal {
			seen = append(seen, env.Context().Value(terminalCtxKey{}))
			return Int(len(seen))
		},
		docs: "Test builtin: records its environment's context.",
	})
	probe := func() *LVal { return SExpr([]*LVal{Symbol("probe-ctx")}) }
	env.AddBuiltins(true, &langBuiltin{
		name:    "terminal-fn",
		formals: Formals(),
		fun: func(env *LEnv, _ *LVal) *LVal {
			return env.Terminal(probe())
		},
		docs: "Test builtin: returns a terminal mark.",
	})
	env.AddMacros(true, &langBuiltin{
		name:    "terminal-mac",
		formals: Formals(),
		fun: func(env *LEnv, _ *LVal) *LVal {
			// The mark's value is the expansion, which eval then evaluates.
			return env.Terminal(SExpr([]*LVal{Symbol("lisp:quote"), probe()}))
		},
		docs: "Test macro: returns a terminal mark.",
	})
	base := context.WithValue(context.Background(), terminalCtxKey{}, "base")
	inner := context.WithValue(context.Background(), terminalCtxKey{}, "inner")
	sexpr := func(cells ...*LVal) *LVal { return SExpr(cells) }
	call := func(name string) *LVal { return sexpr(Symbol(name)) }
	for _, tc := range []struct {
		name string
		form *LVal
	}{
		{"(if true (probe-ctx) ())", sexpr(Symbol("if"), Symbol("true"), probe(), Nil())},
		{"(let ((a 1)) (probe-ctx))", sexpr(Symbol("let"), sexpr(sexpr(Symbol("a"), Int(1))), probe())},
		{"(terminal-fn)", call("terminal-fn")},
		{"(terminal-mac)", call("terminal-mac")},
		{"(progn (if true (terminal-fn) ()))", sexpr(Symbol("progn"), sexpr(Symbol("if"), Symbol("true"), call("terminal-fn"), Nil()))},
	} {
		t.Run(tc.name, func(t *testing.T) {
			seen = nil
			env.evalCtx = base
			v := env.EvalContext(inner, tc.form)
			require.Equal(t, LInt, v.Type, "%v", v)
			assert.Equal(t, []any{"inner"}, seen, "the terminal expression's context")
			assert.Equal(t, "base", env.Context().Value(terminalCtxKey{}), "env's context afterwards")
		})
	}
}
