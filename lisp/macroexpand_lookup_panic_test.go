// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// Issue #657: an ordinary failed macro lookup is a no-op, but a recovered
// host fault during that lookup must retain its diagnostic and stop expansion.
func TestMacroexpandLookupPreservesDebuggerPanic(t *testing.T) {
	for _, name := range []string{"macroexpand", "macroexpand-1"} {
		t.Run(name, func(t *testing.T) {
			env := newLimitTestEnv(t)
			debugger := &entryPanicDebugger{}
			env.Runtime.Debugger = debugger
			// Unknown packages invoke env.Errorf/OnError. Bare unbound names
			// take a different lookup path and are covered by the no-op control.
			form := lisp.QExpr([]*lisp.LVal{lisp.Symbol("missing-package:macro"), lisp.SExpr([]*lisp.LVal{lisp.Symbol("must-not-run")})})
			before := form.String()
			var got *lisp.LVal
			require.NotPanics(t, func() {
				got = env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol(name), form}))
			})
			require.True(t, lisp.IsInternalPanic(got), "%v", got)
			require.Contains(t, got.String(), "error notification fault")
			require.NotNil(t, got.CallStack())
			require.Contains(t, string(got.CallStack().GoStack), "entryPanicDebugger).OnError")
			require.Equal(t, 1, debugger.calls, "the failed observer must not be notified again")
			require.Equal(t, before, form.String(), "failed lookup must leave the source form untouched")
			require.Empty(t, env.Runtime.Stack.Frames)
		})
	}
}

func TestMacroexpandLookupMissingNamesRemainNoop(t *testing.T) {
	for _, name := range []string{"macroexpand", "macroexpand-1"} {
		for _, symbol := range []string{"missing-macro", "missing-package:macro"} {
			t.Run(name+"/"+symbol, func(t *testing.T) {
				env := newLimitTestEnv(t)
				debugger := &entryRecordingDebugger{}
				env.Runtime.Debugger = debugger
				form := lisp.QExpr([]*lisp.LVal{lisp.Symbol(symbol)})
				got := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol(name), form}))
				require.Same(t, form, got)
				require.False(t, lisp.IsInternalPanic(got))
				if symbol == "missing-package:macro" {
					require.Len(t, debugger.observed, 1)
					require.False(t, lisp.IsInternalPanic(debugger.observed[0]))
				} else {
					require.Empty(t, debugger.observed)
				}
			})
		}
	}
}

func TestMacroexpandLookupPreservesExistingPanicIdentity(t *testing.T) {
	for _, name := range []string{"macroexpand", "macroexpand-1"} {
		t.Run(name, func(t *testing.T) {
			env := newLimitTestEnv(t)
			fun := lisp.FunInPackage(lisp.DefaultUserPackage, "lookup-host-fault", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
				panic("original macro lookup fault")
			})
			require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("lookup-host-fault"), fun)))
			original := env.LoadString("original-macro-lookup-fault.lisp", `(lookup-host-fault)`)
			require.True(t, lisp.IsInternalPanic(original), "%v", original)
			stack, rendered := original.CallStack(), original.String()
			goStack := append([]byte(nil), stack.GoStack...)
			require.NoError(t, lisp.GoError(env.Put(lisp.Symbol("failed-macro"), original)))
			debugger := &entryRecordingDebugger{}
			env.Runtime.Debugger = debugger
			got := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol(name), lisp.QExpr([]*lisp.LVal{lisp.Symbol("failed-macro")})}))
			require.Same(t, original, got)
			require.True(t, lisp.IsInternalPanic(got))
			require.Same(t, stack, got.CallStack())
			require.Equal(t, goStack, got.CallStack().GoStack)
			require.Equal(t, rendered, got.String())
			require.Empty(t, debugger.observed, "propagating a host fault must not invoke error observers")
		})
	}
}
