// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// Issue #657: embedding entry points must contain host faults even when a
// native body never passes through the expression evaluator's recovery.
func TestDirectCallContainsHostPanic(t *testing.T) {
	for _, method := range []string{"FunCall", "FunCallContext", "EvalSExpr", "MacroCall", "SpecialOpCall", "New"} {
		t.Run(method, func(t *testing.T) {
			env := newLimitTestEnv(t)
			calls := 0
			body := func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
				calls++
				panic("direct native fault")
			}
			fun := lisp.FunInPackage(lisp.DefaultUserPackage, "direct-fault", lisp.Formals(), body)
			if method == "MacroCall" {
				fun = lisp.MacroInPackage(lisp.DefaultUserPackage, "direct-fault", lisp.Formals(), body)
			} else if method == "SpecialOpCall" {
				fun = lisp.SpecialOpInPackage(lisp.DefaultUserPackage, "direct-fault", lisp.Formals(), body)
			}
			var result *lisp.LVal
			require.NotPanics(t, func() {
				switch method {
				case "FunCall":
					result = env.FunCall(fun, lisp.SExpr(nil))
				case "FunCallContext":
					result = env.FunCallContext(context.Background(), fun, lisp.SExpr(nil))
				case "EvalSExpr":
					result = env.EvalSExpr(lisp.SExpr([]*lisp.LVal{fun}))
				case "MacroCall":
					result = env.MacroCall(fun, lisp.SExpr(nil))
				case "SpecialOpCall":
					result = env.SpecialOpCall(fun, lisp.SExpr(nil))
				case "New":
					typ := env.New(env.Get(lisp.Symbol("lisp:typedef")), lisp.SExpr([]*lisp.LVal{lisp.Symbol("user:box"), fun}))
					require.NotEqual(t, lisp.LError, typ.Type, "%v", typ)
					result = env.New(typ, lisp.SExpr(nil))
				}
			})
			require.Equal(t, 1, calls)
			require.NotNil(t, result)
			require.Equal(t, lisp.LError, result.Type)
			require.True(t, lisp.IsInternalPanic(result), "%v", result)
			require.Contains(t, result.String(), "direct native fault")
			require.NotEmpty(t, result.CallStack().GoStack)
			require.Empty(t, env.Runtime.Stack.Frames, "the failed call must unwind its stack")
			next := env.LoadString("after-direct-fault.lisp", `(+ 1 2)`)
			require.Equal(t, lisp.LInt, next.Type, "%v", next)
			require.Equal(t, 3, next.Int)
		})
	}
}

type entryPanicProfiler struct{ onReturn bool }

func (p entryPanicProfiler) Start(*lisp.LVal) func() {
	if !p.onReturn {
		panic("profiler entry fault")
	}
	return func() { panic("profiler return fault") }
}

func TestDirectCallContainsProfilerPanic(t *testing.T) {
	for _, onReturn := range []bool{false, true} {
		t.Run(map[bool]string{false: "entry", true: "return"}[onReturn], func(t *testing.T) {
			env := newLimitTestEnv(t)
			calls := 0
			fun := lisp.FunInPackage(lisp.DefaultUserPackage, "profiled", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
				calls++
				return lisp.Int(7)
			})
			env.Runtime.Profiler = entryPanicProfiler{onReturn: onReturn}
			var result *lisp.LVal
			require.NotPanics(t, func() { result = env.FunCallContext(context.Background(), fun, lisp.SExpr(nil)) })
			require.True(t, lisp.IsInternalPanic(result), "%v", result)
			require.Contains(t, result.String(), "profiler ")
			require.Empty(t, env.Runtime.Stack.Frames)
			if onReturn {
				require.Equal(t, 1, calls)
			} else {
				require.Zero(t, calls)
			}
			env.Runtime.Profiler = nil
			next := env.LoadString("after-profiler-fault.lisp", `(+ 1 2)`)
			require.Equal(t, lisp.LInt, next.Type, "%v", next)
			require.Equal(t, 3, next.Int)
		})
	}
}

type entryPanicDebugger struct {
	dormantDebugger
	calls int
}

func (*entryPanicDebugger) IsEnabled() bool { return true }
func (d *entryPanicDebugger) OnError(*lisp.LEnv, *lisp.LVal) bool {
	d.calls++
	panic("error notification fault")
}

func TestPanicRecoveryDoesNotReenterFailingDebugger(t *testing.T) {
	env := newLimitTestEnv(t)
	debugger := &entryPanicDebugger{}
	env.Runtime.Debugger = debugger
	var result *lisp.LVal
	require.NotPanics(t, func() { result = env.LoadString("error-hook.lisp", `(error 'example)`) })
	require.True(t, lisp.IsInternalPanic(result), "%v", result)
	require.Contains(t, result.String(), "error notification fault")
	require.Equal(t, 1, debugger.calls, "panic recovery must not invoke the failed hook again")
	require.Empty(t, env.Runtime.Stack.Frames)
}

type entryRecordingDebugger struct {
	dormantDebugger
	pause    bool
	observed []*lisp.LVal
	waited   []*lisp.LVal
}

func (*entryRecordingDebugger) IsEnabled() bool { return true }
func (d *entryRecordingDebugger) OnError(_ *lisp.LEnv, lerr *lisp.LVal) bool {
	d.observed = append(d.observed, lerr)
	return d.pause
}
func (d *entryRecordingDebugger) WaitIfPaused(_ *lisp.LEnv, lerr *lisp.LVal) lisp.DebugAction {
	d.waited = append(d.waited, lerr)
	return lisp.DebugContinue
}

func TestPanicRecoveryPreservesHealthyErrorNotifications(t *testing.T) {
	for _, pause := range []bool{false, true} {
		for _, kind := range []string{"data", "Go error", "formatted"} {
			t.Run(fmt.Sprintf("%s/pause=%t", kind, pause), func(t *testing.T) {
				env := newLimitTestEnv(t)
				debugger := &entryRecordingDebugger{pause: pause}
				env.Runtime.Debugger = debugger
				var result *lisp.LVal
				switch kind {
				case "data":
					result = env.ErrorCondition("ordinary", lisp.Int(7))
				case "Go error":
					result = env.ErrorCondition("ordinary", errors.New("original message"))
				case "formatted":
					result = env.ErrorConditionf("ordinary", "value %d", 7)
				}
				require.Equal(t, lisp.LError, result.Type)
				require.Equal(t, "ordinary", result.Str)
				require.False(t, lisp.IsInternalPanic(result))
				require.Len(t, debugger.observed, 1)
				require.Same(t, result, debugger.observed[0])
				if pause {
					require.Len(t, debugger.waited, 1)
					require.Same(t, result, debugger.waited[0])
				} else {
					require.Empty(t, debugger.waited)
				}

				debugger.observed, debugger.waited = nil, nil
				fun := lisp.FunInPackage(lisp.DefaultUserPackage, "fault", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					panic("host fault bypasses observers")
				})
				result = env.Eval(lisp.SExpr([]*lisp.LVal{fun}))
				require.True(t, lisp.IsInternalPanic(result), "%v", result)
				require.Empty(t, debugger.observed)
				require.Empty(t, debugger.waited)
			})
		}
	}
}

type entryReturnPanicDebugger struct{ dormantDebugger }

func (entryReturnPanicDebugger) IsEnabled() bool { return true }
func (entryReturnPanicDebugger) OnFunReturn(*lisp.LEnv, *lisp.LVal, *lisp.LVal) {
	panic("function return observer fault")
}

func TestDirectCallContainsReturnObserverPanic(t *testing.T) {
	env := newLimitTestEnv(t)
	calls := 0
	fun := lisp.FunInPackage(lisp.DefaultUserPackage, "observed", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
		calls++
		return lisp.Int(7)
	})
	env.Runtime.Debugger = entryReturnPanicDebugger{}
	var result *lisp.LVal
	require.NotPanics(t, func() { result = env.FunCall(fun, lisp.SExpr(nil)) })
	require.True(t, lisp.IsInternalPanic(result), "%v", result)
	require.Contains(t, result.String(), "function return observer fault")
	require.Equal(t, 1, calls)
	require.Empty(t, env.Runtime.Stack.Frames)
}

type entryPanicReader struct{ fail func() }

func (r entryPanicReader) Read(string, io.Reader) ([]*lisp.LVal, error) {
	r.fail()
	return nil, nil
}
func (r entryPanicReader) ReadLocation(string, string, io.Reader) ([]*lisp.LVal, error) {
	r.fail()
	return nil, nil
}

type entryPanicLibrary struct{ fail func() }

func (s entryPanicLibrary) LoadSource(lisp.SourceContext, string) (string, string, []byte, error) {
	s.fail()
	return "", "", nil, nil
}

func TestLoadEntryContainsHostPanic(t *testing.T) {
	for _, method := range []string{"Load", "LoadContext", "LoadString", "LoadStringContext", "LoadLocation", "LoadLocationContext", "LoadFile", "LoadFileContext"} {
		t.Run(method, func(t *testing.T) {
			env := newLimitTestEnv(t)
			calls := 0
			fail := func() { calls++; panic("source callback fault") }
			env.Runtime.Reader = entryPanicReader{fail: fail}
			env.Runtime.Library = entryPanicLibrary{fail: fail}
			var result *lisp.LVal
			require.NotPanics(t, func() {
				ctx := context.Background()
				switch method {
				case "Load":
					result = env.Load("source.lisp", strings.NewReader("7"))
				case "LoadContext":
					result = env.LoadContext(ctx, "source.lisp", strings.NewReader("7"))
				case "LoadString":
					result = env.LoadString("source.lisp", "7")
				case "LoadStringContext":
					result = env.LoadStringContext(ctx, "source.lisp", "7")
				case "LoadLocation":
					result = env.LoadLocation("source.lisp", "/source.lisp", strings.NewReader("7"))
				case "LoadLocationContext":
					result = env.LoadLocationContext(ctx, "source.lisp", "/source.lisp", strings.NewReader("7"))
				case "LoadFile":
					result = env.LoadFile("source.lisp")
				case "LoadFileContext":
					result = env.LoadFileContext(ctx, "source.lisp")
				}
			})
			require.Equal(t, 1, calls)
			require.True(t, lisp.IsInternalPanic(result), "%v", result)
			require.Contains(t, result.String(), "source callback fault")
			require.NotEmpty(t, result.CallStack().GoStack)
			env.Runtime.Reader = parser.NewReader()
			next := env.LoadString("after-source-fault.lisp", `(+ 1 2)`)
			require.Equal(t, lisp.LInt, next.Type, "%v", next)
			require.Equal(t, 3, next.Int)
		})
	}
}
