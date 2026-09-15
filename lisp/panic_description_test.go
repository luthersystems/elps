// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

type panicFormattingPayload struct{ calls int }

func (p *panicFormattingPayload) Format(fmt.State, rune) {
	p.calls++
	panic("formatting the failed object would re-enter host code")
}

type panicPayloadCache struct{ payload any }

func (c panicPayloadCache) Load(string) (*lisp.CachedSource, bool) { panic(c.payload) }
func (panicPayloadCache) Store(string, *lisp.CachedSource)         {}

func TestPanicDiagnosticsDoNotInvokePayloadFormatter(t *testing.T) {
	for _, cached := range []bool{false, true} {
		t.Run(map[bool]string{false: "evaluation", true: "cache diagnostic"}[cached], func(t *testing.T) {
			env := newLimitTestEnv(t)
			payload := &panicFormattingPayload{}
			var diagnostics bytes.Buffer
			env.Runtime.Stderr = &diagnostics
			var result *lisp.LVal
			if cached {
				env.Runtime.LoadCache = panicPayloadCache{payload: payload}
				result = env.LoadString("cache-payload.lisp", "7")
				require.Zero(t, payload.calls, "diagnostics must not call methods on the object that panicked")
				require.Equal(t, lisp.LInt, result.Type, "%v", result)
				require.Equal(t, 7, result.Int)
				require.Contains(t, diagnostics.String(), "panicFormattingPayload")
			} else {
				fun := lisp.FunInPackage(lisp.DefaultUserPackage, "payload-fault", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					panic(payload)
				})
				result = env.Eval(lisp.SExpr([]*lisp.LVal{fun}))
				require.Zero(t, payload.calls, "diagnostics must not call methods on the object that panicked")
				require.True(t, lisp.IsInternalPanic(result), "%v", result)
				require.Contains(t, result.String(), "panicFormattingPayload")
			}
		})
	}
}

type panicNamedString string

func (panicNamedString) String() string { panic("named strings must not call String") }

func TestPanicDiagnosticsRetainPrimitiveAndRuntimeMessages(t *testing.T) {
	for _, tc := range []struct {
		name string
		body lisp.LBuiltin
		want string
	}{
		{"named string", func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { panic(panicNamedString("original message")) }, "original message"},
		{"integer", func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { panic(42) }, "42"},
		{"runtime fault", func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
			var value *int
			return lisp.Int(*value)
		}, "invalid memory address or nil pointer dereference"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newLimitTestEnv(t)
			fun := lisp.FunInPackage(lisp.DefaultUserPackage, "message-fault", lisp.Formals(), tc.body)
			result := env.Eval(lisp.SExpr([]*lisp.LVal{fun}))
			require.True(t, lisp.IsInternalPanic(result), "%v", result)
			require.Contains(t, result.String(), tc.want)
		})
	}
}

type entryPanicStream struct{}

func (entryPanicStream) Read([]byte) (int, error) { panic("input stream fault") }

func TestLoadContainsInputStreamPanic(t *testing.T) {
	for _, cached := range []bool{false, true} {
		t.Run(map[bool]string{false: "streaming", true: "cached"}[cached], func(t *testing.T) {
			env := newLimitTestEnv(t)
			if cached {
				env.Runtime.LoadCache = panicPayloadCache{payload: "cache lookup must not precede reading"}
			}
			var result *lisp.LVal
			require.NotPanics(t, func() { result = env.Load("input.lisp", entryPanicStream{}) })
			require.True(t, lisp.IsInternalPanic(result), "%v", result)
			require.Contains(t, result.String(), "input stream fault")
			require.Contains(t, string(result.CallStack().GoStack), "entryPanicStream.Read")
		})
	}
}
