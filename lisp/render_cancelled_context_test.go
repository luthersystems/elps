// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"context"
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/diagnostic"
	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

func newCancelRenderEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	if err := lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()))); err != nil {
		t.Fatal(err)
	}
	return env
}

// An error outlives the request that produced it: a handler returns it, the
// host cancels the request context, and only then does the caller log it.
// Rendering through the no-context readers must not be blanked.
func TestErrorRendersAfterRequestContextCancelled(t *testing.T) {
	env := newCancelRenderEnv(t)
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	v := env.LoadStringContext(ctx, "x.lisp", `(car 1)`)
	require.Equal(t, lisp.LError, v.Type)
	e := (*lisp.ErrorVal)(v)

	before := e.Error()
	beforeMessage := e.ErrorMessage()
	var beforeTrace bytes.Buffer
	_, err := e.WriteTrace(&beforeTrace)
	require.NoError(t, err)
	beforeString := v.String()
	require.Contains(t, before, "argument is not a list")

	cancel()

	require.Equal(t, before, e.Error(), "Error must not depend on the captured context's liveness")
	require.Equal(t, beforeMessage, e.ErrorMessage())
	require.Equal(t, beforeString, v.String())
	require.Equal(t, beforeString, fmt.Sprintf("%v", v))
	var afterTrace bytes.Buffer
	_, err = e.WriteTrace(&afterTrace)
	require.NoError(t, err)
	require.Equal(t, beforeTrace.String(), afterTrace.String())
	for _, s := range []string{e.Error(), e.ErrorMessage(), v.String(), afterTrace.String()} {
		require.NotContains(t, s, "#<truncated>")
	}
}

// An error raised because the request context was cancelled is rendered by the
// very evaluation that observed the cancellation, so its message must survive
// in-process too.
func TestCancellationErrorRendersInProcess(t *testing.T) {
	env := newCancelRenderEnv(t)
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	env.AddBuiltins(true, elpsutil.Function("cancel-now", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			cancel()
			return lisp.Nil()
		}))
	v := env.LoadStringContext(ctx, "c.lisp", `(with-cleanup ((release)) (cancel-now))`)
	require.Equal(t, lisp.LError, v.Type)
	require.Equal(t, lisp.CondContextCancelled, v.Str)
	e := (*lisp.ErrorVal)(v)
	for _, s := range []string{e.Error(), e.ErrorMessage(), v.String(), fmt.Sprintf("%v", v)} {
		require.NotContains(t, s, "#<truncated>")
		require.Contains(t, s, "context cancelled")
	}
	var trace bytes.Buffer
	_, err := e.WriteTrace(&trace)
	require.NoError(t, err)
	require.Contains(t, trace.String(), "context cancelled")
}

// The byte limit still bounds rendering when the captured context is dead: a
// cancelled request must not turn the output budget off. The limit sits above
// the diagnostic floor (minRenderLimit, 64 KiB), which exists so that a small
// MaxAlloc meant for data does not gag error text; the property under test is
// that a dead context changes nothing about the budget that does apply.
func TestCancelledContextStillHonoursRenderLimit(t *testing.T) {
	const limit = 70_000
	env := lisp.NewEnv(nil)
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env,
		lisp.WithReader(parser.NewReader()), lisp.WithMaxAlloc(limit))))
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	v := env.LoadStringContext(ctx, "big.lisp",
		`(error 'boom "`+strings.Repeat("x", 100_000)+`")`) // under the scanner's 128 KiB token cap, over the limit
	require.Equal(t, lisp.LError, v.Type)
	cancel()
	e := (*lisp.ErrorVal)(v)
	s := e.Error()
	require.LessOrEqual(t, len(s), limit)
	require.Contains(t, s, "#<truncated>")
	require.True(t, strings.HasSuffix(s, "#<truncated>"))
}

// The same shape, but the caller hands the now-dead request context to the
// explicit readers, which is what "log the error after defer cancel()" looks
// like. A dead context bounds no work, so it must not blank the text: each
// reader must produce byte for byte what its no-context form produces.
func TestDeadCallerContextRendersLikeNoContext(t *testing.T) {
	env := newCancelRenderEnv(t)
	ctx, cancel := context.WithCancel(t.Context())
	v := env.LoadStringContext(ctx, "x.lisp", `(car 1)`)
	require.Equal(t, lisp.LError, v.Type)
	e := (*lisp.ErrorVal)(v)
	cancel()
	require.Error(t, ctx.Err())

	require.Equal(t, e.ErrorMessage(), e.ErrorMessageContext(ctx))

	var plainTrace, deadTrace bytes.Buffer
	_, err := e.WriteTrace(&plainTrace)
	require.NoError(t, err)
	_, err = e.WriteTraceContext(ctx, &deadTrace)
	require.NoError(t, err)
	require.Equal(t, plainTrace.String(), deadTrace.String())

	var plainDiag, deadDiag bytes.Buffer
	//nolint:staticcheck // the nil-context render is the comparison baseline
	_, err = e.WriteDiagnosticContext(nil, &plainDiag, &diagnostic.Renderer{})
	require.NoError(t, err)
	_, err = e.WriteDiagnosticContext(ctx, &deadDiag, &diagnostic.Renderer{})
	require.NoError(t, err)
	require.Equal(t, plainDiag.String(), deadDiag.String())

	for _, s := range []string{e.ErrorMessageContext(ctx), deadTrace.String(), deadDiag.String()} {
		require.NotContains(t, s, "#<truncated>")
		require.Contains(t, s, "argument is not a list")
	}
}
