// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/diagnostic"
	"github.com/stretchr/testify/require"
)

// Cancels during the traversal, rather than at the evaluator's entry check.
// A context with dead set is cancelled from the outset, which is what a caller
// holding an already-finished request context passes in.
type renderCancelContext struct {
	context.Context
	checks int
	dead   bool
}

func (c *renderCancelContext) Err() error {
	c.checks++
	if c.dead || c.checks > 100 {
		return context.Canceled
	}
	return nil
}

func TestRenderRuntimePolicy(t *testing.T) {
	env := NewEnv(nil)
	env.Runtime.MaxAlloc = 64
	dag := Int(1)
	for range 40 {
		dag = SExpr([]*LVal{dag, dag})
	}
	var out bytes.Buffer
	env.Runtime.Stderr = &out
	result := builtinDebugPrint(env, SExpr([]*LVal{dag}))
	require.Equal(t, LError, result.Type)
	require.Contains(t, result.String(), "allocation size exceeds maximum (64)")
	require.Empty(t, out.String())
	require.Equal(t, renderTruncatedMark, env.Render(dag))
	e := env.ErrorCondition("boom", dag)
	require.Same(t, dag, e.Cells[0], "rendering must not replace handler data")
	for _, s := range []string{e.String(), (*ErrorVal)(e).Error(), (*ErrorVal)(e).ErrorMessage()} {
		require.LessOrEqual(t, len(s), 64)
		require.Contains(t, s, renderTruncatedMark)
	}
	out.Reset()
	_, err := (*ErrorVal)(e).WriteTrace(&out)
	require.NoError(t, err)
	require.Contains(t, out.String(), renderTruncatedMark)
	require.Contains(t, env.Errorf("bad operand: %v", dag).String(), renderTruncatedMark)
	shared := SExpr([]*LVal{Int(1)})
	require.Equal(t, "((1) (1))", env.Render(SExpr([]*LVal{shared, shared})))
}

func TestRenderCancellationDuringTraversal(t *testing.T) {
	dag := Int(1)
	for range 40 {
		dag = SExpr([]*LVal{dag, dag})
	}
	for _, mode := range []string{"debug", "format", "format-scalars", "diagnostic", "error", "error-message", "trace", "diagnostic-error"} {
		t.Run(mode, func(t *testing.T) {
			env := NewEnv(nil)
			ctx := &renderCancelContext{Context: context.Background()}
			env.evalCtx = ctx
			env.Runtime.MaxAlloc = 1 << 30
			var out bytes.Buffer
			env.Runtime.Stderr = &out
			var result *LVal
			switch mode {
			case "debug":
				result = builtinDebugPrint(env, SExpr([]*LVal{dag}))
			case "format":
				result = builtinFormatString(env, SExpr([]*LVal{String("{}"), dag}))
			case "format-scalars":
				args := []*LVal{String(strings.Repeat("{}", 200))}
				for range 200 {
					args = append(args, String("x"))
				}
				result = builtinFormatString(env, SExpr(args))
			case "diagnostic":
				require.Equal(t, renderTruncatedMark, env.Render(dag))
			default:
				e := (*ErrorVal)(env.ErrorCondition("boom", dag))
				// The error stores no context; the reader is handed the
				// request context explicitly, as an in-request caller does.
				env.evalCtx = nil
				switch mode {
				case "error":
					// Error() has no exported context form, so this is what
					// it would be: the same render with message=false.
					require.Equal(t, renderTruncatedMark, e.renderContext(ctx, false))
				case "error-message":
					require.Equal(t, renderTruncatedMark, e.ErrorMessageContext(ctx))
				case "trace":
					_, err := e.WriteTraceContext(ctx, &out)
					require.NoError(t, err)
					require.Contains(t, out.String(), renderTruncatedMark)
				case "diagnostic-error":
					_, err := e.WriteDiagnosticContext(ctx, &out, &diagnostic.Renderer{})
					require.NoError(t, err)
					require.Contains(t, out.String(), renderTruncatedMark)
				}
			}
			if result != nil {
				require.Equal(t, LError, result.Type)
				require.Equal(t, CondContextCancelled, result.Str)
			}
			require.Greater(t, ctx.checks, 100)
			require.Less(t, ctx.checks, 1000, "must stop promptly after cancellation")
		})
	}
}

func TestRenderWorkBudget(t *testing.T) {
	// Empty error messages have no byte cost. A total work budget still stops
	// an arbitrarily wide sequence of them, including any rendering retries.
	empty := &LVal{Type: LError, Str: "error"}
	v := &LVal{Type: LError, Cells: make([]*LVal, 100)}
	for i := range v.Cells {
		v.Cells[i] = empty
	}
	budget := renderBudget{remaining: 5}
	s, ok := v.boundedRender(1024, &budget, true)
	require.False(t, ok)
	require.Empty(t, s)
	require.Zero(t, budget.remaining)
}

func TestDebugPrintAggregateLimit(t *testing.T) {
	env := NewEnv(nil)
	env.Runtime.MaxAlloc = 9
	var out bytes.Buffer
	env.Runtime.Stderr = &out
	args := SExpr([]*LVal{String("ab"), String("cd")})
	require.Equal(t, LError, builtinDebugPrint(env, args).Type)
	require.Empty(t, out.String())
	env.Runtime.MaxAlloc = 10
	require.True(t, builtinDebugPrint(env, args).IsNil())
	require.Equal(t, "\"ab\" \"cd\"\n", out.String())
	// to-string never traverses DAGs; format-string must reject their expansion.
	require.Equal(t, LError, builtinToString(env, SExpr([]*LVal{args})).Type)
	require.Equal(t, LError, builtinFormatString(env, SExpr([]*LVal{String("{}"), args})).Type)
	require.Equal(t, strings.Repeat("s", 32), builtinToString(env, SExpr([]*LVal{String(strings.Repeat("s", 32))})).Str)
}

func TestErrorFormattingPreservesArguments(t *testing.T) {
	env := NewEnv(nil)
	v := SExpr([]*LVal{Int(1)})
	args := []interface{}{v}
	require.Equal(t, LError, env.Errorf("bad value: %v", args...).Type)
	require.Same(t, v, args[0], "formatting must not replace a caller's arguments")
}

// A trace shares ONE output budget across the message, the frames and any Go
// stack, and stops at it whichever part runs out first. The budget is the
// DIAGNOSTIC limit rather than MaxAlloc (minRenderLimit floors it, so that an
// embedder capping collection sizes still gets a readable failure), so each
// fixture below is sized to overrun that floor. The debug-stack builtin is
// program-facing output and keeps MaxAlloc exactly.
func TestTraceSharedOutputBudget(t *testing.T) {
	for _, mode := range []string{"long-name", "many-frames", "go-stack", "debug-stack"} {
		t.Run(mode, func(t *testing.T) {
			env := NewEnv(nil)
			env.Runtime.MaxAlloc = 64
			limit := diagnosticLimit(env.Runtime.MaxAlloc)
			for range 20000 {
				env.Runtime.Stack.Frames = append(env.Runtime.Stack.Frames, CallFrame{Name: "frame"})
			}
			if mode == "long-name" || mode == "debug-stack" {
				env.Runtime.Stack.Top().Name = strings.Repeat("x", limit+4096)
			}
			if mode == "go-stack" {
				env.Runtime.Stack.Frames = nil
			}
			var out bytes.Buffer
			if mode == "debug-stack" {
				limit = env.Runtime.MaxAlloc
				env.Runtime.Stderr = &out
				builtinDebugStack(env, Nil())
			} else {
				e := (*ErrorVal)(env.ErrorCondition("boom", String("bad")))
				if mode == "go-stack" {
					(*LVal)(e).CallStack().GoStack = bytes.Repeat([]byte("go frame\n"), limit/9+100)
				}
				n, err := e.WriteTrace(&out)
				require.NoError(t, err)
				require.Equal(t, out.Len(), n)
			}
			require.LessOrEqual(t, out.Len(), limit)
			require.True(t, strings.HasSuffix(out.String(), renderTruncatedMark))
		})
	}
}

// The floor's own case: a MaxAlloc small enough to cap collection sizes caps
// no diagnostic. The trace is readable rather than a lone truncation marker.
func TestTraceFloorsTinyOutputBudget(t *testing.T) {
	env := NewEnv(nil)
	env.Runtime.MaxAlloc = 5
	for range 100 {
		env.Runtime.Stack.Frames = append(env.Runtime.Stack.Frames, CallFrame{Name: "frame"})
	}
	e := (*ErrorVal)(env.ErrorCondition("boom", String("bad")))
	var out bytes.Buffer
	n, err := e.WriteTrace(&out)
	require.NoError(t, err)
	require.Equal(t, out.Len(), n)
	require.NotContains(t, out.String(), renderTruncatedMark)
	require.Contains(t, out.String(), "boom")
	require.Contains(t, out.String(), "Stack Trace [100 frames")
}

func TestTraceFrameCancellation(t *testing.T) {
	// "cancelled" is the totality case: the context the caller hands the
	// reader is already dead, which is the ordinary shape of an error logged
	// after its request ended.  A dead context is no context at all, so the
	// whole trace renders under the byte limit alone.  The other modes pin
	// the live-context property: a context that is cancelled DURING the
	// traversal still stops the frame output promptly.
	for _, mode := range []string{"cancelled", "during-frames", "debug-stack"} {
		t.Run(mode, func(t *testing.T) {
			env := NewEnv(nil)
			ctx := &renderCancelContext{Context: context.Background()}
			env.evalCtx = ctx
			for range 1000 {
				env.Runtime.Stack.Frames = append(env.Runtime.Stack.Frames, CallFrame{Name: "frame"})
			}
			var out bytes.Buffer
			if mode == "debug-stack" {
				env.Runtime.Stderr = &out
				builtinDebugStack(env, Nil())
			} else {
				e := (*ErrorVal)(env.ErrorCondition("boom", String("bad")))
				env.evalCtx = nil
				if mode == "cancelled" {
					ctx.dead = true
				}
				_, err := e.WriteTraceContext(ctx, &out)
				require.NoError(t, err)
			}
			if mode == "cancelled" {
				require.Contains(t, out.String(), "boom: bad", "a dead context must not blank the message")
				require.NotContains(t, out.String(), renderTruncatedMark)
				require.Equal(t, 1000, strings.Count(out.String(), "height "))
				require.Equal(t, 1, ctx.checks, "an explicitly passed dead context is consulted once and then dropped")
				return
			}
			require.Less(t, strings.Count(out.String(), "height "), 100, "cancellation must stop frame output")
			require.Contains(t, out.String(), renderTruncatedMark)
			require.Less(t, ctx.checks, 200)
		})
	}
}

type cancelTraceWriter struct {
	bytes.Buffer
	cancel context.CancelFunc
}

func (w *cancelTraceWriter) WriteString(s string) (int, error) {
	n, err := w.Buffer.WriteString(s)
	w.cancel()
	return n, err
}

func TestTraceCancellationWhileWriting(t *testing.T) {
	env := NewEnv(nil)
	for range 100 {
		env.Runtime.Stack.Frames = append(env.Runtime.Stack.Frames, CallFrame{Name: "frame"})
	}
	e := (*ErrorVal)(env.ErrorCondition("boom", String("bad")))
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	out := &cancelTraceWriter{cancel: cancel}
	n, err := e.WriteTraceContext(ctx, out)
	require.NoError(t, err)
	require.Equal(t, out.Len(), n)
	require.Zero(t, strings.Count(out.String(), "height "), "cancellation after the message must stop frames")
	require.Contains(t, out.String(), renderTruncatedMark)
}
