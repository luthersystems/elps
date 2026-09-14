// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"context"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

// Cancels during the traversal, rather than at the evaluator's entry check.
type renderCancelContext struct {
	context.Context
	checks int
}

func (c *renderCancelContext) Err() error {
	c.checks++
	if c.checks > 100 {
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
	for _, mode := range []string{"debug", "format", "format-scalars", "diagnostic", "error", "error-message", "trace"} {
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
				// Retain the original context even after the builtin boundary restores it.
				env.evalCtx = nil
				switch mode {
				case "error":
					require.Equal(t, renderTruncatedMark, e.Error())
				case "error-message":
					require.Equal(t, renderTruncatedMark, e.ErrorMessage())
				case "trace":
					_, err := e.WriteTrace(&out)
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
