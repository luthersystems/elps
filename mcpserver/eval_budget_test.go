// Copyright © 2026 The ELPS authors

package mcpserver

import (
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/modelcontextprotocol/go-sdk/mcp"
	"github.com/stretchr/testify/require"
)

func TestEvalAggregateWireBudget(t *testing.T) {
	for _, batch := range []bool{false, true} {
		t.Run(map[bool]string{false: "single", true: "batch"}[batch], func(t *testing.T) {
			srv := New()
			client, server := connectTestServer(t, srv)
			defer client.Close()
			defer server.Close()
			args := map[string]any{"expression": `(set 'x (string:repeat "a" 1000000))` + strings.Repeat(" x", 12)}
			if batch {
				expressions := make([]string, 13)
				for i := range expressions {
					expressions[i] = `(string:repeat "<" 1000000)`
				}
				args = map[string]any{"expressions": expressions}
			}
			res, err := client.CallTool(context.Background(), &mcp.CallToolParams{Name: "eval", Arguments: args})
			require.NoError(t, err)
			wire, err := json.Marshal(map[string]any{"jsonrpc": "2.0", "id": 1, "result": res})
			require.NoError(t, err)
			t.Logf("complete MCP response bytes=%d cap=%d", len(wire)+1, lisp.DefaultMaxAlloc)
			require.LessOrEqual(t, len(wire)+1, lisp.DefaultMaxAlloc)
			require.Contains(t, string(wire), "truncated")
		})
	}
}

// Cancel only after the builtin returns its value: rendering must keep using
// the request context after EvalContext has restored the environment context.
type evalRenderContext struct {
	context.Context
	armed  bool
	checks int
	err    error
}

func (c *evalRenderContext) Err() error {
	if c.armed {
		c.checks++
		if c.checks >= 100 {
			return c.err
		}
	}
	return nil
}

func evalResponseWire(t *testing.T, out EvalResponse) []byte {
	t.Helper()
	structured, err := json.Marshal(out)
	require.NoError(t, err)
	result := &mcp.CallToolResult{StructuredContent: json.RawMessage(structured), Content: []mcp.Content{&mcp.TextContent{Text: string(structured)}}}
	wire, err := json.Marshal(map[string]any{"jsonrpc": "2.0", "id": 1, "result": result})
	require.NoError(t, err)
	require.True(t, json.Valid(wire))
	return append(wire, '\n')
}

func TestEvalRuntimeAndContextBudget(t *testing.T) {
	for _, mode := range []string{"small", "cancel", "deadline", "aggregate", "echo", "tiny-results"} {
		t.Run(mode, func(t *testing.T) {
			ctx := &evalRenderContext{Context: t.Context(), err: context.Canceled}
			if mode == "deadline" {
				ctx.err = context.DeadlineExceeded
			}
			limit := 4096
			env, err := lisplib.NewDocEnv()
			require.NoError(t, err)
			env.Runtime.MaxAlloc = limit
			value := lisp.String(strings.Repeat("<", 2000))
			if mode == "cancel" || mode == "deadline" {
				value = lisp.Int(1)
				for range 40 {
					value = lisp.SExpr([]*lisp.LVal{value, value})
				}
			}
			env.Put(lisp.Symbol("payload"), lisp.Fun("payload", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
				ctx.armed = mode == "cancel" || mode == "deadline"
				return value
			}))
			srv := New(WithRequestEnvFactory(func(context.Context) (*lisp.LEnv, func(), error) { return env, noopRelease, nil }))
			input := EvalInput{Expression: "(payload)"}
			switch mode {
			case "aggregate":
				input.Expression = strings.Repeat(`"<<<<<" `, 200)
			case "echo":
				input = EvalInput{Expressions: []string{"1 ;" + strings.Repeat("<", 2000)}}
			case "tiny-results":
				input = EvalInput{Expressions: make([]string, 1000)}
				for i := range input.Expressions {
					input.Expressions[i] = "1"
				}
			}
			_, out, err := srv.service.evalTool(ctx, nil, input)
			require.NoError(t, err)
			wire := evalResponseWire(t, out)
			t.Logf("complete MCP response bytes=%d cap=%d", len(wire), limit)
			require.LessOrEqual(t, len(wire), limit)
			require.Contains(t, string(wire), "truncated")
			if ctx.armed {
				require.GreaterOrEqual(t, ctx.checks, 100)
				require.Less(t, ctx.checks, 200)
			}
		})
	}
}

func TestEvalRenderDeadline(t *testing.T) {
	if os.Getenv("ELPS_TEST_MCP_RENDER") == "1" {
		env, err := lisplib.NewDocEnv()
		require.NoError(t, err)
		v := lisp.Int(1)
		for range 40 {
			v = lisp.SExpr([]*lisp.LVal{v, v})
		}
		env.Put(lisp.Symbol("payload"), v)
		srv := New(WithRequestEnvFactory(func(context.Context) (*lisp.LEnv, func(), error) { return env, noopRelease, nil }))
		_, out, err := srv.service.evalTool(t.Context(), nil, EvalInput{Expression: "payload"})
		require.NoError(t, err)
		wire := evalResponseWire(t, out)
		require.LessOrEqual(t, len(wire), lisp.DefaultMaxAlloc)
		require.Contains(t, string(wire), "truncated")
		return
	}
	ctx, cancel := context.WithTimeout(t.Context(), 10*time.Second)
	defer cancel()
	//nolint:gosec // Re-execute the regression with a hard rendering deadline.
	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestEvalRenderDeadline$")
	cmd.Env = append(os.Environ(), "ELPS_TEST_MCP_RENDER=1")
	out, err := cmd.CombinedOutput()
	require.NoError(t, err, "deadline=%v output=%s", ctx.Err(), out)
}

func TestEvalTinyEnvelopeBudget(t *testing.T) {
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	env.Runtime.MaxAlloc = 64
	srv := New(WithRequestEnvFactory(func(context.Context) (*lisp.LEnv, func(), error) { return env, noopRelease, nil }))
	_, _, err = srv.service.evalTool(t.Context(), nil, EvalInput{Expression: "1"})
	require.Error(t, err)
	wire, marshalErr := json.Marshal(map[string]any{"jsonrpc": "2.0", "id": 1, "error": err})
	require.NoError(t, marshalErr)
	t.Logf("complete MCP error response bytes=%d cap=%d", len(wire)+1, env.Runtime.MaxAllocBytes())
	require.LessOrEqual(t, len(wire)+1, env.Runtime.MaxAllocBytes())
	require.Contains(t, string(wire), "~")
}
