// Copyright © 2026 The ELPS authors

package repl

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestJSONEscapingBudget(t *testing.T) {
	env := newTestEnv(t)
	cfg := newConfig(WithJSON(true), WithEval(`(error 'boom (string:repeat "<" 2000000))`))
	var out bytes.Buffer
	require.Equal(t, 1, runEval(env, cfg, &out, io.Discard))
	t.Logf("stdout bytes=%d cap=%d", out.Len(), env.Runtime.MaxAllocBytes())
	require.LessOrEqual(t, out.Len(), env.Runtime.MaxAllocBytes())
	require.True(t, json.Valid(out.Bytes()))
	require.Contains(t, out.String(), `"truncated":true`)
}

func TestJSONSmallBudget(t *testing.T) {
	for _, limit := range []int{5, 32, 64, 256} {
		t.Run(strconv.Itoa(limit), func(t *testing.T) {
			env := newTestEnv(t)
			env.Runtime.MaxAlloc = limit
			v := env.ErrorCondition("boom", lisp.String(strings.Repeat("<", limit/2)))
			v.SetSource(&token.Location{File: strings.Repeat("<&界", 10000), Pos: 0, Line: 1})
			var out bytes.Buffer
			emitResultContext(t.Context(), &out, v, env)
			t.Logf("stdout bytes=%d cap=%d", out.Len(), limit)
			require.LessOrEqual(t, out.Len(), limit)
			require.True(t, json.Valid(out.Bytes()), out.String())
			require.True(t, strings.HasSuffix(out.String(), "\n"))
			if limit >= 32 {
				require.Contains(t, out.String(), `"truncated":true`)
			}
		})
	}
}

func TestJSONCancelWriting(t *testing.T) {
	env := newTestEnv(t)
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	out := &cancelDiagnosticWriter{cancel: cancel}
	emitResultContext(ctx, out, lisp.String(strings.Repeat("<", 200000)), env)
	t.Logf("writes=%d stdout bytes=%d", out.calls, out.Len())
	require.LessOrEqual(t, out.Len(), 16384, "stop after the first bounded write and close the JSON object")
	require.LessOrEqual(t, out.calls, 2)
	require.True(t, json.Valid(out.Bytes()))
	require.Contains(t, out.String(), `"truncated":true`)
}

type cancelJSONContext struct {
	context.Context
	checks int
}

func (c *cancelJSONContext) Err() error {
	c.checks++
	if c.checks >= 4 {
		return context.Canceled
	}
	return nil
}

func TestJSONCancelEncoding(t *testing.T) {
	ctx := &cancelJSONContext{Context: t.Context()}
	var out bytes.Buffer
	emitJSONLine(ctx, &out, lisp.DefaultMaxAlloc, jsonField{"source", []string{strings.Repeat("<", 2000000)}})
	require.True(t, json.Valid(out.Bytes()))
	require.Contains(t, out.String(), `"truncated":true`)
	require.LessOrEqual(t, out.Len(), 16384)
	require.GreaterOrEqual(t, ctx.checks, 4)
	require.Less(t, ctx.checks, 10)
}

func TestJSONEncodingBoundaries(t *testing.T) {
	value := strings.Repeat("<&\"\\\n\t\x00界\u2028\xff", 200)
	for limit := 1; limit < 300; limit++ {
		var out bytes.Buffer
		emitJSONLine(t.Context(), &out, limit, jsonField{"message", []string{value}})
		require.LessOrEqual(t, out.Len(), limit)
		if limit < 4 {
			require.Empty(t, out.Bytes())
			continue
		}
		require.True(t, json.Valid(out.Bytes()), "limit=%d: %q", limit, out.String())
		require.True(t, strings.HasSuffix(out.String(), "\n"))
	}
	var out bytes.Buffer
	emitJSONLine(t.Context(), &out, lisp.DefaultMaxAlloc, jsonField{"message", []string{value}})
	want, err := json.Marshal(map[string]string{"message": value})
	require.NoError(t, err)
	require.Equal(t, string(want)+"\n", out.String())
}
