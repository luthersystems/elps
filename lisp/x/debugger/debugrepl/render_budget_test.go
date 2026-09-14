// Copyright © 2026 The ELPS authors

package debugrepl

import (
	"bytes"
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestLocalsResponseRenderBudget(t *testing.T) {
	env := lisp.NewEnv(lisp.NewEnv(nil))
	env.Runtime.MaxAlloc = 64
	for _, name := range []string{"a", "b", "c"} {
		env.Put(lisp.Symbol(name), lisp.String(strings.Repeat("x", 40)))
	}
	var out bytes.Buffer
	showLocals(&out, env, nil)
	require.Contains(t, out.String(), "#<truncated>")
	require.LessOrEqual(t, out.Len(), env.Runtime.MaxAllocBytes())
}

func TestLocalsNamesWireBudget(t *testing.T) {
	env := lisp.NewEnv(lisp.NewEnv(nil))
	name := strings.Repeat("a", lisp.DefaultMaxAlloc/2)
	for _, suffix := range []string{"b", "c", "d", "e"} {
		env.Put(lisp.Symbol(name+suffix), lisp.Int(1))
	}
	var out bytes.Buffer
	showLocals(&out, env, nil)
	t.Logf("complete REPL output bytes=%d cap=%d", out.Len(), lisp.DefaultMaxAlloc)
	require.LessOrEqual(t, out.Len(), lisp.DefaultMaxAlloc)
	require.Contains(t, out.String(), "truncated")
}

func TestDebuggerDisplayCancellation(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.MaxAlloc = 4096
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	fn := lisp.Fun("inspect", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
		local := lisp.NewEnv(env)
		name := strings.Repeat("a", 3000)
		local.Put(lisp.Symbol(name), lisp.Int(1))
		cancel()
		var out bytes.Buffer
		showLocals(&out, local, nil)
		require.Contains(t, out.String(), "truncated")
		require.LessOrEqual(t, out.Len(), 4096)
		out.Reset()
		showBacktrace(&out, &lisp.CallStack{Frames: []lisp.CallFrame{{Name: name}}}, nil, "", local)
		require.Contains(t, out.String(), "truncated")
		require.LessOrEqual(t, out.Len(), 4096)
		return lisp.Nil()
	})
	env.EvalContext(ctx, lisp.SExpr([]*lisp.LVal{fn}))
}

func TestBacktraceNamesBudget(t *testing.T) {
	name := strings.Repeat("a", lisp.DefaultMaxAlloc/2)
	stack := &lisp.CallStack{Frames: make([]lisp.CallFrame, 4)}
	for i := range stack.Frames {
		stack.Frames[i] = lisp.CallFrame{Name: name, Package: name, Source: &token.Location{File: name}}
	}
	var out bytes.Buffer
	showBacktrace(&out, stack, nil, "")
	require.LessOrEqual(t, out.Len(), lisp.DefaultMaxAlloc)
	require.Contains(t, out.String(), "truncated")
}
