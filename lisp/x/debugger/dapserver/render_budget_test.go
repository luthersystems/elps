// Copyright © 2026 The ELPS authors

package dapserver

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"strings"
	"testing"

	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestVariablesResponseRenderBudget(t *testing.T) {
	v := lisp.Symbol(strings.Repeat("x", lisp.DefaultMaxAlloc/2-32))
	bindings := []debugger.ScopeBinding{{Name: "a", Value: v}, {Name: "b", Value: v}, {Name: "c", Value: v}, {Name: "d", Value: v}}
	noRef := func(*lisp.LVal) int { return 0 }
	for _, expand := range []bool{false, true} {
		vars := translateVariables(bindings, noRef, nil)
		if expand {
			vars = expandVariable(lisp.SExpr([]*lisp.LVal{v, v, v, v}), noRef, nil, nil)
		}
		response := &dap.VariablesResponse{}
		response.Body.Variables = vars
		var out bytes.Buffer
		require.NoError(t, (&Server{writer: &out}).send(response))
		require.LessOrEqual(t, out.Len(), lisp.DefaultMaxAlloc)
		require.Contains(t, out.String(), "truncated")
	}
}

func TestProgramNamesWireBudget(t *testing.T) {
	name := strings.Repeat("<", lisp.DefaultMaxAlloc/2)
	for _, kind := range []string{"variables", "stackTrace"} {
		t.Run(kind, func(t *testing.T) {
			var msg dap.Message
			if kind == "variables" {
				bindings := make([]debugger.ScopeBinding, 4)
				for i := range bindings {
					bindings[i] = debugger.ScopeBinding{Name: name, Value: lisp.Int(1)}
				}
				response := &dap.VariablesResponse{}
				response.Body.Variables = translateVariables(bindings, func(*lisp.LVal) int { return 0 }, nil)
				msg = response
			} else {
				stack := &lisp.CallStack{Frames: make([]lisp.CallFrame, 4)}
				for i := range stack.Frames {
					stack.Frames[i].Name = name
					stack.Frames[i].Source = &token.Location{File: name, Path: name}
				}
				response := &dap.StackTraceResponse{}
				response.Body.StackFrames = translateStackFrames(stack, nil, "")
				msg = response
			}
			var out bytes.Buffer
			server := &Server{writer: &out}
			require.NoError(t, server.send(msg))
			t.Logf("complete DAP response bytes=%d cap=%d", out.Len(), lisp.DefaultMaxAlloc)
			require.LessOrEqual(t, out.Len(), lisp.DefaultMaxAlloc)
			require.Contains(t, out.String(), "truncated")
		})
	}
}

func TestDebuggerResponseCancellation(t *testing.T) {
	for _, canceled := range []bool{false, true} {
		env := lisp.NewEnv(nil)
		env.Runtime.MaxAlloc = 4096
		ctx, cancel := context.WithCancel(t.Context())
		defer cancel()
		fn := lisp.Fun("inspect", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			if canceled {
				cancel()
			}
			name := strings.Repeat("<", 1000)
			variables := &dap.VariablesResponse{}
			variables.Body.Variables = translateVariables([]debugger.ScopeBinding{{Name: name, Value: lisp.Int(1)}}, func(*lisp.LVal) int { return 0 }, nil, env)
			frames := &dap.StackTraceResponse{}
			frames.Body.StackFrames = translateStackFrames(&lisp.CallStack{Frames: []lisp.CallFrame{{Name: name, Source: &token.Location{File: name, Path: name}}}}, nil, "", env)
			for _, msg := range []dap.Message{variables, frames} {
				var out bytes.Buffer
				require.NoError(t, (&Server{writer: &out}).send(msg))
				require.LessOrEqual(t, out.Len(), env.Runtime.MaxAllocBytes())
				require.Contains(t, out.String(), "truncated")
				parts := bytes.SplitN(out.Bytes(), []byte("\r\n\r\n"), 2)
				require.Len(t, parts, 2)
				require.True(t, json.Valid(parts[1]))
			}
			return lisp.Nil()
		})
		env.EvalContext(ctx, lisp.SExpr([]*lisp.LVal{fn}))
	}
}

func TestPausedResponseCancellation(t *testing.T) {
	env := lisp.NewEnv(nil)
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	ctx, cancel := context.WithCancel(t.Context())
	defer cancel()
	eng := debugger.New(debugger.WithStopOnEntry(true))
	eng.Enable()
	env.Runtime.Debugger = eng
	called := false
	eng.SetEventCallback(func(event debugger.Event) {
		if event.Type != debugger.EventStopped {
			return
		}
		called = true
		defer eng.Resume()
		cancel()
		paused, _ := eng.PausedState()
		require.ErrorIs(t, paused.Context().Err(), context.Canceled)
		response := &dap.VariablesResponse{}
		bindings := make([]debugger.ScopeBinding, 200)
		for i := range bindings {
			bindings[i] = debugger.ScopeBinding{Name: "ordinary-name", Value: lisp.Int(1)}
		}
		response.Body.Variables = translateVariables(bindings, func(*lisp.LVal) int { return 0 }, eng)
		var out bytes.Buffer
		require.NoError(t, (&Server{writer: &out, engine: eng}).send(response))
		require.Less(t, out.Len(), 1024)
		require.Contains(t, out.String(), "truncated")
	})
	expr := lisp.Int(1)
	expr.SetSource(&token.Location{File: "test.lisp", Line: 1, Col: 1})
	env.EvalContext(ctx, expr)
	require.True(t, called)
	require.NoError(t, env.Context().Err(), "restore context after resuming")
}

func TestSendEncodedResponseBudget(t *testing.T) {
	response := &dap.VariablesResponse{}
	response.Response = dap.Response{ProtocolMessage: dap.ProtocolMessage{Seq: 1, Type: "response"}, RequestSeq: 1, Success: true, Command: "variables"}
	response.Body.Variables = []dap.Variable{{Name: "x", Value: strings.Repeat("<", lisp.DefaultMaxAlloc/2)}}
	var out bytes.Buffer
	require.NoError(t, (&Server{writer: &out}).send(response))
	require.LessOrEqual(t, out.Len(), lisp.DefaultMaxAlloc)
	decoded, err := dap.ReadProtocolMessage(bufio.NewReader(&out))
	require.NoError(t, err)
	failure, ok := decoded.(*dap.ErrorResponse)
	require.True(t, ok)
	require.Contains(t, failure.Message, "truncated")
}
