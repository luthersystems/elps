package dapserver

import (
	"bufio"
	"io"
	"net"
	"os"
	"testing"
	"time"

	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDAPServer_InitializeAndDisconnect(t *testing.T) {
	t.Parallel()
	e := debugger.New()
	e.Enable()
	srv := New(e)

	// Use a net.Pipe to simulate a DAP client/server connection.
	client, server := net.Pipe()
	defer client.Close() //nolint:errcheck

	go func() {
		_ = srv.ServeConn(server)
	}()

	reader := bufio.NewReader(client)

	// Send Initialize request.
	sendDAPRequest(t, client, &dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 1, Type: "request"},
			Command:         "initialize",
		},
		Arguments: dap.InitializeRequestArguments{
			AdapterID:     "elps",
			LinesStartAt1: true,
		},
	})

	// Read Initialize response.
	msg1, err := dap.ReadProtocolMessage(reader)
	require.NoError(t, err)
	initResp, ok := msg1.(*dap.InitializeResponse)
	require.True(t, ok, "expected InitializeResponse, got %T", msg1)
	assert.True(t, initResp.Success)
	assert.True(t, initResp.Body.SupportsConditionalBreakpoints)

	// Read Initialized event.
	msg2, err := dap.ReadProtocolMessage(reader)
	require.NoError(t, err)
	_, ok = msg2.(*dap.InitializedEvent)
	assert.True(t, ok, "expected InitializedEvent, got %T", msg2)

	// Send Disconnect request.
	sendDAPRequest(t, client, &dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 2, Type: "request"},
			Command:         "disconnect",
		},
	})

	// Read Disconnect response.
	msg3, err := dap.ReadProtocolMessage(reader)
	require.NoError(t, err)
	disconnResp, ok := msg3.(*dap.DisconnectResponse)
	require.True(t, ok, "expected DisconnectResponse, got %T", msg3)
	assert.True(t, disconnResp.Success)

	// Read Terminated event.
	msg4, err := dap.ReadProtocolMessage(reader)
	require.NoError(t, err)
	_, ok = msg4.(*dap.TerminatedEvent)
	assert.True(t, ok, "expected TerminatedEvent, got %T", msg4)
}

func TestDAPServer_SetBreakpoints(t *testing.T) {
	t.Parallel()
	e := debugger.New()
	e.Enable()
	srv := New(e)

	client, server := net.Pipe()
	defer client.Close() //nolint:errcheck

	go func() {
		_ = srv.ServeConn(server)
	}()

	reader := bufio.NewReader(client)

	// Initialize.
	sendDAPRequest(t, client, &dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 1, Type: "request"},
			Command:         "initialize",
		},
	})
	readDAPMessage(t, reader) // InitializeResponse
	readDAPMessage(t, reader) // InitializedEvent

	// Set breakpoints.
	sendDAPRequest(t, client, &dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 2, Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source: dap.Source{Path: "test.lisp"},
			Breakpoints: []dap.SourceBreakpoint{
				{Line: 5, Condition: "(> x 10)"},
				{Line: 10},
			},
		},
	})

	msg := readDAPMessage(t, reader)
	bpResp, ok := msg.(*dap.SetBreakpointsResponse)
	require.True(t, ok, "expected SetBreakpointsResponse, got %T", msg)
	assert.True(t, bpResp.Success)
	assert.Len(t, bpResp.Body.Breakpoints, 2)
	assert.Equal(t, 5, bpResp.Body.Breakpoints[0].Line)
	assert.True(t, bpResp.Body.Breakpoints[0].Verified)
	assert.Equal(t, 10, bpResp.Body.Breakpoints[1].Line)

	// Verify breakpoints are in the engine with correct conditions.
	all := e.Breakpoints().All()
	assert.Len(t, all, 2)
	condMap := make(map[int]string)
	for _, bp := range all {
		condMap[bp.Line] = bp.Condition
	}
	assert.Equal(t, "(> x 10)", condMap[5], "condition should propagate to engine store")
	assert.Empty(t, condMap[10], "unconditional breakpoint should have empty condition")

	// Cleanup.
	sendDAPRequest(t, client, &dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 3, Type: "request"},
			Command:         "disconnect",
		},
	})
	readDAPMessage(t, reader)
	readDAPMessage(t, reader)
}

func TestDAPServer_Threads(t *testing.T) {
	t.Parallel()
	e := debugger.New()
	e.Enable()
	srv := New(e)

	client, server := net.Pipe()
	defer client.Close() //nolint:errcheck

	go func() {
		_ = srv.ServeConn(server)
	}()

	reader := bufio.NewReader(client)

	// Initialize.
	sendDAPRequest(t, client, &dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 1, Type: "request"},
			Command:         "initialize",
		},
	})
	readDAPMessage(t, reader)
	readDAPMessage(t, reader)

	// Request threads.
	sendDAPRequest(t, client, &dap.ThreadsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 2, Type: "request"},
			Command:         "threads",
		},
	})

	msg := readDAPMessage(t, reader)
	threadsResp, ok := msg.(*dap.ThreadsResponse)
	require.True(t, ok, "expected ThreadsResponse, got %T", msg)
	assert.Len(t, threadsResp.Body.Threads, 1)
	assert.Equal(t, elpsThreadID, threadsResp.Body.Threads[0].Id)
	assert.Equal(t, "ELPS Main", threadsResp.Body.Threads[0].Name)

	// Cleanup.
	sendDAPRequest(t, client, &dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 3, Type: "request"},
			Command:         "disconnect",
		},
	})
	readDAPMessage(t, reader)
	readDAPMessage(t, reader)
}

// TestDAPServer_FullSession exercises the complete debug flow:
// initialize → set breakpoint → launch program → read stopped event →
// stackTrace → scopes → variables → evaluate → continue → disconnect.
func TestDAPServer_FullSession(t *testing.T) {
	t.Parallel()
	e := debugger.New(debugger.WithStopOnEntry(true))
	e.Enable()
	srv := New(e)

	client, server := net.Pipe()
	defer client.Close() //nolint:errcheck

	go func() {
		_ = srv.ServeConn(server)
	}()

	reader := bufio.NewReader(client)
	seq := 0
	nextSeq := func() int {
		seq++
		return seq
	}

	// === Initialize ===
	sendDAPRequest(t, client, &dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "initialize",
		},
		Arguments: dap.InitializeRequestArguments{
			AdapterID:     "elps",
			LinesStartAt1: true,
		},
	})
	msg := readDAPMessage(t, reader)
	initResp, ok := msg.(*dap.InitializeResponse)
	require.True(t, ok, "expected InitializeResponse, got %T", msg)
	assert.True(t, initResp.Success)
	readDAPMessage(t, reader) // InitializedEvent

	// === Set Breakpoints ===
	// Set a breakpoint on line 2 (inside the function body).
	// Multi-line program:
	//   Line 1: (defun add (a b)
	//   Line 2:   (+ a b))
	//   Line 3: (add 1 2)
	sendDAPRequest(t, client, &dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 2}},
		},
	})
	msg = readDAPMessage(t, reader)
	bpResp, ok := msg.(*dap.SetBreakpointsResponse)
	require.True(t, ok, "expected SetBreakpointsResponse, got %T", msg)
	assert.True(t, bpResp.Success)
	assert.Len(t, bpResp.Body.Breakpoints, 1)
	assert.True(t, bpResp.Body.Breakpoints[0].Verified)

	// === Configuration Done ===
	sendDAPRequest(t, client, &dap.ConfigurationDoneRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "configurationDone",
		},
	})
	readDAPMessage(t, reader) // ConfigurationDoneResponse

	// === Launch program ===
	env := newDAPTestEnv(t, e)
	resultCh := make(chan *lisp.LVal, 1)
	program := "(defun add (a b)\n  (+ a b))\n(add 1 2)"
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// === First stop: stop-on-entry (top level) ===
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not stop on entry")

	stoppedMsg := readDAPMessage(t, reader)
	stoppedEvt, ok := stoppedMsg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", stoppedMsg)
	assert.Equal(t, elpsThreadID, stoppedEvt.Body.ThreadId)

	// Continue past stop-on-entry — we want to hit the breakpoint inside add.
	sendDAPRequest(t, client, &dap.ContinueRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "continue",
		},
		Arguments: dap.ContinueArguments{ThreadId: elpsThreadID},
	})
	readDAPMessage(t, reader) // ContinueResponse

	// === Second stop: breakpoint inside add at line 2 ===
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")

	stoppedMsg = readDAPMessage(t, reader)
	stoppedEvt, ok = stoppedMsg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", stoppedMsg)
	assert.Equal(t, "breakpoint", stoppedEvt.Body.Reason)

	// === StackTrace (now inside function, should have frames) ===
	sendDAPRequest(t, client, &dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg = readDAPMessage(t, reader)
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	assert.True(t, stResp.Success)
	require.Greater(t, len(stResp.Body.StackFrames), 0, "expected at least one stack frame")

	topFrameID := stResp.Body.StackFrames[0].Id

	// === Scopes ===
	sendDAPRequest(t, client, &dap.ScopesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "scopes",
		},
		Arguments: dap.ScopesArguments{FrameId: topFrameID},
	})
	msg = readDAPMessage(t, reader)
	scopesResp, ok := msg.(*dap.ScopesResponse)
	require.True(t, ok, "expected ScopesResponse, got %T", msg)
	assert.True(t, scopesResp.Success)
	require.GreaterOrEqual(t, len(scopesResp.Body.Scopes), 1, "expected at least one scope")

	// Get the local scope's variables reference.
	var localRef int
	for _, s := range scopesResp.Body.Scopes {
		if s.Name == "Local" {
			localRef = s.VariablesReference
			break
		}
	}
	require.Greater(t, localRef, 0, "expected local scope reference")

	// === Variables ===
	sendDAPRequest(t, client, &dap.VariablesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "variables",
		},
		Arguments: dap.VariablesArguments{VariablesReference: localRef},
	})
	msg = readDAPMessage(t, reader)
	varsResp, ok := msg.(*dap.VariablesResponse)
	require.True(t, ok, "expected VariablesResponse, got %T", msg)
	assert.True(t, varsResp.Success)

	// Verify variable values (a=1, b=2 from add(1, 2)).
	varMap := make(map[string]string)
	for _, v := range varsResp.Body.Variables {
		varMap[v.Name] = v.Value
	}
	assert.Equal(t, "1", varMap["a"], "expected a=1")
	assert.Equal(t, "2", varMap["b"], "expected b=2")

	// === Evaluate ===
	sendDAPRequest(t, client, &dap.EvaluateRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "evaluate",
		},
		Arguments: dap.EvaluateArguments{
			Expression: "(+ 10 20)",
			FrameId:    topFrameID,
		},
	})
	msg = readDAPMessage(t, reader)
	evalResp, ok := msg.(*dap.EvaluateResponse)
	require.True(t, ok, "expected EvaluateResponse, got %T", msg)
	assert.True(t, evalResp.Success)
	assert.Equal(t, "30", evalResp.Body.Result)

	// === Continue to finish ===
	sendDAPRequest(t, client, &dap.ContinueRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "continue",
		},
		Arguments: dap.ContinueArguments{ThreadId: elpsThreadID},
	})
	msg = readDAPMessage(t, reader)
	contResp, ok := msg.(*dap.ContinueResponse)
	require.True(t, ok, "expected ContinueResponse, got %T", msg)
	assert.True(t, contResp.Success)

	// Wait for program to finish.
	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout waiting for program result")
	}

	// === Disconnect ===
	sendDAPRequest(t, client, &dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "disconnect",
		},
	})
	msg = readDAPMessage(t, reader)
	disconnResp, ok := msg.(*dap.DisconnectResponse)
	require.True(t, ok, "expected DisconnectResponse, got %T", msg)
	assert.True(t, disconnResp.Success)

	// Read Terminated event.
	msg = readDAPMessage(t, reader)
	_, ok = msg.(*dap.TerminatedEvent)
	assert.True(t, ok, "expected TerminatedEvent, got %T", msg)
}

// --- helpers ---

// newDAPTestEnv creates a minimal ELPS environment wired to a debugger engine.
func newDAPTestEnv(t *testing.T, dbg *debugger.Engine) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Debugger = dbg
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "InitializeUserEnv failed: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil(), "LoadLibrary failed: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "InPackage failed: %v", rc)
	return env
}

func sendDAPRequest(t *testing.T, w io.Writer, msg dap.Message) {
	t.Helper()
	err := dap.WriteProtocolMessage(w, msg)
	require.NoError(t, err)
}

func readDAPMessage(t *testing.T, r *bufio.Reader) dap.Message {
	t.Helper()
	done := make(chan dap.Message, 1)
	errCh := make(chan error, 1)
	go func() {
		msg, err := dap.ReadProtocolMessage(r)
		if err != nil {
			errCh <- err
			return
		}
		done <- msg
	}()
	select {
	case msg := <-done:
		return msg
	case err := <-errCh:
		t.Fatalf("error reading DAP message: %v", err)
		return nil
	case <-time.After(5 * time.Second):
		t.Fatal("timeout reading DAP message")
		return nil
	}
}

// dapTestSession reduces boilerplate for DAP protocol tests.
type dapTestSession struct {
	t      *testing.T
	engine *debugger.Engine
	client net.Conn
	reader *bufio.Reader
	seq    int
}

func setupDAPSession(t *testing.T, opts ...debugger.Option) *dapTestSession {
	t.Helper()
	e := debugger.New(opts...)
	e.Enable()
	srv := New(e)

	client, server := net.Pipe()
	t.Cleanup(func() { client.Close() }) //nolint:errcheck,gosec

	go func() {
		_ = srv.ServeConn(server)
	}()

	s := &dapTestSession{
		t:      t,
		engine: e,
		client: client,
		reader: bufio.NewReader(client),
	}

	// Initialize the DAP session.
	s.send(&dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "initialize",
		},
		Arguments: dap.InitializeRequestArguments{
			AdapterID:     "elps",
			LinesStartAt1: true,
		},
	})
	msg := s.read()
	initResp, ok := msg.(*dap.InitializeResponse)
	require.True(t, ok, "expected InitializeResponse, got %T", msg)
	require.True(t, initResp.Success)
	s.read() // InitializedEvent

	return s
}

func (s *dapTestSession) nextSeq() int {
	s.seq++
	return s.seq
}

func (s *dapTestSession) send(msg dap.Message) {
	sendDAPRequest(s.t, s.client, msg)
}

func (s *dapTestSession) read() dap.Message {
	return readDAPMessage(s.t, s.reader)
}

func (s *dapTestSession) configDone() {
	s.send(&dap.ConfigurationDoneRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "configurationDone",
		},
	})
	s.read() // ConfigurationDoneResponse
}

func (s *dapTestSession) continueExec() {
	s.send(&dap.ContinueRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "continue",
		},
		Arguments: dap.ContinueArguments{ThreadId: elpsThreadID},
	})
	s.read() // ContinueResponse
}

func (s *dapTestSession) disconnect() {
	s.send(&dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "disconnect",
		},
	})
	s.read() // DisconnectResponse
	s.read() // TerminatedEvent
}

func TestDAPServer_StepNext(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint inside outer's body (line 3: call to inner).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 3}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	// outer calls inner on line 3, then has another expression on line 4.
	// Step-over from line 3 should NOT enter inner's body.
	go func() {
		res := env.LoadString("test",
			"(defun inner (x) (+ x 100))\n"+
				"(defun outer (n)\n"+
				"  (inner n)\n"+ // line 3
				"  (+ n 1))\n"+ // line 4
				"(outer 5)")
		resultCh <- res
	}()

	// Wait for breakpoint inside outer (line 3).
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Get initial stack trace — should be inside outer.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	initialFrameCount := len(stResp.Body.StackFrames)
	require.Greater(t, initialFrameCount, 0)
	assert.Equal(t, 3, stResp.Body.StackFrames[0].Line, "should be on line 3")

	// Clear breakpoints to avoid re-fire.
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{},
		},
	})
	s.read() // SetBreakpointsResponse

	// Step-over (inner n) repeatedly until we reach line 4.
	for range 20 {
		s.send(&dap.NextRequest{
			Request: dap.Request{
				ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
				Command:         "next",
			},
			Arguments: dap.NextArguments{ThreadId: elpsThreadID},
		})
		s.read() // NextResponse

		require.Eventually(t, func() bool {
			return s.engine.IsPaused()
		}, 2*time.Second, 10*time.Millisecond)
		s.read() // StoppedEvent

		// Check stack trace — frame count should NOT exceed initial (did not enter inner).
		s.send(&dap.StackTraceRequest{
			Request: dap.Request{
				ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
				Command:         "stackTrace",
			},
			Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
		})
		msg = s.read()
		stResp, ok = msg.(*dap.StackTraceResponse)
		require.True(t, ok)
		assert.LessOrEqual(t, len(stResp.Body.StackFrames), initialFrameCount,
			"step-over should not increase stack depth (entered nested function)")

		if stResp.Body.StackFrames[0].Line == 4 {
			break
		}
	}
	assert.Equal(t, 4, stResp.Body.StackFrames[0].Line,
		"step-over should eventually reach line 4")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

func TestDAPServer_StepIn(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	// Wait for stop-on-entry.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Send StepIn — should advance to next expression.
	s.send(&dap.StepInRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stepIn",
		},
		Arguments: dap.StepInArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	stepResp, ok := msg.(*dap.StepInResponse)
	require.True(t, ok, "expected StepInResponse, got %T", msg)
	assert.True(t, stepResp.Success)

	// Should pause again on next expression.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Verify we actually paused (PausedState returns non-nil).
	_, pausedExpr := s.engine.PausedState()
	require.NotNil(t, pausedExpr, "step-in should have paused on an expression")
	require.NotNil(t, pausedExpr.Source, "paused expression should have source location")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

func TestDAPServer_StepOut(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint inside function body (line 2).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 2}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	// Multi-line: breakpoint fires on line 2 inside f, step-out should
	// return to caller on line 3.
	go func() {
		res := env.LoadString("test", "(defun f (x)\n  (+ x 1))\n(f 10)\n(+ 1 1)")
		resultCh <- res
	}()

	// Wait for breakpoint inside function (line 2).
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Get stack trace to record depth inside function.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	insideFrameCount := len(stResp.Body.StackFrames)
	require.Greater(t, insideFrameCount, 0, "should have at least 1 frame inside function")
	assert.Equal(t, 2, stResp.Body.StackFrames[0].Line,
		"should be paused on line 2 inside function body")

	// Clear breakpoints before stepping out to avoid re-fire.
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{},
		},
	})
	s.read() // SetBreakpointsResponse

	// Send StepOut — should return to caller scope.
	s.send(&dap.StepOutRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stepOut",
		},
		Arguments: dap.StepOutArguments{ThreadId: elpsThreadID},
	})
	msg = s.read()
	stepResp, ok := msg.(*dap.StepOutResponse)
	require.True(t, ok, "expected StepOutResponse, got %T", msg)
	assert.True(t, stepResp.Success)

	// Should pause after returning from f.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause after step-out")
	s.read() // StoppedEvent

	// Verify stack depth decreased after step-out.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg = s.read()
	stResp, ok = msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	assert.Less(t, len(stResp.Body.StackFrames), insideFrameCount,
		"step-out should decrease stack depth")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

func TestDAPServer_SetExceptionBreakpoints(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	s.send(&dap.SetExceptionBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setExceptionBreakpoints",
		},
		Arguments: dap.SetExceptionBreakpointsArguments{
			Filters: []string{"all"},
		},
	})
	msg := s.read()
	resp, ok := msg.(*dap.SetExceptionBreakpointsResponse)
	require.True(t, ok, "expected SetExceptionBreakpointsResponse, got %T", msg)
	assert.True(t, resp.Success)

	// Verify engine state.
	assert.Equal(t, debugger.ExceptionBreakAll, s.engine.Breakpoints().ExceptionBreak())

	s.disconnect()
}

func TestDAPServer_EvaluateNotPaused(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)
	s.configDone()

	// Evaluate without being paused — should fail gracefully.
	// go-dap deserializes responses with Success=false as ErrorResponse.
	s.send(&dap.EvaluateRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "evaluate",
		},
		Arguments: dap.EvaluateArguments{
			Expression: "(+ 1 2)",
		},
	})
	msg := s.read()
	errResp, ok := msg.(*dap.ErrorResponse)
	require.True(t, ok, "expected ErrorResponse, got %T", msg)
	assert.False(t, errResp.Success)
	assert.Equal(t, "not paused", errResp.Message)

	s.disconnect()
}

func TestDAPServer_StackTracePaging(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Set breakpoint inside function body (line 2).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 2}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(defun f (x)\n  (+ x 1))\n(f 10)")
		resultCh <- res
	}()

	// Stop on entry, then continue to breakpoint.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent (entry)
	s.continueExec()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent (breakpoint)

	// Full stack trace.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	totalFrames := stResp.Body.TotalFrames
	require.Greater(t, totalFrames, 0, "expected at least one stack frame")

	// Paged request: start at frame 0, limit 1.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{
			ThreadId:   elpsThreadID,
			StartFrame: 0,
			Levels:     1,
		},
	})
	msg = s.read()
	pagedResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	assert.Equal(t, totalFrames, pagedResp.Body.TotalFrames, "total should not change with paging")
	assert.Len(t, pagedResp.Body.StackFrames, 1, "should return exactly 1 frame")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

func TestDAPServer_PackageScopeVariables(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Set breakpoint inside function body (line 2).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 2}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(defun add (a b)\n  (+ a b))\n(add 1 2)")
		resultCh <- res
	}()

	// Stop on entry, then continue to breakpoint inside function.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent (entry)
	s.continueExec()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent (breakpoint)

	// Get stack trace to trigger frame caching.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	require.Greater(t, len(stResp.Body.StackFrames), 0, "expected stack frames")

	topFrameID := stResp.Body.StackFrames[0].Id

	// Request scopes for the top frame.
	s.send(&dap.ScopesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "scopes",
		},
		Arguments: dap.ScopesArguments{FrameId: topFrameID},
	})
	msg = s.read()
	scopesResp, ok := msg.(*dap.ScopesResponse)
	require.True(t, ok, "expected ScopesResponse, got %T", msg)
	require.Len(t, scopesResp.Body.Scopes, 2, "expected Local and Package scopes")

	// Find the package scope reference.
	var pkgRef int
	for _, scope := range scopesResp.Body.Scopes {
		if scope.Name == "Package" {
			pkgRef = scope.VariablesReference
			break
		}
	}
	require.Greater(t, pkgRef, 0, "expected package scope reference")

	// Request package scope variables.
	s.send(&dap.VariablesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "variables",
		},
		Arguments: dap.VariablesArguments{VariablesReference: pkgRef},
	})
	msg = s.read()
	varsResp, ok := msg.(*dap.VariablesResponse)
	require.True(t, ok, "expected VariablesResponse, got %T", msg)
	assert.True(t, varsResp.Success)
	// Package scope should contain user-defined symbols (at least 'add').
	require.Greater(t, len(varsResp.Body.Variables), 0, "expected package variables")
	// Verify the 'add' function is among the package variables.
	var foundAdd bool
	for _, v := range varsResp.Body.Variables {
		if v.Name == "add" {
			foundAdd = true
			break
		}
	}
	assert.True(t, foundAdd, "expected 'add' function in package scope variables, got: %v",
		func() []string {
			names := make([]string, len(varsResp.Body.Variables))
			for i, v := range varsResp.Body.Variables {
				names[i] = v.Name
			}
			return names
		}())

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

// TestDAPServer_LateConnectStoppedEvent verifies that when the eval goroutine
// pauses (stopOnEntry) before the DAP client connects, the client receives
// the stopped event when it sends configurationDone.
func TestDAPServer_LateConnectStoppedEvent(t *testing.T) {
	t.Parallel()
	e := debugger.New(debugger.WithStopOnEntry(true))
	e.Enable()

	env := newDAPTestEnv(t, e)

	// Start evaluation BEFORE any DAP client connects. The engine will
	// pause on the first expression (stopOnEntry), but no event callback
	// is registered yet.
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	// Wait for the engine to actually pause.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not stop on entry")

	// NOW connect a DAP client (late connect).
	srv := New(e)
	client, server := net.Pipe()
	t.Cleanup(func() { client.Close() }) //nolint:errcheck,gosec

	go func() {
		_ = srv.ServeConn(server)
	}()

	reader := bufio.NewReader(client)
	seq := 0
	nextSeq := func() int {
		seq++
		return seq
	}

	// Initialize.
	sendDAPRequest(t, client, &dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "initialize",
		},
	})
	readDAPMessage(t, reader) // InitializeResponse
	readDAPMessage(t, reader) // InitializedEvent

	// ConfigurationDone — this should detect the pending pause and send
	// a stopped event.
	sendDAPRequest(t, client, &dap.ConfigurationDoneRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "configurationDone",
		},
	})
	readDAPMessage(t, reader) // ConfigurationDoneResponse

	// Read the stopped event that configurationDone should have sent.
	msg := readDAPMessage(t, reader)
	stoppedEvt, ok := msg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent after late connect, got %T", msg)
	assert.Equal(t, "entry", stoppedEvt.Body.Reason)
	assert.Equal(t, elpsThreadID, stoppedEvt.Body.ThreadId)

	// Continue to let the program finish.
	sendDAPRequest(t, client, &dap.ContinueRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "continue",
		},
		Arguments: dap.ContinueArguments{ThreadId: elpsThreadID},
	})
	readDAPMessage(t, reader) // ContinueResponse

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type)
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout waiting for eval result")
	}

	// Disconnect.
	sendDAPRequest(t, client, &dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: nextSeq(), Type: "request"},
			Command:         "disconnect",
		},
	})
	readDAPMessage(t, reader) // DisconnectResponse
	readDAPMessage(t, reader) // TerminatedEvent
}

// TestDAPServer_ReadyChSignaledOnConfigDone verifies that the engine's
// ReadyCh is closed when configurationDone is received.
func TestDAPServer_ReadyChSignaledOnConfigDone(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// ReadyCh should not be closed yet.
	select {
	case <-s.engine.ReadyCh():
		t.Fatal("ReadyCh should not be closed before configurationDone")
	default:
	}

	s.configDone()

	// ReadyCh should now be closed.
	select {
	case <-s.engine.ReadyCh():
		// expected
	case <-time.After(time.Second):
		t.Fatal("ReadyCh should be closed after configurationDone")
	}

	s.disconnect()
}

// TestDAPServer_PathNormalization verifies that breakpoints set with absolute
// paths from the IDE match ELPS runtime locations that use basenames.
func TestDAPServer_PathNormalization(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Set breakpoints using absolute paths (like VS Code sends).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "/Users/dev/project/phylum/test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 2}},
		},
	})
	s.read() // SetBreakpointsResponse

	s.configDone()

	// The ELPS runtime uses "test" as the source name (basename-like).
	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(defun f (x)\n  (+ x 1))\n(f 10)")
		resultCh <- res
	}()

	// First stop: stop-on-entry.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent (entry)
	s.continueExec()

	// Second stop: breakpoint on line 2 should fire even though the path
	// was set as /Users/dev/project/phylum/test but runtime uses "test".
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "breakpoint should fire with normalized path")
	msg := s.read()
	stoppedEvt, ok := msg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", msg)
	assert.Equal(t, "breakpoint", stoppedEvt.Body.Reason)

	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

// TestDAPServer_FileDebugSession exercises the full debug lifecycle with a
// real file, mirroring what `elps debug --stop-on-entry testdata/simple.lisp`
// does: set up environment with FSLibrary, load file, pause at breakpoint
// inside function, inspect variables, evaluate expression, and continue.
func TestDAPServer_FileDebugSession(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Set breakpoint on line 3 (inside add function body: (+ a b)).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "simple.lisp"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 3}},
		},
	})
	msg := s.read()
	bpResp, ok := msg.(*dap.SetBreakpointsResponse)
	require.True(t, ok, "expected SetBreakpointsResponse, got %T", msg)
	assert.True(t, bpResp.Success)

	s.configDone()

	// Set up ELPS environment with FSLibrary, like cmd/debug.go does.
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS("testdata")}
	env.Runtime.Debugger = s.engine
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "InitializeUserEnv failed: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil(), "LoadLibrary failed: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "InPackage failed: %v", rc)

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadFile("simple.lisp")
		resultCh <- res
	}()

	// === Stop on entry ===
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not stop on entry")
	s.read() // StoppedEvent

	// Continue past stop-on-entry to hit breakpoint inside add.
	s.continueExec()

	// === Breakpoint inside add ===
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")
	stoppedMsg := s.read()
	stoppedEvt, ok := stoppedMsg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", stoppedMsg)
	assert.Equal(t, "breakpoint", stoppedEvt.Body.Reason)

	// === StackTrace ===
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg = s.read()
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	require.Greater(t, len(stResp.Body.StackFrames), 0, "expected stack frames")
	// Top frame should reference simple.lisp.
	assert.Contains(t, stResp.Body.StackFrames[0].Source.Path, "simple.lisp")

	topFrameID := stResp.Body.StackFrames[0].Id

	// === Scopes + Variables ===
	s.send(&dap.ScopesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "scopes",
		},
		Arguments: dap.ScopesArguments{FrameId: topFrameID},
	})
	msg = s.read()
	scopesResp, ok := msg.(*dap.ScopesResponse)
	require.True(t, ok, "expected ScopesResponse, got %T", msg)

	var localRef int
	for _, scope := range scopesResp.Body.Scopes {
		if scope.Name == "Local" {
			localRef = scope.VariablesReference
			break
		}
	}
	require.Greater(t, localRef, 0, "expected local scope reference")

	s.send(&dap.VariablesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "variables",
		},
		Arguments: dap.VariablesArguments{VariablesReference: localRef},
	})
	msg = s.read()
	varsResp, ok := msg.(*dap.VariablesResponse)
	require.True(t, ok, "expected VariablesResponse, got %T", msg)
	assert.True(t, varsResp.Success)

	// Verify function parameters: a=1, b=2.
	varMap := make(map[string]string)
	for _, v := range varsResp.Body.Variables {
		varMap[v.Name] = v.Value
	}
	assert.Equal(t, "1", varMap["a"], "expected a=1")
	assert.Equal(t, "2", varMap["b"], "expected b=2")

	// === Evaluate ===
	s.send(&dap.EvaluateRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "evaluate",
		},
		Arguments: dap.EvaluateArguments{
			Expression: "(+ a b)",
			FrameId:    topFrameID,
		},
	})
	msg = s.read()
	evalResp, ok := msg.(*dap.EvaluateResponse)
	require.True(t, ok, "expected EvaluateResponse, got %T", msg)
	assert.True(t, evalResp.Success)
	assert.Equal(t, "3", evalResp.Body.Result)

	// === Continue to finish ===
	s.continueExec()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout waiting for program result")
	}

	s.disconnect()
}

// dialWithRetry retries net.Dial until it succeeds or the deadline passes.
// This replaces time.Sleep for waiting on a server to start listening.
func dialWithRetry(t *testing.T, addr string, timeout time.Duration) net.Conn {
	t.Helper()
	deadline := time.Now().Add(timeout)
	for {
		conn, err := net.DialTimeout("tcp", addr, 100*time.Millisecond)
		if err == nil {
			return conn
		}
		if time.Now().After(deadline) {
			t.Fatalf("failed to connect to %s after %v: %v", addr, timeout, err)
		}
		time.Sleep(5 * time.Millisecond)
	}
}

// TestServeTCPLoop_MultipleConnections verifies Bug 3: the old ServeTCP
// accepted exactly one connection. After the first client disconnected,
// the server exited and no further debugging was possible.
//
// ServeTCPLoop fixes this by looping on Accept. This test connects twice
// to verify the server survives client reconnections.
func TestServeTCPLoop_MultipleConnections(t *testing.T) {
	t.Parallel()
	e := debugger.New()
	e.Enable()
	srv := New(e)

	// Use a random available port.
	ln, err := net.Listen("tcp", "localhost:0")
	require.NoError(t, err)
	addr := ln.Addr().String()
	ln.Close() //nolint:errcheck,gosec // best-effort cleanup for port reservation

	go func() {
		if err := srv.ServeTCPLoop(addr); err != nil {
			// Accept error after test cleanup is expected.
			t.Logf("ServeTCPLoop exited: %v", err)
		}
	}()

	// --- First connection (retry until server is listening) ---
	conn1 := dialWithRetry(t, addr, 2*time.Second)
	reader1 := bufio.NewReader(conn1)

	sendDAPRequest(t, conn1, &dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 1, Type: "request"},
			Command:         "initialize",
		},
	})
	msg := readDAPMessage(t, reader1)
	_, ok := msg.(*dap.InitializeResponse)
	require.True(t, ok, "expected InitializeResponse on first connection")
	readDAPMessage(t, reader1) // InitializedEvent

	// Disconnect first client.
	sendDAPRequest(t, conn1, &dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 2, Type: "request"},
			Command:         "disconnect",
		},
	})
	readDAPMessage(t, reader1) // DisconnectResponse
	readDAPMessage(t, reader1) // TerminatedEvent
	conn1.Close() //nolint:errcheck,gosec // best-effort cleanup

	// --- Second connection (retry until server re-enters Accept) ---
	conn2 := dialWithRetry(t, addr, 2*time.Second)
	reader2 := bufio.NewReader(conn2)

	sendDAPRequest(t, conn2, &dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 1, Type: "request"},
			Command:         "initialize",
		},
	})
	msg = readDAPMessage(t, reader2)
	initResp, ok := msg.(*dap.InitializeResponse)
	require.True(t, ok, "expected InitializeResponse on second connection, got %T", msg)
	assert.True(t, initResp.Success)

	conn2.Close() //nolint:errcheck,gosec // best-effort cleanup
}

func TestDAPServer_AttachRequest(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	s.send(&dap.AttachRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "attach",
		},
	})
	msg := s.read()
	resp, ok := msg.(*dap.AttachResponse)
	require.True(t, ok, "expected AttachResponse, got %T", msg)
	assert.True(t, resp.Success)

	s.disconnect()
}

func TestDAPServer_LaunchRequest(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	s.send(&dap.LaunchRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "launch",
		},
	})
	msg := s.read()
	resp, ok := msg.(*dap.LaunchResponse)
	require.True(t, ok, "expected LaunchResponse, got %T", msg)
	assert.True(t, resp.Success)

	s.disconnect()
}

func TestDAPServer_PauseRequest(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)
	s.configDone()

	env := newDAPTestEnv(t, s.engine)

	// Use a long-running recursive program so we can pause mid-execution.
	// Large countdown ensures the program is still running when the pause
	// request takes effect — even on slow CI machines.
	program := "(defun countdown (n)\n" +
		"  (if (<= n 0)\n" +
		"    0\n" +
		"    (countdown (- n 1))))\n" +
		"(countdown 100000)"

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Wait for eval goroutine to be well into execution before pausing.
	require.Eventually(t, func() bool {
		return s.engine.EvalCount() > 10
	}, 2*time.Second, time.Millisecond, "eval goroutine did not start")

	// Send Pause request.
	s.send(&dap.PauseRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "pause",
		},
		Arguments: dap.PauseArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	pauseResp, ok := msg.(*dap.PauseResponse)
	require.True(t, ok, "expected PauseResponse, got %T", msg)
	assert.True(t, pauseResp.Success)

	// Wait for the engine to actually pause.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause after pause request")

	// Read the stopped event.
	stoppedMsg := s.read()
	stoppedEvt, ok := stoppedMsg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", stoppedMsg)
	assert.Equal(t, "pause", stoppedEvt.Body.Reason)

	// Continue to let the program finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		if s.engine.IsPaused() {
			s.engine.Resume()
		}
		t.Fatal("timeout waiting for program to finish")
	}

	s.disconnect()
}

func TestDAPServer_SourceRequest(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	s.send(&dap.SourceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "source",
		},
		Arguments: dap.SourceArguments{
			SourceReference: 1,
		},
	})
	msg := s.read()
	// go-dap deserializes responses with Success=false as ErrorResponse.
	errResp, ok := msg.(*dap.ErrorResponse)
	require.True(t, ok, "expected ErrorResponse, got %T", msg)
	assert.False(t, errResp.Success)
	assert.Equal(t, "source not available", errResp.Message)

	s.disconnect()
}

func TestDAPServer_StackTraceUsesPausedExpr(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Set breakpoint on line 2 (inside function body: (+ a b)).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 2}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	program := "(defun add (a b)\n  (+ a b))\n(add 1 2)"
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// First stop: stop-on-entry.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent (entry)
	s.continueExec()

	// Second stop: breakpoint on line 2 inside add.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")
	s.read() // StoppedEvent (breakpoint)

	// Request stack trace — top frame should show line 2 (paused expression),
	// not the call site line 3.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	require.Greater(t, len(stResp.Body.StackFrames), 0)

	topFrame := stResp.Body.StackFrames[0]
	assert.Equal(t, 2, topFrame.Line,
		"top frame line should be 2 (paused expression), not the call site")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

func TestDAPServer_SetFunctionBreakpoints(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)
	s.configDone()

	env := newDAPTestEnv(t, s.engine)

	// Set a function breakpoint on "add".
	s.send(&dap.SetFunctionBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setFunctionBreakpoints",
		},
		Arguments: dap.SetFunctionBreakpointsArguments{
			Breakpoints: []dap.FunctionBreakpoint{{Name: "add"}},
		},
	})
	msg := s.read()
	fbResp, ok := msg.(*dap.SetFunctionBreakpointsResponse)
	require.True(t, ok, "expected SetFunctionBreakpointsResponse, got %T", msg)
	require.Len(t, fbResp.Body.Breakpoints, 1)
	assert.True(t, fbResp.Body.Breakpoints[0].Verified)

	// Run a program with "add" function.
	program := `(defun add (a b) (+ a b)) (add 10 20)`
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Should pause when "add" is entered.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause on function breakpoint")

	// Read stopped event.
	stoppedMsg := s.read()
	stoppedEvt, ok := stoppedMsg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", stoppedMsg)
	assert.Equal(t, "function breakpoint", stoppedEvt.Body.Reason)

	// Continue to finish.
	s.continueExec()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type)
		assert.Equal(t, 30, res.Int)
	case <-time.After(5 * time.Second):
		if s.engine.IsPaused() {
			s.engine.Resume()
		}
		t.Fatal("timeout")
	}

	s.disconnect()
}

func TestDAPServer_InitializeReportsFunctionBreakpointCapability(t *testing.T) {
	t.Parallel()
	e := debugger.New()
	e.Enable()
	srv := New(e)

	client, server := net.Pipe()
	defer client.Close() //nolint:errcheck

	go func() {
		_ = srv.ServeConn(server)
	}()

	reader := bufio.NewReader(client)

	sendDAPRequest(t, client, &dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 1, Type: "request"},
			Command:         "initialize",
		},
		Arguments: dap.InitializeRequestArguments{
			AdapterID: "elps",
		},
	})

	msg, err := dap.ReadProtocolMessage(reader)
	require.NoError(t, err)
	initResp, ok := msg.(*dap.InitializeResponse)
	require.True(t, ok)
	assert.True(t, initResp.Body.SupportsFunctionBreakpoints,
		"capabilities should report function breakpoints support")

	// Clean up.
	readDAPMessage(t, reader) // InitializedEvent
	sendDAPRequest(t, client, &dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 2, Type: "request"},
			Command:         "disconnect",
		},
	})
	readDAPMessage(t, reader) // DisconnectResponse
	readDAPMessage(t, reader) // TerminatedEvent
}

func TestDAPServer_SourceRootResolvesAbsolutePaths(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t,
		debugger.WithSourceRoot("/my/project"),
	)

	// Set breakpoint on line 2 (inside function body).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 2}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	program := "(defun add (a b)\n  (+ a b))\n(add 1 2)"
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Wait for breakpoint hit inside function.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Request stack trace.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	require.Greater(t, len(stResp.Body.StackFrames), 0)

	// Stack frame Source.Path should be resolved to an absolute path
	// using the sourceRoot prefix.
	topFrame := stResp.Body.StackFrames[0]
	require.NotNil(t, topFrame.Source)
	assert.Equal(t, "/my/project/test", topFrame.Source.Path,
		"expected source root + relative file path")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(2 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

func TestDAPServer_TerminatedEventOnProgramFinish(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)
	s.configDone()

	env := newDAPTestEnv(t, s.engine)

	// Run a simple program that finishes quickly, then call NotifyExit
	// from the eval goroutine (like cmd/debug.go does). NotifyExit fires
	// the event callback which writes to the pipe, so it must run on a
	// separate goroutine from the reader.
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		exitCode := 0
		if res.Type == lisp.LError {
			exitCode = 1
		}
		s.engine.NotifyExit(exitCode)
	}()

	// Should receive ExitedEvent.
	msg := s.read()
	exitedEvt, ok := msg.(*dap.ExitedEvent)
	require.True(t, ok, "expected ExitedEvent, got %T", msg)
	assert.Equal(t, 0, exitedEvt.Body.ExitCode)

	// Should receive TerminatedEvent.
	msg = s.read()
	_, ok = msg.(*dap.TerminatedEvent)
	assert.True(t, ok, "expected TerminatedEvent, got %T", msg)
}

func TestDAPServer_SetBreakpointsWithHitCondition(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint with a hit condition.
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source: dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{
				{Line: 4, HitCondition: "==2"},
			},
		},
	})
	msg := s.read()
	bpResp, ok := msg.(*dap.SetBreakpointsResponse)
	require.True(t, ok, "expected SetBreakpointsResponse, got %T", msg)
	assert.True(t, bpResp.Success)
	require.Len(t, bpResp.Body.Breakpoints, 1)
	assert.Equal(t, 4, bpResp.Body.Breakpoints[0].Line)

	s.configDone()

	env := newDAPTestEnv(t, s.engine)

	// Recursive countdown from 3. Breakpoint on line 4 (recursive call).
	// The recursive call executes 3 times (n=3,2,1). With ==2, it should
	// fire on the 2nd hit.
	program := "(defun countdown (n)\n" +
		"  (if (<= n 0)\n" +
		"    0\n" +
		"    (countdown (- n 1))))\n" +
		"(countdown 3)"

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Should pause on 2nd hit.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause on hit condition")
	stoppedMsg := s.read()
	stoppedEvt, ok := stoppedMsg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", stoppedMsg)
	assert.Equal(t, "breakpoint", stoppedEvt.Body.Reason)

	// Continue to finish. The program should complete without further stops
	// because ==2 fires only once (on the 2nd hit).
	s.continueExec()

	select {
	case <-resultCh:
		// Program completed without additional stops — correct for ==2.
	case <-time.After(5 * time.Second):
		if s.engine.IsPaused() {
			s.engine.Resume()
		}
		t.Fatal("timeout — hit condition fired more than once")
	}

	s.disconnect()
}

func TestDAPServer_LogPoint(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set a log point breakpoint (LogMessage set, no condition).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source: dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{
				{Line: 2, LogMessage: "result is {(+ a b)}"},
			},
		},
	})
	msg := s.read()
	bpResp, ok := msg.(*dap.SetBreakpointsResponse)
	require.True(t, ok, "expected SetBreakpointsResponse, got %T", msg)
	assert.True(t, bpResp.Success)

	s.configDone()

	env := newDAPTestEnv(t, s.engine)

	program := "(defun add (a b)\n  (+ a b))\n(add 3 4)"

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// The log point should emit an OutputEvent but NOT pause.
	// Read the output event.
	outputMsg := s.read()
	outputEvt, ok := outputMsg.(*dap.OutputEvent)
	require.True(t, ok, "expected OutputEvent, got %T", outputMsg)
	assert.Equal(t, "console", outputEvt.Body.Category)
	assert.Contains(t, outputEvt.Body.Output, "result is 7")

	// Program should complete without pausing.
	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type)
		assert.Equal(t, 7, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout — log point should not have paused")
	}

	s.disconnect()
}

func TestDAPServer_EvaluateWatch(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Set breakpoint inside function body (line 2).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 2}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(defun add (a b)\n  (+ a b))\n(add 10 20)")
		resultCh <- res
	}()

	// Stop on entry, then continue to breakpoint.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent (entry)
	s.continueExec()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")
	s.read() // StoppedEvent (breakpoint)

	// Get stack trace to cache frame environments.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrameID := stResp.Body.StackFrames[0].Id

	// Evaluate with "watch" context — VS Code sends this for watch expressions.
	s.send(&dap.EvaluateRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "evaluate",
		},
		Arguments: dap.EvaluateArguments{
			Expression: "a",
			FrameId:    topFrameID,
			Context:    "watch",
		},
	})
	msg = s.read()
	evalResp, ok := msg.(*dap.EvaluateResponse)
	require.True(t, ok, "expected EvaluateResponse, got %T", msg)
	assert.True(t, evalResp.Success)
	assert.Equal(t, "10", evalResp.Body.Result)

	// Evaluate a computed expression.
	s.send(&dap.EvaluateRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "evaluate",
		},
		Arguments: dap.EvaluateArguments{
			Expression: "(+ a b)",
			FrameId:    topFrameID,
			Context:    "watch",
		},
	})
	msg = s.read()
	evalResp, ok = msg.(*dap.EvaluateResponse)
	require.True(t, ok, "expected EvaluateResponse, got %T", msg)
	assert.True(t, evalResp.Success)
	assert.Equal(t, "30", evalResp.Body.Result)

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

// TestDAPServer_EvaluateWhilePausedInRecursion is a regression test for the
// critical deadlock where EvalInContext called env.Eval() without the
// evaluatingCondition guard, causing OnEval to re-enter and block on
// WaitIfPaused. This test breaks a recursive factorial at line 5 (the
// recursive branch), then evaluates (+ n 100) — which previously returned
// an ErrorResponse with "<error: error>".
func TestDAPServer_EvaluateWhilePausedInRecursion(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Set breakpoint on line 5 (* n (factorial (- n 1)))
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "factorial.lisp"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 5}},
		},
	})
	msg := s.read()
	bpResp, ok := msg.(*dap.SetBreakpointsResponse)
	require.True(t, ok, "expected SetBreakpointsResponse, got %T", msg)
	assert.True(t, bpResp.Success)

	s.configDone()

	// Set up ELPS environment with FSLibrary.
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS("testdata")}
	env.Runtime.Debugger = s.engine
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "InitializeUserEnv failed: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil(), "LoadLibrary failed: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "InPackage failed: %v", rc)

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadFile("factorial.lisp")
		resultCh <- res
	}()

	// Stop on entry.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not stop on entry")
	s.read() // StoppedEvent

	// Continue past stop-on-entry to hit breakpoint in factorial.
	s.continueExec()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")
	stoppedMsg := s.read()
	stoppedEvt, ok := stoppedMsg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", stoppedMsg)
	assert.Equal(t, "breakpoint", stoppedEvt.Body.Reason)

	// Get stack trace to find the top frame.
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg = s.read()
	stResp, ok := msg.(*dap.StackTraceResponse)
	require.True(t, ok, "expected StackTraceResponse, got %T", msg)
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrameID := stResp.Body.StackFrames[0].Id

	// Verify local variables are visible (the core of the locals bug fix).
	s.send(&dap.ScopesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "scopes",
		},
		Arguments: dap.ScopesArguments{FrameId: topFrameID},
	})
	msg = s.read()
	scopesResp, ok := msg.(*dap.ScopesResponse)
	require.True(t, ok, "expected ScopesResponse, got %T", msg)
	var localRef int
	for _, scope := range scopesResp.Body.Scopes {
		if scope.Name == "Local" {
			localRef = scope.VariablesReference
		}
	}
	require.Greater(t, localRef, 0, "expected local scope reference")

	s.send(&dap.VariablesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "variables",
		},
		Arguments: dap.VariablesArguments{VariablesReference: localRef},
	})
	msg = s.read()
	varsResp, ok := msg.(*dap.VariablesResponse)
	require.True(t, ok, "expected VariablesResponse, got %T", msg)
	varMap := make(map[string]string)
	for _, v := range varsResp.Body.Variables {
		varMap[v.Name] = v.Value
	}
	assert.Contains(t, varMap, "n",
		"local scope should contain 'n' even when paused in sub-expression")

	// Evaluate expression while paused (the core of the deadlock bug fix).
	s.send(&dap.EvaluateRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "evaluate",
		},
		Arguments: dap.EvaluateArguments{
			Expression: "(+ n 100)",
			FrameId:    topFrameID,
			Context:    "watch",
		},
	})
	msg = s.read()
	evalResp, ok := msg.(*dap.EvaluateResponse)
	require.True(t, ok, "expected EvaluateResponse (not ErrorResponse), got %T", msg)
	assert.True(t, evalResp.Success, "evaluate should succeed")
	// n is 5 on the first hit (factorial(5)), so (+ n 100) = 105.
	assert.Equal(t, "105", evalResp.Body.Result,
		"(+ n 100) should equal 105 when n=5")

	// Continue to finish — drain any more breakpoint hits.
	s.continueExec()
	for waitForEnginePause(t, s.engine, 500*time.Millisecond) {
		s.read() // StoppedEvent
		s.continueExec()
	}

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 120, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout waiting for program result")
	}

	s.disconnect()
}

// waitForEnginePause waits for the engine to enter paused state, returning true
// if it paused within the timeout, false otherwise.
func waitForEnginePause(t *testing.T, e *debugger.Engine, timeout time.Duration) bool {
	t.Helper()
	deadline := time.After(timeout)
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for {
		select {
		case <-deadline:
			return false
		case <-ticker.C:
			if e.IsPaused() {
				return true
			}
		}
	}
}

func TestDAPServer_InitializeReportsHitAndLogCapabilities(t *testing.T) {
	t.Parallel()
	e := debugger.New()
	e.Enable()
	srv := New(e)

	client, server := net.Pipe()
	defer client.Close() //nolint:errcheck

	go func() {
		_ = srv.ServeConn(server)
	}()

	reader := bufio.NewReader(client)

	sendDAPRequest(t, client, &dap.InitializeRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 1, Type: "request"},
			Command:         "initialize",
		},
		Arguments: dap.InitializeRequestArguments{AdapterID: "elps"},
	})

	msg, err := dap.ReadProtocolMessage(reader)
	require.NoError(t, err)
	initResp, ok := msg.(*dap.InitializeResponse)
	require.True(t, ok)
	assert.True(t, initResp.Body.SupportsHitConditionalBreakpoints,
		"capabilities should report hit conditional breakpoints support")
	assert.True(t, initResp.Body.SupportsLogPoints,
		"capabilities should report log points support")

	// Clean up.
	readDAPMessage(t, reader) // InitializedEvent
	sendDAPRequest(t, client, &dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 2, Type: "request"},
			Command:         "disconnect",
		},
	})
	readDAPMessage(t, reader) // DisconnectResponse
	readDAPMessage(t, reader) // TerminatedEvent
}
