package dapserver

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"os"
	"testing"
	"testing/fstest"
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
	assert.True(t, initResp.Body.SupportsSteppingGranularity)

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

// tryReadDAPMessage attempts to read a DAP message within the given timeout
// using a connection deadline (no leaked goroutines). The conn must be the
// underlying net.Conn for the bufio.Reader.
func tryReadDAPMessage(conn net.Conn, r *bufio.Reader, timeout time.Duration) (dap.Message, bool) {
	_ = conn.SetReadDeadline(time.Now().Add(timeout))
	msg, err := dap.ReadProtocolMessage(r)
	_ = conn.SetReadDeadline(time.Time{}) // clear deadline
	if err != nil {
		return nil, false
	}
	return msg, true
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

// tryRead attempts to read a DAP message within the given timeout.
// Returns the message and true if received, or nil and false on timeout.
func (s *dapTestSession) tryRead(timeout time.Duration) (dap.Message, bool) {
	return tryReadDAPMessage(s.client, s.reader, timeout)
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

// stackTrace requests a stack trace and returns the response.
func (s *dapTestSession) stackTrace() *dap.StackTraceResponse {
	s.send(&dap.StackTraceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stackTrace",
		},
		Arguments: dap.StackTraceArguments{ThreadId: elpsThreadID},
	})
	msg := s.read()
	resp, ok := msg.(*dap.StackTraceResponse)
	require.True(s.t, ok, "expected StackTraceResponse, got %T", msg)
	return resp
}

// scopes requests scopes for a frame and returns the local scope ref.
func (s *dapTestSession) localScopeRef(frameID int) int {
	s.send(&dap.ScopesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "scopes",
		},
		Arguments: dap.ScopesArguments{FrameId: frameID},
	})
	msg := s.read()
	scopesResp, ok := msg.(*dap.ScopesResponse)
	require.True(s.t, ok, "expected ScopesResponse, got %T", msg)
	for _, sc := range scopesResp.Body.Scopes {
		if sc.Name == "Local" {
			return sc.VariablesReference
		}
	}
	s.t.Fatal("no local scope found")
	return 0
}

// variables requests variables for a given reference and returns the response.
func (s *dapTestSession) variables(ref int) []dap.Variable {
	s.send(&dap.VariablesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "variables",
		},
		Arguments: dap.VariablesArguments{VariablesReference: ref},
	})
	msg := s.read()
	varsResp, ok := msg.(*dap.VariablesResponse)
	require.True(s.t, ok, "expected VariablesResponse, got %T", msg)
	return varsResp.Body.Variables
}

// varNames returns a list of variable names for debug output.
func varNames(vars []dap.Variable) []string {
	names := make([]string, len(vars))
	for i, v := range vars {
		names[i] = fmt.Sprintf("%s=%s", v.Name, v.Value)
	}
	return names
}

// evaluate sends an evaluate request and returns the response.
func (s *dapTestSession) evaluate(expr string, frameID int) *dap.EvaluateResponse {
	return s.evaluateWithContext(expr, frameID, "")
}

// evaluateWithContext sends an evaluate request with the given DAP context
// ("hover", "repl", "watch", etc.) and returns the response.
func (s *dapTestSession) evaluateWithContext(expr string, frameID int, ctx string) *dap.EvaluateResponse {
	s.send(&dap.EvaluateRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "evaluate",
		},
		Arguments: dap.EvaluateArguments{
			Expression: expr,
			FrameId:    frameID,
			Context:    ctx,
		},
	})
	msg := s.read()
	evalResp, ok := msg.(*dap.EvaluateResponse)
	require.True(s.t, ok, "expected EvaluateResponse, got %T", msg)
	return evalResp
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
	// Multi-line program: step-into from line 1 should advance to line 2
	// (line-level granularity skips sub-expressions on the same line).
	go func() {
		res := env.LoadString("test", "(+ 1 2)\n(+ 3 4)")
		resultCh <- res
	}()

	// Wait for stop-on-entry.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Send StepIn — should advance to line 2 (skipping line 1 sub-expressions).
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

	// Should pause again on line 2.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Verify we paused on line 2.
	_, pausedExpr := s.engine.PausedState()
	require.NotNil(t, pausedExpr, "step-in should have paused on an expression")
	require.NotNil(t, pausedExpr.Source, "paused expression should have source location")
	assert.Equal(t, 2, pausedExpr.Source.Line, "step-in should advance to line 2")

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

// TestDAPServer_VariableExpansion verifies that structured types (lists)
// return non-zero VariablesReference and that requesting children returns
// the individual elements.
func TestDAPServer_VariableExpansion(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Breakpoint on line 3 (+ a b) inside the function body.
	// The function receives a list, sorted-map, and evaluates expressions.
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
	// Simple program: function receives a list and map, breakpoint on line 3.
	program := "(defun inspect-vars (items m)\n" +
		"  (let ((x (car items)))\n" +
		"    (+ x 1)))\n" + // line 3: breakpoint
		"(inspect-vars (list 10 20 30) (sorted-map \"a\" 1 \"b\" 2))"
	go func() {
		resultCh <- env.LoadString("test", program)
	}()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Get stack trace and local variables.
	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrame := stResp.Body.StackFrames[0].Id
	localRef := s.localScopeRef(topFrame)
	vars := s.variables(localRef)

	// Find variables — items (list), m (sorted-map), x (scalar).
	varMap := make(map[string]dap.Variable)
	for _, v := range vars {
		varMap[v.Name] = v
	}

	// --- List expansion ---
	listVar, ok := varMap["items"]
	require.True(t, ok, "should find items variable, got vars: %v", varNames(vars))
	assert.Greater(t, listVar.VariablesReference, 0,
		"list should have expandable children")
	children := s.variables(listVar.VariablesReference)
	require.Len(t, children, 3, "list should have 3 elements")
	assert.Equal(t, "[0]", children[0].Name)
	assert.Equal(t, "10", children[0].Value)
	assert.Equal(t, "[1]", children[1].Name)
	assert.Equal(t, "20", children[1].Value)
	assert.Equal(t, "[2]", children[2].Name)
	assert.Equal(t, "30", children[2].Value)

	// --- Sorted-map expansion ---
	mapVar, ok := varMap["m"]
	require.True(t, ok, "should find m variable, got vars: %v", varNames(vars))
	assert.Greater(t, mapVar.VariablesReference, 0,
		"sorted-map should have expandable children")
	mapChildren := s.variables(mapVar.VariablesReference)
	require.Len(t, mapChildren, 2, "sorted-map should have 2 entries")
	mapVals := make(map[string]string)
	for _, ch := range mapChildren {
		mapVals[ch.Name] = ch.Value
	}
	assert.Equal(t, "1", mapVals[`"a"`], "map key 'a' should have value 1")
	assert.Equal(t, "2", mapVals[`"b"`], "map key 'b' should have value 2")

	// --- Scalar should NOT be expandable ---
	xVar, ok := varMap["x"]
	require.True(t, ok, "should find x variable")
	assert.Equal(t, 0, xVar.VariablesReference,
		"scalar should not be expandable")

	// Continue and finish.
	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

// TestDAPServer_EvaluateResultExpansion verifies that evaluate results for
// structured types return a non-zero VariablesReference for drill-down.
func TestDAPServer_EvaluateResultExpansion(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint inside function body so we have stack frames.
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
		resultCh <- env.LoadString("test", "(defun f (x)\n  (+ x 1))\n(f 10)")
	}()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrame := stResp.Body.StackFrames[0].Id

	// Evaluate a list expression — result should be expandable.
	evalResp := s.evaluate("(list 1 2 3)", topFrame)
	assert.True(t, evalResp.Success, "evaluate should succeed")
	assert.Greater(t, evalResp.Body.VariablesReference, 0,
		"list evaluate result should be expandable")

	// Expand the result's children.
	children := s.variables(evalResp.Body.VariablesReference)
	require.Len(t, children, 3)
	assert.Equal(t, "[0]", children[0].Name)
	assert.Equal(t, "1", children[0].Value)

	// Evaluate a scalar — result should NOT be expandable.
	scalarResp := s.evaluate("42", topFrame)
	assert.True(t, scalarResp.Success)
	assert.Equal(t, 0, scalarResp.Body.VariablesReference,
		"scalar evaluate result should not be expandable")

	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

// TestDAPServer_SourceByPath verifies that the source request handler
// can serve source content from a configured SourceLibrary.
func TestDAPServer_SourceByPath(t *testing.T) {
	t.Parallel()
	content := "; hello from embedded source\n(+ 1 2)\n"
	fs := fstest.MapFS{
		"hello.lisp": &fstest.MapFile{Data: []byte(content)},
	}
	s := setupDAPSession(t, debugger.WithSourceLibrary(&lisp.FSLibrary{FS: fs}))
	s.configDone()

	// Request source by path.
	s.send(&dap.SourceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "source",
		},
		Arguments: dap.SourceArguments{
			Source: &dap.Source{Path: "hello.lisp"},
		},
	})
	msg := s.read()
	srcResp, ok := msg.(*dap.SourceResponse)
	require.True(t, ok, "expected SourceResponse, got %T", msg)
	assert.True(t, srcResp.Success)
	assert.Equal(t, content, srcResp.Body.Content)
	assert.Equal(t, "text/x-lisp", srcResp.Body.MimeType)

	s.disconnect()
}

// TestDAPServer_SourceByReference verifies that source content can be
// served by reference ID for virtual sources.
func TestDAPServer_SourceByReference(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)
	s.configDone()

	// Allocate a source reference.
	content := "(defun hello () (print \"hi\"))\n"
	ref := s.engine.AllocSourceRef("virtual.lisp", content)

	// Request source by reference.
	s.send(&dap.SourceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "source",
		},
		Arguments: dap.SourceArguments{
			SourceReference: ref,
		},
	})
	msg := s.read()
	srcResp, ok := msg.(*dap.SourceResponse)
	require.True(t, ok, "expected SourceResponse, got %T", msg)
	assert.True(t, srcResp.Success)
	assert.Equal(t, content, srcResp.Body.Content)
	assert.Equal(t, "text/x-lisp", srcResp.Body.MimeType)

	s.disconnect()
}

// TestDAPServer_SourceNotAvailable verifies that the source request handler
// returns an error when no source library is configured and no ref matches.
func TestDAPServer_SourceNotAvailable(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)
	s.configDone()

	s.send(&dap.SourceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "source",
		},
		Arguments: dap.SourceArguments{
			Source: &dap.Source{Path: "nonexistent.lisp"},
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

// TestDAPServer_VariableFormatterNative verifies that custom native type
// formatters are wired through the DAP Variables response. A native
// value in scope should display using the registered formatter's output.
func TestDAPServer_VariableFormatterNative(t *testing.T) {
	t.Parallel()

	type myStruct struct {
		Name string
		Age  int
	}
	typeName := fmt.Sprintf("%T", myStruct{})

	s := setupDAPSession(t, debugger.WithFormatters(map[string]debugger.VariableFormatter{
		typeName: debugger.FormatterFunc(func(v any) string {
			ms, ok := v.(myStruct)
			if !ok {
				return "<unknown>"
			}
			return fmt.Sprintf("Person(%s, %d)", ms.Name, ms.Age)
		}),
	}))

	// Breakpoint on line 2 inside function body.
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

	// Create an env that puts a native value into scope before evaluation.
	env := newDAPTestEnv(t, s.engine)
	env.Put(lisp.Symbol("person"), lisp.Native(myStruct{Name: "Alice", Age: 30}))

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		// Line 1: defun with person in scope via closure
		// Line 2: breakpoint fires here — person visible as local
		resultCh <- env.LoadString("test", "(defun check ()\n  person)\n(check)")
	}()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Request stack trace → scopes → variables through the DAP protocol.
	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)

	// Evaluate the native value through the DAP evaluate handler.
	evalResp := s.evaluate("person", stResp.Body.StackFrames[0].Id)
	assert.True(t, evalResp.Success)
	assert.Equal(t, "Person(Alice, 30)", evalResp.Body.Result,
		"evaluate should use registered formatter for native value")

	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

// TestDAPServer_StepIntoNestedCalls exercises step-in through nested function
// calls (outer → middle → inner), then step-out back up the call stack. This
// stresses the stepper's depth tracking and the DAP message ordering between
// step responses and stopped events from different goroutines.
func TestDAPServer_StepIntoNestedCalls(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint inside outer's body (line 9: the (middle 5) call).
	// This ensures we have at least one stack frame when the breakpoint fires.
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 9}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	// Program with three levels of nesting. Each function has a
	// continuation expression AFTER the nested call so step-out has
	// somewhere to land (otherwise tail-position calls unwind without
	// any OnEval, and the stepper can never fire).
	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test",
			"(defun inner (x)\n"+   //  1
				"  (+ x 100))\n"+   //  2
				"\n"+               //  3
				"(defun middle (x)\n"+     //  4
				"  (set 'tmp (inner (* x 2)))\n"+ //  5
				"  tmp)\n"+         //  6
				"\n"+               //  7
				"(defun outer ()\n"+       //  8
				"  (set 'res (middle 5))\n"+ //  9
				"  res)\n"+         // 10
				"\n"+               // 11
				"(outer)\n")        // 12
		resultCh <- res
	}()

	// Wait for breakpoint at line 9 (inside outer, on the (middle 5) call).
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent (breakpoint)

	st0 := s.stackTrace()
	outerDepth := len(st0.Body.StackFrames)
	require.Greater(t, outerDepth, 0, "should have frames inside outer")
	assert.Equal(t, 9, st0.Body.StackFrames[0].Line, "should be at line 9")

	// Clear breakpoints to avoid re-fire inside nested calls.
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

	// stepInUntilDeeper steps in repeatedly until the stack depth increases
	// beyond the given baseline. In ELPS, step-in advances one expression
	// at a time — the interpreter may evaluate sub-expressions (symbol lookup,
	// argument evaluation) at the current depth before entering a function call.
	stepInUntilDeeper := func(baseline int, label string) int {
		t.Helper()
		for range 20 {
			s.send(&dap.StepInRequest{
				Request: dap.Request{
					ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
					Command:         "stepIn",
				},
				Arguments: dap.StepInArguments{ThreadId: elpsThreadID},
			})
			s.read() // StepInResponse
			require.Eventually(t, func() bool {
				return s.engine.IsPaused()
			}, 2*time.Second, 10*time.Millisecond, "%s: engine did not pause", label)
			s.read() // StoppedEvent

			st := s.stackTrace()
			if len(st.Body.StackFrames) > baseline {
				return len(st.Body.StackFrames)
			}
		}
		t.Fatalf("%s: stack depth never exceeded %d after 20 step-ins", label, baseline)
		return 0
	}

	// --- Step into middle ---
	middleDepth := stepInUntilDeeper(outerDepth, "entering middle")

	// --- Step into inner ---
	innerDepth := stepInUntilDeeper(middleDepth, "entering inner")

	// --- Step out from inner back toward outer ---
	s.send(&dap.StepOutRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stepOut",
		},
		Arguments: dap.StepOutArguments{ThreadId: elpsThreadID},
	})
	s.read() // StepOutResponse
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	st3 := s.stackTrace()
	assert.Less(t, len(st3.Body.StackFrames), innerDepth,
		"step-out should decrease stack depth (returned from inner)")

	// Continue to finish.
	s.continueExec()

	select {
	case res := <-resultCh:
		require.False(t, res.Type == lisp.LError, "program error: %v", res)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout waiting for program to finish")
	}

	s.disconnect()
}

// TestDAPServer_RapidStepSequence sends multiple step operations in quick
// succession to stress test the synchronization between the DAP handler
// goroutine and the eval goroutine. Each step must properly wait for the
// previous one to complete (engine paused) before proceeding.
func TestDAPServer_RapidStepSequence(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test",
			"(defun f (n)\n"+
				"  (if (<= n 0)\n"+
				"    0\n"+
				"    (+ n (f (- n 1)))))\n"+
				"(f 3)\n")
		resultCh <- res
	}()

	// Wait for stop-on-entry.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Perform 10 rapid step-in operations, verifying each one pauses.
	for i := range 10 {
		s.send(&dap.StepInRequest{
			Request: dap.Request{
				ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
				Command:         "stepIn",
			},
			Arguments: dap.StepInArguments{ThreadId: elpsThreadID},
		})
		msg := s.read()
		_, ok := msg.(*dap.StepInResponse)
		require.True(t, ok, "step %d: expected StepInResponse, got %T", i, msg)

		require.Eventually(t, func() bool {
			return s.engine.IsPaused()
		}, 2*time.Second, 10*time.Millisecond, "step %d: engine did not pause", i)
		s.read() // StoppedEvent
	}

	// Continue to finish.
	s.continueExec()

	select {
	case res := <-resultCh:
		require.False(t, res.Type == lisp.LError, "program error: %v", res)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}

	s.disconnect()
}

// TestDAPServer_StepOutTailPosition verifies that step-out from a function
// in tail position correctly pauses at the call site instead of running to
// completion. The post-call check in Eval catches the return and the
// AfterFunCall hook detects the depth decrease.
func TestDAPServer_StepOutTailPosition(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint inside inner (line 2).
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

	// All calls are in tail position: outer → middle → inner.
	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test",
			"(defun inner (x)\n"+ //  1
				"  (+ x 100))\n"+ //  2
				"(defun middle (x)\n"+  //  3
				"  (inner (* x 2)))\n"+ //  4
				"(defun outer ()\n"+    //  5
				"  (middle 5))\n"+      //  6
				"(outer)\n")            //  7
		resultCh <- res
	}()

	// Wait for breakpoint inside inner.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	st := s.stackTrace()
	require.Greater(t, len(st.Body.StackFrames), 0)
	assert.Equal(t, 2, st.Body.StackFrames[0].Line)
	innerDepth := len(st.Body.StackFrames)

	// Clear breakpoints, then step-out.
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

	s.send(&dap.StepOutRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stepOut",
		},
		Arguments: dap.StepOutArguments{ThreadId: elpsThreadID},
	})
	s.read() // StepOutResponse

	// Step-out from tail position should now pause at the call site.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond,
		"step-out from tail position should pause at call site")
	s.read() // StoppedEvent

	st2 := s.stackTrace()
	require.Greater(t, len(st2.Body.StackFrames), 0)
	assert.Less(t, len(st2.Body.StackFrames), innerDepth,
		"step-out should decrease stack depth")
	assert.NotEqual(t, 2, st2.Body.StackFrames[0].Line,
		"should not be paused inside inner after step-out")

	// Resume to finish.
	s.send(&dap.ContinueRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "continue",
		},
		Arguments: dap.ContinueArguments{ThreadId: elpsThreadID},
	})
	s.read() // ContinueResponse

	select {
	case res := <-resultCh:
		require.False(t, res.Type == lisp.LError, "program error: %v", res)
		assert.Equal(t, 110, res.Int, "inner(5*2) = 10+100 = 110")
	case <-time.After(5 * time.Second):
		if s.engine.IsPaused() {
			s.engine.Resume()
		}
		t.Fatal("timeout waiting for eval result")
	}

	s.disconnect()
}

// TestDAPServer_SourceLibraryMissing verifies that when a SourceLibrary
// is configured but the requested file does not exist, the error response
// is returned instead of crashing.
func TestDAPServer_SourceLibraryMissing(t *testing.T) {
	t.Parallel()
	fs := fstest.MapFS{
		"exists.lisp": &fstest.MapFile{Data: []byte("(+ 1 2)")},
	}
	s := setupDAPSession(t, debugger.WithSourceLibrary(&lisp.FSLibrary{FS: fs}))
	s.configDone()

	// Request a file that does NOT exist in the library.
	s.send(&dap.SourceRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "source",
		},
		Arguments: dap.SourceArguments{
			Source: &dap.Source{Path: "nonexistent.lisp"},
		},
	})
	msg := s.read()
	errResp, ok := msg.(*dap.ErrorResponse)
	require.True(t, ok, "expected ErrorResponse, got %T", msg)
	assert.False(t, errResp.Success)
	assert.Equal(t, "source not available", errResp.Message)

	s.disconnect()
}

// TestDAPServer_StopOnEntry_AttachTrue verifies that when the client sends
// attach with stopOnEntry:true, the handler sends a stopped event on entry.
func TestDAPServer_StopOnEntry_AttachTrue(t *testing.T) {
	t.Parallel()
	// Engine is configured to stop on entry.
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Client sends attach with stopOnEntry: true.
	args, err := json.Marshal(map[string]interface{}{"stopOnEntry": true})
	require.NoError(t, err)
	s.send(&dap.AttachRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "attach",
		},
		Arguments: json.RawMessage(args),
	})
	msg := s.read()
	_, ok := msg.(*dap.AttachResponse)
	require.True(t, ok, "expected AttachResponse, got %T", msg)

	s.configDone()

	// Launch program -- engine should pause on entry.
	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	// Should stop on entry.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not stop on entry")

	stoppedMsg := s.read()
	stoppedEvent, ok := stoppedMsg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", stoppedMsg)
	assert.Equal(t, "entry", stoppedEvent.Body.Reason, "stopOnEntry should produce entry reason")

	// Continue to let the program finish.
	s.continueExec()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type)
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout waiting for program result")
	}

	s.disconnect()
}

// TestDAPServer_StopOnEntry_AttachFalse verifies that when the client sends
// attach with stopOnEntry:false, the engine resumes silently even though the
// engine was configured with WithStopOnEntry(true).
func TestDAPServer_StopOnEntry_AttachFalse(t *testing.T) {
	t.Parallel()
	// Engine is configured to stop on entry, but client overrides.
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Client sends attach with stopOnEntry: false.
	args, err := json.Marshal(map[string]interface{}{"stopOnEntry": false})
	require.NoError(t, err)
	s.send(&dap.AttachRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "attach",
		},
		Arguments: json.RawMessage(args),
	})
	msg := s.read()
	_, ok := msg.(*dap.AttachResponse)
	require.True(t, ok, "expected AttachResponse, got %T", msg)

	s.configDone()

	// Verify no StoppedEvent was sent after configurationDone.
	if msg2, ok2 := s.tryRead(200 * time.Millisecond); ok2 {
		t.Fatalf("expected no StoppedEvent, but got %T", msg2)
	}

	// Launch program -- engine should NOT stop on entry because client
	// said stopOnEntry: false.
	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	// The program should run to completion without stopping.
	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result")
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout: program did not complete -- engine likely paused on entry when it should not have")
	}

	// Verify engine is not paused.
	assert.False(t, s.engine.IsPaused(), "engine should not be paused after stopOnEntry:false")

	s.disconnect()
}

// TestDAPServer_StopOnEntry_AttachAbsent verifies that when the client sends
// attach without a stopOnEntry field, the default behavior is to NOT stop on
// entry (per DAP spec, the default for stopOnEntry is false).
func TestDAPServer_StopOnEntry_AttachAbsent(t *testing.T) {
	t.Parallel()
	// Engine is configured to stop on entry, but client does not include
	// stopOnEntry in the attach arguments -- default should be false.
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	// Client sends attach with no stopOnEntry field.
	s.send(&dap.AttachRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "attach",
		},
	})
	msg := s.read()
	_, ok := msg.(*dap.AttachResponse)
	require.True(t, ok, "expected AttachResponse, got %T", msg)

	s.configDone()

	// Verify no StoppedEvent was sent after configurationDone.
	if msg2, ok2 := s.tryRead(200 * time.Millisecond); ok2 {
		t.Fatalf("expected no StoppedEvent, but got %T", msg2)
	}

	// Launch program -- engine should NOT stop on entry because the
	// default for stopOnEntry is false.
	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	// The program should run to completion without stopping.
	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result")
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout: program did not complete -- engine likely paused on entry when it should not have")
	}

	// Verify engine is not paused.
	assert.False(t, s.engine.IsPaused(), "engine should not be paused after absent stopOnEntry")

	s.disconnect()
}

// TestParseStopOnEntry verifies the parseStopOnEntry helper handles all JSON
// edge cases correctly, defaulting to false per DAP spec.
func TestParseStopOnEntry(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		args json.RawMessage
		want bool
	}{
		{"nil", nil, false},
		{"empty_object", json.RawMessage(`{}`), false},
		{"true", json.RawMessage(`{"stopOnEntry":true}`), true},
		{"false", json.RawMessage(`{"stopOnEntry":false}`), false},
		{"malformed_json", json.RawMessage(`{invalid}`), false},
		{"extra_fields", json.RawMessage(`{"stopOnEntry":true,"other":"field"}`), true},
		{"empty_bytes", json.RawMessage(``), false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := parseStopOnEntry(tt.args)
			assert.Equal(t, tt.want, got)
		})
	}
}

// TestDAPServer_StopOnEntry_LaunchTrue verifies that launch with
// stopOnEntry:true sends a stopped event on entry.
func TestDAPServer_StopOnEntry_LaunchTrue(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	args, err := json.Marshal(map[string]any{"stopOnEntry": true})
	require.NoError(t, err)
	s.send(&dap.LaunchRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "launch",
		},
		Arguments: json.RawMessage(args),
	})
	msg := s.read()
	_, ok := msg.(*dap.LaunchResponse)
	require.True(t, ok, "expected LaunchResponse, got %T", msg)

	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not stop on entry")

	stoppedMsg := s.read()
	stoppedEvent, ok := stoppedMsg.(*dap.StoppedEvent)
	require.True(t, ok, "expected StoppedEvent, got %T", stoppedMsg)
	assert.Equal(t, "entry", stoppedEvent.Body.Reason, "stopOnEntry should produce entry reason")

	s.continueExec()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type)
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout waiting for program result")
	}

	s.disconnect()
}

// TestDAPServer_StopOnEntry_LaunchFalse verifies that launch with
// stopOnEntry:false resumes silently.
func TestDAPServer_StopOnEntry_LaunchFalse(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	args, err := json.Marshal(map[string]any{"stopOnEntry": false})
	require.NoError(t, err)
	s.send(&dap.LaunchRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "launch",
		},
		Arguments: json.RawMessage(args),
	})
	msg := s.read()
	_, ok := msg.(*dap.LaunchResponse)
	require.True(t, ok, "expected LaunchResponse, got %T", msg)

	s.configDone()

	if msg2, ok2 := s.tryRead(200 * time.Millisecond); ok2 {
		t.Fatalf("expected no StoppedEvent, but got %T", msg2)
	}

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result")
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout: program did not complete -- engine likely paused on entry when it should not have")
	}

	assert.False(t, s.engine.IsPaused(), "engine should not be paused after stopOnEntry:false")
	s.disconnect()
}

// TestDAPServer_StopOnEntry_LaunchAbsent verifies that launch without
// stopOnEntry defaults to not stopping (per DAP spec).
func TestDAPServer_StopOnEntry_LaunchAbsent(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

	s.send(&dap.LaunchRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "launch",
		},
	})
	msg := s.read()
	_, ok := msg.(*dap.LaunchResponse)
	require.True(t, ok, "expected LaunchResponse, got %T", msg)

	s.configDone()

	if msg2, ok2 := s.tryRead(200 * time.Millisecond); ok2 {
		t.Fatalf("expected no StoppedEvent, but got %T", msg2)
	}

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result")
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout: program did not complete -- engine likely paused on entry when it should not have")
	}

	assert.False(t, s.engine.IsPaused(), "engine should not be paused after absent stopOnEntry")
	s.disconnect()
}

// TestDAPServer_StepIn_LineGranularity verifies that step-in with default
// (line-level) granularity skips sub-expressions on the same line and only
// pauses when the line changes or a function is entered.
func TestDAPServer_StepIn_LineGranularity(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint on line 4 (the call to (inner 5) inside outer).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 4}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	// inner's body is on line 2 (multi-line defun so body line is unambiguous).
	// outer calls (inner 5) on line 4.
	// A single step-in from line 4 should enter inner's body (line 2),
	// skipping sub-expressions (inner, 5) that are on line 4.
	go func() {
		res := env.LoadString("test",
			"(defun inner (x)\n"+  // line 1
				"  (+ x 100))\n"+      // line 2 (inner's body)
				"(defun outer ()\n"+   // line 3
				"  (inner 5))\n"+      // line 4
				"(outer)")             // line 5
		resultCh <- res
	}()

	// Wait for breakpoint on line 4.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	st0 := s.stackTrace()
	require.Greater(t, len(st0.Body.StackFrames), 0)
	assert.Equal(t, 4, st0.Body.StackFrames[0].Line, "should be on line 4")
	outerDepth := len(st0.Body.StackFrames)

	// Clear breakpoints.
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

	// Step in with default (line) granularity — should enter inner's body
	// in a single step, skipping the sub-expressions on line 4.
	s.send(&dap.StepInRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stepIn",
		},
		Arguments: dap.StepInArguments{ThreadId: elpsThreadID},
	})
	s.read() // StepInResponse

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	st1 := s.stackTrace()
	require.Greater(t, len(st1.Body.StackFrames), 0)
	// Should have entered inner's body — stack is deeper and line is 2.
	assert.Greater(t, len(st1.Body.StackFrames), outerDepth,
		"step-in should enter inner (deeper stack)")
	assert.Equal(t, 2, st1.Body.StackFrames[0].Line,
		"step-in should enter inner's body on line 2")

	// Continue to finish.
	s.continueExec()

	select {
	case res := <-resultCh:
		require.False(t, res.Type == lisp.LError, "program error: %v", res)
		assert.Equal(t, 105, res.Int, "expected (inner 5) = 105")
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

// TestDAPServer_StepIn_InstructionGranularity verifies that step-in with
// "instruction" granularity pauses on every sub-expression, preserving the
// old per-expression behavior.
func TestDAPServer_StepIn_InstructionGranularity(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	// Single-line expression: with instruction granularity, step-in should
	// pause on each sub-expression (not skip to end of line).
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	// Wait for stop-on-entry.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Record the initial expression at stop-on-entry: (+ 1 2) is an s-expr.
	_, entryExpr := s.engine.PausedState()
	require.NotNil(t, entryExpr)
	assert.Equal(t, lisp.LSExpr, entryExpr.Type,
		"stop-on-entry should pause on the s-expression (+ 1 2)")

	// Step in with "instruction" granularity — should pause on the very next
	// sub-expression even though it's on the same line.
	s.send(&dap.StepInRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stepIn",
		},
		Arguments: dap.StepInArguments{
			ThreadId:    elpsThreadID,
			Granularity: "instruction",
		},
	})
	s.read() // StepInResponse

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Verify we paused on a sub-expression (still line 1, but a different expr).
	_, pausedExpr := s.engine.PausedState()
	require.NotNil(t, pausedExpr)
	require.NotNil(t, pausedExpr.Source)
	assert.Equal(t, 1, pausedExpr.Source.Line,
		"instruction granularity should pause on same-line sub-expression")
	// The paused expression must be a sub-expression of (+ 1 2), not the
	// s-expression itself — confirming we advanced within the same line.
	assert.NotEqual(t, lisp.LSExpr, pausedExpr.Type,
		"instruction step should advance to a sub-expression (atom), not remain on the s-expression")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

// TestDAPServer_StepNext_LineGranularity verifies that step-over with
// line-level granularity advances to the next source line in one step,
// rather than pausing on each sub-expression.
func TestDAPServer_StepNext_LineGranularity(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint on line 3 (call to inner).
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
	go func() {
		res := env.LoadString("test",
			"(defun inner (x) (+ x 100))\n"+
				"(defun outer (n)\n"+
				"  (inner n)\n"+ // line 3
				"  (+ n 1))\n"+ // line 4
				"(outer 5)")
		resultCh <- res
	}()

	// Wait for breakpoint on line 3.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	st0 := s.stackTrace()
	assert.Equal(t, 3, st0.Body.StackFrames[0].Line)
	preStepDepth := len(st0.Body.StackFrames)

	// Clear breakpoints.
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

	// Step over with default (line) granularity — should reach line 4 in
	// a single step (skipping sub-expressions on line 3).
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

	st1 := s.stackTrace()
	assert.Equal(t, 4, st1.Body.StackFrames[0].Line,
		"step-over with line granularity should advance to line 4 in one step")
	assert.LessOrEqual(t, len(st1.Body.StackFrames), preStepDepth,
		"step-over should not enter a deeper call frame")

	// Continue to finish.
	s.continueExec()

	select {
	case res := <-resultCh:
		require.False(t, res.Type == lisp.LError, "program error: %v", res)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

// scopes requests scopes for a frame and returns all scopes.
func (s *dapTestSession) scopes(frameID int) []dap.Scope {
	s.send(&dap.ScopesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "scopes",
		},
		Arguments: dap.ScopesArguments{FrameId: frameID},
	})
	msg := s.read()
	scopesResp, ok := msg.(*dap.ScopesResponse)
	require.True(s.t, ok, "expected ScopesResponse, got %T", msg)
	return scopesResp.Body.Scopes
}

// mockScopeProvider implements debugger.ScopeProvider for tests.
type mockScopeProvider struct {
	name      string
	expensive bool
	vars      func(env *lisp.LEnv) []debugger.ScopeVariable
}

func (m *mockScopeProvider) Name() string    { return m.name }
func (m *mockScopeProvider) Expensive() bool { return m.expensive }
func (m *mockScopeProvider) Variables(env *lisp.LEnv) []debugger.ScopeVariable {
	if m.vars != nil {
		return m.vars(env)
	}
	return nil
}

func TestDAPServer_CustomScopeProvider(t *testing.T) {
	t.Parallel()

	var capturedEnv *lisp.LEnv
	provider := &mockScopeProvider{
		name:      "State DB",
		expensive: true,
		vars: func(env *lisp.LEnv) []debugger.ScopeVariable {
			capturedEnv = env
			return []debugger.ScopeVariable{
				{Name: "balance", Value: "1000", Type: "int"},
				{
					Name:  "account",
					Value: "{...}",
					Type:  "object",
					Children: []debugger.ScopeVariable{
						{Name: "owner", Value: "Alice", Type: "string"},
						{Name: "id", Value: "42", Type: "int"},
					},
				},
				{
					Name:  "cluster",
					Value: "{...}",
					Type:  "object",
					Children: []debugger.ScopeVariable{
						{
							Name:  "node-1",
							Value: "{...}",
							Type:  "object",
							Children: []debugger.ScopeVariable{
								{Name: "status", Value: "healthy", Type: "string"},
							},
						},
					},
				},
			}
		},
	}

	s := setupDAPSession(t, debugger.WithScopeProviders(provider))

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

	// Wait for breakpoint inside function.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Get stack trace to get frame ID.
	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrameID := stResp.Body.StackFrames[0].Id

	// Request scopes — should have Local, Package, State DB.
	allScopes := s.scopes(topFrameID)
	require.Len(t, allScopes, 3, "expected 3 scopes (Local, Package, State DB)")
	assert.Equal(t, "Local", allScopes[0].Name)
	assert.Equal(t, "Package", allScopes[1].Name)
	assert.Equal(t, "State DB", allScopes[2].Name)
	assert.True(t, allScopes[2].Expensive)

	// Request variables for the custom scope.
	customRef := allScopes[2].VariablesReference
	vars := s.variables(customRef)
	require.Len(t, vars, 3)
	assert.Equal(t, "balance", vars[0].Name)
	assert.Equal(t, "1000", vars[0].Value)
	assert.Equal(t, "int", vars[0].Type)
	assert.Equal(t, 0, vars[0].VariablesReference, "leaf variable should have ref 0")

	assert.Equal(t, "account", vars[1].Name)
	assert.Equal(t, "{...}", vars[1].Value)
	assert.Greater(t, vars[1].VariablesReference, 0, "variable with children should have ref > 0")

	// Verify the env parameter was passed to the provider.
	require.NotNil(t, capturedEnv, "Variables() should receive non-nil env")

	// Drill into the account variable's children (2-level).
	children := s.variables(vars[1].VariablesReference)
	require.Len(t, children, 2)
	assert.Equal(t, "owner", children[0].Name)
	assert.Equal(t, "Alice", children[0].Value)
	assert.Equal(t, "string", children[0].Type)
	assert.Equal(t, "id", children[1].Name)
	assert.Equal(t, "42", children[1].Value)
	assert.Equal(t, "int", children[1].Type)

	// Drill into the cluster variable (3-level nesting).
	assert.Equal(t, "cluster", vars[2].Name)
	assert.Greater(t, vars[2].VariablesReference, 0)
	clusterChildren := s.variables(vars[2].VariablesReference)
	require.Len(t, clusterChildren, 1)
	assert.Equal(t, "node-1", clusterChildren[0].Name)
	assert.Greater(t, clusterChildren[0].VariablesReference, 0, "nested node should have children ref")

	// Drill to leaf level (3rd level).
	nodeChildren := s.variables(clusterChildren[0].VariablesReference)
	require.Len(t, nodeChildren, 1)
	assert.Equal(t, "status", nodeChildren[0].Name)
	assert.Equal(t, "healthy", nodeChildren[0].Value)
	assert.Equal(t, "string", nodeChildren[0].Type)
	assert.Equal(t, 0, nodeChildren[0].VariablesReference, "leaf should have ref 0")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

// TestDAPServer_CustomScopeProvider_Cleanup verifies that custom variable
// references are cleaned up when execution resumes (EventContinued). Old
// drill-down refs from a previous pause should not resolve after continue.
func TestDAPServer_CustomScopeProvider_Cleanup(t *testing.T) {
	t.Parallel()

	provider := &mockScopeProvider{
		name:      "State DB",
		expensive: false,
		vars: func(env *lisp.LEnv) []debugger.ScopeVariable {
			return []debugger.ScopeVariable{
				{
					Name:  "record",
					Value: "{...}",
					Type:  "object",
					Children: []debugger.ScopeVariable{
						{Name: "key", Value: "abc", Type: "string"},
					},
				},
			}
		},
	}

	s := setupDAPSession(t, debugger.WithScopeProviders(provider))

	// Set breakpoints on line 2 (inside f) and line 4 (inside g).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 2}, {Line: 4}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test",
			"(defun f (x)\n  (+ x 1))\n(defun g (y)\n  (+ y 2))\n(f 10)\n(g 20)")
		resultCh <- res
	}()

	// First breakpoint (line 2 inside f).
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrameID := stResp.Body.StackFrames[0].Id

	allScopes := s.scopes(topFrameID)
	require.Len(t, allScopes, 3)
	vars := s.variables(allScopes[2].VariablesReference)
	require.Len(t, vars, 1)
	assert.Greater(t, vars[0].VariablesReference, 0)

	// Save the old drill-down ref.
	oldChildRef := vars[0].VariablesReference

	// Verify it works while paused.
	children := s.variables(oldChildRef)
	require.Len(t, children, 1)
	assert.Equal(t, "key", children[0].Name)

	// Continue — this should clear customVarRefs.
	s.continueExec()

	// Second breakpoint (line 4 inside g).
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// The old drill-down ref should no longer resolve.
	staleVars := s.variables(oldChildRef)
	assert.Empty(t, staleVars, "old custom var ref should be invalidated after continue")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

func TestDAPServer_MultipleScopeProviders(t *testing.T) {
	t.Parallel()

	p1 := &mockScopeProvider{
		name:      "State DB",
		expensive: true,
		vars: func(env *lisp.LEnv) []debugger.ScopeVariable {
			return []debugger.ScopeVariable{
				{Name: "key1", Value: "val1"},
			}
		},
	}
	p2 := &mockScopeProvider{
		name:      "Cache",
		expensive: false,
		vars: func(env *lisp.LEnv) []debugger.ScopeVariable {
			return []debugger.ScopeVariable{
				{Name: "hit-rate", Value: "0.95", Type: "float"},
			}
		},
	}

	s := setupDAPSession(t, debugger.WithScopeProviders(p1, p2))

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

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrameID := stResp.Body.StackFrames[0].Id

	// Should have 4 scopes: Local, Package, State DB, Cache.
	allScopes := s.scopes(topFrameID)
	require.Len(t, allScopes, 4)
	assert.Equal(t, "Local", allScopes[0].Name)
	assert.Equal(t, "Package", allScopes[1].Name)
	assert.Equal(t, "State DB", allScopes[2].Name)
	assert.True(t, allScopes[2].Expensive)
	assert.Equal(t, "Cache", allScopes[3].Name)
	assert.False(t, allScopes[3].Expensive)

	// Verify each provider returns correct variables.
	vars1 := s.variables(allScopes[2].VariablesReference)
	require.Len(t, vars1, 1)
	assert.Equal(t, "key1", vars1[0].Name)
	assert.Equal(t, "val1", vars1[0].Value)

	vars2 := s.variables(allScopes[3].VariablesReference)
	require.Len(t, vars2, 1)
	assert.Equal(t, "hit-rate", vars2[0].Name)
	assert.Equal(t, "0.95", vars2[0].Value)
	assert.Equal(t, "float", vars2[0].Type)

	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

func TestDAPServer_CustomScopeProvider_Empty(t *testing.T) {
	t.Parallel()

	provider := &mockScopeProvider{
		name:      "Empty Scope",
		expensive: false,
		vars:      func(env *lisp.LEnv) []debugger.ScopeVariable { return nil },
	}

	s := setupDAPSession(t, debugger.WithScopeProviders(provider))

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

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrameID := stResp.Body.StackFrames[0].Id

	// Scope should appear even when empty.
	allScopes := s.scopes(topFrameID)
	require.Len(t, allScopes, 3)
	assert.Equal(t, "Empty Scope", allScopes[2].Name)
	assert.Greater(t, allScopes[2].VariablesReference, 0,
		"empty scope should still have a valid VariablesReference for DAP clients")

	// Variables request returns empty list.
	vars := s.variables(allScopes[2].VariablesReference)
	assert.Empty(t, vars)

	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

// stepInTargets sends a stepInTargets request and returns the response.
func (s *dapTestSession) stepInTargets(frameID int) *dap.StepInTargetsResponse {
	s.send(&dap.StepInTargetsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stepInTargets",
		},
		Arguments: dap.StepInTargetsArguments{FrameId: frameID},
	})
	msg := s.read()
	resp, ok := msg.(*dap.StepInTargetsResponse)
	require.True(s.t, ok, "expected StepInTargetsResponse, got %T", msg)
	return resp
}

// stepInWithTarget sends a stepIn request with a specific target ID.
func (s *dapTestSession) stepInWithTarget(targetID int) {
	s.send(&dap.StepInRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stepIn",
		},
		Arguments: dap.StepInArguments{
			ThreadId: elpsThreadID,
			TargetId: targetID,
		},
	})
	s.read() // StepInResponse
}

func TestDAPServer_StepInTargets(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint on line 4 (inside main, the call expression).
	s.send(&dap.SetBreakpointsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "setBreakpoints",
		},
		Arguments: dap.SetBreakpointsArguments{
			Source:      dap.Source{Path: "test"},
			Breakpoints: []dap.SourceBreakpoint{{Line: 4}},
		},
	})
	s.read() // SetBreakpointsResponse
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test",
			"(defun f (x) (+ x 1))\n"+   // line 1
				"(defun g (x) (+ x 2))\n"+ // line 2
				"(defun main ()\n"+          // line 3
				"  (f (g 10)))\n"+           // line 4
				"(main)")                    // line 5
		resultCh <- res
	}()

	// Wait for breakpoint on line 4.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Get stack trace to find the top frame ID.
	st := s.stackTrace()
	require.Greater(t, len(st.Body.StackFrames), 0)
	topFrameID := st.Body.StackFrames[0].Id

	// Request step-in targets.
	targetsResp := s.stepInTargets(topFrameID)
	assert.True(t, targetsResp.Success)

	// Should find exactly "f" and "g" as targets (no builtins like +).
	require.Len(t, targetsResp.Body.Targets, 2, "should find exactly 2 user-defined targets")
	var labels []string
	for _, tgt := range targetsResp.Body.Targets {
		labels = append(labels, tgt.Label)
	}
	assert.Contains(t, labels, "f", "should include f as a step-in target")
	assert.Contains(t, labels, "g", "should include g as a step-in target")

	// Find the target ID for "f".
	var fTargetID int
	for _, tgt := range targetsResp.Body.Targets {
		if tgt.Label == "f" {
			fTargetID = tgt.Id
			break
		}
	}
	require.NotZero(t, fTargetID, "should have found target ID for f")

	// Clear breakpoints before stepping.
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

	// Step-in with target "f" — should pause inside f's body.
	s.stepInWithTarget(fTargetID)

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Verify we paused inside f (line 1).
	st2 := s.stackTrace()
	require.Greater(t, len(st2.Body.StackFrames), 0)
	assert.Equal(t, 1, st2.Body.StackFrames[0].Line,
		"should be paused inside f's body on line 1")

	// Continue to finish.
	s.continueExec()

	select {
	case res := <-resultCh:
		require.False(t, res.Type == lisp.LError, "program error: %v", res)
		assert.Equal(t, lisp.LInt, res.Type, "expected int result")
		assert.Equal(t, 13, res.Int, "f(g(10)) = f(12) = 13")
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

func TestDAPServer_StepInTargets_Empty(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))
	s.configDone()

	env := newDAPTestEnv(t, s.engine)
	resultCh := make(chan *lisp.LVal, 1)
	// Expression with no user-defined function calls.
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	// Wait for stop-on-entry.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Request step-in targets — should be empty (+ is a builtin).
	// On stop-on-entry the stack may be empty, so use frame 0.
	targetsResp := s.stepInTargets(0)
	assert.True(t, targetsResp.Success)
	assert.Empty(t, targetsResp.Body.Targets,
		"no user-defined targets should be found for builtin-only expression")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

func TestDAPServer_StepInTargets_RegularStepIn(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

	// Set breakpoint on line 3 (call to f).
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
	go func() {
		res := env.LoadString("test",
			"(defun f (x)\n"+  // line 1
				"  (+ x 1))\n"+ // line 2
				"(f 10)")        // line 3
		resultCh <- res
	}()

	// Wait for breakpoint on line 3.
	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Clear breakpoints.
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

	// Regular step-in (no target ID) — should enter f's body.
	s.send(&dap.StepInRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "stepIn",
		},
		Arguments: dap.StepInArguments{ThreadId: elpsThreadID},
	})
	s.read() // StepInResponse

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	// Should be inside f's body (line 2).
	st := s.stackTrace()
	require.Greater(t, len(st.Body.StackFrames), 0)
	assert.Equal(t, 2, st.Body.StackFrames[0].Line,
		"regular step-in should enter f's body on line 2")

	// Continue to finish.
	s.continueExec()

	select {
	case res := <-resultCh:
		require.False(t, res.Type == lisp.LError, "program error: %v", res)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

func TestDAPServer_StepInTargets_Capability(t *testing.T) {
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
	msg, err := dap.ReadProtocolMessage(reader)
	require.NoError(t, err)
	initResp, ok := msg.(*dap.InitializeResponse)
	require.True(t, ok, "expected InitializeResponse, got %T", msg)
	assert.True(t, initResp.Success)
	assert.True(t, initResp.Body.SupportsStepInTargetsRequest,
		"SupportsStepInTargetsRequest should be true in capabilities")

	// Read Initialized event.
	_, err = dap.ReadProtocolMessage(reader)
	require.NoError(t, err)

	// Disconnect.
	sendDAPRequest(t, client, &dap.DisconnectRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: 2, Type: "request"},
			Command:         "disconnect",
		},
	})
	_, err = dap.ReadProtocolMessage(reader) // DisconnectResponse
	require.NoError(t, err)
}

// TestDAPServer_EvaluateContextSemantics verifies context-aware evaluation:
// repl supports multi-expression eval (progn semantics), hover uses a child
// env and single-expression eval, and the default (empty) context uses
// multi-expression eval like repl.
func TestDAPServer_EvaluateContextSemantics(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t, debugger.WithStopOnEntry(true))

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
		resultCh <- env.LoadString("test", "(defun add (a b)\n  (+ a b))\n(add 10 20)")
	}()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent (entry)
	s.continueExec()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")
	s.read() // StoppedEvent (breakpoint)

	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrameID := stResp.Body.StackFrames[0].Id

	// --- Repl: multi-expression with progn semantics ---
	evalResp := s.evaluateWithContext("(set 'repl-x 10) (+ repl-x 5)", topFrameID, "repl")
	assert.True(t, evalResp.Success, "repl multi-expr should succeed")
	assert.Equal(t, "15", evalResp.Body.Result)

	// --- Repl: mutations persist ---
	evalResp2 := s.evaluateWithContext("repl-x", topFrameID, "repl")
	assert.True(t, evalResp2.Success)
	assert.Equal(t, "10", evalResp2.Body.Result)

	// --- Hover: reads repl-created bindings via package lookup ---
	hoverResp := s.evaluateWithContext("repl-x", topFrameID, "hover")
	assert.True(t, hoverResp.Success, "hover should read repl-created variable")
	assert.Equal(t, "10", hoverResp.Body.Result)

	// --- Hover: single-expression semantics ---
	hoverResp2 := s.evaluateWithContext("(+ 1 2) (+ 3 4)", topFrameID, "hover")
	assert.True(t, hoverResp2.Success)
	assert.Equal(t, "3", hoverResp2.Body.Result, "hover should return first expression only")

	// --- Default context (empty string): multi-expression like repl ---
	defaultResp := s.evaluateWithContext("(set 'default-y 7) (+ default-y 3)", topFrameID, "")
	assert.True(t, defaultResp.Success, "default context multi-expr should succeed")
	assert.Equal(t, "10", defaultResp.Body.Result)

	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

// completions sends a completions request and returns the response.
func (s *dapTestSession) completions(text string, column int, frameID int) *dap.CompletionsResponse {
	s.send(&dap.CompletionsRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "completions",
		},
		Arguments: dap.CompletionsArguments{
			Text:    text,
			Column:  column,
			FrameId: frameID,
		},
	})
	msg := s.read()
	resp, ok := msg.(*dap.CompletionsResponse)
	require.True(s.t, ok, "expected CompletionsResponse, got %T", msg)
	return resp
}

func TestDAPServer_Completions(t *testing.T) {
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
	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrameID := stResp.Body.StackFrames[0].Id

	// --- Test 1: Complete local variable "a" (and verify "b" is excluded) ---
	compResp := s.completions("(+ a", 5, topFrameID)
	assert.True(t, compResp.Success)
	labels := completionLabels(compResp.Body.Targets)
	assert.Contains(t, labels, "a", "local variable 'a' should appear in completions")
	assert.NotContains(t, labels, "b", "'b' should not match prefix 'a'")

	// --- Test 1b: Complete local variable "b" ---
	compRespB := s.completions("(+ b", 5, topFrameID)
	assert.True(t, compRespB.Success)
	labelsB := completionLabels(compRespB.Body.Targets)
	assert.Contains(t, labelsB, "b", "local variable 'b' should appear in completions")

	// --- Test 2: Complete builtin prefix "defu" ---
	compResp2 := s.completions("(defu", 6, topFrameID)
	assert.True(t, compResp2.Success)
	labels2 := completionLabels(compResp2.Body.Targets)
	assert.Contains(t, labels2, "defun", "builtin 'defun' should appear in completions")

	// --- Test 3: Complete package name "stri" ---
	compResp3 := s.completions("(stri", 6, topFrameID)
	assert.True(t, compResp3.Success)
	labels3 := completionLabels(compResp3.Body.Targets)
	assert.Contains(t, labels3, "string:", "package name 'string:' should appear")

	// --- Test 4: Empty prefix returns no targets ---
	compResp4 := s.completions("(", 2, topFrameID)
	assert.True(t, compResp4.Success)
	assert.Empty(t, compResp4.Body.Targets, "empty prefix should produce no targets")

	// Continue to finish.
	s.continueExec()

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

func TestDAPServer_CompletionsNotPaused(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)
	s.configDone()

	// Completions when not paused should return empty targets, not an error.
	// Test with frameId 0 and a non-zero frameId to ensure the not-paused
	// check (not the frame ID) is what produces the empty result.
	compResp := s.completions("(+ a", 5, 0)
	assert.True(t, compResp.Success)
	assert.Empty(t, compResp.Body.Targets, "completions when not paused should be empty (frameId=0)")

	compResp2 := s.completions("(+ a", 5, 1)
	assert.True(t, compResp2.Success)
	assert.Empty(t, compResp2.Body.Targets, "completions when not paused should be empty (frameId=1)")

	s.disconnect()
}

func TestDAPServer_CompletionsCapability(t *testing.T) {
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

	// Send Initialize request.
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
	require.True(t, ok, "expected InitializeResponse, got %T", msg)
	assert.True(t, initResp.Body.SupportsCompletionsRequest,
		"SupportsCompletionsRequest should be true")
	assert.Equal(t, []string{"(", ":", "'"}, initResp.Body.CompletionTriggerCharacters)
}

// completionLabels extracts labels from completion items for easy assertion.
func completionLabels(items []dap.CompletionItem) []string {
	labels := make([]string, len(items))
	for i, item := range items {
		labels[i] = item.Label
	}
	return labels
}

// variablesWithPagination requests variables with Start/Count pagination.
func (s *dapTestSession) variablesWithPagination(ref, start, count int) []dap.Variable {
	s.send(&dap.VariablesRequest{
		Request: dap.Request{
			ProtocolMessage: dap.ProtocolMessage{Seq: s.nextSeq(), Type: "request"},
			Command:         "variables",
		},
		Arguments: dap.VariablesArguments{
			VariablesReference: ref,
			Start:              start,
			Count:              count,
		},
	})
	msg := s.read()
	varsResp, ok := msg.(*dap.VariablesResponse)
	require.True(s.t, ok, "expected VariablesResponse, got %T", msg)
	return varsResp.Body.Variables
}

func TestDAPServer_VariablePagination(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

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
		resultCh <- env.LoadString("test", "(defun f (x)\n  (+ x 1))\n(f 10)")
	}()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrame := stResp.Body.StackFrames[0].Id

	// Evaluate a 5-element list to get an expandable reference.
	evalResp := s.evaluate("(list 10 20 30 40 50)", topFrame)
	assert.True(t, evalResp.Success)
	ref := evalResp.Body.VariablesReference
	require.Greater(t, ref, 0, "list should be expandable")

	// Request with Start=1, Count=2 — should get elements [1] and [2].
	page := s.variablesWithPagination(ref, 1, 2)
	require.Len(t, page, 2)
	assert.Equal(t, "[1]", page[0].Name)
	assert.Equal(t, "20", page[0].Value)
	assert.Equal(t, "[2]", page[1].Name)
	assert.Equal(t, "30", page[1].Value)

	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

func TestDAPServer_VariablePaginationStartOnly(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

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
		resultCh <- env.LoadString("test", "(defun f (x)\n  (+ x 1))\n(f 10)")
	}()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrame := stResp.Body.StackFrames[0].Id

	evalResp := s.evaluate("(list 10 20 30 40 50)", topFrame)
	assert.True(t, evalResp.Success)
	ref := evalResp.Body.VariablesReference
	require.Greater(t, ref, 0)

	// Start=3, Count=0 (no count) — should return tail from [3] onward.
	page := s.variablesWithPagination(ref, 3, 0)
	require.Len(t, page, 2, "should return last 2 elements")
	assert.Equal(t, "[3]", page[0].Name)
	assert.Equal(t, "40", page[0].Value)
	assert.Equal(t, "[4]", page[1].Name)
	assert.Equal(t, "50", page[1].Value)

	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

func TestDAPServer_VariablePaginationBeyondEnd(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

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
		resultCh <- env.LoadString("test", "(defun f (x)\n  (+ x 1))\n(f 10)")
	}()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrame := stResp.Body.StackFrames[0].Id

	evalResp := s.evaluate("(list 1 2 3)", topFrame)
	assert.True(t, evalResp.Success)
	ref := evalResp.Body.VariablesReference
	require.Greater(t, ref, 0)

	// Start past end — should return empty.
	page := s.variablesWithPagination(ref, 100, 0)
	assert.Empty(t, page, "start far past end should return empty")

	// Start exactly at length — should also return empty.
	page = s.variablesWithPagination(ref, 3, 0)
	assert.Empty(t, page, "start at exact length should return empty")

	// Start=0 with Count — should return first N without triggering offset.
	page = s.variablesWithPagination(ref, 0, 2)
	require.Len(t, page, 2, "start=0 count=2 should return first 2")
	assert.Equal(t, "[0]", page[0].Name)
	assert.Equal(t, "[1]", page[1].Name)

	// Count=1 — minimal count value.
	page = s.variablesWithPagination(ref, 1, 1)
	require.Len(t, page, 1, "count=1 should return exactly 1")
	assert.Equal(t, "[1]", page[0].Name)

	// Count exceeds remaining — should return what's left.
	page = s.variablesWithPagination(ref, 2, 100)
	require.Len(t, page, 1, "count exceeding remaining should return rest")
	assert.Equal(t, "[2]", page[0].Name)

	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}

func TestDAPServer_EvaluatePaginationHints(t *testing.T) {
	t.Parallel()
	s := setupDAPSession(t)

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
		resultCh <- env.LoadString("test", "(defun f (x)\n  (+ x 1))\n(f 10)")
	}()

	require.Eventually(t, func() bool {
		return s.engine.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)
	s.read() // StoppedEvent

	stResp := s.stackTrace()
	require.Greater(t, len(stResp.Body.StackFrames), 0)
	topFrame := stResp.Body.StackFrames[0].Id

	// Evaluate a list — IndexedVariables should be set.
	listResp := s.evaluate("(list 1 2 3 4)", topFrame)
	assert.True(t, listResp.Success)
	assert.Equal(t, 4, listResp.Body.IndexedVariables,
		"list eval should report 4 indexed children")
	assert.Equal(t, 0, listResp.Body.NamedVariables,
		"list eval should report 0 named children")

	// Evaluate a sorted-map — NamedVariables should be set.
	mapResp := s.evaluate(`(sorted-map :a 1 :b 2)`, topFrame)
	assert.True(t, mapResp.Success)
	assert.Greater(t, mapResp.Body.VariablesReference, 0,
		"map should be expandable")
	assert.Equal(t, 0, mapResp.Body.IndexedVariables,
		"map eval should report 0 indexed children")
	assert.Equal(t, 2, mapResp.Body.NamedVariables,
		"map eval should report 2 named children")

	// Evaluate a scalar — no hints.
	scalarResp := s.evaluate("42", topFrame)
	assert.True(t, scalarResp.Success)
	assert.Equal(t, 0, scalarResp.Body.VariablesReference,
		"scalar should not be expandable")
	assert.Equal(t, 0, scalarResp.Body.IndexedVariables,
		"scalar eval should report 0 indexed children")
	assert.Equal(t, 0, scalarResp.Body.NamedVariables,
		"scalar eval should report 0 named children")

	s.continueExec()
	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	s.disconnect()
}
