package dapserver

import (
	"bufio"
	"io"
	"net"
	"testing"
	"time"

	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDAPServer_InitializeAndDisconnect(t *testing.T) {
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

	// Verify breakpoints are in the engine.
	all := e.Breakpoints().All()
	assert.Len(t, all, 2)

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

// --- helpers ---

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
