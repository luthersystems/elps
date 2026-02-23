// Copyright © 2018 The ELPS authors

// Package dapserver implements a DAP (Debug Adapter Protocol) server for
// the ELPS debugger engine. It translates between the DAP wire protocol
// and the debugger.Engine interface.
//
// The server supports two transport modes:
//   - TCP: Primary mode for embedded/attached debugging. The server listens
//     on a TCP port and accepts a single client connection.
//   - Stdio: For CLI use (e.g., "elps debug --stdio"). The server reads
//     from stdin and writes to stdout, as expected by editors like VS Code
//     when launching a debug adapter as a child process.
package dapserver

import (
	"bufio"
	"io"
	"net"
	"sync"

	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp/x/debugger"
)

// Server is a DAP protocol server that wraps a debugger Engine.
type Server struct {
	engine *debugger.Engine

	mu     sync.Mutex
	seq    int
	writer io.Writer
	reader *bufio.Reader

	// done is closed when the server should stop processing messages.
	done chan struct{}
}

// New creates a new DAP server wrapping the given debugger engine.
func New(engine *debugger.Engine) *Server {
	s := &Server{
		engine: engine,
		done:   make(chan struct{}),
	}
	return s
}

// ServeConn serves DAP messages on a single connection. It blocks until
// the connection is closed or a disconnect request is received.
func (s *Server) ServeConn(conn io.ReadWriteCloser) error {
	defer conn.Close() //nolint:errcheck // best-effort cleanup
	s.mu.Lock()
	s.writer = conn
	s.reader = bufio.NewReader(conn)
	s.mu.Unlock()

	// Register event callback so the engine notifies us of state changes.
	s.engine.Breakpoints() // ensure initialized
	handler := newHandler(s, s.engine)

	for {
		select {
		case <-s.done:
			return nil
		default:
		}

		msg, err := dap.ReadProtocolMessage(s.reader)
		if err != nil {
			select {
			case <-s.done:
				return nil
			default:
				if err == io.EOF {
					return nil
				}
				return err
			}
		}

		handler.handle(msg)
	}
}

// ServeTCP listens on the given address and serves a single DAP client.
// It blocks until the client disconnects.
func (s *Server) ServeTCP(addr string) error {
	ln, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	defer ln.Close() //nolint:errcheck // best-effort cleanup
	return s.ServeListener(ln)
}

// ServeListener accepts a single connection from the listener and serves
// DAP messages on it.
func (s *Server) ServeListener(ln net.Listener) error {
	conn, err := ln.Accept()
	if err != nil {
		return err
	}
	return s.ServeConn(conn)
}

// ServeStdio serves DAP messages on the given reader and writer,
// typically os.Stdin and os.Stdout.
func (s *Server) ServeStdio(r io.Reader, w io.Writer) error {
	s.mu.Lock()
	s.writer = w
	s.reader = bufio.NewReader(r)
	s.mu.Unlock()

	handler := newHandler(s, s.engine)

	for {
		select {
		case <-s.done:
			return nil
		default:
		}

		msg, err := dap.ReadProtocolMessage(s.reader)
		if err != nil {
			select {
			case <-s.done:
				return nil
			default:
				if err == io.EOF {
					return nil
				}
				return err
			}
		}

		handler.handle(msg)
	}
}

// send writes a DAP protocol message to the client.
// The caller is responsible for setting the Seq field before calling send
// (via the newResponse/newEvent helpers which call nextSeq).
func (s *Server) send(msg dap.Message) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	return dap.WriteProtocolMessage(s.writer, msg)
}

// nextSeq returns the next sequence number for outgoing messages.
func (s *Server) nextSeq() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.seq++
	return s.seq
}

// close signals the server to stop processing messages.
func (s *Server) close() {
	select {
	case <-s.done:
	default:
		close(s.done)
	}
}
