package dapserver

// implements VSCode's DAP protocol (https://microsoft.github.io/debug-adapter-protocol/specification)
// to keep Amir happy.
//
// Extension point docs at https://code.visualstudio.com/api/extension-guides/debugger-extension
// More useful info on how to integrate in vscode at https://marketplace.visualstudio.com/items?itemName=andreweinand.mock-debug
// and https://github.com/Microsoft/vscode-debugadapter-node

import (
	"bufio"
	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp/x/debugger/delveserver"
	"github.com/luthersystems/elps/lisp/x/debugger/events"
	log "github.com/sirupsen/logrus"
	"io"
	"net"
	"runtime"
	"sync"
	"time"
)

type Server struct {
	endChannel   chan bool
	Address      string
	listener     net.Listener
	connQueue    chan net.Conn
	connection   *connection
	sequence     int
	sequenceLock sync.Mutex
	wg           *sync.WaitGroup
	debugger     *debuggerwrapper
}

type connection struct {
	s         *Server
	rw        *bufio.ReadWriter
	queue     chan dap.Message
	kill      chan bool
	Connected bool
	Event     chan events.EventType
}

func NewServer(debugger delveserver.ServerDebugger, address string, handlers int) (*Server, error) {
	server := &Server{
		Address:    address,
		connection: &connection{},
		connQueue:  make(chan net.Conn, 10),
		sequence:   0,
		wg:         new(sync.WaitGroup),
		debugger:   &debuggerwrapper{debugger: debugger},
	}
	server.connection.s = server
	return server, nil
}

func (s *Server) Run() error {
	go startMonitor()
	log.Infof("Listening on %s", s.Address)
	listener, err := net.Listen("tcp", s.Address)
	if err != nil {
		return err
	}

	s.listener = listener
	s.wg.Add(1)
	go s.listen()

	return nil
}

func startMonitor() {
	for {
		log.Debugf("Monitor heartbeat")
		time.Sleep(1 * time.Second)
		runtime.Gosched()
	}
}

func (s *Server) Stop() error {
	s.endChannel <- true
	s.wg.Wait()
	return nil
}

func (s *Server) Event(x events.EventType) {
	if s.connection.Connected {
		go func(ch chan events.EventType) { ch <- x }(s.connection.Event)
	}
}

func (s *Server) listen() {
	defer s.listener.Close()
	for {
		conn, err := s.listener.Accept()
		if err != nil {
			log.Errorf("Connection failed:", err)
			continue
		}
		log.Errorf("Accepted connection from", conn.RemoteAddr())
		s.connection.start(conn)
		return
	}
}

func (s *Server) getSequence() int {
	s.sequenceLock.Lock()
	defer s.sequenceLock.Unlock()
	return s.sequence
}

func (s *Server) incSequence() int {
	s.sequenceLock.Lock()
	defer s.sequenceLock.Unlock()
	s.sequence += 1
	return s.sequence
}

func (h *connection) killconnection() {
	h.kill <- true
}

func (h *connection) start(conn net.Conn) {
	h.rw = bufio.NewReadWriter(bufio.NewReader(conn), bufio.NewWriter(conn))
	h.queue = make(chan dap.Message)
	h.Connected = true
	h.Event = make(chan events.EventType)
	go func() {
		for {
			select {
			case <-h.kill:
				h.kill <- true
				return
			case event := <-h.Event:
				h.sendEventMessage(event)
			}
		}
	}()
	go func() {
		for {
			select {
			case message := <-h.queue:
				h.sendMessage(message)
			case <-h.kill:
				h.kill <- true
				return

			}
		}
	}()
	for {
		select {
		case <-h.kill:
			h.kill <- true
			return
		default:
			// no-op
		}
		err := h.handleRequest()
		if err != nil {
			if err == io.EOF {
				log.Errorf("Connection closed: ", err)
				h.s.endChannel <- true
				return
			}
			log.Fatal("Server error: ", err)
		}
	}
}

func (h *connection) handleRequest() error {
	request, err := dap.ReadProtocolMessage(h.rw.Reader)
	if err != nil {
		return err
	}
	log.Infof("Received request\n\t%#v\n", request)
	h.s.wg.Add(1)
	go func() {
		h.dispatchRequest(request)
		h.s.wg.Done()
	}()
	return nil
}

func (h *connection) sendHandler() {
	for message := range h.queue {
		log.Infof("Message: %s", message)
		err := dap.WriteProtocolMessage(h.rw.Writer, message)
		if err != nil {
			log.Errorf("Error sending message: %s", err.Error())
		}
		_ = h.rw.Flush()
	}
}

func (h *connection) dispatchRequest(request dap.Message) {
	switch request := request.(type) {
	case *dap.InitializeRequest:
		h.s.debugger.onInitializeRequest(request, h)
	case *dap.LaunchRequest:
		h.s.debugger.onLaunchRequest(request, h.queue, h.s.incSequence())
	case *dap.AttachRequest:
		h.s.debugger.onAttachRequest(request, h.queue, h.s.incSequence())
	case *dap.DisconnectRequest:
		h.s.debugger.onDisconnectRequest(request, h.queue, h.s.incSequence())
		h.killconnection()
	case *dap.TerminateRequest:
		h.s.debugger.onTerminateRequest(request, h.queue, h.s.incSequence())
	case *dap.RestartRequest:
		h.s.debugger.onRestartRequest(request, h.queue, h.s.incSequence())
	case *dap.SetBreakpointsRequest:
		h.s.debugger.onSetBreakpointsRequest(request, h.queue, h.s.incSequence())
	case *dap.SetFunctionBreakpointsRequest:
		h.s.debugger.onSetFunctionBreakpointsRequest(request, h.queue, h.s.incSequence())
	case *dap.SetExceptionBreakpointsRequest:
		h.s.debugger.onSetExceptionBreakpointsRequest(request, h.queue, h.s.incSequence())
	case *dap.ConfigurationDoneRequest:
		h.s.debugger.onConfigurationDoneRequest(request, h.queue, h.s.incSequence())
	case *dap.ContinueRequest:
		h.s.debugger.onContinueRequest(request, h.queue, h.s.incSequence())
	case *dap.NextRequest:
		h.s.debugger.onNextRequest(request, h.queue, h.s.incSequence())
	case *dap.StepInRequest:
		h.s.debugger.onStepInRequest(request, h.queue, h.s.incSequence())
	case *dap.StepOutRequest:
		h.s.debugger.onStepOutRequest(request, h.queue, h.s.incSequence())
	case *dap.StepBackRequest:
		h.s.debugger.onStepBackRequest(request, h.queue, h.s.incSequence())
	case *dap.ReverseContinueRequest:
		h.s.debugger.onReverseContinueRequest(request, h.queue, h.s.incSequence())
	case *dap.RestartFrameRequest:
		h.s.debugger.onRestartFrameRequest(request, h.queue, h.s.incSequence())
	case *dap.GotoRequest:
		h.s.debugger.onGotoRequest(request, h.queue, h.s.incSequence())
	case *dap.PauseRequest:
		h.s.debugger.onPauseRequest(request, h.queue, h.s.incSequence())
	case *dap.StackTraceRequest:
		h.s.debugger.onStackTraceRequest(request, h.queue, h.s.incSequence())
	case *dap.ScopesRequest:
		h.s.debugger.onScopesRequest(request, h.queue)
	case *dap.VariablesRequest:
		h.s.debugger.onVariablesRequest(request, h.queue)
	case *dap.SetVariableRequest:
		h.s.debugger.onSetVariableRequest(request, h.queue)
	case *dap.SetExpressionRequest:
		h.s.debugger.onSetExpressionRequest(request, h.queue, h.s.incSequence())
	case *dap.SourceRequest:
		h.s.debugger.onSourceRequest(request, h.queue, h.s.incSequence())
	case *dap.ThreadsRequest:
		h.s.debugger.onThreadsRequest(request, h.queue, h.s.incSequence())
	case *dap.TerminateThreadsRequest:
		h.s.debugger.onTerminateThreadsRequest(request, h.queue, h.s.incSequence())
	case *dap.EvaluateRequest:
		h.s.debugger.onEvaluateRequest(request, h.queue, h.s.incSequence())
	case *dap.StepInTargetsRequest:
		h.s.debugger.onStepInTargetsRequest(request, h.queue)
	case *dap.GotoTargetsRequest:
		h.s.debugger.onGotoTargetsRequest(request, h.queue, h.s.incSequence())
	case *dap.CompletionsRequest:
		h.s.debugger.onCompletionsRequest(request, h.queue, h.s.incSequence())
	case *dap.ExceptionInfoRequest:
		h.s.debugger.onExceptionInfoRequest(request, h.queue, h.s.incSequence())
	case *dap.LoadedSourcesRequest:
		h.s.debugger.onLoadedSourcesRequest(request, h.queue, h.s.incSequence())
	case *dap.DataBreakpointInfoRequest:
		h.s.debugger.onDataBreakpointInfoRequest(request, h.queue, h.s.incSequence())
	case *dap.SetDataBreakpointsRequest:
		h.s.debugger.onSetDataBreakpointsRequest(request, h.queue, h.s.incSequence())
	case *dap.ReadMemoryRequest:
		h.s.debugger.onReadMemoryRequest(request, h.queue, h.s.incSequence())
	case *dap.DisassembleRequest:
		h.s.debugger.onDisassembleRequest(request, h.queue, h.s.incSequence())
	case *dap.CancelRequest:
		h.s.debugger.onCancelRequest(request, h.queue, h.s.incSequence())
	case *dap.BreakpointLocationsRequest:
		h.s.debugger.onBreakpointLocationsRequest(request, h.queue, h.s.incSequence())
	default:
		log.Fatalf("Unable to process %#v", request)
	}
}

func (h *connection) sendEventMessage(event events.EventType) {
	log.Infof("Sending event %s", event)
	switch event {
	case events.EventTypeContinued:
		h.queue <- &dap.ContinuedEvent{
			Event: dap.Event{Event: "continued",
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  h.s.incSequence(),
					Type: "event",
				},
			},
			Body: dap.ContinuedEventBody{
				AllThreadsContinued: true,
				ThreadId:            1,
			},
		}
	case events.EventTypeExited:
		h.queue <- &dap.ExitedEvent{
			Event: dap.Event{Event: "continued",
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  h.s.incSequence(),
					Type: "event",
				},
			},
			Body: dap.ExitedEventBody{
				ExitCode: 0, //TODO this is so not true
			},
		}
	case events.EventTypeStarted:
		// this is a no-op for us here
	case events.EventTypeStoppedPaused:
		h.queue <- &dap.StoppedEvent{
			Event: dap.Event{
				Event: "stopped",
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  h.s.incSequence(),
					Type: "event",
				},
			},
			Body: dap.StoppedEventBody{
				AllThreadsStopped: true,
				ThreadId:          1,
				Reason:            "pause", // we need to propagate this by using more reasons
			},
		}
	case events.EventTypeStoppedBreakpoint:
		h.queue <- &dap.StoppedEvent{
			Event: dap.Event{Event: "stopped",
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  h.s.incSequence(),
					Type: "event",
				},
			},
			Body: dap.StoppedEventBody{
				AllThreadsStopped: true,
				ThreadId:          1,
				Reason:            "breakpoint", // we need to propagate this by using more reasons
			},
		}
	case events.EventTypeStoppedEntry:
		h.queue <- &dap.StoppedEvent{
			Event: dap.Event{Event: "stopped",
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  h.s.incSequence(),
					Type: "event",
				},
			},
			Body: dap.StoppedEventBody{
				AllThreadsStopped: true,
				ThreadId:          1,
				Reason:            "entry", // we need to propagate this by using more reasons
			},
		}
	case events.EventTypeStoppedStep:
		h.queue <- &dap.StoppedEvent{
			Event: dap.Event{Event: "stopped",
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  h.s.incSequence(),
					Type: "event",
				},
			},
			Body: dap.StoppedEventBody{
				AllThreadsStopped: true,
				ThreadId:          1,
				Reason:            "step", // we need to propagate this by using more reasons
			},
		}
	case events.EventTypeTerminated:
		h.queue <- &dap.TerminatedEvent{
			Event: dap.Event{
				Event: "terminated",
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  h.s.incSequence(),
					Type: "event",
				},
			},
			Body: dap.TerminatedEventBody{
				Restart: false,
			},
		}
	}
}

func (h *connection) sendMessage(message dap.Message) {
	log.Infof("Sending message over wire: %#v", message)
	err := dap.WriteProtocolMessage(h.rw, message)
	if err != nil {
		log.Warnf("Error sending: %s", err.Error())
	}
	err = h.rw.Flush()
	if err != nil {
		log.Warnf("Error flushing: %s", err.Error())
	}
}
