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
	"sync"
)

type Server struct {
	endChannel     chan bool
	Address        string
	listener       net.Listener
	connQueue      chan net.Conn
	connectionPool []*connection
	sequence       uint32
	sequenceLock   sync.Mutex
	wg             *sync.WaitGroup
	debugger       *debuggerwrapper
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
	pool := make([]*connection, handlers)
	for x := 0; x < handlers; x++ {
		pool[x] = &connection{}
	}
	server := &Server{
		Address:        address,
		connectionPool: pool,
		connQueue:      make(chan net.Conn, 10),
		sequence:       0,
		wg:             new(sync.WaitGroup),
		debugger:       &debuggerwrapper{debugger: debugger},
	}
	for x := 0; x < handlers; x++ {
		pool[x].s = server
	}
	return server, nil
}

func (s *Server) Run() error {
	listener, err := net.Listen("tcp", s.Address)
	if err != nil {
		return err
	}
	for _, h := range s.connectionPool {
		go h.start()
		s.wg.Add(1)
	}
	s.listener = listener
	go s.listen()
	return nil
}

func (s *Server) Stop() error {
	s.endChannel <- true
	s.wg.Wait()
	return nil
}

func (s *Server) Event(x events.EventType) {
	for _, v := range s.connectionPool {
		if v.Connected {
			go func(ch chan events.EventType) { ch <- x }(v.Event)
		}
	}
}

func (s *Server) listen() {
	defer s.listener.Close()
	for {
		conn, err := s.listener.Accept()
		if err != nil {
			log.Println("Connection failed:", err)
			continue
		}
		log.Println("Accepted connection from", conn.RemoteAddr())
		s.connQueue <- conn
	}
}

func (s *Server) getSequence() uint32 {
	s.sequenceLock.Lock()
	defer s.sequenceLock.Unlock()
	return s.sequence
}

func (s *Server) incSequence() uint32 {
	s.sequenceLock.Lock()
	defer s.sequenceLock.Unlock()
	s.sequence += 1
	return s.sequence
}

func (h *connection) killconnection() {
	h.kill <- true
}

func (h *connection) start() {
	for {
		select {
		case conn := <-h.s.connQueue:
			h.rw = bufio.NewReadWriter(bufio.NewReader(conn), bufio.NewWriter(conn))
			h.queue = make(chan dap.Message, 5)
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
						log.Println("No more data to read:", err)
						break
					}
					log.Fatal("Server error: ", err)
				}
			}
		case <-h.s.endChannel:
			h.s.endChannel <- true
			h.Connected = false
			h.s.wg.Done()
			return
		}
	}
}

func (h *connection) handleRequest() error {
	request, err := dap.ReadProtocolMessage(h.rw.Reader)
	if err != nil {
		return err
	}
	log.Printf("Received request\n\t%#v\n", request)
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
		h.s.debugger.onLaunchRequest(request, h.queue)
	case *dap.AttachRequest:
		h.s.debugger.onAttachRequest(request, h.queue)
	case *dap.DisconnectRequest:
		h.s.debugger.onDisconnectRequest(request, h.queue)
		h.killconnection()
	case *dap.TerminateRequest:
		h.s.debugger.onTerminateRequest(request, h.queue)
	case *dap.RestartRequest:
		h.s.debugger.onRestartRequest(request, h.queue)
	case *dap.SetBreakpointsRequest:
		h.s.debugger.onSetBreakpointsRequest(request, h.queue)
	case *dap.SetFunctionBreakpointsRequest:
		h.s.debugger.onSetFunctionBreakpointsRequest(request, h.queue)
	case *dap.SetExceptionBreakpointsRequest:
		h.s.debugger.onSetExceptionBreakpointsRequest(request, h.queue)
	case *dap.ConfigurationDoneRequest:
		h.s.debugger.onConfigurationDoneRequest(request, h.queue)
	case *dap.ContinueRequest:
		h.s.debugger.onContinueRequest(request, h.queue)
	case *dap.NextRequest:
		h.s.debugger.onNextRequest(request, h.queue)
	case *dap.StepInRequest:
		h.s.debugger.onStepInRequest(request, h.queue)
	case *dap.StepOutRequest:
		h.s.debugger.onStepOutRequest(request, h.queue)
	case *dap.StepBackRequest:
		h.s.debugger.onStepBackRequest(request, h.queue)
	case *dap.ReverseContinueRequest:
		h.s.debugger.onReverseContinueRequest(request, h.queue)
	case *dap.RestartFrameRequest:
		h.s.debugger.onRestartFrameRequest(request, h.queue)
	case *dap.GotoRequest:
		h.s.debugger.onGotoRequest(request, h.queue)
	case *dap.PauseRequest:
		h.s.debugger.onPauseRequest(request, h.queue)
	case *dap.StackTraceRequest:
		h.s.debugger.onStackTraceRequest(request, h.queue)
	case *dap.ScopesRequest:
		h.s.debugger.onScopesRequest(request, h.queue)
	case *dap.VariablesRequest:
		h.s.debugger.onVariablesRequest(request, h.queue)
	case *dap.SetVariableRequest:
		h.s.debugger.onSetVariableRequest(request, h.queue)
	case *dap.SetExpressionRequest:
		h.s.debugger.onSetExpressionRequest(request, h.queue)
	case *dap.SourceRequest:
		h.s.debugger.onSourceRequest(request, h.queue)
	case *dap.ThreadsRequest:
		h.s.debugger.onThreadsRequest(request, h.queue)
	case *dap.TerminateThreadsRequest:
		h.s.debugger.onTerminateThreadsRequest(request, h.queue)
	case *dap.EvaluateRequest:
		h.s.debugger.onEvaluateRequest(request, h.queue)
	case *dap.StepInTargetsRequest:
		h.s.debugger.onStepInTargetsRequest(request, h.queue)
	case *dap.GotoTargetsRequest:
		h.s.debugger.onGotoTargetsRequest(request, h.queue)
	case *dap.CompletionsRequest:
		h.s.debugger.onCompletionsRequest(request, h.queue)
	case *dap.ExceptionInfoRequest:
		h.s.debugger.onExceptionInfoRequest(request, h.queue)
	case *dap.LoadedSourcesRequest:
		h.s.debugger.onLoadedSourcesRequest(request, h.queue)
	case *dap.DataBreakpointInfoRequest:
		h.s.debugger.onDataBreakpointInfoRequest(request, h.queue)
	case *dap.SetDataBreakpointsRequest:
		h.s.debugger.onSetDataBreakpointsRequest(request, h.queue)
	case *dap.ReadMemoryRequest:
		h.s.debugger.onReadMemoryRequest(request, h.queue)
	case *dap.DisassembleRequest:
		h.s.debugger.onDisassembleRequest(request, h.queue)
	case *dap.CancelRequest:
		h.s.debugger.onCancelRequest(request, h.queue)
	case *dap.BreakpointLocationsRequest:
		h.s.debugger.onBreakpointLocationsRequest(request, h.queue)
	default:
		log.Fatalf("Unable to process %#v", request)
	}
}

func (h *connection) sendEventMessage(event events.EventType) {
	log.Infof("Sending event %s", event)
	switch event {
	case events.EventTypeContinued:
		h.queue <- &dap.ContinuedEvent{
			Event: dap.Event{Event: "continued"},
			Body: dap.ContinuedEventBody{
				AllThreadsContinued: true,
			},
		}
	case events.EventTypeExited:
		h.queue <- &dap.ExitedEvent{
			Event: dap.Event{Event: "continued"},
			Body: dap.ExitedEventBody{
				ExitCode: 0, //TODO this is so not true
			},
		}
	case events.EventTypeStarted:
		// this is a no-op for us here
	case events.EventTypeStoppedPaused:
		h.queue <- &dap.StoppedEvent{
			Event: dap.Event{Event: "stopped"},
			Body: dap.StoppedEventBody{
				AllThreadsStopped: true,
				Reason:            "pause", // we need to propagate this by using more reasons
			},
		}
	case events.EventTypeStoppedBreakpoint:
		h.queue <- &dap.StoppedEvent{
			Event: dap.Event{Event: "stopped"},
			Body: dap.StoppedEventBody{
				AllThreadsStopped: true,
				Reason:            "breakpoint", // we need to propagate this by using more reasons
			},
		}
	case events.EventTypeStoppedEntry:
		h.queue <- &dap.StoppedEvent{
			Event: dap.Event{Event: "stopped"},
			Body: dap.StoppedEventBody{
				AllThreadsStopped: true,
				Reason:            "entry", // we need to propagate this by using more reasons
			},
		}
	case events.EventTypeStoppedStep:
		h.queue <- &dap.StoppedEvent{
			Event: dap.Event{Event: "stopped"},
			Body: dap.StoppedEventBody{
				AllThreadsStopped: true,
				Reason:            "step", // we need to propagate this by using more reasons
			},
		}
	case events.EventTypeTerminated:
		h.queue <- &dap.TerminatedEvent{
			Event: dap.Event{Event: "terminated"},
			Body: dap.TerminatedEventBody{
				Restart: false,
			},
		}
	}
}
