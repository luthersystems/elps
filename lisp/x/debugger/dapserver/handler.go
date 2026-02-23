// Copyright © 2018 The ELPS authors

package dapserver

import (
	"log"
	"sync"

	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
)

// scopeRef constants encode scope type into variable references.
// The frame index is embedded: ref = base + frameIndex.
const (
	scopeLocalBase   = 1000
	scopePackageBase = 3000
)

// handler dispatches incoming DAP messages to the appropriate method.
type handler struct {
	server *Server
	engine *debugger.Engine

	mu          sync.Mutex
	initialized bool
	launched    bool

	// frameEnvs caches the environments for each stack frame when paused.
	// Indexed by frame ID (1-based, most recent first).
	frameEnvs map[int]*lisp.LEnv
}

func newHandler(s *Server, e *debugger.Engine) *handler {
	h := &handler{
		server: s,
		engine: e,
	}
	// Wire the engine's event callback to forward stopped events to the DAP client.
	e.SetEventCallback(func(evt debugger.Event) {
		if evt.Type == debugger.EventStopped {
			var bpIDs []int
			if evt.BP != nil {
				bpIDs = []int{evt.BP.ID}
			}
			h.sendStoppedEvent(evt.Reason, bpIDs)
		}
	})
	return h
}

// send sends a DAP message and logs any write error.
func (h *handler) send(msg dap.Message) {
	if err := h.server.send(msg); err != nil {
		log.Printf("dap: send error: %v", err)
	}
}

func (h *handler) handle(msg dap.Message) {
	switch req := msg.(type) {
	case *dap.InitializeRequest:
		h.onInitialize(req)
	case *dap.SetBreakpointsRequest:
		h.onSetBreakpoints(req)
	case *dap.SetExceptionBreakpointsRequest:
		h.onSetExceptionBreakpoints(req)
	case *dap.ConfigurationDoneRequest:
		h.onConfigurationDone(req)
	case *dap.ThreadsRequest:
		h.onThreads(req)
	case *dap.StackTraceRequest:
		h.onStackTrace(req)
	case *dap.ScopesRequest:
		h.onScopes(req)
	case *dap.VariablesRequest:
		h.onVariables(req)
	case *dap.ContinueRequest:
		h.onContinue(req)
	case *dap.NextRequest:
		h.onNext(req)
	case *dap.StepInRequest:
		h.onStepIn(req)
	case *dap.StepOutRequest:
		h.onStepOut(req)
	case *dap.EvaluateRequest:
		h.onEvaluate(req)
	case *dap.DisconnectRequest:
		h.onDisconnect(req)
	default:
		log.Printf("dap: unhandled message type: %T", msg)
	}
}

func (h *handler) onInitialize(req *dap.InitializeRequest) {
	h.mu.Lock()
	h.initialized = true
	h.mu.Unlock()

	resp := &dap.InitializeResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	resp.Body = dap.Capabilities{
		SupportsConfigurationDoneRequest:  true,
		SupportsConditionalBreakpoints:    true,
		SupportsEvaluateForHovers:         true,
		SupportTerminateDebuggee:          true,
		ExceptionBreakpointFilters: []dap.ExceptionBreakpointsFilter{
			{
				Filter:  "all",
				Label:   "All Exceptions",
				Default: false,
			},
		},
	}
	h.send(resp)

	// Send initialized event to tell the client it can send configuration.
	h.send(&dap.InitializedEvent{
		Event: h.newEvent("initialized"),
	})
}

func (h *handler) onSetBreakpoints(req *dap.SetBreakpointsRequest) {
	file := req.Arguments.Source.Path
	if file == "" {
		file = req.Arguments.Source.Name
	}

	lines := make([]int, len(req.Arguments.Breakpoints))
	conditions := make([]string, len(req.Arguments.Breakpoints))
	for i, bp := range req.Arguments.Breakpoints {
		lines[i] = bp.Line
		conditions[i] = bp.Condition
	}

	bps := h.engine.Breakpoints().SetForFile(file, lines, conditions)

	resp := &dap.SetBreakpointsResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	resp.Body.Breakpoints = translateBreakpoints(bps)
	h.send(resp)
}

func (h *handler) onSetExceptionBreakpoints(req *dap.SetExceptionBreakpointsRequest) {
	mode := debugger.ExceptionBreakNever
	for _, filter := range req.Arguments.Filters {
		if filter == "all" {
			mode = debugger.ExceptionBreakAll
		}
	}
	h.engine.Breakpoints().SetExceptionBreak(mode)

	resp := &dap.SetExceptionBreakpointsResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)
}

func (h *handler) onConfigurationDone(req *dap.ConfigurationDoneRequest) {
	resp := &dap.ConfigurationDoneResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)

	h.mu.Lock()
	h.launched = true
	h.mu.Unlock()
}

func (h *handler) onThreads(req *dap.ThreadsRequest) {
	resp := &dap.ThreadsResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	resp.Body.Threads = []dap.Thread{
		{Id: elpsThreadID, Name: "ELPS Main"},
	}
	h.send(resp)
}

func (h *handler) onStackTrace(req *dap.StackTraceRequest) {
	env, _ := h.engine.PausedState()

	resp := &dap.StackTraceResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)

	if env == nil {
		h.send(resp)
		return
	}

	frames := translateStackFrames(env.Runtime.Stack)
	resp.Body.TotalFrames = len(frames)

	// Apply paging.
	start := req.Arguments.StartFrame
	if start > len(frames) {
		start = len(frames)
	}
	end := len(frames)
	if req.Arguments.Levels > 0 && start+req.Arguments.Levels < end {
		end = start + req.Arguments.Levels
	}
	resp.Body.StackFrames = frames[start:end]

	// Cache frame environments for variable inspection.
	h.cacheFrameEnvs(env)

	h.send(resp)
}

// cacheFrameEnvs walks up the env parent chain and maps frame IDs to envs.
// This is a simplification: we map the paused env to frame 1 (top of stack).
// For deeper frames, we walk up the parent chain, which approximates the
// lexical scopes at each call depth.
func (h *handler) cacheFrameEnvs(env *lisp.LEnv) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.frameEnvs = make(map[int]*lisp.LEnv)
	nframes := len(env.Runtime.Stack.Frames)
	current := env
	for i := 0; i < nframes && current != nil; i++ {
		h.frameEnvs[i+1] = current // 1-based frame IDs
		current = current.Parent
	}
}

func (h *handler) onScopes(req *dap.ScopesRequest) {
	resp := &dap.ScopesResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)

	frameID := req.Arguments.FrameId
	resp.Body.Scopes = []dap.Scope{
		{
			Name:               "Local",
			VariablesReference: scopeLocalBase + frameID,
			Expensive:          false,
		},
		{
			Name:               "Package",
			VariablesReference: scopePackageBase + frameID,
			Expensive:          true,
		},
	}
	h.send(resp)
}

func (h *handler) onVariables(req *dap.VariablesRequest) {
	resp := &dap.VariablesResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)

	ref := req.Arguments.VariablesReference
	var bindings []debugger.ScopeBinding

	switch {
	case ref >= scopePackageBase:
		frameID := ref - scopePackageBase
		env := h.getFrameEnv(frameID)
		if env != nil && env.Runtime.Package != nil {
			// Show package-level symbols.
			pkg := env.Runtime.Package
			for name := range pkg.Symbols {
				v := pkg.Get(lisp.Symbol(name))
				if v.Type != lisp.LError {
					bindings = append(bindings, debugger.ScopeBinding{Name: name, Value: v})
				}
			}
		}
	case ref >= scopeLocalBase:
		frameID := ref - scopeLocalBase
		env := h.getFrameEnv(frameID)
		if env != nil {
			bindings = debugger.InspectLocals(env)
		}
	}

	resp.Body.Variables = translateVariables(bindings)
	h.send(resp)
}

func (h *handler) getFrameEnv(frameID int) *lisp.LEnv {
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.frameEnvs == nil {
		return nil
	}
	return h.frameEnvs[frameID]
}

func (h *handler) onContinue(req *dap.ContinueRequest) {
	resp := &dap.ContinueResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	resp.Body.AllThreadsContinued = true
	h.send(resp)
	h.engine.Resume()
}

func (h *handler) onNext(req *dap.NextRequest) {
	resp := &dap.NextResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)
	h.engine.StepOver()
}

func (h *handler) onStepIn(req *dap.StepInRequest) {
	resp := &dap.StepInResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)
	h.engine.StepInto()
}

func (h *handler) onStepOut(req *dap.StepOutRequest) {
	resp := &dap.StepOutResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)
	h.engine.StepOut()
}

func (h *handler) onEvaluate(req *dap.EvaluateRequest) {
	resp := &dap.EvaluateResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)

	env, _ := h.engine.PausedState()
	if env == nil {
		resp.Success = false
		resp.Message = "not paused"
		h.send(resp)
		return
	}

	// Use the frame env if specified.
	if req.Arguments.FrameId > 0 {
		if fenv := h.getFrameEnv(req.Arguments.FrameId); fenv != nil {
			env = fenv
		}
	}

	result := debugger.EvalInContext(env, req.Arguments.Expression)
	if result != nil && result.Type == lisp.LError {
		resp.Success = false
		resp.Message = debugger.FormatValue(result)
	} else {
		resp.Body.Result = debugger.FormatValue(result)
		resp.Body.Type = lvalTypeName(result)
	}
	h.send(resp)
}

func (h *handler) onDisconnect(req *dap.DisconnectRequest) {
	resp := &dap.DisconnectResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)

	h.engine.Disconnect()

	// Send terminated event.
	h.send(&dap.TerminatedEvent{
		Event: h.newEvent("terminated"),
	})
	h.server.close()
}

// sendStoppedEvent sends a DAP stopped event to the client.
func (h *handler) sendStoppedEvent(reason debugger.StopReason, bpIDs []int) {
	evt := &dap.StoppedEvent{
		Event: h.newEvent("stopped"),
	}
	evt.Body.Reason = string(reason)
	evt.Body.ThreadId = elpsThreadID
	evt.Body.AllThreadsStopped = true
	if len(bpIDs) > 0 {
		evt.Body.HitBreakpointIds = bpIDs
	}
	h.send(evt)
}

// --- helpers ---

func (h *handler) newResponse(reqSeq int, command string) dap.Response {
	return dap.Response{
		ProtocolMessage: dap.ProtocolMessage{Seq: h.server.nextSeq(), Type: "response"},
		RequestSeq:      reqSeq,
		Success:         true,
		Command:         command,
	}
}

func (h *handler) newEvent(event string) dap.Event {
	return dap.Event{
		ProtocolMessage: dap.ProtocolMessage{Seq: h.server.nextSeq(), Type: "event"},
		Event:           event,
	}
}
