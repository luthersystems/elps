// Copyright © 2018 The ELPS authors

package dapserver

import (
	"encoding/json"
	"log"
	"sync"

	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
)

// scopeRef constants encode scope type into variable references.
// The frame index is embedded: ref = base + frameIndex.
// variableRefBase is used for structured variable expansion (lists, maps, etc.).
const (
	scopeLocalBase   = 1000
	scopePackageBase = 3000
	variableRefBase  = 5000
)

// handler dispatches incoming DAP messages to the appropriate method.
type handler struct {
	server *Server
	engine *debugger.Engine

	mu              sync.Mutex
	initialized     bool
	stopOnEntry     bool // client's stopOnEntry preference from attach/launch args
	stopOnEntrySet  bool // true if client sent attach/launch with stopOnEntry preference
	stoppedSent     bool // true if a stopped event has been sent via the event callback

	// frameEnvs caches the environments for each stack frame when paused.
	// Indexed by frame ID (1-based, most recent first).
	frameEnvs map[int]*lisp.LEnv

	// varRefs maps variable reference IDs to LVal pointers for structured
	// variable expansion (lists, sorted-maps, arrays, tagged values).
	// Cleared when execution resumes (LVal pointers only valid while paused).
	varRefs    map[int]*lisp.LVal
	nextVarRef int
}

func newHandler(s *Server, e *debugger.Engine) *handler {
	h := &handler{
		server: s,
		engine: e,
	}
	// Wire the engine's event callback to forward events to the DAP client.
	e.SetEventCallback(func(evt debugger.Event) {
		switch evt.Type {
		case debugger.EventStopped:
			h.mu.Lock()
			h.stoppedSent = true
			h.mu.Unlock()
			var bpIDs []int
			if evt.BP != nil {
				bpIDs = []int{evt.BP.ID}
			}
			h.sendStoppedEvent(evt.Reason, bpIDs)
		case debugger.EventContinued:
			h.mu.Lock()
			h.varRefs = nil
			h.nextVarRef = 0
			h.mu.Unlock()
		case debugger.EventOutput:
			h.sendOutputEvent(evt.Output)
		case debugger.EventExited:
			// Guard against server already closed (e.g., disconnect raced).
			select {
			case <-h.server.done:
				return
			default:
			}
			h.sendExitedEvent(evt.ExitCode)
			h.sendTerminatedEvent()
			h.server.close()
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
	case *dap.SetFunctionBreakpointsRequest:
		h.onSetFunctionBreakpoints(req)
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
	case *dap.AttachRequest:
		h.onAttach(req)
	case *dap.LaunchRequest:
		h.onLaunch(req)
	case *dap.PauseRequest:
		h.onPause(req)
	case *dap.SourceRequest:
		h.onSource(req)
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
		SupportsConfigurationDoneRequest:      true,
		SupportsFunctionBreakpoints:           true,
		SupportsConditionalBreakpoints:        true,
		SupportsHitConditionalBreakpoints:     true,
		SupportsLogPoints:                     true,
		SupportsEvaluateForHovers:             true,
		SupportTerminateDebuggee:              true,
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

	specs := make([]debugger.BreakpointSpec, len(req.Arguments.Breakpoints))
	for i, bp := range req.Arguments.Breakpoints {
		specs[i] = debugger.BreakpointSpec{
			Line:         bp.Line,
			Condition:    bp.Condition,
			HitCondition: bp.HitCondition,
			LogMessage:   bp.LogMessage,
		}
	}

	bps := h.engine.Breakpoints().SetForFileSpecs(file, specs)

	resp := &dap.SetBreakpointsResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	resp.Body.Breakpoints = translateBreakpoints(bps)
	h.send(resp)
}

func (h *handler) onSetFunctionBreakpoints(req *dap.SetFunctionBreakpointsRequest) {
	names := make([]string, len(req.Arguments.Breakpoints))
	for i, fb := range req.Arguments.Breakpoints {
		names[i] = fb.Name
	}

	set := h.engine.SetFunctionBreakpoints(names)

	resp := &dap.SetFunctionBreakpointsResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	bps := make([]dap.Breakpoint, len(set))
	for i, name := range set {
		bps[i] = dap.Breakpoint{
			Id:       i + 1,
			Verified: true,
			Source:   &dap.Source{Name: name},
		}
	}
	resp.Body.Breakpoints = bps
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

	// Signal that the client has finished configuration (breakpoints set).
	h.engine.SignalReady()

	// If the eval goroutine already paused (e.g., stopOnEntry fired before
	// the client connected), handle according to client preference:
	// - If client sent attach/launch with stopOnEntry:false, resume silently
	// - Otherwise, send the stopped event (late-connect case)
	h.mu.Lock()
	alreadySent := h.stoppedSent
	clientSet := h.stopOnEntrySet
	clientStopOnEntry := h.stopOnEntry
	h.mu.Unlock()
	if !alreadySent && h.engine.IsPaused() {
		if clientSet && !clientStopOnEntry {
			h.engine.Resume()
		} else {
			reason := debugger.StopEntry
			h.sendStoppedEvent(reason, nil)
		}
	}
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
	env, pausedExpr := h.engine.PausedState()

	resp := &dap.StackTraceResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)

	if env == nil {
		h.send(resp)
		return
	}

	frames := translateStackFrames(env.Runtime.Stack, pausedExpr, h.engine.SourceRoot())
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
// Frame IDs must match translateStackFrames: the top stack frame (most
// recent call) gets Id = len(Stack.Frames), and deeper frames count down
// to 1. The paused env corresponds to the top frame.
func (h *handler) cacheFrameEnvs(env *lisp.LEnv) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.frameEnvs = make(map[int]*lisp.LEnv)
	nframes := len(env.Runtime.Stack.Frames)
	current := env
	for i := nframes; i >= 1 && current != nil; i-- {
		h.frameEnvs[i] = current
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

	allocRef := func(v *lisp.LVal) int {
		h.mu.Lock()
		defer h.mu.Unlock()
		return h.allocVarRef(v)
	}

	switch {
	case ref >= variableRefBase:
		// Expand a structured variable (list, map, array, tagged, native).
		h.mu.Lock()
		parent := h.varRefs[ref]
		h.mu.Unlock()
		if parent != nil {
			resp.Body.Variables = expandVariable(parent, allocRef, h.engine)
		}
	case ref >= scopePackageBase:
		frameID := ref - scopePackageBase
		env := h.getFrameEnv(frameID)
		if env != nil && env.Runtime.Package != nil {
			pkg := env.Runtime.Package
			var bindings []debugger.ScopeBinding
			for name := range pkg.Symbols {
				v := pkg.Get(lisp.Symbol(name))
				if v.Type != lisp.LError {
					bindings = append(bindings, debugger.ScopeBinding{Name: name, Value: v})
				}
			}
			resp.Body.Variables = translateVariables(bindings, allocRef, h.engine)
		}
	case ref >= scopeLocalBase:
		frameID := ref - scopeLocalBase
		env := h.getFrameEnv(frameID)
		if env != nil {
			bindings := debugger.InspectFunctionLocals(env)
			resp.Body.Variables = translateVariables(bindings, allocRef, h.engine)
		}
	}

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

// allocVarRef assigns a variable reference ID for structured types that
// support drill-down. Returns 0 if the value has no expandable children.
// Must be called with h.mu held.
func (h *handler) allocVarRef(v *lisp.LVal) int {
	if !hasChildren(v, h.engine) {
		return 0
	}
	if h.varRefs == nil {
		h.varRefs = make(map[int]*lisp.LVal)
	}
	id := variableRefBase + h.nextVarRef
	h.nextVarRef++
	h.varRefs[id] = v
	return id
}

// hasChildren returns true if the LVal has expandable child elements.
func hasChildren(v *lisp.LVal, eng *debugger.Engine) bool {
	if v == nil {
		return false
	}
	switch v.Type {
	case lisp.LSExpr:
		return !v.IsNil()
	case lisp.LSortMap:
		return v.Len() > 0
	case lisp.LArray:
		return v.Len() > 0
	case lisp.LTaggedVal:
		return len(v.Cells) > 0
	case lisp.LNative:
		return eng != nil && len(eng.NativeChildren(v.Native)) > 0
	default:
		return false
	}
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

	result := h.engine.EvalInContext(env, req.Arguments.Expression)
	if result != nil && result.Type == lisp.LError {
		resp.Success = false
		resp.Message = debugger.FormatValueWith(result, h.engine)
	} else {
		resp.Body.Result = debugger.FormatValueWith(result, h.engine)
		resp.Body.Type = lvalTypeName(result)
		h.mu.Lock()
		resp.Body.VariablesReference = h.allocVarRef(result)
		h.mu.Unlock()
	}
	h.send(resp)
}

func (h *handler) onAttach(req *dap.AttachRequest) {
	resp := &dap.AttachResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)

	// Parse stopOnEntry from client arguments. Default is false per DAP spec.
	// Override the engine's stopOnEntry immediately so it is set before the
	// eval goroutine starts (which may happen right after configurationDone).
	stop := parseStopOnEntry(req.Arguments)
	h.mu.Lock()
	h.stopOnEntry = stop
	h.stopOnEntrySet = true
	h.mu.Unlock()
	h.engine.SetStopOnEntry(stop)
}

func (h *handler) onLaunch(req *dap.LaunchRequest) {
	resp := &dap.LaunchResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)

	// Parse stopOnEntry from client arguments. Default is false per DAP spec.
	// Override the engine's stopOnEntry immediately so it is set before the
	// eval goroutine starts (which may happen right after configurationDone).
	stop := parseStopOnEntry(req.Arguments)
	h.mu.Lock()
	h.stopOnEntry = stop
	h.stopOnEntrySet = true
	h.mu.Unlock()
	h.engine.SetStopOnEntry(stop)
}

func (h *handler) onPause(req *dap.PauseRequest) {
	resp := &dap.PauseResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)
	h.engine.RequestPause()
}

func (h *handler) onSource(req *dap.SourceRequest) {
	resp := &dap.SourceResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)

	// Try source reference first (integer ID for virtual sources).
	if ref := req.Arguments.SourceReference; ref > 0 {
		if content, ok := h.engine.GetSourceRef(ref); ok {
			resp.Body.Content = content
			resp.Body.MimeType = "text/x-lisp"
			h.send(resp)
			return
		}
	}

	// Try loading from the source library by path.
	path := ""
	if req.Arguments.Source != nil {
		path = req.Arguments.Source.Path
		if path == "" {
			path = req.Arguments.Source.Name
		}
	}
	if path != "" {
		if lib := h.engine.SourceLibrary(); lib != nil {
			_, _, data, err := lib.LoadSource(lisp.NewSourceContext("", ""), path)
			if err == nil {
				resp.Body.Content = string(data)
				resp.Body.MimeType = "text/x-lisp"
				h.send(resp)
				return
			}
		}
	}

	resp.Success = false
	resp.Message = "source not available"
	h.send(resp)
}

func (h *handler) onDisconnect(req *dap.DisconnectRequest) {
	resp := &dap.DisconnectResponse{}
	resp.Response = h.newResponse(req.Seq, req.Command)
	h.send(resp)

	h.engine.Disconnect()

	h.sendTerminatedEvent()
	h.server.close()
}

// sendExitedEvent sends a DAP exited event to the client.
func (h *handler) sendExitedEvent(exitCode int) {
	evt := &dap.ExitedEvent{
		Event: h.newEvent("exited"),
	}
	evt.Body.ExitCode = exitCode
	h.send(evt)
}

// sendTerminatedEvent sends a DAP terminated event to the client.
func (h *handler) sendTerminatedEvent() {
	h.send(&dap.TerminatedEvent{
		Event: h.newEvent("terminated"),
	})
}

// sendOutputEvent sends a DAP output event to the client (used for log points).
func (h *handler) sendOutputEvent(output string) {
	evt := &dap.OutputEvent{
		Event: h.newEvent("output"),
	}
	evt.Body.Category = "console"
	evt.Body.Output = output + "\n"
	h.send(evt)
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

// parseStopOnEntry extracts the boolean stopOnEntry field from raw JSON
// arguments (as sent by attach/launch requests). Returns false if the
// field is absent, the JSON is nil/empty, or parsing fails — matching the
// DAP specification default.
func parseStopOnEntry(args json.RawMessage) bool {
	if len(args) == 0 {
		return false
	}
	var parsed struct {
		StopOnEntry bool `json:"stopOnEntry"`
	}
	if err := json.Unmarshal(args, &parsed); err != nil {
		return false
	}
	return parsed.StopOnEntry
}
