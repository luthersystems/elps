package dapserver

import (
	"github.com/go-delve/delve/service/api"
	"github.com/go-delve/delve/service/rpc2"
	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp/x/debugger/delveserver"
	"github.com/luthersystems/elps/lisp/x/debugger/events"
	"math/rand"
)

type debuggerwrapper struct {
	debugger delveserver.ServerDebugger
}

func (d *debuggerwrapper) onInitializeRequest(request *dap.InitializeRequest, handler *connection) {
	handler.queue <- &dap.InitializeResponse{
		Response: dap.Response{
			Success:    true,
			RequestSeq: request.Seq,
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  handler.s.incSequence(),
				Type: "response",
			},
		},
		Body: dap.Capabilities{
			SupportsConfigurationDoneRequest:   true,
			SupportsFunctionBreakpoints:        true,
			SupportsConditionalBreakpoints:     false,
			SupportsHitConditionalBreakpoints:  false,
			SupportsEvaluateForHovers:          true,
			SupportsStepBack:                   false,
			SupportsSetVariable:                true,
			SupportsRestartFrame:               false,
			SupportsGotoTargetsRequest:         false,
			SupportsStepInTargetsRequest:       false,
			SupportsCompletionsRequest:         false,
			SupportsModulesRequest:             true,
			SupportsRestartRequest:             false,
			SupportsExceptionOptions:           false,
			SupportsValueFormattingOptions:     false,
			SupportsExceptionInfoRequest:       false,
			SupportTerminateDebuggee:           true,
			SupportsDelayedStackTraceLoading:   false,
			SupportsLoadedSourcesRequest:       false,
			SupportsLogPoints:                  false,
			SupportsTerminateThreadsRequest:    false,
			SupportsSetExpression:              true,
			SupportsTerminateRequest:           true,
			SupportsDataBreakpoints:            false,
			SupportsReadMemoryRequest:          false,
			SupportsDisassembleRequest:         false,
			SupportsCancelRequest:              false,
			SupportsBreakpointLocationsRequest: true,
		},
	}
	if d.debugger.IsStopped() {
		handler.Event <- events.EventTypeStoppedEntry
	}
}

func (d *debuggerwrapper) onLaunchRequest(request *dap.LaunchRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.LaunchResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success:    true,
			Message:    "Not supported",
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
		},
	}
}

func (d *debuggerwrapper) onAttachRequest(request *dap.AttachRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
		},
	}
}

func (d *debuggerwrapper) onDisconnectRequest(request *dap.DisconnectRequest, returnchan chan dap.Message, seq int) {
	if request.Arguments.TerminateDebuggee {
		d.debugger.Complete()
		panic("DONE")
	}
	returnchan <- &dap.DisconnectResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
			Message:    "Complete",
		},
	}
}

func (d *debuggerwrapper) onTerminateRequest(request *dap.TerminateRequest, returnchan chan dap.Message, seq int) {
	d.debugger.Complete()
	returnchan <- &dap.TerminateResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
			Message:    "Complete",
		},
	}
	panic("DONE")
}

func (d *debuggerwrapper) onRestartRequest(request *dap.RestartRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSetBreakpointsRequest(request *dap.SetBreakpointsRequest, returnchan chan dap.Message, seq int) {
	if request.Arguments.SourceModified {
		returnchan <- &dap.ErrorResponse{
			Response: dap.Response{
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  seq,
					Type: "response",
				},
				RequestSeq: request.GetSeq(),
				Success:    false,
				Message:    "Cannot modify source whilst running",
			},
		}
		return
	}
	out := make([]dap.Breakpoint, 0)
	for _, v := range request.Arguments.Breakpoints {
		id := rand.Int()
		d.debugger.CreateBreakpoint(&api.Breakpoint{
			ID:            id,
			Name:          request.Arguments.Source.Name,
			Addr:          0,
			Addrs:         nil,
			File:          request.Arguments.Source.Path,
			Line:          v.Line,
			FunctionName:  "",
			Cond:          v.Condition,
			Tracepoint:    false,
			TraceReturn:   false,
			Goroutine:     false,
			Stacktrace:    0,
			TotalHitCount: 0,
		})
		out = append(out, dap.Breakpoint{
			Id:       id,
			Line:     v.Line,
			Column:   0,
			Source:   request.Arguments.Source,
			Verified: false,
		})
	}
	returnchan <- &dap.SetBreakpointsResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Success:    true,
			RequestSeq: request.GetSeq(),
		},
		Body: dap.SetBreakpointsResponseBody{Breakpoints: out},
	}
}

func (d *debuggerwrapper) onSetFunctionBreakpointsRequest(request *dap.SetFunctionBreakpointsRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSetExceptionBreakpointsRequest(request *dap.SetExceptionBreakpointsRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onConfigurationDoneRequest(request *dap.ConfigurationDoneRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ConfigurationDoneResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
	}
}

func (d *debuggerwrapper) onContinueRequest(request *dap.ContinueRequest, returnchan chan dap.Message, seq int) {
	d.debugger.Continue()
	returnchan <- &dap.ContinueResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Success:    true,
			RequestSeq: request.GetSeq(),
		},
		Body: dap.ContinueResponseBody{
			AllThreadsContinued: true,
		},
	}
}

func (d *debuggerwrapper) onNextRequest(request *dap.NextRequest, returnchan chan dap.Message, seq int) {
	d.debugger.Step()
	returnchan <- &dap.NextResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Success:    true,
			RequestSeq: request.GetSeq(),
		},
	}
}

func (d *debuggerwrapper) onStepInRequest(request *dap.StepInRequest, returnchan chan dap.Message, seq int) {
	d.debugger.Step()
	returnchan <- &dap.StepInResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Success:    true,
			RequestSeq: request.GetSeq(),
		},
	}
}

func (d *debuggerwrapper) onStepOutRequest(request *dap.StepOutRequest, returnchan chan dap.Message, seq int) {
	d.debugger.Step()
	returnchan <- &dap.StepOutResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Success:    true,
			RequestSeq: request.GetSeq(),
		},
	}
}

func (d *debuggerwrapper) onStepBackRequest(request *dap.StepBackRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onReverseContinueRequest(request *dap.ReverseContinueRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onRestartFrameRequest(request *dap.RestartFrameRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onGotoRequest(request *dap.GotoRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onPauseRequest(request *dap.PauseRequest, returnchan chan dap.Message, seq int) {
	d.debugger.Halt()
	returnchan <- &dap.PauseResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Success:    true,
			RequestSeq: request.GetSeq(),
		},
	}
}

func (d *debuggerwrapper) onStackTraceRequest(request *dap.StackTraceRequest, returnchan chan dap.Message, seq int) {
	st := &rpc2.StacktraceOut{}
	d.debugger.GetStacktrace(st)
	out := d.debugger.GetDapStacktrace()
	returnchan <- &dap.StackTraceResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Success:    true,
			RequestSeq: request.Seq,
		},
		Body: dap.StackTraceResponseBody{
			StackFrames: out,
			TotalFrames: len(out),
		},
	}
}

func (d *debuggerwrapper) onScopesRequest(request *dap.ScopesRequest, returnchan chan dap.Message, seq int) {
	source := d.debugger.GetDapStacktrace()[0]
	returnchan <- &dap.ScopesResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
		Body: dap.ScopesResponseBody{
			Scopes: []dap.Scope{
				{
					VariablesReference: 2,
					PresentationHint:   "locals",
					Name:               "Scope",
				},
				{
					Name:               "Arguments",
					PresentationHint:   "locals",
					VariablesReference: 1,
					Expensive:          false,
					Source:             source.Source,
					Line:               source.Line,
					Column:             source.Column,
					EndLine:            source.EndLine,
					EndColumn:          source.EndColumn,
				},
			},
		},
	}
}

func (d *debuggerwrapper) onVariablesRequest(request *dap.VariablesRequest, returnchan chan dap.Message, seq int) {
	variables := make([]dap.Variable, 0)
	if request.Arguments.VariablesReference == 1 {
		variables := d.debugger.GetArguments()
		returnchan <- &dap.VariablesResponse{
			Response: dap.Response{
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  seq,
					Type: "response",
				},
				RequestSeq: request.GetSeq(),
				Success:    true,
			},
			Body: dap.VariablesResponseBody{
				Variables: variables,
			},
		}
		return
	}

	for _, vari := range d.debugger.GetVariables() {
		outVar := dap.Variable{
			Name:  vari.Name,
			Value: vari.Value,
			Type:  vari.Type,
			PresentationHint: dap.VariablePresentationHint{
				Kind:       "data",
				Visibility: "public",
				Attributes: []string{},
			},
			VariablesReference: 0, // TODO support children
		}
		variables = append(variables, outVar)
	}
	returnchan <- &dap.VariablesResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
		Body: dap.VariablesResponseBody{
			Variables: variables,
		},
	}
}

func (d *debuggerwrapper) onSetVariableRequest(request *dap.SetVariableRequest, returnchan chan dap.Message, seq int) {
	err := d.debugger.SetVariableInScope(api.EvalScope{}, request.Arguments.Name, request.Arguments.Value)
	if err != nil {
		returnchan <- &dap.SetVariableResponse{
			Response: dap.Response{
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  seq,
					Type: "response",
				},
				RequestSeq: request.GetSeq(),
				Success:    false,
				Message:    err.Error(),
			},
			Body: dap.SetVariableResponseBody{},
		}
		return
	}
	returnchan <- &dap.SetVariableResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
		Body: dap.SetVariableResponseBody{
			Value: request.Arguments.Value,
			Type:  "LVal",
		},
	}
}

func (d *debuggerwrapper) onSetExpressionRequest(request *dap.SetExpressionRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSourceRequest(request *dap.SourceRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onThreadsRequest(request *dap.ThreadsRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ThreadsResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
		Body: dap.ThreadsResponseBody{
			Threads: []dap.Thread{
				{
					Id:   1,
					Name: "Execution thread",
				},
			},
		},
	}
}

func (d *debuggerwrapper) onTerminateThreadsRequest(request *dap.TerminateThreadsRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onEvaluateRequest(request *dap.EvaluateRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.EvaluateResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
		Body: dap.EvaluateResponseBody{
			Result: d.debugger.Eval(request.Arguments.Expression),
			Type:   "LVal",
			// TODO support children
		},
	}
}

func (d *debuggerwrapper) onStepInTargetsRequest(request *dap.StepInTargetsRequest, returnchan chan dap.Message) {
}

func (d *debuggerwrapper) onGotoTargetsRequest(request *dap.GotoTargetsRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onCompletionsRequest(request *dap.CompletionsRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onExceptionInfoRequest(request *dap.ExceptionInfoRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onLoadedSourcesRequest(request *dap.LoadedSourcesRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.LoadedSourcesResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
		Body: dap.LoadedSourcesResponseBody{
			Sources: []dap.Source{

			},
		},
	}
}

func (d *debuggerwrapper) onDataBreakpointInfoRequest(request *dap.DataBreakpointInfoRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSetDataBreakpointsRequest(request *dap.SetDataBreakpointsRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onReadMemoryRequest(request *dap.ReadMemoryRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onDisassembleRequest(request *dap.DisassembleRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onCancelRequest(request *dap.CancelRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onBreakpointLocationsRequest(request *dap.BreakpointLocationsRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.BreakpointLocationsResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
		Body: dap.BreakpointLocationsResponseBody{
			Breakpoints: []dap.BreakpointLocation{ // TODO HANDLE COLUMNS
				{
					Line:      request.Arguments.Line,
					Column:    0,
					EndLine:   0,
					EndColumn: 0,
				},
			},
		},
	}
}

func (d *debuggerwrapper) onModulesRequest(request *dap.ModulesRequest, returnchan chan dap.Message, seq int) {
	modules := d.debugger.GetModules()
	returnchan <- &dap.ModulesResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
		Body: dap.ModulesResponseBody{
			Modules:      modules,
			TotalModules: len(modules),
		},
	}
}
