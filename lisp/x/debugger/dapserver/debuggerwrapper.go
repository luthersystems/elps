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
			Success: true,
			RequestSeq: request.Seq,
		},
		Body:     dap.Capabilities{
			SupportsConfigurationDoneRequest:   true,
			SupportsFunctionBreakpoints:        true,
			SupportsConditionalBreakpoints:     false,
			SupportsHitConditionalBreakpoints:  false,
			SupportsEvaluateForHovers:          false,
			SupportsStepBack:                   false,
			SupportsSetVariable:                true,
			SupportsRestartFrame:               false,
			SupportsGotoTargetsRequest:         false,
			SupportsStepInTargetsRequest:       false,
			SupportsCompletionsRequest:         false,
			SupportsModulesRequest:             false,
			SupportsRestartRequest:             false,
			SupportsExceptionOptions:           false,
			SupportsValueFormattingOptions:     false,
			SupportsExceptionInfoRequest:       false,
			SupportTerminateDebuggee:           true,
			SupportsDelayedStackTraceLoading:   false,
			SupportsLoadedSourcesRequest:       true,
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

func (d *debuggerwrapper) onLaunchRequest(request *dap.LaunchRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onAttachRequest(request *dap.AttachRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onDisconnectRequest(request *dap.DisconnectRequest , returnchan chan dap.Message) {
	if request.Arguments.TerminateDebuggee {
		d.debugger.Complete()
	}
	returnchan <- &dap.DisconnectResponse{}
}

func (d *debuggerwrapper) onTerminateRequest(request *dap.TerminateRequest , returnchan chan dap.Message) {
	d.debugger.Complete()
	returnchan <- &dap.TerminateResponse{}
}

func (d *debuggerwrapper) onRestartRequest(request *dap.RestartRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSetBreakpointsRequest(request *dap.SetBreakpointsRequest , returnchan chan dap.Message) {
	if request.Arguments.SourceModified {
		returnchan <- &dap.ErrorResponse{
			Response: dap.Response{
				RequestSeq: request.GetSeq(),
				Success: false,
				Message: "Cannot modify source whilst running",
			},
		}
		return
	}
	out:= make([]dap.Breakpoint, 0)
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
			Id: id,
			Line: v.Line,
			Column: 0,
			Source: request.Arguments.Source,
			Verified: false,
		})
	}
	returnchan <- &dap.SetBreakpointsResponse{
		Response: dap.Response{
			Success: true,
			RequestSeq: request.GetSeq(),
		},
		Body:     dap.SetBreakpointsResponseBody{Breakpoints: out},
	}
}

func (d *debuggerwrapper) onSetFunctionBreakpointsRequest(request *dap.SetFunctionBreakpointsRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSetExceptionBreakpointsRequest(request *dap.SetExceptionBreakpointsRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onConfigurationDoneRequest(request *dap.ConfigurationDoneRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ConfigurationDoneResponse{
		Response: dap.Response{RequestSeq: request.GetSeq(), Success: true},
	}
}

func (d *debuggerwrapper) onContinueRequest(request *dap.ContinueRequest , returnchan chan dap.Message) {
	d.debugger.Continue()
	returnchan <- &dap.ContinueResponse{
		Response: dap.Response{Success: true, RequestSeq: request.GetSeq()},
		Body:     dap.ContinueResponseBody{
			AllThreadsContinued: true,
		},
	}
}

func (d *debuggerwrapper) onNextRequest(request *dap.NextRequest , returnchan chan dap.Message) {
	d.debugger.Step()
	returnchan <- &dap.NextResponse{
		Response: dap.Response{Success: true, RequestSeq: request.GetSeq()},
	}
}

func (d *debuggerwrapper) onStepInRequest(request *dap.StepInRequest , returnchan chan dap.Message) {
	d.debugger.Step()
	returnchan <- &dap.StepInResponse{
		Response: dap.Response{Success: true, RequestSeq: request.GetSeq()},
	}
}

func (d *debuggerwrapper) onStepOutRequest(request *dap.StepOutRequest , returnchan chan dap.Message) {
	d.debugger.Step()
	returnchan <- &dap.StepOutResponse{
		Response: dap.Response{Success: true, RequestSeq: request.GetSeq()},
	}
}

func (d *debuggerwrapper) onStepBackRequest(request *dap.StepBackRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onReverseContinueRequest(request *dap.ReverseContinueRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onRestartFrameRequest(request *dap.RestartFrameRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onGotoRequest(request *dap.GotoRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onPauseRequest(request *dap.PauseRequest , returnchan chan dap.Message) {
	d.debugger.Halt()
	returnchan <- &dap.PauseResponse{
		Response: dap.Response{Success: true, RequestSeq: request.GetSeq()},
	}
}

func (d *debuggerwrapper) onStackTraceRequest(request *dap.StackTraceRequest , returnchan chan dap.Message) {
	st := &rpc2.StacktraceOut{}
	d.debugger.GetStacktrace(st)
	out := make([]dap.StackFrame, 0)
	for x:=0; x < len(st.Locations); x++ {
		out = append(out, dap.StackFrame{
			Id:                          x,
			Name:                        st.Locations[x].Function.Name(),
			Source:                      dap.Source{
				Name: st.Locations[x].File,
			},
			Line:                        st.Locations[x].Line,
			Column:                      0,
		})
	}
	returnchan <- &dap.StackTraceResponse{
		Response: dap.Response{Success: true, RequestSeq: request.Seq},
		Body:     dap.StackTraceResponseBody{
			StackFrames: out,
			TotalFrames: len(out),
		},
	}
}

func (d *debuggerwrapper) onScopesRequest(request *dap.ScopesRequest , returnchan chan dap.Message) {}

func (d *debuggerwrapper) onVariablesRequest(request *dap.VariablesRequest , returnchan chan dap.Message) {}

func (d *debuggerwrapper) onSetVariableRequest(request *dap.SetVariableRequest , returnchan chan dap.Message) {}

func (d *debuggerwrapper) onSetExpressionRequest(request *dap.SetExpressionRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSourceRequest(request *dap.SourceRequest , returnchan chan dap.Message) {}

func (d *debuggerwrapper) onThreadsRequest(request *dap.ThreadsRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onTerminateThreadsRequest(request *dap.TerminateThreadsRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onEvaluateRequest(request *dap.EvaluateRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onStepInTargetsRequest(request *dap.StepInTargetsRequest , returnchan chan dap.Message) {}

func (d *debuggerwrapper) onGotoTargetsRequest(request *dap.GotoTargetsRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onCompletionsRequest(request *dap.CompletionsRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onExceptionInfoRequest(request *dap.ExceptionInfoRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onLoadedSourcesRequest(request *dap.LoadedSourcesRequest , returnchan chan dap.Message) {

}

func (d *debuggerwrapper) onDataBreakpointInfoRequest(request *dap.DataBreakpointInfoRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSetDataBreakpointsRequest(request *dap.SetDataBreakpointsRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onReadMemoryRequest(request *dap.ReadMemoryRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onDisassembleRequest(request *dap.DisassembleRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onCancelRequest(request *dap.CancelRequest , returnchan chan dap.Message) {
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success: false,
			Message: "Not supported",
		},
	}
}

func (d *debuggerwrapper) onBreakpointLocationsRequest(request *dap.BreakpointLocationsRequest , returnchan chan dap.Message) {

}

