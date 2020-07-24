package dapserver

import (
	"encoding/json"
	"github.com/go-delve/delve/service/api"
	"github.com/go-delve/delve/service/rpc2"
	"github.com/golang-collections/collections/stack"
	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp/x/debugger/delveserver"
	"github.com/luthersystems/elps/lisp/x/debugger/events"
	log "github.com/sirupsen/logrus"
	"io/ioutil"
	"math/rand"
	"strings"
)

type debuggerwrapper struct {
	debugger delveserver.ServerDebugger
	files    map[string][]string
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
			Command: request.Command,
		},
		Body: dap.Capabilities{
			SupportsConfigurationDoneRequest:   true,
			SupportsFunctionBreakpoints:        false,
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

	handler.Event <- events.EventTypeInitialized
	if d.debugger.IsStopped() {
		handler.Event <- events.EventTypeStoppedEntry
	}
}

func (d *debuggerwrapper) onLaunchRequest(request *dap.LaunchRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.LaunchResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success:    true,
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command: request.Command,
		},
	}
}

func (d *debuggerwrapper) onAttachRequest(request *dap.AttachRequest, returnchan chan dap.Message, seq int) {
	returnchan <- &dap.AttachResponse{
		Response: dap.Response{
			RequestSeq: request.GetSeq(),
			Success:    true,
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command: request.Command,
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
			Command:    request.Command,
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
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    true,
			Message:    "Complete",
		},
	}
	panic("DONE")
}

func (d *debuggerwrapper) onRestartRequest(request *dap.RestartRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported restartRequest was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSetBreakpointsRequest(request *dap.SetBreakpointsRequest, returnchan chan dap.Message, seq int) {
	log.Infof("BREAKPOINT request %v", request)
	if request.Arguments.SourceModified {
		returnchan <- &dap.ErrorResponse{
			Response: dap.Response{
				ProtocolMessage: dap.ProtocolMessage{
					Seq:  seq,
					Type: "response",
				},
				Command:    request.Command,
				RequestSeq: request.GetSeq(),
				Success:    false,
				Message:    "Cannot modify source whilst running",
			},
		}
		return
	}
	log.Infof("Name is %s", request.Arguments.Source.Name)
	out := make([]dap.Breakpoint, 0)
	for id, bp := range d.debugger.GetAllBreakpoints() {
		if bp.File == request.Arguments.Source.Path {
			err := d.debugger.RemoveBreakpoint(id)
			if err != nil {
				log.Errorf("Error removing breakpoint: %s", err.Error())
			}
		}
	}
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
	}
	for id, bp := range d.debugger.GetAllBreakpoints() {
		if bp.File == request.Arguments.Source.Path {
			out = append(out, dap.Breakpoint{
				Id:        id,
				Verified:  true,
				Source:    request.Arguments.Source,
				Line:      bp.Line,
				Column:    1,
				EndLine:   bp.Line,
				EndColumn: 1,
			})
		} else {
			log.Infof("%s didn't match %s", bp.File, request.Arguments.Source.Path)
		}
	}

	bpResponse :=  &dap.SetBreakpointsResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			Success:    true,
			RequestSeq: request.GetSeq(),
		},
		Body: dap.SetBreakpointsResponseBody{Breakpoints: out},
	}
	returnchan <- bpResponse
	o, _ := json.Marshal(bpResponse)
	log.Info(string(o))
}

func (d *debuggerwrapper) onSetFunctionBreakpointsRequest(request *dap.SetFunctionBreakpointsRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported setFunctionBreakpoint was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSetExceptionBreakpointsRequest(request *dap.SetExceptionBreakpointsRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported setExceptionBreakpoint was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
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
			Command:    request.Command,
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
			Command:    request.Command,
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
			Command:    request.Command,
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
			Command:    request.Command,
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
			Command:    request.Command,
			Success:    true,
			RequestSeq: request.GetSeq(),
		},
	}
}

func (d *debuggerwrapper) onStepBackRequest(request *dap.StepBackRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported stepBack was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onReverseContinueRequest(request *dap.ReverseContinueRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported reverseContinue was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onRestartFrameRequest(request *dap.RestartFrameRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported restartFrame was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onGotoRequest(request *dap.GotoRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported goto was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
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
			Command:    request.Command,
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
			Command:    request.Command,
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
			Command:    request.Command,
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
				Command:    request.Command,
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
			Command:    request.Command,
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
				Command:    request.Command,
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
			Command:    request.Command,
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
	log.Error("Invalid request to non-supported setExpression was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSourceRequest(request *dap.SourceRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported source request was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
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
			Command:    request.Command,
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
	log.Error("Invalid request to non-supported terminateThreads was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
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
			Command:    request.Command,
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

func (d *debuggerwrapper) onStepInTargetsRequest(request *dap.StepInTargetsRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported stepInTargets was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onGotoTargetsRequest(request *dap.GotoTargetsRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported gotoTargets was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onCompletionsRequest(request *dap.CompletionsRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported completions was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onExceptionInfoRequest(request *dap.ExceptionInfoRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported exceptionInfo was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onLoadedSourcesRequest(request *dap.LoadedSourcesRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported loadedSources was made by client")
	returnchan <- &dap.LoadedSourcesResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
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
	log.Error("Invalid request to non-supported dataBreakpointInfo was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onSetDataBreakpointsRequest(request *dap.SetDataBreakpointsRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported setDataBreakpoints was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onReadMemoryRequest(request *dap.ReadMemoryRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported readMemory was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onDisassembleRequest(request *dap.DisassembleRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported disassemble was made by client")
	returnchan <- &dap.ErrorResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    false,
			Message:    "Not supported",
		},
	}
}

func (d *debuggerwrapper) onCancelRequest(request *dap.CancelRequest, returnchan chan dap.Message, seq int) {
	log.Error("Invalid request to non-supported cancelRequest was made by client")
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

func (d *debuggerwrapper) getCodeLine(file string, line int) string {
	var data []string
	var ok bool
	if data, ok = d.files[file]; !ok {
		dataBytes, err := ioutil.ReadFile(file)
		if err != nil {
			panic(err)
		}
		data = strings.Split(string(dataBytes), "\n")
		d.files[file] = data
	}
	return data[line-1]
}

func (d *debuggerwrapper) getBreakpointsFromLine(file string, lineNumber int) []dap.BreakpointLocation {
	locations := make([]dap.BreakpointLocation, 0)
	stk := stack.New()
	for {
		line := d.getCodeLine(file, lineNumber)
		for pos, r := range []rune(line) {
			switch r {
			case '(', '{':
				stk.Push([]int{pos + 1, lineNumber})
			case '}', ')':
				start := stk.Pop().([]int)
				locations = append(locations, dap.BreakpointLocation{
					Line:      start[1],
					Column:    start[0],
					EndLine:   lineNumber,
					EndColumn: pos + 1,
				})
			}
		}
		if stk.Len() == 0 || lineNumber == len(d.files[file]) {
			break
		}
		lineNumber += 1
	}
	return locations
}

func (d *debuggerwrapper) onBreakpointLocationsRequest(request *dap.BreakpointLocationsRequest, returnchan chan dap.Message, seq int) {
	// NB This is the POSSIBLE locations for breakpoints - not the actual locations
	locations := d.getBreakpointsFromLine(request.Arguments.Source.Path, request.Arguments.Line)
	resp := &dap.BreakpointLocationsResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
		Body: dap.BreakpointLocationsResponseBody{
			Breakpoints: locations,
		},
	}
	returnchan <- resp
}

func (d *debuggerwrapper) onModulesRequest(request *dap.ModulesRequest, returnchan chan dap.Message, seq int) {
	modules := d.debugger.GetModules()
	returnchan <- &dap.ModulesResponse{
		Response: dap.Response{
			ProtocolMessage: dap.ProtocolMessage{
				Seq:  seq,
				Type: "response",
			},
			Command:    request.Command,
			RequestSeq: request.GetSeq(),
			Success:    true,
		},
		Body: dap.ModulesResponseBody{
			Modules:      modules,
			TotalModules: len(modules),
		},
	}
}
