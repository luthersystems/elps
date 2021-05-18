package delveserver

import (
	"errors"
	"fmt"
	"github.com/go-delve/delve/pkg/logflags"
	"github.com/go-delve/delve/service"
	"github.com/go-delve/delve/service/api"
	"github.com/go-delve/delve/service/rpc2"
	"github.com/luthersystems/elps/lisp/x/debugger/events"
	"github.com/sirupsen/logrus"
	"net"
)

type RPCServer struct {
	// debugger is a debugger service.
	debugger ServerDebugger
	// listener is used to serve HTTP.
	listener net.Listener
	// stopChan is used to stop the listener goroutine.
	stopChan chan struct{}
	log      *logrus.Entry
	methods  map[string]*methodType
}

func NewServer(debugger ServerDebugger, address string) *RPCServer {
	listener, err := net.Listen("tcp", address)
	if err != nil {
		panic(fmt.Sprintf("Can't bind to %s: %v", address, err))
	}
	logger := logflags.RPCLogger()
	logflags.Setup(true, "", "")
	logflags.WriteAPIListeningMessage(address)
	logger.Level = logrus.DebugLevel
	rpc := &RPCServer{
		listener: listener,
		debugger: debugger,
		stopChan: make(chan struct{}),
		log:      logger,
	}
	methods := make(map[string]*methodType)
	suitableMethods(rpc, methods, rpc.log)
	rpc.methods = methods
	return rpc
}

func (s *RPCServer) Breakpoint(v *api.Breakpoint) {
	// no-op
}

func (s *RPCServer) LastModified(arg rpc2.LastModifiedIn, out *rpc2.LastModifiedOut) error {
	out.Time = s.debugger.LastModified()
	return nil
}

func (s *RPCServer) Event(x events.EventType) {
	// no-op
}

// Detach detaches the debugger, optionally killing the process.
func (s *RPCServer) Detach(arg rpc2.DetachIn, out *rpc2.DetachOut) error {
	err := s.debugger.Complete()
	if arg.Kill {
		panic("Quitting on command of debugger")
	}
	return err
}

// Restart restarts program.
func (s *RPCServer) Restart(arg rpc2.RestartIn, cb service.RPCCallback) {
	panic("Not implemented")
}

// State returns the current debugger state.
func (s *RPCServer) State(arg rpc2.StateIn, cb service.RPCCallback) {
	var out rpc2.StateOut
	st, err := s.debugger.State(arg.NonBlocking)
	if err != nil {
		cb.Return(nil, err)
		return
	}
	out.State = st
	cb.Return(out, nil)
}

// Command interrupts, continues and steps through the program.
func (s *RPCServer) Command(command api.DebuggerCommand, cb service.RPCCallback) {
	st, err := s.debugger.Command(&command)
	if err != nil {
		cb.Return(nil, err)
		return
	}
	var out rpc2.CommandOut
	out.State = *st
	cb.Return(out, nil)
}

// GetBreakpoint gets a breakpoint by Name (if Name is not an empty string) or by ID.
func (s *RPCServer) GetBreakpoint(arg rpc2.GetBreakpointIn, out *rpc2.GetBreakpointOut) error {
	var bp *api.Breakpoint
	var err error
	if arg.Name != "" {
		bp, err = s.debugger.GetBreakpointByName(arg.Name)
		if err == nil {
			return fmt.Errorf("no breakpoint with name %s", arg.Name)
		}
	} else {
		bp, err = s.debugger.GetBreakpoint(arg.Id)
		if err == nil {
			return fmt.Errorf("no breakpoint with id %d", arg.Id)
		}
	}
	out.Breakpoint = *bp
	return nil
}

// Stacktrace returns stacktrace of goroutine Id up to the specified Depth.
//
// If Full is set it will also the variable of all local variables
// and function arguments of all stack frames.
func (s *RPCServer) Stacktrace(arg rpc2.StacktraceIn, out *rpc2.StacktraceOut) error {
	s.debugger.GetStacktrace(out)
	return nil
}

// Ancestors returns the stacktraces for the ancestors of a goroutine.
func (s *RPCServer) Ancestors(arg rpc2.AncestorsIn, out *rpc2.AncestorsOut) error {
	out.Ancestors = []api.Ancestor{}
	return nil
}

// ListBreakpoints gets all breakpoints.
func (s *RPCServer) ListBreakpoints(arg rpc2.ListBreakpointsIn, out *rpc2.ListBreakpointsOut) error {
	breakpoints := s.debugger.GetAllBreakpoints()
	for _, v := range breakpoints {
		out.Breakpoints = append(out.Breakpoints, v)
	}
	return nil
}

func (s *RPCServer) CreateBreakpoint(arg rpc2.CreateBreakpointIn, out *rpc2.CreateBreakpointOut) error {
	out.Breakpoint = *s.debugger.CreateBreakpoint(&arg.Breakpoint)
	return nil
}

func (s *RPCServer) ClearBreakpoint(arg rpc2.ClearBreakpointIn, out *rpc2.ClearBreakpointOut) error {
	var bp *api.Breakpoint
	var err error
	if arg.Name != "" {
		bp, err = s.debugger.GetBreakpointByName(arg.Name)
		if err == nil {
			return fmt.Errorf("no breakpoint with name %s", arg.Name)
		}
	} else {
		bp, err = s.debugger.GetBreakpoint(arg.Id)
		if err == nil {
			return fmt.Errorf("no breakpoint with id %d", arg.Id)
		}
	}
	err = s.debugger.RemoveBreakpoint(bp.ID)
	if err != nil {
		return err
	}
	out.Breakpoint = bp
	return nil
}

func (s *RPCServer) AmendBreakpoint(arg rpc2.AmendBreakpointIn, out *rpc2.AmendBreakpointOut) error {
	return s.debugger.AmendBreakpoint(&arg.Breakpoint)
}

func (s *RPCServer) CancelNext(arg rpc2.CancelNextIn, out *rpc2.CancelNextOut) error {
	return nil
}

func (s *RPCServer) ListThreads(arg rpc2.ListThreadsIn, out *rpc2.ListThreadsOut) (err error) {
	out.Threads = []*api.Thread{
		s.debugger.GetThread(),
	}
	return nil
}

func (s *RPCServer) GetThread(arg rpc2.GetThreadIn, out *rpc2.GetThreadOut) error {
	if arg.Id != 0 {
		return errors.New("We've only got one thread...")
	}
	out.Thread = s.debugger.GetThread()
	return nil
}

func (s *RPCServer) ListPackageVars(arg rpc2.ListPackageVarsIn, out *rpc2.ListPackageVarsOut) error {
	out.Variables = s.debugger.GetVariables()
	return nil
}

func (s *RPCServer) ListRegisters(arg rpc2.ListRegistersIn, out *rpc2.ListRegistersOut) error {
	out.Registers = ""
	out.Regs = []api.Register{}
	return nil
}

func (s *RPCServer) ListLocalVars(arg rpc2.ListLocalVarsIn, out *rpc2.ListLocalVarsOut) error {
	out.Variables = s.debugger.GetVariables()
	return nil
}

func (s *RPCServer) ListFunctionArgs(arg rpc2.ListFunctionArgsIn, out *rpc2.ListFunctionArgsOut) error {
	out.Args = s.debugger.GetFunctionArgs()
	return nil
}

func (s *RPCServer) Eval(arg rpc2.EvalIn, out *rpc2.EvalOut) error {
	return nil
}

func (s *RPCServer) Set(arg rpc2.SetIn, out *rpc2.SetOut) error {
	return s.debugger.SetVariableInScope(arg.Scope, arg.Symbol, arg.Value)
}

func (s *RPCServer) ListSources(arg rpc2.ListSourcesIn, out *rpc2.ListSourcesOut) error {
	ss, err := s.debugger.Sources(arg.Filter)
	if err != nil {
		return err
	}
	out.Sources = ss
	return nil
}

func (s *RPCServer) ListFunctions(arg rpc2.ListFunctionsIn, out *rpc2.ListFunctionsOut) error {
	fns, err := s.debugger.Functions(arg.Filter)
	if err != nil {
		return err
	}
	out.Funcs = fns
	return nil
}

func (s *RPCServer) ListTypes(arg rpc2.ListTypesIn, out *rpc2.ListTypesOut) error {
	return nil
}

func (s *RPCServer) ListGoroutines(arg rpc2.ListGoroutinesIn, out *rpc2.ListGoroutinesOut) error {
	out.Goroutines = []*api.Goroutine{
		s.debugger.GetGoRoutine(),
	}
	return nil
}

func (c *RPCServer) AttachedToExistingProcess(arg rpc2.AttachedToExistingProcessIn, out *rpc2.AttachedToExistingProcessOut) error {
	out.Answer = true
	return nil
}

// FindLocation returns concrete location information described by a location expression.
//
//  loc ::= <filename>:<line> | <function>[:<line>] | /<regex>/ | (+|-)<offset> | <line> | *<address>
//  * <filename> can be the full path of a file or just a suffix
//  * <function> ::= <package>.<receiver type>.<name> | <package>.(*<receiver type>).<name> | <receiver type>.<name> | <package>.<name> | (*<receiver type>).<name> | <name>
//  * <function> must be unambiguous
//  * /<regex>/ will return a location for each function matched by regex
//  * +<offset> returns a location for the line that is <offset> lines after the current line
//  * -<offset> returns a location for the line that is <offset> lines before the current line
//  * <line> returns a location for a line in the current file
//  * *<address> returns the location corresponding to the specified address
//
// NOTE: this function does not actually set breakpoints.
func (c *RPCServer) FindLocation(arg rpc2.FindLocationIn, out *rpc2.FindLocationOut) error {
	var err error
	out.Locations, err = c.debugger.FindLocation(arg.Scope, arg.Loc, arg.IncludeNonExecutableLines)
	return err
}

func (c *RPCServer) Disassemble(arg rpc2.DisassembleIn, out *rpc2.DisassembleOut) error {
	return errors.New("Can't dissasemble Lisp...")
}

func (s *RPCServer) Recorded(arg rpc2.RecordedIn, out *rpc2.RecordedOut) error {
	return nil
}

func (s *RPCServer) Checkpoint(arg rpc2.CheckpointIn, out *rpc2.CheckpointOut) error {
	return errors.New("Not implemented")
}

func (s *RPCServer) ListCheckpoints(arg rpc2.ListCheckpointsIn, out *rpc2.ListCheckpointsOut) error {
	return errors.New("Not implemented")
}

func (s *RPCServer) ClearCheckpoint(arg rpc2.ClearCheckpointIn, out *rpc2.ClearCheckpointOut) error {
	return errors.New("Not implemented")
}

func (s *RPCServer) IsMulticlient(arg rpc2.IsMulticlientIn, out *rpc2.IsMulticlientOut) error {
	out.IsMulticlient = false
	return nil
}

func (s *RPCServer) FunctionReturnLocations(in rpc2.FunctionReturnLocationsIn, out *rpc2.FunctionReturnLocationsOut) error {
	return nil
}

func (s *RPCServer) ListDynamicLibraries(in rpc2.ListDynamicLibrariesIn, out *rpc2.ListDynamicLibrariesOut) error {
	return nil
}

func (s *RPCServer) ListPackagesBuildInfo(in rpc2.ListPackagesBuildInfoIn, out *rpc2.ListPackagesBuildInfoOut) error {
	out.List = s.debugger.ListPackagesBuildInfo(in.IncludeFiles)
	return nil
}

type StopRecordingIn struct {
}

type StopRecordingOut struct {
}

func (s *RPCServer) StopRecording(arg StopRecordingIn, cb service.RPCCallback) {
	var out StopRecordingOut
	err := s.debugger.Complete()
	if err != nil {
		cb.Return(nil, err)
		return
	}
	cb.Return(out, nil)
}
