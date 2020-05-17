package debugger

import (
	"errors"
	"github.com/go-delve/delve/service/rpc2"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger/server"
	"github.com/sirupsen/logrus"
	"math/rand"
	"reflect"
	"strconv"
	"sync"
	"time"
)
import "github.com/go-delve/delve/service/api"

type Debugger struct {
	sync.Mutex
	enabled      bool
	breakpoints  map[int]*api.Breakpoint
	runtime      *lisp.Runtime
	lastModified time.Time
	stopped      bool
	step         chan bool
	run          chan bool
	server       *server.RPCServer
	currentOp    *lisp.LVal
	logger       *logrus.Logger
}

func NewDebugger(runtime *lisp.Runtime, address string) lisp.Profiler {
	db := &Debugger{
		runtime:      runtime,
		enabled:      true,
		stopped:      true,
		run:          make(chan bool),
		step:         make(chan bool),
		lastModified: time.Now(),
		breakpoints:  make(map[int]*api.Breakpoint),
		logger:       logrus.New(),
	}
	srv := server.NewServer(db, address)
	srv.Run()
	db.server = srv
	runtime.Profiler = db
	return db
}

func (d *Debugger) IsEnabled() bool {
	return d.enabled
}

func (d *Debugger) Enable() error {
	if d.enabled {
		return nil
	}
	return errors.New("Cannot re-enable debugger from here")
}

func (d *Debugger) SetFile(filename string) error {
	return errors.New("Not used")
}

func (d *Debugger) Complete() error {
	d.Lock()
	defer d.Unlock()
	d.logger.Infof("COMPLETE")
	d.enabled = false
	d.server.Stop()
	d.run <- true
	return nil
}

func (d *Debugger) Start(function *lisp.LVal) {
	d.logger.Infof("Instr %v", function.Type)
	switch function.Type {
	case lisp.LInt:
		return
	case lisp.LBytes:
		return
	case lisp.LString:
		return
	case lisp.LFloat:
		return
	case lisp.LSortMap:
		return
	case lisp.LArray:
		return
	}
	if !d.stopped {
		d.Lock()
		for _, v := range d.breakpoints {
			if v.File == function.Source.File && v.Line == function.Source.Line {
				d.logger.Infof("BREAKPOINT")
				d.stopped = true
				d.Unlock()
				d.Start(function)
				return
			}
		}
		d.Unlock()
		return
	}
	select {
	case <-d.run:
		d.logger.Infof("RUN")
		d.stopped = false
	case <-d.step:
		d.logger.Infof("STEP")
	}
	d.currentOp = function
}

func (d *Debugger) End(function *lisp.LVal) {
	d.logger.Infof("End %v",function)
	// no op for now
}

func (d *Debugger) GetBreakpoint(id int) (*api.Breakpoint, error) {
	d.Lock()
	defer d.Unlock()
	if bp, ok := d.breakpoints[id]; ok {
		return bp, nil
	}
	return nil, errors.New("not found")
}

func (d *Debugger) GetBreakpointByName(name string) (*api.Breakpoint, error) {
	d.Lock()
	defer d.Unlock()
	for _, v := range d.breakpoints {
		if v.Name == name {
			return v, nil
		}
	}
	return nil, errors.New("not found")
}

func (d *Debugger) GetStacktrace(st *rpc2.StacktraceOut) {
	d.Lock()
	defer d.Unlock()
	st.Locations = make([]api.Stackframe, 0)
	for _, frame := range d.runtime.Stack.Frames {
		st.Locations = append(st.Locations, api.Stackframe{
			Location: api.Location{
				PC:   0,
				File: frame.Source.File,
				Line: frame.Source.Line,
				Function: &api.Function{
					Name_:     "",
					Value:     0,
					Type:      0,
					GoType:    0,
					Optimized: false,
				},
				PCs: nil,
			},
			Locals:             nil,
			Arguments:          nil,
			FrameOffset:        0,
			FramePointerOffset: 0,
			Defers:             nil,
			Bottom:             false,
			Err:                "",
		})
	}
}

func (d *Debugger) GetAllBreakpoints() map[int]*api.Breakpoint {
	d.Lock()
	defer d.Unlock()
	return d.breakpoints
}

func (d *Debugger) CreateBreakpoint(breakpoint *api.Breakpoint) *api.Breakpoint {
	d.Lock()
	defer d.Unlock()
	d.lastModified = time.Now()
	if breakpoint.ID == 0 {
		breakpoint.ID = rand.Int()
	}
	d.breakpoints[breakpoint.ID] = breakpoint
	return breakpoint
}

func (d *Debugger) RemoveBreakpoint(id int) error {
	d.Lock()
	defer d.Unlock()
	d.lastModified = time.Now()
	_, err := d.GetBreakpoint(id)
	if err != nil {
		return err
	}
	d.breakpoints[id] = nil
	return nil
}

func (d *Debugger) AmendBreakpoint(bp *api.Breakpoint) error {
	d.Lock()
	defer d.Unlock()
	d.lastModified = time.Now()
	_, err := d.GetBreakpoint(bp.ID)
	if err != nil {
		return err
	}
	d.breakpoints[bp.ID] = bp
	return nil
}

func (d *Debugger) GetThread() *api.Thread {
	return &api.Thread{
		ID:             1,
		PC:             0,
		File:           "",
		Line:           0,
		Function:       nil,
		GoroutineID:    0,
		Breakpoint:     nil,
		BreakpointInfo: nil,
		ReturnValues:   nil,
	}
}

func mapLispType(in lisp.LType) string {
	switch in {
	case lisp.LFloat:
		return "float"
	case lisp.LInt:
		return "int"
	case lisp.LString:
		return "string"
	case lisp.LQSymbol:
		return "q_symbol"
	case lisp.LSExpr:
		return "s_expr"
	case lisp.LSortMap:
		return "sorted map"
	case lisp.LNative:
		return "native"
	case lisp.LFun:
		return "function"
	case lisp.LQuote:
		return "quote"
	case lisp.LArray:
		return "array"
	case lisp.LError:
		return "error"
	case lisp.LBytes:
		return "[]byte"
	case lisp.LInvalid:
		return "invalid"
	default:
		return "unknown"

	}
}

func mapKind(in *lisp.LVal) reflect.Kind {
	switch in.Type {
	case lisp.LFloat:
		return reflect.Float64
	case lisp.LInt:
		return reflect.Int
	case lisp.LString:
		return reflect.String
	case lisp.LQSymbol:
		return reflect.Struct
	case lisp.LSExpr:
		return reflect.Struct
	case lisp.LSortMap:
		return reflect.Map
	case lisp.LNative:
		return reflect.ValueOf(in.Native).Kind()
	case lisp.LFun:
		return reflect.Func
	case lisp.LQuote:
		return reflect.String
	case lisp.LArray:
		return reflect.Slice
	case lisp.LError:
		return reflect.Struct
	case lisp.LBytes:
		return reflect.Slice
	case lisp.LInvalid:
		return reflect.Invalid
	default:
		return reflect.Invalid
	}
}

func (d *Debugger) GetVariables() []api.Variable {
	d.Lock()
	defer d.Unlock()
	out := make([]api.Variable, len(d.runtime.Package.Symbols))
	count := 0
	for k, v := range d.runtime.Package.Symbols {
		out[count] = api.Variable{
			Name:         k,
			Addr:         uintptr(count),
			OnlyAddr:     false,
			Type:         mapLispType(v.Type),
			RealType:     mapLispType(v.Type),
			Flags:        0,
			Kind:         mapKind(v),
			Value:        v.Str,
			Len:          0,
			Cap:          0,
			Children:     []api.Variable{},
			Base:         0,
			Unreadable:   "",
			LocationExpr: "",
			DeclLine:     int64(v.Source.Line),
		}
		count++
	}
	return out
}

func (d *Debugger) GetFunctionArgs() []api.Variable {
	return []api.Variable{}
}

func (d *Debugger) GetGoRoutine() *api.Goroutine {
	d.Lock()
	defer d.Unlock()
	var loc *api.Location
	if d.currentOp != nil {
		loc = &api.Location{
			PC:   uint64(d.currentOp.Source.Pos),
			File: d.currentOp.Source.File,
			Line: d.currentOp.Source.Line,
			Function: &api.Function{
				Name_:     "",
				Value:     0,
				Type:      0,
				GoType:    0,
				Optimized: false,
			},
			PCs: nil,
		}
	} else {
		loc = &api.Location{
			PC: 0,
		}
	}
	return &api.Goroutine{
		ID:             0,
		CurrentLoc:     *loc,
		UserCurrentLoc: *loc,
		GoStatementLoc: *loc,
		StartLoc:       api.Location{},
		ThreadID:       0,
		Unreadable:     "",
	}
}

func (d *Debugger) SetVariableInScope(scope api.EvalScope, symbol string, value string) error {
	d.Lock()
	defer d.Unlock()
	d.lastModified = time.Now()
	if existing, exists := d.runtime.Package.Symbols[symbol]; exists {
		switch existing.Type {
		case lisp.LString:
			d.runtime.Package.Symbols[symbol].Str = value
			return nil
		case lisp.LInt:
			v, err := strconv.ParseInt(value, 10, 64)
			if err != nil {
				return err
			}
			d.runtime.Package.Symbols[symbol].Int = int(v)
			return nil
		case lisp.LFloat:
			v, err := strconv.ParseFloat(value, 64)
			if err != nil {
				return err
			}
			d.runtime.Package.Symbols[symbol].Float = v
			return nil
		}
		return errors.New("Cannot set this type")
	}
	return errors.New("Symbol not in scope")
}

func (d *Debugger) Sources(filter string) ([]string, error) {
	return []string{}, nil
}

func (d *Debugger) Functions(filter string) ([]string, error) {
	d.Lock()
	defer d.Unlock()
	out := make([]string, len(d.runtime.Package.FunNames))
	count := 0
	for _, v := range d.runtime.Package.FunNames {
		out[count] = v
		count++
	}
	return out, nil
}

func (d *Debugger) FindLocation(scope api.EvalScope, loc string, lines bool) ([]api.Location, error) {
	return nil, errors.New("Not implemented... yet")
}

func (d *Debugger) ListPackagesBuildInfo(files bool) []api.PackageBuildInfo {
	d.Lock()
	defer d.Unlock()
	out := make([]api.PackageBuildInfo, 0)
	for k, v := range d.runtime.Registry.Packages {
		out = append(out, api.PackageBuildInfo{
			ImportPath:    k,
			DirectoryPath: v.Name,
			Files:         nil,
		})
	}
	return out
}

func (d *Debugger) State(blocking bool) (*api.DebuggerState, error) {
	d.Lock()
	defer d.Unlock()
	state := &api.DebuggerState{
		Running:           !d.stopped,
		CurrentThread:     d.GetThread(),
		SelectedGoroutine: nil,
		Threads:           []*api.Thread{d.GetThread()},
		NextInProgress:    false,
		Exited:            !d.enabled,
		ExitStatus:        0,
		When:              "",
		Err:               nil,
	}
	return state, nil
}

func (d *Debugger) Command(a *api.DebuggerCommand) (*api.DebuggerState, error) {
	d.logger.Infof("Command: %s", a.Name)
	d.lastModified = time.Now()
	switch a.Name {
	case api.Halt:
		d.stopped = true
	case api.Continue:
		go func(d *Debugger) {
			d.run <- true
		}(d)
	case api.Next:
		return nil, errors.New("Not implemented")
	case api.Step:
		go func(d *Debugger) {
			d.step <- true
		}(d)
	case api.Call:
		return nil, errors.New("Not implemented")
	case api.ReverseStepInstruction:
		return nil, errors.New("Not implemented")
	case api.Rewind:
		return nil, errors.New("Not implemented")
	case api.SwitchGoroutine:
		return nil, errors.New("Not implemented")
	case api.SwitchThread:
		return nil, errors.New("Not implemented")
	}
	return d.State(false)
}

func (d *Debugger) LastModified() time.Time {
	return d.lastModified
}
