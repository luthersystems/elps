package debugger

import (
	"errors"
	"fmt"
	"github.com/go-delve/delve/service/rpc2"
	"github.com/google/go-dap"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger/dapserver"
	"github.com/luthersystems/elps/lisp/x/debugger/delveserver"
	"github.com/luthersystems/elps/lisp/x/debugger/events"
	"github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser/token"
	"github.com/sirupsen/logrus"
	"math/rand"
	"os"
	"reflect"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"time"
)
import "github.com/go-delve/delve/service/api"

type Debugger struct {
	sync.Mutex
	enabled         bool
	breakpoints     map[int]*api.Breakpoint
	env             *lisp.LEnv
	runtime         *lisp.Runtime
	lastModified    time.Time
	stopped         bool
	step            chan bool
	run             chan bool
	server          DebugServer
	currentOp       *op
	logger          *logrus.Logger
	pwd             string
	callRefs        *callRef
	isEvaling       bool
	reenteringStart bool
}

func (d *Debugger) Done() bool {
	panic("implement me")
}

type op struct {
	name   string
	source *token.Location
	args   map[string]*lisp.LVal
}

type DebugServer interface {
	Run() error
	Stop() error
	Event(events.EventType)
	Breakpoint(v *api.Breakpoint)
}

type DebugMode string

const DebugModeDelve = DebugMode("delve")
const DebugModeDAP = DebugMode("dap")

func NewDebugger(env *lisp.LEnv, address string, mode DebugMode) lisp.Debugger {
	db := &Debugger{
		runtime:      env.Runtime,
		env:          env,
		enabled:      true,
		stopped:      true,
		run:          make(chan bool),
		step:         make(chan bool),
		lastModified: time.Now(),
		breakpoints:  make(map[int]*api.Breakpoint),
		logger:       logrus.New(),
	}
	var srv DebugServer
	if mode == DebugModeDelve {
		srv = delveserver.NewServer(db, address)
	} else if mode == DebugModeDAP {
		var err error
		srv, err = dapserver.NewServer(db, address, 2)
		if err != nil {
			panic(err.Error())
		}
		go func(end chan bool) {
			die := <-end
			if die {
				panic("Remote process terminated debuggee")
			}
		}(srv.(*dapserver.Server).EndChannel)
	}
	srv.Run()
	db.server = srv
	db.runtime.Debugger = db
	return db
}

func (d *Debugger) IsStopped() bool {
	return d.stopped
}

func (d *Debugger) SetLispPath(path string) {
	d.pwd = strings.ReplaceAll(path, `\`, `/`)
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
	d.server.Event(events.EventTypeTerminated)
	return nil
}

func (d *Debugger) Start(expr *lisp.LVal, function *lisp.LVal) {
	if d.isEvaling {
		return
	}
	fname := ""
	if function.FunData() != nil {
		fname = function.FunData().FID
	}
	source := expr.Source
	if source == nil || source.File == "<native code>" {
		for f := len(d.runtime.Stack.Frames) - 1; f >= 0; f-- {
			frame := d.runtime.Stack.Frames[f]
			if frame.Source != nil || frame.Source.File == "<native code>" {
				continue
			}
			source = frame.Source
			break
		}
	}
	if source == nil {
		source = &token.Location{
			File: "Unknown",
			Line: 0,
		}
	}
	sourceStr := fmt.Sprintf("@%s:%d", source.File, source.Line)
	d.logger.Debugf("Instr %v %s params %v %s %d", function.Type, fname, function.Cells, sourceStr, len(d.runtime.Stack.Frames))
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
	args := make(map[string]*lisp.LVal)
	paramCounter := 0
	fname = d.runtime.Package.FunNames[fname]
	if function.Type == lisp.LFun {
		argList := function.Cells[0]
		restMode := false
		for count, argName := range argList.Cells {
			if argName.Str == lisp.VarArgSymbol {
				restMode = true
				continue
			}
			if restMode {
				for pos := count; pos < len(expr.Cells); pos++ {
					args[fmt.Sprintf("%s[%d]", argName.Str, pos)] = expr.Cells[pos]
				}
				break
			}
			val := &lisp.LVal{
				Type: lisp.LInvalid,
			}
			if len(expr.Cells) >= count {
				val = expr.Cells[count+1]
			}
			args[argName.Str] = val
		}
	} else {
		for k, v := range function.Cells {
			argName := d.runtime.Package.Symbols[fname].Cells[paramCounter].String()
			if argName == "" {
				argName = strconv.Itoa(k)
			}
			d.logger.Debugf("Arg %s %v is %v", argName, d.runtime.Package.Symbols[fname].Cells[paramCounter], d.mapValue(v))
			if cell := expr.Cells[k+1]; cell != nil {
				args[argName] = cell
			} else {
				args[argName] = v
			}
		}
	}
	d.currentOp = &op{
		name:   fname,
		source: source,
		args:   args,
	}
	if !d.reenteringStart {
		d.incrementCallRef(expr, function)
	} else {
		d.reenteringStart = false
	}
	// don't want to pause on code we can't see...
	if source.File == "<native code>" {
		runtime.Gosched()
		return
	}
	if !d.stopped {
		d.Lock()
		if expr.Source != nil {
			fstat, _ := os.Stat(expr.Source.Path)
			for _, v := range d.breakpoints {
				vStat, _ := os.Stat(v.File)
				if (os.SameFile(vStat, fstat) || fmt.Sprintf("%s%c%s", d.pwd, os.PathSeparator, expr.Source.File) == v.File) &&
					v.Line == expr.Source.Line {
					d.logger.Infof("BREAKPOINT")
					d.stopped = true
					d.Unlock()
					d.server.Breakpoint(v)
					d.server.Event(events.EventTypeStoppedBreakpoint)
					// or it won't yield...
					runtime.Gosched()
					d.reenteringStart = true
					d.Start(expr, function)
					return
				}
			}
		}
		d.Unlock()
		runtime.Gosched()
		return
	}
	select {
	case <-d.run:
		d.server.Event(events.EventTypeContinued)
		d.logger.Infof("RUN")
		d.stopped = false
	case <-d.step:
		d.server.Event(events.EventTypeContinued)
		d.server.Event(events.EventTypeStoppedStep)
		d.logger.Infof("STEP")
	}
	// if we don't do this, ELPS' indomitable stack traces prevent the debugger receiving
	// calls if we only have a few processor cores
	runtime.Gosched()
}

func (p *Debugger) getFunctionParameters(function *lisp.LVal) (string, string) {
	return profiler.GetFunNameFromFID(p.runtime, function.FunData().FID), function.FunData().Package
}

func (d *Debugger) End(function *lisp.LVal) {
	if d.isEvaling {
		return
	}
	d.logger.Debugf("End %v ", function.FunData().FID)
	switch function.Type {
	case lisp.LFun, lisp.LSymbol, lisp.LSExpr, lisp.LQSymbol:
		d.decrementCallRef()
	}
	// no op for now except that we yield the CPU
	runtime.Gosched()
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

func (d *Debugger) GetModules() []dap.Module {
	modules := make([]dap.Module, 0)
	for k := range d.runtime.Registry.Packages {
		modules = append(modules, dap.Module{
			Id:   k,
			Name: k,
		})
	}
	return modules
}

func (d *Debugger) Eval(text string) string {
	d.Lock()
	d.isEvaling = true
	defer func() {
		d.isEvaling = false
		d.Unlock()
	}()
	d.logger.Debugf("Evaluating %s", text)
	tEnv := lisp.NewEnv(nil)
	tEnv.Runtime.Registry = d.runtime.Registry
	tEnv.Runtime.Package = d.runtime.Package
	tEnv.Runtime.Reader = d.runtime.Reader
	tEnv.Runtime.Debugger = nil
	v := tEnv.LoadString("eval", text)
	return d.mapValue(v)
}

func (d *Debugger) GetDapStacktrace() []dap.StackFrame {
	current := d.callRefs
	out := make([]dap.StackFrame, 0)
	for {
		if current == nil {
			break
		}
		d.logger.Debugf("Line %s in %s line %d col %d", current.name, current.file, current.line, current.col)
		hint := "normal"
		origin := ""
		if current.file == "Unknown file" || current.file == "<native code>" {
			hint = "deemphasize"
			origin = "internal module"
		}
		out = append(out, dap.StackFrame{
			Id:   0,
			Name: current.name,
			Source: dap.Source{
				Name:             current.file,
				Path:             current.path,
				PresentationHint: hint,
				Origin:           origin,
			},
			Line:             current.line,
			Column:           current.col,
			ModuleId:         current.packageName,
			PresentationHint: hint,
		})
		current = current.prev
	}
	return out
}

func (d *Debugger) GetStacktrace(st *rpc2.StacktraceOut) {
	d.logger.Debug("Returning STACK")
	st.Locations = make([]api.Stackframe, 0)
	for _, frame := range d.runtime.Stack.Frames {
		var source = frame.Source
		if source == nil {
			source = &token.Location{
				File: "Unknown file",
				Path: "",
				Pos:  0,
				Line: 0,
				Col:  0,
			}
		}
		st.Locations = append(st.Locations, api.Stackframe{
			Location: api.Location{
				PC:   0,
				File: fmt.Sprintf("%s", source.Path),
				Line: source.Line,
				Function: &api.Function{
					Name_:     "f",
					Value:     0,
					Type:      0,
					GoType:    0,
					Optimized: false,
				},
				PCs: []uint64{},
			},
			Locals: []api.Variable{

			},
			Arguments: []api.Variable{

			},
			FrameOffset:        0,
			FramePointerOffset: 0,
			Defers:             []api.Defer{},
			Bottom:             false,
			Err:                "",
		})
	}
}

func (d *Debugger) GetAllBreakpoints() map[int]*api.Breakpoint {
	d.logger.Debug("Returning BREAKPOINTS")
	d.Lock()
	defer d.Unlock()
	return d.breakpoints
}

func (d *Debugger) CreateBreakpoint(breakpoint *api.Breakpoint) *api.Breakpoint {
	// TODO ADD COLUMNS
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
	d.logger.Debug("Returning THREADS")
	var loc *api.Thread
	if d.currentOp != nil {
		var source = d.currentOp.source
		if source == nil {
			source = &token.Location{
				File: "Unknown file",
				Path: "",
				Pos:  0,
				Line: 0,
				Col:  0,
			}
		}
		loc = &api.Thread{
			PC:   uint64(source.Pos),
			File: fmt.Sprintf("%s/%s", d.pwd, source.File),
			Line: source.Line,
			Function: &api.Function{
				Name_:     d.currentOp.name,
				Value:     0,
				Type:      0,
				GoType:    0,
				Optimized: false,
			},
			ID:          1,
			GoroutineID: 1,
		}
	} else {
		loc = &api.Thread{
			ID:          1,
			GoroutineID: 1,
		}
	}
	return loc
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

func (d *Debugger) sexprAsString(in *lisp.LVal) string {
	out := ""
	for _, v := range in.Cells {
		out += d.mapValue(v) + " "
	}
	return out
}

func (d *Debugger) mapValue(in *lisp.LVal) string {
	switch in.Type {
	case lisp.LFloat:
		return strconv.FormatFloat(in.Float, 'f', 8, 64)
	case lisp.LInt:
		return strconv.FormatInt(int64(in.Int), 10)
	case lisp.LString:
		return in.Str
	case lisp.LQSymbol:
		return in.Str
	case lisp.LSymbol:
		return in.Str
	case lisp.LSExpr:
		return d.sexprAsString(in)
	case lisp.LSortMap:
		return "map"
	case lisp.LNative:
		return fmt.Sprintf("%v", in.Native)
	case lisp.LFun:
		return in.FunData().FID
	case lisp.LQuote:
		return in.Str
	case lisp.LArray:
		return "array"
	case lisp.LError:
		return fmt.Sprintf("%v", lisp.GoError(in))
	case lisp.LBytes:
		return "array"
	case lisp.LInvalid:
		return "INVALID"
	default:
		return in.Type.String()
	}
}

func (d *Debugger) extractChildren(in *lisp.LVal) []api.Variable {
	children := make([]api.Variable, 0)
	switch in.Type {
	case lisp.LSortMap:
		for _, v := range in.Cells[:1] {
			children = append(children, api.Variable{
				Type:     mapLispType(v.Type),
				RealType: mapLispType(v.Type),
				Value:    d.mapValue(v),
				Kind:     mapKind(v),
			})
		}
	case lisp.LArray:
		for _, v := range in.Cells[:1] {
			children = append(children, api.Variable{
				Type:     mapLispType(v.Type),
				RealType: mapLispType(v.Type),
				Value:    d.mapValue(v),
				Kind:     mapKind(v),
			})
		}
	case lisp.LBytes:
		for _, v := range in.Bytes() {
			children = append(children, api.Variable{
				Type:     "byte",
				RealType: "byte",
				Value:    string(v),
				Kind:     reflect.Uint8,
			})
		}
	}
	return children
}

func (d *Debugger) GetVariables() []api.Variable {
	d.logger.Debug("Returning VARS")
	d.Lock()
	defer d.Unlock()
	out := make([]api.Variable, 0)
	count := 0
	// deliberate copy - prevents us having to stop
	symbols := d.runtime.Package.Symbols
	for k, v := range symbols {
		if v.Type == lisp.LFun && v.FunData().Builtin != nil {
			continue
		}
		var source = v.Source
		if source == nil {
			source = &token.Location{
				File: "Unknown file",
				Path: "",
				Pos:  0,
				Line: 0,
				Col:  0,
			}
		}
		children := make([]api.Variable, 0)
		strVal := d.mapValue(v)
		if strVal == "map" || strVal == "array" {
			children = d.extractChildren(v)
		}
		out = append(out, api.Variable{
			Name:         k,
			Addr:         uintptr(count),
			OnlyAddr:     false,
			Type:         mapLispType(v.Type),
			RealType:     mapLispType(v.Type),
			Flags:        0,
			Kind:         mapKind(v),
			Value:        strVal,
			Len:          int64(len(children)),
			Cap:          int64(len(children)),
			Children:     children,
			Base:         0,
			Unreadable:   "",
			LocationExpr: "",
			DeclLine:     int64(source.Line),
		})
		count++
	}
	return out
}

func (d *Debugger) GetFunctionArgs() []api.Variable {
	d.logger.Debug("Returning ARGS")
	return []api.Variable{}
}

func (d *Debugger) GetGoRoutine() *api.Goroutine {
	d.logger.Debug("Returning GOROUTINE")
	d.Lock()
	defer d.Unlock()
	var loc *api.Location
	if d.currentOp != nil {
		var source = d.currentOp.source
		if source == nil {
			source = &token.Location{
				File: "Unknown file",
				Path: "",
				Pos:  0,
				Line: 0,
				Col:  0,
			}
		}
		loc = &api.Location{
			PC:   uint64(source.Pos),
			File: fmt.Sprintf("%s/%s", d.pwd, source.File),
			Line: source.Line,
			Function: &api.Function{
				Name_:     d.currentOp.name,
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
		ID:             1,
		CurrentLoc:     *loc,
		UserCurrentLoc: *loc,
		GoStatementLoc: *loc,
		StartLoc:       api.Location{},
		ThreadID:       1,
		Unreadable:     "",
	}
}

func (d *Debugger) SetVariableInScope(scope api.EvalScope, symbol string, value string) error {
	d.logger.Infof("SETTING variable %s to %s", symbol, value)
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
	d.logger.Debug("Returning SOURCES")
	intermediate := make(map[string]bool)
	for _, currPkg := range d.runtime.Registry.Packages {
		d.appendSourcesToMap(intermediate, currPkg, filter)
	}
	out := make([]string, 0)
	for k := range intermediate {
		out = append(out, k)
	}
	return out, nil
}

func (d *Debugger) getSourcesForPackage(currPkg *lisp.Package) []string {
	intermediate := make(map[string]bool)
	d.appendSourcesToMap(intermediate, currPkg, "")
	out := make([]string, 0)
	for k := range intermediate {
		out = append(out, k)
	}
	return out
}

func (d *Debugger) GetArguments() []dap.Variable {
	out := make([]dap.Variable, 0)
	for k, v := range d.currentOp.args {
		argValue := d.mapValue(v)
		if d.currentOp.name != "defun" && d.currentOp.name != "set" && d.currentOp.name != "let" && d.currentOp.name != "let*" && d.currentOp.name != "defmacro" {
			if v.Type == lisp.LSymbol {
				d.logger.Infof("Remapping %s", argValue)
				argValue = d.Eval(argValue)
			}
		}
		out = append(out, dap.Variable{
			Name:  k,
			Value: argValue,
			Type:  mapLispType(v.Type),
		})
		d.logger.Infof("Sending arg %s as %s", out[len(out)-1].Name, out[len(out)-1].Value)
	}
	d.logger.Debugf("Args: %v", out)
	return out
}

func (d *Debugger) appendSourcesToMap(intermediate map[string]bool, currPkg *lisp.Package, filter string) {
	for _, sym := range currPkg.Symbols {
		if sym.Source == nil {
			continue
		}
		if filter != "" && !strings.HasPrefix(fmt.Sprintf("%s/%s", d.pwd, sym.Source.File), filter) {
			continue
		}
		intermediate[fmt.Sprintf("%s/%s", d.pwd, sym.Source.File)] = true
	}
}

func (d *Debugger) Functions(filter string) ([]string, error) {
	d.logger.Debug("Returning FUNCTIONS")
	d.Lock()
	defer d.Unlock()
	out := make([]string, 0)
	seen := make(map[string]bool)
	for _, v := range d.runtime.Package.FunNames {
		if seen[v] {
			continue
		}
		out = append(out, v)
	}
	return out, nil
}

func (d *Debugger) FindLocation(scope api.EvalScope, loc string, lines bool) ([]api.Location, error) {
	return nil, errors.New("Not implemented... yet")
}

func (d *Debugger) ListPackagesBuildInfo(files bool) []api.PackageBuildInfo {
	d.logger.Debug("Returning BUILD INFO")
	d.Lock()
	defer d.Unlock()
	out := make([]api.PackageBuildInfo, 0)
	for k, v := range d.runtime.Registry.Packages {
		out = append(out, api.PackageBuildInfo{
			ImportPath:    k,
			DirectoryPath: v.Name,
			Files:         d.getSourcesForPackage(v),
		})
	}
	return out
}

func (d *Debugger) State(blocking bool) (*api.DebuggerState, error) {
	d.logger.Debug("Returning STATE")
	state := &api.DebuggerState{
		Running:           !d.stopped,
		CurrentThread:     d.GetThread(),
		SelectedGoroutine: d.GetGoRoutine(),
		Threads:           []*api.Thread{d.GetThread()},
		NextInProgress:    false,
		Exited:            !d.enabled,
		ExitStatus:        0,
		When:              "",
		Err:               nil,
	}
	return state, nil
}

func (d *Debugger) Continue() {
	if d.stopped {
		go func(d *Debugger) {
			d.run <- true
		}(d)
	}
}

func (d *Debugger) Step() {
	if d.stopped {
		go func(d *Debugger) {
			d.step <- true
		}(d)
	}
}

func (d *Debugger) Halt() {
	d.stopped = true
}

func (d *Debugger) Command(a *api.DebuggerCommand) (*api.DebuggerState, error) {
	// TODO this is Delve specific
	d.logger.Debug("Command: %s", a.Name)
	d.lastModified = time.Now()
	started := false
	switch a.Name {
	case api.Halt:
		d.Halt()
	case api.Continue:
		started = true
		d.Continue()
	case api.Next:
		return nil, errors.New("Not implemented")
	case api.Step:
		started = true
		d.Step()
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
	state, err := d.State(false)
	if err != nil && started == true {
		state.Running = true
	}
	return state, err
}

func (d *Debugger) LastModified() time.Time {
	return d.lastModified
}

// Generates a call ref so the same item can be located again
func (p *Debugger) incrementCallRef(expr, function *lisp.LVal) *callRef {
	p.Lock()
	defer p.Unlock()
	name, module := p.getFunctionParameters(function)
	frameRef := new(callRef)
	frameRef.name = name
	frameRef.children = make([]*callRef, 0)
	if function.Source != nil {
		frameRef.file = expr.Source.File
		frameRef.line = expr.Source.Line
		frameRef.col = expr.Source.Col
		frameRef.path = expr.Source.Path
		frameRef.packageName = module
	}
	if len(p.runtime.Stack.Frames) > 0 {
		p.logger.Debug("Overriding...")
		current := p.runtime.Stack.Frames[len(p.runtime.Stack.Frames)-1]
		if frameRef.file == "" {
			p.logger.Debugf("Overriding... file %s", current.Source.File)
			frameRef.file = current.Source.File
		}
		if frameRef.path == "" {
			p.logger.Debugf("Overriding... path %s", current.Source.Path)
			frameRef.path = current.Source.Path
		}
		if frameRef.line == 0 {
			frameRef.line = current.Source.Line
		}
		if frameRef.col == 0 {
			frameRef.col = current.Source.Col
		}
	}
	current := p.callRefs
	if current != nil {
		frameRef.prev = current
		frameRef.prev.children = append(frameRef.prev.children, frameRef)
	}
	frameRef.start = time.Now()
	p.callRefs = frameRef
	return frameRef
}

// Finds a call ref for the current scope
func (p *Debugger) decrementCallRef() *callRef {
	current := p.callRefs
	p.callRefs = current.prev
	return current
}

// Represents something that got called
type callRef struct {
	start       time.Time
	prev        *callRef
	name        string
	children    []*callRef
	file        string
	path        string
	line        int
	col         int
	packageName string
}
