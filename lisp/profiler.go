package lisp

import (
	"errors"
	"fmt"
	"github.com/luthersystems/elps/parser/token"
	"os"
	"regexp"
	"runtime"
	"sync"
	"time"
)


const ElpsVersion = "1.7"
var builtinRegex = regexp.MustCompile("\\<(?:builtin|special)-[a-z]+ \\`\\`(.*)\\'\\'\\>")

// Interface for a profiler
type Profiler interface {
	// Is the profiler enabled?
	IsEnabled() bool
	// Enable the profiler
	Enable() error
	// Set the file to output to
	SetFile(filename string) error
	// End the profiling session and output summary lines
	Complete() error
	// Marks the start of a process
	Start(function *LVal)
	// Marks the end of a process
	End(function *LVal)
}

// A profiler implementation that builds Callgrind files.
// Heavily influenced by how XDebug works for PHP. Not because
// I like PHP but because XDebug is very light
// The resulting files can be opened in KCacheGrind or QCacheGrind.
type profiler struct {
	sync.Mutex
	writer *os.File
	runtime *Runtime
	enabled bool
	startTime time.Time
	refs map[string]int
	sourceCache map[int]*token.Location
	refCounter int
	callRefs map[int32]*callRef
}

// Returns a new Callgrind processor
func NewProfiler(runtime *Runtime) Profiler {
	p := new(profiler)
	p.runtime = runtime
	runtime.Profiler = p
	return p
}

// Represents something that got called
type callRef struct {
	start time.Time
	prev *callRef
	name string
	children []*callRef
	duration time.Duration
	file string
	line int
}

func (p *profiler) IsEnabled() bool {
	return p.enabled
}

func (p *profiler) Enable() error {
	p.Lock()
	defer p.Unlock()
	if p.enabled {
		return errors.New("Profiler already enabled")
	}
	if p.writer == nil {
		return errors.New("No output set in profiler")
	}
	fmt.Fprintf(p.writer, "version: 1\ncreator: elps %s (Go %s)\n", ElpsVersion, runtime.Version());
	fmt.Fprintf(p.writer, "cmd: %s\npart: 1\npositions: line\n\n", "TODO");
	fmt.Fprintf(p.writer, "events: Time_(ns) Memory_(bytes)\n\n");
	p.callRefs = make(map[int32]*callRef)
	p.startTime = time.Now()
	p.refs = make(map[string]int)
	p.refCounter = 0
	p.enabled = true
	return nil
}

func (p *profiler) SetFile(filename string) error {
	p.Lock()
	defer p.Unlock()
	if p.enabled {
		return errors.New("Profiler already enabled")
	}
	pointer, err := os.Create(filename)
	if err != nil {
		return err
	}
	p.writer = pointer
	return nil
}

func (p *profiler) Complete() error {
	p.Lock()
	defer p.Unlock()
	duration := time.Now().Sub(p.startTime)
	ms := &runtime.MemStats{}
	runtime.ReadMemStats(ms)
	fmt.Fprintf(p.writer, "summary %d %d\n\n", duration.Nanoseconds(), ms.TotalAlloc)
	return p.writer.Close()
}

func (p *profiler) getRef(name string) string {
	if ref, ok := p.refs[name]; ok {
		return fmt.Sprintf("(%d)", ref)
	}
	p.refCounter++
	p.refs[name] = p.refCounter
	return fmt.Sprintf("(%d) %s", p.refCounter, name)
}

func (p *profiler) Start(function *LVal) {
	if !p.enabled {
		return
	}
	switch function.Type {
	case LInt, LString, LFloat, LBytes, LError, LArray, LQuote, LNative, LQSymbol, LSortMap:
		// We don't need to profile these types. We could, but we're not that LISP :D
		return
	case LFun, LSymbol, LSExpr:
		// Mark the time and point of entry. It feels like we're building the call stack in Runtime
		// again, but we're not - it's called, not callers.
		_, _, name := p.getFunctionParameters(function)
		p.incrementCallRef(name, function.Source)
	default:
		panic(fmt.Sprintf("Missing type %d", function.Type))
	}
}

// Generates a call ref so the same item can be located again
func (p *profiler) incrementCallRef(name string, loc *token.Location) *callRef {
	p.Lock()
	defer p.Unlock()
	var thread *int32
	thread = &([]int32{1}[0]);
	frameRef := new(callRef)
	frameRef.name = name
	frameRef.children = make([]*callRef, 0)
	if loc != nil {
		frameRef.file = loc.File
		frameRef.line = loc.Line
	}
	if current, ok := p.callRefs[*thread]; ok && current != nil {
		frameRef.prev = current
		frameRef.prev.children = append(frameRef.prev.children, frameRef)
	}
	frameRef.start = time.Now()
	p.callRefs[*thread] = frameRef
	return frameRef
}

// Finds a call ref for the current scope
func (p *profiler) getCallRefAndDecrement() *callRef {
	var thread *int32
	//C.GetGoId(thread)
	thread = &([]int32{1}[0]);
	if current, ok := p.callRefs[*thread]; ok {
		p.callRefs[*thread] = current.prev
		return current
	}
	panic(fmt.Sprintf("Unset thread ref %d", *thread))
}

// Gets a canonical version of the function name suitable for viewing in KCacheGrind
func (p *profiler) getFunNameFromFID(in string) string {
	// Most of the time we can just look this up in FunNames
	if name, ok := p.runtime.Package.FunNames[in]; ok {
		return name
	}
	// but sometimes something doesn't match - so we'll try to regexp it out
	if !builtinRegex.MatchString(in) {
		return in
	}
	return builtinRegex.FindStringSubmatch(in)[1]
}

// Gets file parameters from the LVal
func (p *profiler) getFunctionParameters(function *LVal) (string, int, string) {
	var source string
	line := 0
	if function.Source == nil {
		if cell := function.Cells[0]; cell != nil && cell.Source != nil {
			source = cell.Source.File
			line = cell.Source.Line
		} else {
			source = "no-source"
		}
	} else {
		source = function.Source.File
		line = function.Source.Line
	}
	fName := fmt.Sprintf("%s:%s", function.FunData().Package, p.getFunNameFromFID(function.FunData().FID))
	return source, line, fName
}

func (p *profiler) End(function *LVal) {
	if !p.enabled {
		return
	}
	p.Lock()
	defer p.Unlock()
	switch function.Type {
	case LInt, LString, LFloat, LBytes, LError, LArray, LQuote, LNative, LQSymbol, LSortMap:
		// Again, we can ignore these as we never put them on the stack
		return
	case LFun, LSymbol, LSExpr:
		source, line, fName := p.getFunctionParameters(function)
		// Write what function we've been observing and where to find it
		fmt.Fprintf(p.writer, "fl=%s\n", p.getRef(source))
		fmt.Fprintf(p.writer, "fn=%s\n", p.getRef(fName))
		// TODO track memory
		memory := 0
		ref := p.getCallRefAndDecrement()
		ref.duration = time.Since(ref.start)
		if ref.duration == 0 {
			ref.duration = 1
		}
		// Cache the location - we won't be able to get it again when we build the maps for
		// things that call this.
		if ref.line == 0 && function.Source != nil {
			ref.line = function.Source.Line
			ref.file = function.Source.File
		}
		// Output timing and line ref
		fmt.Fprintf(p.writer, "%d %d %d\n", line, ref.duration, memory)
		// Output the things we called
		for _, entry := range ref.children {
			fmt.Fprintf(p.writer, "cfl=%s\n", p.getRef(entry.file))
			fmt.Fprintf(p.writer, "cfn=%s\n", p.getRef(entry.name))
			fmt.Fprint(p.writer, "calls=1 0 0\n")
			fmt.Fprintf(p.writer, "%d %d %d\n", entry.line, entry.duration, memory)
		}
		// and end the entry
		fmt.Fprint(p.writer, "\n")
		// if we're a top level caller we need to generate a file entry
		if len(p.runtime.Stack.Frames) == 0 {
			// just show the file
			fmt.Fprintf(p.writer, "fl=%s\n", p.getRef(source))
			fmt.Fprintf(p.writer, "%d %d %d\n", line, ref.duration, memory)
			// and call ourselves
			fmt.Fprintf(p.writer, "cfl=%s\n", p.getRef(source))
			fmt.Fprintf(p.writer, "cfn=%s\n", p.getRef(fName))
			fmt.Fprintf(p.writer, "%d %d %d\n", line, ref.duration, memory)
			fmt.Fprint(p.writer, "calls=1 0 0\n")
			// and end the entry
			fmt.Fprint(p.writer, "\n")
		}
	default:
		panic(fmt.Sprintf("Missing type %d", function.Type))
	}
}


