package profiler

import (
	"errors"
	"fmt"
	"os"
	"runtime"
	"sync"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// A profiler implementation that builds Callgrind files.
// Heavily influenced by how XDebug works for PHP. Not because
// I like PHP but because XDebug is very light
// The resulting files can be opened in KCacheGrind or QCacheGrind.
type callgrindProfiler struct {
	sync.Mutex
	writer     *os.File
	runtime    *lisp.Runtime
	enabled    bool
	startTime  time.Time
	refs       map[string]int
	refCounter int
	callRefs   map[int32]*callRef
}

// Returns a new Callgrind processor
func NewCallgrindProfiler(runtime *lisp.Runtime) lisp.Profiler {
	p := new(callgrindProfiler)
	p.runtime = runtime
	runtime.Profiler = p
	return p
}

// Represents something that got called
type callRef struct {
	start       time.Time
	prev        *callRef
	name        string
	children    []*callRef
	duration    time.Duration
	startMemory uint64
	endMemory   uint64
	file        string
	line        int
}

func (p *callgrindProfiler) IsEnabled() bool {
	return p.enabled
}

func (p *callgrindProfiler) Enable() error {
	p.Lock()
	if p.enabled {
		return errors.New("profiler already enabled")
	}
	if p.writer == nil {
		return errors.New("no output set in profiler")
	}
	fmt.Fprintf(p.writer, "version: 1\ncreator: elps %s (Go %s)\n", lisp.ElpsVersion, runtime.Version())
	fmt.Fprintf(p.writer, "cmd: Eval\npart: 1\npositions: line\n\n")
	fmt.Fprintf(p.writer, "events: Time_(ns) Memory_(bytes)\n\n")
	p.callRefs = make(map[int32]*callRef)
	p.startTime = time.Now()
	p.refs = make(map[string]int)
	p.refCounter = 0
	p.enabled = true
	p.Unlock()
	p.incrementCallRef("ENTRYPOINT", &token.Location{
		File: "-",
		Path: "-",
		Pos:  0,
		Line: 0,
		Col:  0,
	})
	return nil
}

func (p *callgrindProfiler) SetFile(filename string) error {
	p.Lock()
	defer p.Unlock()
	if p.enabled {
		return errors.New("profiler already enabled")
	}
	pointer, err := os.Create(filename) //#nosec G304
	if err != nil {
		return err
	}
	p.writer = pointer
	return nil
}

func (p *callgrindProfiler) Complete() error {
	ref := p.getCallRefAndDecrement()
	p.Lock()
	defer p.Unlock()
	// Generate entrypoint
	ref.duration = time.Since(ref.start)
	fmt.Fprintf(p.writer, "fl=%s\n", p.getRef(ref.file))
	fmt.Fprintf(p.writer, "fn=%s\n", p.getRef(ref.name))
	fmt.Fprintf(p.writer, "%d %d %d\n", 0, ref.duration, 0)
	// Output the things we called
	for _, entry := range ref.children {
		fmt.Fprintf(p.writer, "cfl=%s\n", p.getRef(entry.file))
		fmt.Fprintf(p.writer, "cfn=%s\n", p.getRef(entry.name))
		fmt.Fprint(p.writer, "calls=1 0 0\n")
		fmt.Fprintf(p.writer, "%d %d %d\n", entry.line, entry.duration, 0)
	}
	fmt.Fprint(p.writer, "\n")
	duration := time.Since(p.startTime)
	ms := &runtime.MemStats{}
	runtime.ReadMemStats(ms)
	fmt.Fprintf(p.writer, "summary %d %d\n\n", duration.Nanoseconds(), ms.TotalAlloc)
	return p.writer.Close()
}

func (p *callgrindProfiler) getRef(name string) string {
	if ref, ok := p.refs[name]; ok {
		return fmt.Sprintf("(%d)", ref)
	}
	p.refCounter++
	p.refs[name] = p.refCounter
	return fmt.Sprintf("(%d) %s", p.refCounter, name)
}

func (p *callgrindProfiler) Start(function *lisp.LVal) func() {
	if !p.enabled {
		return func() {}
	}
	switch function.Type {
	case lisp.LInt, lisp.LString, lisp.LFloat, lisp.LBytes, lisp.LError, lisp.LArray, lisp.LQuote, lisp.LNative, lisp.LQSymbol, lisp.LSortMap:
		// We don't need to profile these types. We could, but we're not that LISP :D
		return func() {}
	case lisp.LFun, lisp.LSymbol, lisp.LSExpr:
		// Mark the time and point of entry. It feels like we're building the call stack in Runtime
		// again, but we're not - it's called, not callers.
		_, _, name := p.getFunctionParameters(function)
		p.incrementCallRef(name, function.Source)
	default:
		panic(fmt.Sprintf("Missing type %d", function.Type))
	}

	return func() {
		p.End(function)
	}
}

// Generates a call ref so the same item can be located again
func (p *callgrindProfiler) incrementCallRef(name string, loc *token.Location) *callRef {
	p.Lock()
	defer p.Unlock()
	thread := &([]int32{1}[0])
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
	ms := &runtime.MemStats{}
	runtime.ReadMemStats(ms)
	frameRef.startMemory = ms.TotalAlloc
	frameRef.start = time.Now()
	p.callRefs[*thread] = frameRef
	return frameRef
}

// Finds a call ref for the current scope
func (p *callgrindProfiler) getCallRefAndDecrement() *callRef {
	//C.GetGoId(thread)
	thread := &([]int32{1}[0])
	if current, ok := p.callRefs[*thread]; ok {
		p.callRefs[*thread] = current.prev
		return current
	}
	panic(fmt.Sprintf("Unset thread ref %d", *thread))
}

// Gets file parameters from the LVal
func (p *callgrindProfiler) getFunctionParameters(function *lisp.LVal) (string, int, string) {
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
	fName := fmt.Sprintf("%s:%s", function.FunData().Package, getFunNameFromFID(p.runtime, function.FunData().FID))
	return source, line, fName
}

func (p *callgrindProfiler) End(function *lisp.LVal) {
	if !p.enabled {
		return
	}
	p.Lock()
	defer p.Unlock()
	switch function.Type {
	case lisp.LInt, lisp.LString, lisp.LFloat, lisp.LBytes, lisp.LError, lisp.LArray, lisp.LQuote, lisp.LNative, lisp.LSortMap:
		// Again, we can ignore these as we never put them on the stack
		return
	case lisp.LFun, lisp.LSymbol, lisp.LSExpr, lisp.LQSymbol:
		source, line, fName := p.getFunctionParameters(function)
		// Write what function we've been observing and where to find it
		fmt.Fprintf(p.writer, "fl=%s\n", p.getRef(source))
		fmt.Fprintf(p.writer, "fn=%s\n", p.getRef(fName))
		ref := p.getCallRefAndDecrement()
		ref.duration = time.Since(ref.start)
		if ref.duration == 0 {
			ref.duration = 1
		}
		ms := &runtime.MemStats{}
		runtime.ReadMemStats(ms)
		ref.endMemory = ms.TotalAlloc
		memory := ref.endMemory - ref.startMemory
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
	default:
		panic(fmt.Sprintf("Missing type %d", function.Type))
	}
}
