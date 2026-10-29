// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"log"
	"os"

	"github.com/luthersystems/elps/parser/token"
)

// CallStack is a function call stack.
//
// For errors produced by recovery of a Go panic, GoStack carries
// the runtime.Stack output captured at the panic site so callers (via
// ErrorVal.WriteTrace or direct access) can render the Go-level origin
// alongside the ELPS frames. It is nil for non-panic errors.
//
// Keep slice headers before scalars.
type CallStack struct {
	// wraps holds the lines WrapError added in front of the error's
	// message, outermost first (luthersystems/elps#831).  A wrap prepends a
	// new node and never writes one, so copies share the list.
	wraps *wrapLine

	Frames  []CallFrame
	GoStack []byte

	// Captured only for error reporting; ordinary live stacks leave this zero.
	// The stack deliberately retains no context: an error outlives the request
	// that produced it, so every renderer is handed a context by its caller.
	renderLimit int

	// MaxHeightLogical bounds CallFrame.HeightLogical, which accumulates
	// every frame elided by tail-call optimization.  Its unit is *elided
	// frames*, not loop turns: one iteration of a tail loop adds the length
	// of the elided terminal chain (2 for a trivial (defun f (n) (if ...
	// (f ...))), more for a body with deeper terminal nesting).  Because
	// the ratio depends on the shape of the loop body, this is a poor knob
	// for bounding iteration count — use MaxTailIterations for that.  Zero
	// disables the check (the default).
	MaxHeightLogical int

	// MaxHeightPhysical bounds the number of frames physically on the
	// stack.  This is the memory guard: exceeding the Go goroutine stack
	// aborts the process with an unrecoverable "stack overflow", so this
	// limit must stay comfortably below that threshold.  Frames elided by
	// tail-call optimization do not count toward it.  Zero disables the
	// check.
	MaxHeightPhysical int

	// MaxTailIterations bounds CallFrame.TailIterations — the number of
	// tail-call iterations performed at a single stack frame.  Unlike
	// MaxHeightLogical its unit is loop turns, independent of the shape of
	// the loop body, so an operator can reason about it directly.  It is a
	// runaway-loop backstop, not a business limit.  Zero disables the
	// check.
	MaxTailIterations int
}

// CallFrame is one frame in the CallStack
type CallFrame struct {
	Source  *token.Location
	FID     string
	Package string
	Name    string

	// HeightLogical is the frame's virtual depth: its physical depth plus
	// every frame elided beneath it by tail-call optimization.  It is a
	// diagnostic quantity — a tail loop runs in constant physical height
	// but its HeightLogical grows without bound.
	HeightLogical int

	Terminal bool
	TROBlock bool // Stop tail-recursion optimization from collapsing this frame

	// lispFun is true when the frame calls a function defined in Lisp: a
	// lambda, defun, defmacro, or flet or labels function.  The evaluator
	// sets it from the function value, not from its FID, and a tail call
	// reuses only a frame of the same kind (terminalCallFID).  The byte fits in
	// the padding after TROBlock, so the struct stays 72 bytes.
	// LispFunction reads it.
	lispFun bool

	// TailIterations counts tail-call iterations performed at this frame.
	// Each turn of a tail loop increments it by exactly one, regardless of
	// how many frames that turn elided.
	//
	// DELIBERATELY int32, AND DELIBERATELY PLACED AFTER THE BOOLS.  A
	// CallFrame is allocated per call, so its size is on the hot path: as
	// an `int` declared above Terminal/TROBlock this field grew the struct
	// from 72 to 80 bytes (+11.1%), which the benchmark gate caught as
	// +8.44% B/op on EnvFunCallRecursion with allocs/op unchanged — same
	// allocation count, larger allocations.  The three bools (Terminal,
	// TROBlock, lispFun) take 3 bytes of the last 8-byte word, and the 5
	// left over hold a 4-byte int32 but not an 8-byte int, so this
	// placement is free: the struct stays 72 bytes.
	//
	// int32 caps at ~2.1e9 iterations against a default budget of 1e6.  If
	// MaxTailIterations is ever raised near that ceiling, widen this field
	// and accept the 8 bytes rather than letting the counter wrap.
	TailIterations int32
}

// LispFunction reports whether the frame calls a function defined in Lisp
// (see CallStack.InLispFunction).  It returns false for a nil frame.
func (f *CallFrame) LispFunction() bool {
	return f != nil && f.lispFun
}

// QualifiedFunName returns the qualified name for the function on the top of
// the stack.  If ignore is non-empty QualifiedFunName returns unqualified
// names for functions in the given packages.
func (f *CallFrame) QualifiedFunName(ignore ...string) string {
	if f == nil {
		return ""
	}
	name := f.Name
	if f.Name == "" {
		name = f.FID
	}
	if f.Package == "" {
		return name
	}
	for _, pkg := range ignore {
		if pkg == f.Package {
			return name
		}
	}
	var buf bytes.Buffer
	buf.WriteString(f.Package)
	buf.WriteString(":")
	buf.WriteString(name)
	return buf.String()
}

func (f *CallFrame) String() string {
	if f.Source != nil {
		return fmt.Sprintf("%s: %s", f.Source, f.desc())
	}
	return f.desc()
}

func (f *CallFrame) desc() string {
	var mod bytes.Buffer
	if f.Terminal {
		mod.WriteString(" [terminal]")
	}
	if f.TROBlock {
		mod.WriteString(" [tro-blocked]")
	}
	name := f.FID
	if f.Name != "" {
		name = f.Name
	}
	if f.Package != "" {
		name = f.Package + ":" + name
	}
	return fmt.Sprintf("%s%s", name, mod.String())
}

// Copy creates a copy of the current stack so that it can be attach to a
// runtime error.
func (s *CallStack) Copy() *CallStack {
	frames := make([]CallFrame, len(s.Frames))
	copy(frames, s.Frames)
	return &CallStack{
		renderLimit:       s.renderLimit,
		wraps:             s.wraps,
		MaxHeightLogical:  s.MaxHeightLogical,
		MaxHeightPhysical: s.MaxHeightPhysical,
		MaxTailIterations: s.MaxTailIterations,
		Frames:            frames,
		GoStack:           s.GoStack,
	}
}

// Top returns the CallFrame at the top of the stack or nil if none exists.
func (s *CallStack) Top() *CallFrame {
	if s == nil || len(s.Frames) == 0 {
		return nil
	}
	return &s.Frames[len(s.Frames)-1]
}

// SourceSite names where the innermost call with a source position runs and
// the top-level form that reached it.  It returns "" when no frame has a
// source position, a nil stack included.  Otherwise it returns
//
//	inner <- reach
//
// where inner is the position of the innermost frame with a source position.
// reach is the position of the outermost frame with a source position inside
// the innermost source stream that encloses inner.  A source stream starts
// after a load-file, load-string or load-bytes frame below inner; with no
// such frame, the stream is the whole stack.  So reach is the top-level form,
// in the file being loaded, whose evaluation reached inner.  When inner and
// reach are the same position, SourceSite returns inner alone.
//
// A position is the frame's token.Location.String: "file:line:col",
// "file:line" when Col is 0, or "file[pos]" when Line is 0.  A frame has a
// source position when its Source is not nil, its Pos is not negative and
// its File is neither "" nor token.NativeFile.  A builtin that Go calls has
// token.NativeLocation, which has no source position.
//
// Limits:
//
//   - Only the load-file, load-string and load-bytes builtins start a source
//     stream.  A load that Go starts (LEnv.LoadFile, LoadString,
//     LoadProgramContext, an elpsutil.FileLoader) pushes no frame, so for
//     code it loads, reach is a form of the enclosing stream.
//   - A load frame counts only when its package is DefaultLangPackage
//     ("lisp"), the language package InitializeUserEnv sets.  A CallStack
//     does not know its runtime's Registry.Lang, so an embedder that names
//     another language package gets no stream boundaries.
//   - A position names the file as the reader named it, so two files with
//     the same name in different directories give the same position.
//
// SourceSite only reads the frames.  It takes no lock, so call it on the
// goroutine that runs the evaluation, for example from a builtin, or when no
// evaluation runs.
func (s *CallStack) SourceSite() string {
	if s == nil {
		return ""
	}
	frames := s.Frames
	innerIdx := -1
	inner := ""
	for i := len(frames) - 1; i >= 0; i-- {
		if pos := sourcePos(frames[i].Source); pos != "" {
			innerIdx, inner = i, pos
			break
		}
	}
	if innerIdx < 0 {
		return ""
	}
	// A load frame below inner is an ancestor of inner, so the last one is
	// the innermost load that encloses inner.  A load frame above inner does
	// not enclose it.
	start := 0
	for i := range innerIdx {
		if isSourceLoad(&frames[i]) {
			start = i + 1
		}
	}
	for i := start; i <= innerIdx; i++ {
		pos := sourcePos(frames[i].Source)
		if pos == "" {
			continue
		}
		if pos == inner {
			return inner
		}
		return inner + " <- " + pos
	}
	return inner
}

// isSourceLoad reports whether f is a call of a builtin of package
// DefaultLangPackage that starts a new source stream: load-file, load-string
// or load-bytes.
func isSourceLoad(f *CallFrame) bool {
	if f.Package != DefaultLangPackage {
		return false
	}
	return f.Name == "load-file" || f.Name == "load-string" || f.Name == "load-bytes"
}

// sourcePos returns loc.String(), or "" when loc has no source position: a
// nil loc, a negative Pos, or an empty or native File.
func sourcePos(loc *token.Location) string {
	if loc == nil || loc.Pos < 0 || loc.File == "" || loc.File == token.NativeFile {
		return ""
	}
	return loc.String()
}

// InLispFunction reports whether a function defined in Lisp is running: a
// frame of s is a call of a lambda, a defun, a defmacro expansion, or a flet
// or labels function.  The evaluator marks such a frame when it calls a
// function value that has no Go implementation.  A frame of a builtin, a
// special operator, a macro defined in Go, or a frame pushed with PushFID
// does not count.  So the top-level forms of a load, also of a load-file or
// load-string, run with no Lisp function on the stack unless a Lisp
// function started the load.
//
// The answer depends only on the frames.  A template VM starts with an
// empty stack, as a quiescent source has, so the same evaluation from an
// empty stack gives the same answer in a cold-loaded VM and in every VM of
// a template.  A tail call keeps at least one frame of the function it
// loops in.
func (s *CallStack) InLispFunction() bool {
	if s == nil {
		return false
	}
	for i := len(s.Frames) - 1; i >= 0; i-- {
		if s.Frames[i].lispFun {
			return true
		}
	}
	return false
}

// TerminalFID determines if a chain of terminal stack frames that ends with
// fid (i.e. fid is a candidate for tail-recursion optimization) and returns
// the number of frames in the shortest such chain.  If no such chain of
// terminal frames can be found then 0 is returned.
//
// If a stack frame with TROBlock is found then the search for a terminal chain
// is prematurely terminated as a failure.
//
// NOTE:  If tail-recursion optimization is working then the chain of calls
// found by TerminalFID is unique.
//
// TerminalFID matches a frame by FID alone.  The evaluator also matches the
// frame's kind, a Lisp function or not (see InLispFunction).
func (s *CallStack) TerminalFID(fid string) int {
	return s.terminalFID(fid, false, false)
}

// terminalCallFID is TerminalFID for the evaluator's call of a function of
// the given kind: it skips a frame with the FID whose kind differs.  A Go
// function may take the FID of a lambda (_fun<N>), so an FID alone does not
// tell the kinds apart, and a frame must not run a function of the other
// kind: its Lisp-function mark, name and package would describe the wrong
// function.  The search goes on below a skipped frame, which is terminal,
// so a loop of calls between the two kinds still runs in bounded height.
func (s *CallStack) terminalCallFID(fid string, lispFun bool) int {
	return s.terminalFID(fid, true, lispFun)
}

// terminalFID finds the chain for TerminalFID and terminalCallFID.  With
// matchKind, a frame matches only when its kind is lispFun.
func (s *CallStack) terminalFID(fid string, matchKind, lispFun bool) int {
	top := s.Top()
	if top == nil {
		return 0
	}
	for i := len(s.Frames) - 1; i >= 0; i-- {
		if !s.Frames[i].Terminal {
			// A non-terminal frame before finding a terminal fid frame.. no
			return 0
		}
		if s.Frames[i].TROBlock {
			// This stack frame contains critical state which cannot be
			// collapsed even if it is a terminal call.  This is not an
			// expected state and signals a problem with the implementation of
			// a builtin or the primary evaluation routine.
			_, _ = s.DebugPrint(os.Stderr)
			// It's unclear if a panic is the correct action here.  While the
			// existence of the TROBlock should prevent severe harm using the
			// standard language builtins if someone wrote
			// alternative/replacement builtins incorrectly their improper use
			// of teminal expressions may lead to incorrect evaluation, runtime
			// errors, or later panics.
			log.Panicf("tail-recursion-optimization is blocked -- inconsistent stack")
			return 0
		}
		if s.Frames[i].FID == fid && (!matchKind || s.Frames[i].lispFun == lispFun) {
			// We found the chain.  We want the returned length to be 1 when i
			// is on the first iteration.
			return len(s.Frames) - i
		}
	}
	return 0
}

// PushFID pushes a new stack frame with the given FID onto s.  The frame
// does not count as a call of a function defined in Lisp (InLispFunction).
func (s *CallStack) PushFID(src *token.Location, fid string, pkg string, name string) error {
	return s.pushFrame(src, fid, pkg, name, false)
}

// pushFrame is PushFID for the evaluator, which also marks a frame that
// calls a function defined in Lisp.
//
//nolint:revive // PushFID's four arguments plus the mark
func (s *CallStack) pushFrame(src *token.Location, fid string, pkg string, name string, lispFun bool) error {
	heff := 0
	if len(s.Frames) > 0 {
		heff = s.Top().HeightLogical + 1
	}
	err := s.checkHeightPush()
	if err != nil {
		return err
	}
	n := len(s.Frames)
	if n < cap(s.Frames) {
		s.Frames = s.Frames[:n+1]
	} else {
		s.Frames = append(s.Frames, CallFrame{})
	}
	// Write the frame field by field.  Assigning a whole CallFrame moves it
	// with a bulk write barrier, which costs every call while the garbage
	// collector marks; a field store takes the compiler's batched barrier.
	// Every field is written, so the slot need not be zero.
	f := &s.Frames[n]
	f.Source = src
	f.FID = fid
	f.Package = pkg
	f.Name = name
	f.HeightLogical = heff
	f.Terminal = false
	f.TROBlock = false
	f.lispFun = lispFun
	f.TailIterations = 0
	return nil
}

func (s *CallStack) checkHeightPush() error {
	err := s.checkHeightPhysical()
	if err != nil {
		return err
	}
	return s.CheckHeight()
}

// checkHeightPhysical performs an inclusive check against s.MaxHeightPhysical
// because it is assumed to be called preemptively, before a new frame is
// pushed onto the stack.  To account for this checkHeightPhysical adds one to
// the current physical height in any error produced.
func (s *CallStack) checkHeightPhysical() error {
	if s.MaxHeightPhysical <= 0 {
		return nil
	}
	if s.MaxHeightPhysical <= len(s.Frames) {
		return &PhysicalStackOverflowError{len(s.Frames) + 1}
	}
	return nil
}

func (s *CallStack) CheckHeight() error {
	if s.MaxHeightLogical <= 0 {
		return nil
	}
	if len(s.Frames) == 0 {
		return nil
	}
	if s.MaxHeightLogical < s.Top().HeightLogical {
		return &LogicalStackOverflowError{s.Top().HeightLogical}
	}
	return nil
}

// CheckTailIterations returns an error if the top frame has performed more
// tail-call iterations than MaxTailIterations allows.
func (s *CallStack) CheckTailIterations() error {
	if s.MaxTailIterations <= 0 {
		return nil
	}
	if len(s.Frames) == 0 {
		return nil
	}
	// Widen for the comparison rather than narrowing MaxTailIterations: an
	// int32 counter against an int limit must not truncate the limit.
	if int64(s.MaxTailIterations) < int64(s.Top().TailIterations) {
		return &TailIterationLimitError{int(s.Top().TailIterations)}
	}
	return nil
}

// CheckTailCall runs the limit checks that apply after a tail-call
// optimization elides frames: the (opt-in) logical height bound and the
// tail-iteration bound.  Physical height is unchanged by a tail call, so it
// is deliberately not rechecked here.
func (s *CallStack) CheckTailCall() error {
	if err := s.CheckHeight(); err != nil {
		return err
	}
	return s.CheckTailIterations()
}

// Pop removes the top CallFrame from the stack and returns it.  If the stack
// is empty Pop returns nil.
func (s *CallStack) Pop() CallFrame {
	if len(s.Frames) < 1 {
		// NOT LISP-REACHABLE (#367): all three callers (funCall, MacroCall,
		// SpecialOpCall) install the Pop as a `defer` on the line after a
		// PushFID that returned no error, so pushes and pops are balanced by
		// construction along every exit path, panics included.  Tail-call
		// elision does not pop frames -- it decrements a counter on the mark
		// and reuses the frame already pushed -- so it cannot unbalance them
		// either.  An empty stack here means a push/pop pair was separated.
		panic("pop called on an empty stack")
	}
	top := &s.Frames[len(s.Frames)-1]
	f := *top
	// Clear the pointers field by field, so that the slot keeps nothing
	// alive, for the reason PushFID writes its fields one by one.  PushFID
	// writes every field, so the scalars may stay.
	top.Source = nil
	top.FID = ""
	top.Package = ""
	top.Name = ""
	s.Frames = s.Frames[:len(s.Frames)-1]
	return f
}

// DebugPrint prints s under its captured output limit, or the default output
// limit for a live stack, with no cancellation. Exhaustion emits #<truncated>.
// A caller with a request context in hand renders the stack through the
// error's WriteTraceContext instead.
func (s *CallStack) DebugPrint(w io.Writer) (int, error) {
	limit := s.renderLimit
	if limit <= 0 {
		limit = DefaultMaxAlloc
	}
	return s.debugPrintContext(nil, w, limit) //nolint:staticcheck // a stack carries no context; nil disables cancellation checks
}

func (s *CallStack) debugPrintContext(ctx context.Context, w io.Writer, limit int) (int, error) {
	r := valueRenderer{limit: limit, budget: newRenderBudget(limit, ctx)}
	r.stack(s)
	return writeDiagnostic(ctx, w, r.diagnosticText(), limit)
}

type LogicalStackOverflowError struct {
	Height int
}

func (e *LogicalStackOverflowError) Error() string {
	return fmt.Sprintf("logical stack height exceeded maximum: %v"+
		" (logical height counts frames elided by tail-call optimization, so a long"+
		" tail-recursive loop reaches it without using stack space; raise or disable"+
		" the limit with WithMaximumLogicalStackHeight)", e.Height)
}

type PhysicalStackOverflowError struct {
	Height int
}

func (e *PhysicalStackOverflowError) Error() string {
	return fmt.Sprintf("physical stack height exceeded maximum: %v"+
		" (recursion too deep; the limit stops the Go runtime from aborting the"+
		" process with an unrecoverable stack overflow — rewrite the recursion as a"+
		" tail call, or raise the limit with WithMaximumPhysicalStackHeight)", e.Height)
}

// TailIterationLimitError reports that a single tail-recursive loop performed
// more iterations than the runtime allows.  It is a runaway-loop backstop, not
// a stack-depth problem: tail calls run in constant stack space.
type TailIterationLimitError struct {
	Iterations int
}

func (e *TailIterationLimitError) Error() string {
	return fmt.Sprintf("tail-call iteration limit exceeded: %v"+
		" (a single tail-recursive loop ran this many turns; this is a runaway-loop"+
		" backstop, not a stack overflow — raise or disable the limit with"+
		" WithMaxTailIterations)", e.Iterations)
}

// holdNonTerminal clears the top frame's terminal mark while Go code calls
// Lisp, and returns what restoreTerminal needs to put it back.  funcall and
// apply mark the top frame terminal so a callee's tail call can reuse it, but
// a Go frame cannot be unwound, so a call made from Go must find the top frame
// non-terminal.  It returns the frame's index, not a pointer: a push while
// the callee runs can move the frames (luthersystems/elps#852).  top is -1
// for a nil or empty stack.
func (s *CallStack) holdNonTerminal() (int, bool) {
	if s == nil || len(s.Frames) == 0 {
		return -1, false
	}
	top := len(s.Frames) - 1
	terminal := s.Frames[top].Terminal
	s.Frames[top].Terminal = false
	return top, terminal
}

// restoreTerminal puts back the mark holdNonTerminal cleared on frame top.
func (s *CallStack) restoreTerminal(top int, terminal bool) {
	if top >= 0 && top < len(s.Frames) {
		s.Frames[top].Terminal = terminal
	}
}
