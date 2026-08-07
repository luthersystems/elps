// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"

	"github.com/luthersystems/elps/parser/token"
)

// CallStack is a function call stack.
//
// For errors produced by env.eval's recover() of a Go panic, GoStack carries
// the runtime.Stack output captured at the panic site so callers (via
// ErrorVal.WriteTrace or direct access) can render the Go-level origin
// alongside the ELPS frames. It is nil for non-panic errors.
type CallStack struct {
	Frames []CallFrame

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

	GoStack []byte
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

	// TailIterations counts tail-call iterations performed at this frame.
	// Each turn of a tail loop increments it by exactly one, regardless of
	// how many frames that turn elided.
	//
	// DELIBERATELY int32, AND DELIBERATELY PLACED AFTER THE BOOLS.  A
	// CallFrame is allocated per call, so its size is on the hot path: as
	// an `int` declared above Terminal/TROBlock this field grew the struct
	// from 72 to 80 bytes (+11.1%), which the benchmark gate caught as
	// +8.44% B/op on EnvFunCallRecursion with allocs/op unchanged — same
	// allocation count, larger allocations.  Two bools leave 6 bytes of
	// tail padding that an 8-byte int cannot use but a 4-byte int32 can, so
	// this placement is free: the struct stays 72 bytes.
	//
	// int32 caps at ~2.1e9 iterations against a default budget of 1e6.  If
	// MaxTailIterations is ever raised near that ceiling, widen this field
	// and accept the 8 bytes rather than letting the counter wrap.
	TailIterations int32
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
func (s *CallStack) TerminalFID(fid string) int {
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
		if s.Frames[i].FID == fid {
			// We found the chain.  We want the returned length to be 1 when i
			// is on the first iteration.
			return len(s.Frames) - i
		}
	}
	return 0
}

// PushFID pushes a new stack frame with the given FID onto s.
func (s *CallStack) PushFID(src *token.Location, fid string, pkg string, name string) error {
	heff := 0
	if len(s.Frames) > 0 {
		heff = s.Top().HeightLogical + 1
	}
	err := s.checkHeightPush()
	if err != nil {
		return err
	}
	s.Frames = append(s.Frames, CallFrame{
		Source:        src,
		FID:           fid,
		Package:       pkg,
		Name:          name,
		HeightLogical: heff,
	})
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
		panic("pop called on an empty stack")
	}
	f := s.Frames[len(s.Frames)-1]
	s.Frames[len(s.Frames)-1] = CallFrame{}
	s.Frames = s.Frames[:len(s.Frames)-1]
	return f
}

// DebugPrint prints s
func (s *CallStack) DebugPrint(w io.Writer) (int, error) {
	n, err := fmt.Fprintf(w, "Stack Trace [%d frames -- entrypoint last]:\n", len(s.Frames))
	if err != nil {
		return n, err
	}
	indent := "  "
	for i := len(s.Frames) - 1; i >= 0; i-- {
		fstr := s.Frames[i].String()
		_n, err := fmt.Fprintf(w, "%sheight %d: %s\n", indent, i, fstr)
		n += _n
		if err != nil {
			return n, err
		}
	}
	return n, nil
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
