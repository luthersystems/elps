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
type CallStack struct {
	Frames            []CallFrame
	MaxHeightLogical  int
	MaxHeightPhysical int
}

// CallFrame is one frame in the CallStack
type CallFrame struct {
	Source        *token.Location
	FID           string
	Package       string
	Name          string
	HeightLogical int
	Terminal      bool
	TROBlock      bool // Stop tail-recursion optimization from collapsing this frame
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
		MaxHeightLogical: s.MaxHeightLogical,
		Frames:           frames,
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
	return fmt.Sprintf("logical stack height exceeded maximum: %v", e.Height)
}

type PhysicalStackOverflowError struct {
	Height int
}

func (e *PhysicalStackOverflowError) Error() string {
	return fmt.Sprintf("physical stack height exceeded maximum: %v", e.Height)
}
