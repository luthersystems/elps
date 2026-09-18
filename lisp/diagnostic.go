// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"io"
	"strconv"

	"github.com/luthersystems/elps/diagnostic"
	"github.com/luthersystems/elps/parser/token"
)

// WriteDiagnosticContext renders the complete CLI/REPL diagnostic under the
// originating runtime's byte/work cap and request context. Prefixes, messages,
// source snippets, stack notes, hints and writes all share one session. Hint
// fields are passed separately to avoid allocating concatenated strings.
// A nil context, or one that is already cancelled, disables cancellation
// checks so the whole diagnostic still renders under the byte cap.
func (e *ErrorVal) WriteDiagnosticContext(ctx context.Context, w io.Writer, renderer *diagnostic.Renderer, hint ...string) (int, error) {
	limit, ctx := e.renderPolicy(ctx)
	limit = diagnosticLimit(limit)
	s := renderer.NewSession(ctx, w, limit)
	s.Header(func() {
		if e == nil {
			s.Text(nilErrorMessage)
			return
		}
		if e.Str != "" && e.Str != "error" {
			s.Text(e.Str, ": ")
		}
		stack := (*LVal)(e).CallStack()
		if stack != nil && diagnosticFrameNamed(stack.Top()) {
			diagnosticFrameName(s, stack.Top())
			s.Text(": ")
		}
		if !s.Step() {
			return
		}
		budget := newRenderBudget(limit, ctx)
		budget.stepFn = s.Step
		message, ok := (*LVal)(e).boundedRender(s.Remaining(), &budget, true)
		if ok {
			s.Text(message)
		} else {
			s.Stop()
		}
	})
	if e != nil && s.Step() {
		if loc, ok := e.Source(); ok && loc.Pos >= 0 {
			file := loc.File
			if loc.Path != "" {
				file = loc.Path
			}
			s.Span(diagnostic.Span{File: file, Line: loc.Line, Col: loc.Col})
		}
		if stack := (*LVal)(e).CallStack(); stack != nil {
			for i := len(stack.Frames) - 1; i >= 0 && s.Step(); i-- {
				frame := &stack.Frames[i]
				if !diagnosticFrameNamed(frame) {
					continue
				}
				s.Note(func() {
					s.Text("in ")
					diagnosticFrameName(s, frame)
					s.Text(" at ")
					diagnosticLocation(s, frame.Source)
				})
			}
		}
	}
	if len(hint) > 0 && s.Step() {
		s.Note(func() { s.Text(hint...) })
	}
	return s.Finish()
}

func diagnosticFrameNamed(f *CallFrame) bool {
	return f != nil && (f.Name != "" || f.FID != "" || (f.Package != "" && f.Package != DefaultUserPackage))
}

func diagnosticFrameName(s *diagnostic.Session, f *CallFrame) {
	if f.Package != "" && f.Package != DefaultUserPackage {
		s.Text(f.Package, ":")
	}
	name := f.Name
	if name == "" {
		name = f.FID
	}
	s.Text(name)
}

func diagnosticLocation(s *diagnostic.Session, loc *token.Location) {
	if loc == nil {
		s.Text("unknown")
		return
	}
	s.Text(loc.File)
	if loc.Pos < 0 {
		return
	}
	if loc.Line == 0 {
		s.Text("[", strconv.Itoa(loc.Pos), "]")
	} else {
		s.Text(":", strconv.Itoa(loc.Line))
		if loc.Col != 0 {
			s.Text(":", strconv.Itoa(loc.Col))
		}
	}
}
