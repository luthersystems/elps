// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"io"
	"strconv"
	"strings"
)

// Check again between output lines: the writer itself may cancel the request.
// Even then, the marker must fit the bytes that have not yet been written.
func writeDiagnostic(ctx context.Context, w io.Writer, s string, limit int) (int, error) {
	n := 0
	for len(s) > 0 {
		cancelled := ctx != nil && ctx.Err() != nil
		if cancelled {
			s = renderTruncatedMark[:min(len(renderTruncatedMark), max(0, limit-n))]
		}
		chunk := s[:min(len(s), 4096)]
		if i := strings.IndexByte(chunk, '\n'); i >= 0 {
			chunk = chunk[:i+1]
		}
		wrote, err := io.WriteString(w, chunk)
		n += wrote
		if err != nil {
			return n, err
		}
		if wrote != len(chunk) {
			return n, io.ErrShortWrite
		}
		if cancelled {
			return n, nil
		}
		s = s[len(chunk):]
	}
	return n, nil
}

func (r *valueRenderer) diagnosticText() string {
	if r.full {
		return truncatedRender(r.out.String(), r.limit)
	}
	return r.out.String()
}

func (r *valueRenderer) traceText(s string) {
	for len(s) > 0 && !r.full {
		n := min(len(s), 4096)
		r.text(s[:n])
		s = s[n:]
	}
}

// Render fields separately: frame names and source filenames can be much
// larger than the remaining budget, so CallFrame.String is not safe here.
func (r *valueRenderer) stack(s *CallStack) {
	r.text("Stack Trace [")
	r.text(strconv.Itoa(len(s.Frames)))
	r.text(" frames -- entrypoint last]:\n")
	for i := len(s.Frames) - 1; i >= 0 && !r.full; i-- {
		if !r.budget.step() {
			r.full = true
			return
		}
		f := &s.Frames[i]
		r.text("  height ")
		r.text(strconv.Itoa(i))
		r.text(": ")
		if loc := f.Source; loc != nil {
			r.traceText(loc.File)
			if loc.Pos >= 0 {
				if loc.Line == 0 {
					r.text("[")
					r.text(strconv.Itoa(loc.Pos))
					r.text("]")
				} else {
					r.text(":")
					r.text(strconv.Itoa(loc.Line))
					if loc.Col != 0 {
						r.text(":")
						r.text(strconv.Itoa(loc.Col))
					}
				}
			}
			r.text(": ")
		}
		if f.Package != "" {
			r.traceText(f.Package)
			r.text(":")
		}
		if f.Name != "" {
			r.traceText(f.Name)
		} else {
			r.traceText(f.FID)
		}
		if f.Terminal {
			r.text(" [terminal]")
		}
		if f.TROBlock {
			r.text(" [tro-blocked]")
		}
		r.text("\n")
	}
}
