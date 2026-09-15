// Copyright © 2026 The ELPS authors

package diagnostic

import (
	"context"
	"io"
)

const truncationMarker = "#<truncated>"

// Session streams one complete diagnostic under a shared byte/work budget.
// Callers must check Step before constructing fields and use Remaining before
// allocating variable-sized text. Text writes existing fields in bounded chunks.
// Finish emits a fitting marker on exhaustion, including cancellation by a writer.
// Like io.Writer, a session cannot interrupt a Write already in progress.
type Session struct {
	ctx       context.Context
	w         io.Writer
	renderer  *Renderer
	palette   palette
	remaining int
	work      int
	written   int
	stopped   bool
	err       error
}

// NewSession reserves space for a truncation marker within limit bytes. The
// request context and work budget cover construction, source reads and writing.
func (r *Renderer) NewSession(ctx context.Context, w io.Writer, limit int) *Session {
	return &Session{ctx: ctx, w: w, renderer: r, palette: choosePalette(r.Color, fileFromWriter(w)),
		remaining: max(0, limit), work: 4 * max(1<<20, min(limit, int(^uint(0)>>1)/4))}
}

// Remaining is the space available for content, excluding the reserved marker.
func (s *Session) Remaining() int { return max(0, s.remaining-len(truncationMarker)) }

// Step charges one unit of work and checks cancellation. All producers share it.
func (s *Session) Step() bool {
	if s.stopped || s.err != nil {
		return false
	}
	if s.work <= 0 || (s.ctx != nil && s.ctx.Err() != nil) {
		s.stopped = true
		return false
	}
	s.work--
	return true
}

// Stop records that the complete diagnostic could not be produced.
func (s *Session) Stop() { s.stopped = true }

// Text writes fields separately so concatenation cannot allocate beyond the cap.
func (s *Session) Text(parts ...string) {
	for _, part := range parts {
		if !s.Step() {
			return
		}
		for len(part) > 0 && s.Step() {
			n := min(len(part), 4096, s.Remaining())
			if n == 0 {
				s.Stop()
				return
			}
			s.write(part[:n])
			part = part[n:]
		}
	}
}

func (s *Session) write(text string) {
	n, err := io.WriteString(s.w, text)
	s.remaining -= n
	s.written += n
	s.err = err
	if err == nil && n != len(text) {
		s.err = io.ErrShortWrite
	}
}

// Write implements io.Writer for the renderer's fixed-size formatting pieces.
func (s *Session) Write(p []byte) (int, error) {
	size := len(p)
	before := s.written
	for len(p) > 0 && s.Step() {
		n := min(len(p), 4096, s.Remaining())
		if n == 0 {
			s.Stop()
			break
		}
		s.Text(string(p[:n]))
		p = p[n:]
	}
	if s.err != nil {
		return s.written - before, s.err
	}
	if s.stopped {
		return s.written - before, io.ErrShortWrite
	}
	return size, nil
}

// Header writes an error header, invoking message only while work remains.
func (s *Session) Header(message func()) {
	p := s.palette
	s.Text(p.boldRed, p.bold, "error", p.reset, ":", p.reset, " ", p.bold)
	if s.Step() {
		message()
	}
	s.Text(p.reset, "\n")
}

// Note streams a note without building an intermediate concatenated string.
func (s *Session) Note(note func()) {
	s.Text("   ", s.palette.boldCyan, "=", s.palette.reset, " note: ")
	if s.Step() {
		note()
	}
	s.Text("\n")
}

// Span displays a source location and snippet under this session's budget.
func (s *Session) Span(span Span) {
	if !s.Step() {
		return
	}
	ew := &errWriter{w: s, session: s}
	s.renderer.writeSpan(ew, span, s.palette)
}

// Finish completes the report, returning the actual bytes written and I/O error.
func (s *Session) Finish() (int, error) {
	s.Step()
	if s.stopped && s.err == nil {
		s.write(truncationMarker[:min(len(truncationMarker), max(0, s.remaining))])
	}
	return s.written, s.err
}
