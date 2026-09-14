// Copyright © 2026 The ELPS authors

// Package diagnosticsource supplies evaluated inline text and library-confined
// file contents to the CLI and REPL diagnostic renderers.
package diagnosticsource

import (
	"bytes"
	"io"
	"os"

	"github.com/luthersystems/elps/lisp"
)

const inlineByteBudget = 4 << 20

// Reader retains up to 4 MiB of inline sources by display label, evicting oldest
// entries first. Physical sources continue through the underlying reader and are
// retrieved through the active library.
// Like the runtime reader it wraps, it is used by one evaluation at a time.
type Reader struct {
	lisp.Reader
	inline      map[string][]byte
	order       []string
	inlineBytes int
}

// NewReader wraps the runtime's parser to retain load-string/load-bytes text.
func NewReader(reader lisp.Reader) *Reader {
	return &Reader{Reader: reader, inline: make(map[string][]byte)}
}

// Read records the bytes actually parsed, never treating name as a file path.
func (r *Reader) Read(name string, input io.Reader) ([]*lisp.LVal, error) {
	source, err := io.ReadAll(input)
	if err != nil {
		return nil, err
	}
	r.retain(name, source)
	return r.Reader.Read(name, bytes.NewReader(source))
}

// retain refreshes a label's position and drops sources too large for the cache.
func (r *Reader) retain(name string, source []byte) {
	if previous, ok := r.inline[name]; ok {
		r.inlineBytes -= len(previous)
		delete(r.inline, name)
		for i, label := range r.order {
			if label == name {
				copy(r.order[i:], r.order[i+1:])
				r.order[len(r.order)-1] = ""
				r.order = r.order[:len(r.order)-1]
				break
			}
		}
	}
	// Empty inputs have no snippet and must not accumulate cache entries.
	if len(source) == 0 || len(source) > inlineByteBudget {
		return
	}
	for r.inlineBytes+len(source) > inlineByteBudget {
		oldest := r.order[0]
		r.inlineBytes -= len(r.inline[oldest])
		delete(r.inline, oldest)
		r.order[0] = ""
		r.order = r.order[1:]
	}
	// io.ReadAll may allocate excess capacity; retain only the budgeted bytes.
	retained := make([]byte, len(source))
	copy(retained, source)
	r.inline[name] = retained
	r.order = append(r.order, name)
	r.inlineBytes += len(retained)
}

// ReadLocation preserves physical source locations without caching file contents.
func (r *Reader) ReadLocation(name, loc string, input io.Reader) ([]*lisp.LVal, error) {
	if reader, ok := r.Reader.(lisp.LocationReader); ok {
		return reader.ReadLocation(name, loc, input)
	}
	return r.Reader.Read(loc, input)
}

// SourceReader returns a diagnostic source callback using the active runtime.
// Inline labels prefer retained text; all file reads go through the library.
// A missing or refusing library never falls back to the host filesystem.
func SourceReader(runtime *lisp.Runtime, lerr *lisp.LVal) func(string) ([]byte, error) {
	return func(file string) ([]byte, error) {
		if loc, ok := lerr.Source(); ok && loc.Path == "" && loc.File == file {
			if reader, ok := runtime.Reader.(*Reader); ok {
				if source, ok := reader.inline[file]; ok {
					return source, nil
				}
			}
			return nil, os.ErrNotExist
		}
		if runtime.Library == nil {
			return nil, os.ErrNotExist
		}
		_, _, source, err := runtime.Library.LoadSource(lisp.NewSourceContext("", ""), file)
		return source, err
	}
}
