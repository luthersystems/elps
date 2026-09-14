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

// Reader retains inline sources by display label. Physical sources continue
// through the underlying reader and are retrieved through the active library.
// Like the runtime reader it wraps, it is used by one evaluation at a time.
type Reader struct {
	lisp.Reader
	inline map[string][]byte
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
	r.inline[name] = source
	return r.Reader.Read(name, bytes.NewReader(source))
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
		}
		if runtime.Library == nil {
			return nil, os.ErrNotExist
		}
		_, _, source, err := runtime.Library.LoadSource(lisp.NewSourceContext("", ""), file)
		return source, err
	}
}
