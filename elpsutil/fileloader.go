// Copyright © 2026 The ELPS authors

package elpsutil

import (
	"bytes"
	"fmt"
	"io"
	"runtime/debug"
	"sync"
	"sync/atomic"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// FileLoader loads constant source, such as a //go:embed file, into each
// environment it is given.  It parses the source once, at the first Load,
// into a sealed lisp.Program, and evaluates that one Program in every
// environment.  Hold a FileLoader at package scope so that every environment
// in the process shares the parse.
//
// Each environment gets its own bindings.  The program's literals are sealed
// and shared, so Lisp code that mutates one raises modify-literal-error; copy
// a literal before mutating it.  Go code must not write the exported fields
// of the program's values.
//
// FileLoader parses with its own parser.NewReader, not with an environment's
// Runtime.Reader, because every environment shares the one parse.  A reader
// that the embedder sets or wraps on a runtime does not see these sources.
//
// A FileLoader is safe for concurrent use.
type FileLoader struct {
	err    error
	loader lisp.Loader
	// newLoader builds the Loader from the source.  It is programLoader
	// except in the test of the recover in Load: no input makes the parser
	// panic, so the test injects a parser that does.
	newLoader func(lisp.Reader, string, io.Reader) (lisp.Loader, error)
	filename  string
	bytecode  []byte
	// parses counts completed parses, so a test can assert that the source
	// is parsed once.  Load writes it under once and ParseCount reads it
	// without once, so it is atomic.
	parses atomic.Uint64
	once   sync.Once
}

// NewFileLoader returns a FileLoader for the source bytecode.  Errors and
// stack traces name the source filename.  NewFileLoader does not copy
// bytecode, so do not modify it afterwards.  NewFileLoader parses nothing;
// the first Load parses the source.
func NewFileLoader(bytecode []byte, filename string) *FileLoader {
	return &FileLoader{
		bytecode:  bytecode,
		filename:  filename,
		newLoader: programLoader,
	}
}

// programLoader parses stream once into a sealed lisp.Program and returns a
// Loader that evaluates that Program in each environment it is given, with
// the environment's context.  It reads with Reader.Read, so a token's
// location names only the stream name.  The source has no filesystem
// location, so a relative load-file in it does not resolve against one.
func programLoader(r lisp.Reader, name string, stream io.Reader) (lisp.Loader, error) {
	prog, err := lisp.ReadProgram(r, name, stream)
	if err != nil {
		return nil, err
	}
	return func(env *lisp.LEnv) *lisp.LVal {
		return env.LoadProgramContext(env.Context(), prog)
	}, nil
}

// Load implements Loader.  It evaluates the parsed source in env and returns
// the value of the last expression, or the first error.
//
// When the source does not parse, Load returns an error that names the
// source.  It returns the same error on every call, not only on the first,
// and does not parse again.  The source is a constant, so a parse failure is
// a defect in the program that embeds it.
//
// Load also recovers a panic out of the parser and returns it as the parse
// error, with the Go stack of the panic.  sync.Once counts a call that
// panics as done, so without the recover every later Load would dereference
// a nil Loader and the original panic would be lost.
func (l *FileLoader) Load(env *lisp.LEnv) *lisp.LVal {
	l.once.Do(l.parse)
	if l.err != nil {
		return env.Error(l.err)
	}
	return l.loader(env)
}

// parse builds l.loader, or sets l.err.
func (l *FileLoader) parse() {
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		l.loader = nil
		if err, ok := r.(error); ok {
			l.err = fmt.Errorf("parsing %s: parser panic: %w\n%s", l.filename, err, debug.Stack())
		} else {
			l.err = fmt.Errorf("parsing %s: parser panic: %v\n%s", l.filename, r, debug.Stack())
		}
	}()
	loader, err := l.newLoader(parser.NewReader(), l.filename, bytes.NewReader(l.bytecode))
	if err != nil {
		l.err = fmt.Errorf("parsing %s: %w", l.filename, err)
		return
	}
	l.loader = loader
	l.parses.Add(1)
}

// ParseCount returns the number of times the loader has parsed its source:
// 0 before the first Load and 1 after a Load that parsed it.  A source that
// fails to parse leaves it at 0.
func (l *FileLoader) ParseCount() uint64 {
	return l.parses.Load()
}
