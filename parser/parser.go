// Copyright © 2018 The ELPS authors

package parser

import (
	"io"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// ReaderOption configures a Reader created by NewReader.
type ReaderOption func(*readerConfig)

type readerConfig struct {
	preserveFormat bool
}

// WithFormatPreserving enables format-preserving mode. When enabled,
// the parser populates LVal.Meta with SourceMeta containing comments,
// bracket types, blank lines, and original literal text.
func WithFormatPreserving() ReaderOption {
	return func(c *readerConfig) { c.preserveFormat = true }
}

// NewReader returns a new lisp.Reader. With no options, returns a
// standard reader. Pass WithFormatPreserving() for tooling use.
func NewReader(opts ...ReaderOption) lisp.Reader {
	var cfg readerConfig
	for _, opt := range opts {
		opt(&cfg)
	}
	if cfg.preserveFormat {
		return &formattingReader{}
	}
	return rdparser.NewReader()
}

type formattingReader struct{}

func (*formattingReader) Read(name string, r io.Reader) ([]*lisp.LVal, error) {
	s := token.NewScanner(name, r)
	p := rdparser.NewFormatting(s)
	return p.ParseProgram()
}

func (*formattingReader) ReadLocation(name string, loc string, r io.Reader) ([]*lisp.LVal, error) {
	s := token.NewScanner(name, r)
	s.SetPath(loc)
	p := rdparser.NewFormatting(s)
	return p.ParseProgram()
}
