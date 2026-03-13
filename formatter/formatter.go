// Copyright © 2024 The ELPS authors

// Package formatter provides source code formatting for ELPS lisp files.
// It uses the ELPS parser in format-preserving mode to parse source code
// into an annotated AST, then walks the tree to produce formatted output.
package formatter

import (
	"bytes"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// Format formats ELPS source code. If cfg is nil, DefaultConfig() is used.
func Format(source []byte, cfg *Config) ([]byte, error) {
	if cfg == nil {
		cfg = DefaultConfig()
	}

	name := "<stdin>"
	s := token.NewScanner(name, bytes.NewReader(source))
	p := rdparser.NewFormatting(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil, err
	}

	// Collect any trailing comments after the last expression
	trailingComments := p.PendingComments()

	pr := newPrinter(cfg)
	if cfg.Compact {
		pr.writeTopLevelCompact(exprs, trailingComments)
	} else {
		pr.writeTopLevel(exprs, trailingComments)
	}

	result := pr.buf.String()

	// Ensure exactly one trailing newline (if there's any content)
	if len(result) > 0 {
		result = strings.TrimRight(result, "\n") + "\n"
	}

	return []byte(result), nil
}

// FormatFile formats ELPS source code, using filename for error messages.
func FormatFile(source []byte, filename string, cfg *Config) ([]byte, error) {
	if cfg == nil {
		cfg = DefaultConfig()
	}

	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.NewFormatting(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil, err
	}

	trailingComments := p.PendingComments()

	pr := newPrinter(cfg)
	if cfg.Compact {
		pr.writeTopLevelCompact(exprs, trailingComments)
	} else {
		pr.writeTopLevel(exprs, trailingComments)
	}

	result := pr.buf.String()

	if len(result) > 0 {
		result = strings.TrimRight(result, "\n") + "\n"
	}

	return []byte(result), nil
}

// FormatProgram formats an already-parsed ELPS program plus any trailing
// comments collected after the last expression.
func FormatProgram(exprs []*lisp.LVal, trailingComments []*token.Token, cfg *Config) []byte {
	if cfg == nil {
		cfg = DefaultConfig()
	}

	pr := newPrinter(cfg)
	if cfg.Compact {
		pr.writeTopLevelCompact(exprs, trailingComments)
	} else {
		pr.writeTopLevel(exprs, trailingComments)
	}

	result := pr.buf.String()
	if len(result) > 0 {
		result = strings.TrimRight(result, "\n") + "\n"
	}
	return []byte(result)
}
