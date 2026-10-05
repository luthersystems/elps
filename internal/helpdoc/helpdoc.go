// Copyright © 2026 The ELPS authors

// Package helpdoc formats the text that lisp:help and the help package print.
//
// It is the single definition of that format.  Core lisp:help (package lisp)
// and the help package's registry functions (lisp/lisplib/libhelp) both write
// through it, and neither can import the other's rendering: libhelp imports
// lisp, so lisp cannot import libhelp.  Everything here therefore works on
// text the caller has already rendered, and nothing here imports lisp.
package helpdoc

import (
	"fmt"
	"io"
	"strings"

	"github.com/muesli/reflow/indent"
	"github.com/muesli/reflow/wordwrap"
)

// ValueDoc holds a value's name, representation and documentation.
type ValueDoc struct {
	// TypeName is the value type name.
	TypeName string
	// Name is the display name.
	Name string
	// Rendered is the rendered value.
	Rendered string
	// Doc contains the symbol documentation.
	Doc string
}

// WriteVal writes the entry for a variable: its type name, its display name
// and its rendered value on one line, then its cleaned doc, if any.
func WriteVal(w io.Writer, opts ValueDoc) error {
	typeName, name, rendered, doc := opts.TypeName, opts.Name, opts.Rendered, opts.Doc

	_, err := fmt.Fprintf(w, "%v %s %v\n", typeName, name, rendered)
	if err != nil {
		return err
	}
	if doc != "" {
		_, err = fmt.Fprintln(w, CleanDocstring(doc))
	}
	return err
}

// FunctionDoc holds a function's signature and documentation.
type FunctionDoc struct {
	// FunType is the function kind.
	FunType string
	// Signature is the rendered function signature.
	Signature string
	// Docstring contains the function documentation.
	Docstring string
	// SymbolDoc contains binding documentation.
	SymbolDoc string
}

// WriteFun writes the entry for a function: its kind and rendered signature
// on one line, then its docstring, or symbolDoc when the function carries
// no docstring of its own.
func WriteFun(w io.Writer, opts FunctionDoc) error {
	funType, signature, docstring, symbolDoc := opts.FunType, opts.Signature, opts.Docstring, opts.SymbolDoc

	_, err := fmt.Fprintf(w, "%s ", funType)
	if err != nil {
		return fmt.Errorf("rendering function type: %w", err)
	}
	_, err = fmt.Fprintln(w, signature)
	if err != nil {
		return fmt.Errorf("rendering signature: %w", err)
	}
	doc := CleanDocstring(docstring)
	if doc == "" {
		doc = CleanDocstring(symbolDoc)
	}
	if doc != "" {
		_, err = fmt.Fprintln(w, doc)
		return err
	}
	return nil
}

// CleanDocstring dedents doc, wraps it at 72 columns and indents it two
// spaces, the layout help prints.
func CleanDocstring(doc string) string {
	if doc == "" {
		return ""
	}
	if doc[0] == '\n' {
		doc = doc[1:]
	}
	doc = indent.String(wordwrap.String(DedentDoc(doc), 72), 2)
	doc = strings.TrimSuffix(doc, "\n")
	return doc
}

// CleanDocRaw dedents a docstring without word-wrapping.  JSON consumers get
// clean text they can format themselves.
func CleanDocRaw(doc string) string {
	if doc == "" {
		return ""
	}
	if doc[0] == '\n' {
		doc = doc[1:]
	}
	doc = DedentDoc(doc)
	doc = strings.TrimSpace(doc)
	return doc
}

// DedentDoc removes common leading whitespace from all non-empty lines.
// It handles Go raw string literals where the first line may have less
// indentation than continuation lines (which inherit the source code's
// tab indentation). Tabs are normalized to spaces before processing.
func DedentDoc(s string) string {
	s = strings.ReplaceAll(s, "\t", "    ")
	lines := strings.Split(s, "\n")

	// Find minimum leading spaces across non-empty lines, skipping
	// the first line (which in raw strings often has no indentation).
	minWS := -1
	start := 0
	if len(lines) > 1 {
		start = 1
	}
	for _, line := range lines[start:] {
		trimmed := strings.TrimLeft(line, " ")
		if trimmed == "" {
			continue
		}
		ws := len(line) - len(trimmed)
		if minWS < 0 || ws < minWS {
			minWS = ws
		}
	}
	if minWS <= 0 {
		return strings.TrimLeft(lines[0], " ") + "\n" + strings.Join(lines[1:], "\n")
	}

	lines[0] = strings.TrimLeft(lines[0], " ")
	for i := 1; i < len(lines); i++ {
		if strings.TrimSpace(lines[i]) == "" {
			lines[i] = ""
		} else if len(lines[i]) >= minWS {
			lines[i] = lines[i][minWS:]
		}
	}
	return strings.Join(lines, "\n")
}
