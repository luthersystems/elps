// Copyright © 2026 The ELPS authors

package elpsutil

import (
	"fmt"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Template is a quasiquote-style form, read once by the elps reader and
// expanded by Go macros into fresh syntax.  It is a lisp.FormTemplate read
// from source; see lisp.FormTemplate for the placeholders, the forms a
// template may hold and what Expand builds.
type Template struct {
	t *lisp.FormTemplate
}

// MustTemplate reads src with the elps reader and compiles it, with params
// declaring placeholder names in argument order.  It is meant for package
// initialization and panics if src is not exactly one form, fails to parse,
// or contains a value the reader produces that a template cannot copy; on an
// undeclared placeholder, a duplicate or unused parameter, a malformed
// unquote, or a misplaced unquote-splicing.
func MustTemplate(src string, params ...string) *Template {
	forms, err := parser.NewReader().Read("template", strings.NewReader(src))
	if err != nil {
		templatePanic("%v", err)
	}
	if len(forms) != 1 {
		templatePanic("expected one form, got %d", len(forms))
	}
	t, err := lisp.NewFormTemplate(forms[0], params...)
	if err != nil {
		templatePanic("%v", err)
	}
	return &Template{t: t}
}

func templatePanic(format string, args ...any) {
	panic("elpsutil.MustTemplate: " + fmt.Sprintf(format, args...))
}

// Expand returns a fresh expansion, inserting args by pointer without copying
// or evaluating them (lisp.FormTemplate.Expand).  It panics if the argument
// count differs from the declared parameter count or a spliced argument is
// not an unquoted list (nil is accepted).
func (t *Template) Expand(args ...*lisp.LVal) *lisp.LVal {
	return t.t.Expand(args...)
}
