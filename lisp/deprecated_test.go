// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
)

// TestDeprecationNotice covers the shapes real docstrings arrive in: the
// hand-written one-liner a lisp defun carries, the tab-indented Go raw string
// literal every builtin in this repository is written as, and the multi-line
// paragraph a real deprecation notice runs to.
func TestDeprecationNotice(t *testing.T) {
	tests := []struct {
		name   string
		doc    string
		notice string
		ok     bool
	}{{
		name:   "first line marker",
		doc:    "Deprecated: use new-fn instead.",
		notice: "use new-fn instead.",
		ok:     true,
	}, {
		name:   "later paragraph",
		doc:    "Blend two paths.\n\nDeprecated: use join-paths instead.",
		notice: "use join-paths instead.",
		ok:     true,
	}, {
		name: "tab indented raw string literal",
		doc: `Blend two paths.

	Deprecated: use join-paths instead.`,
		notice: "use join-paths instead.",
		ok:     true,
	}, {
		name:   "shouting marker",
		doc:    "DEPRECATED: use new-fn instead.",
		notice: "use new-fn instead.",
		ok:     true,
	}, {
		name:   "marker with no text",
		doc:    "Blend two paths.\n\nDeprecated:",
		notice: "",
		ok:     true,
	}, {
		name:   "marker with trailing space only",
		doc:    "Deprecated:   ",
		notice: "",
		ok:     true,
	}, {
		name:   "mid paragraph mention is not a marker",
		doc:    "Blend two paths.\nDeprecated: not at a paragraph start.",
		notice: "",
		ok:     false,
	}, {
		name:   "mid line mention is not a marker",
		doc:    "This is Deprecated: honest.",
		notice: "",
		ok:     false,
	}, {
		name:   "empty doc",
		doc:    "",
		notice: "",
		ok:     false,
	}, {
		name:   "ordinary doc",
		doc:    "Blend two paths.\n\nThe result is a new path.",
		notice: "",
		ok:     false,
	}, {
		name:   "continuation lines joined with spaces",
		doc:    "Blend two paths.\n\nDeprecated: use join-paths instead.\nIt handles the empty case,\nwhich this does not.\n\nSee also: split-path.",
		notice: "use join-paths instead. It handles the empty case, which this does not.",
		ok:     true,
	}, {
		name:   "notice stops at the paragraph break",
		doc:    "Deprecated: use join-paths.\n\nThis paragraph is not part of the notice.",
		notice: "use join-paths.",
		ok:     true,
	}, {
		name:   "leading blank lines do not hide the marker",
		doc:    "\n\n\nDeprecated: use new-fn instead.",
		notice: "use new-fn instead.",
		ok:     true,
	}, {
		name:   "lowercase marker is not recognized",
		doc:    "deprecated: use new-fn instead.",
		notice: "",
		ok:     false,
	}}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			notice, ok := lisp.DeprecationNotice(tt.doc)
			assert.Equal(t, tt.ok, ok, "deprecation detection")
			assert.Equal(t, tt.notice, notice, "notice text")
		})
	}
}

// TestDeprecationNotice_BuiltinDocstrings guards against a stdlib docstring
// accidentally reading as a deprecation. No builtin, special operator or macro
// compiled into the interpreter is deprecated today, and the deprecated lint
// check reports every use of one that is — so a false positive here would fire
// on every file in the repository.
func TestDeprecationNotice_BuiltinDocstrings(t *testing.T) {
	type documented interface {
		Name() string
		Docstring() string
	}
	check := func(kind string, defs []lisp.LBuiltinDef) {
		t.Helper()
		for _, def := range defs {
			doc, ok := def.(documented)
			if !ok {
				continue
			}
			notice, deprecated := lisp.DeprecationNotice(doc.Docstring())
			assert.False(t, deprecated,
				"%s %q reads as deprecated (%q); if that is intended, the deprecated"+
					" lint check will now fire on every use of it", kind, doc.Name(), notice)
		}
	}
	check("builtin", lisp.DefaultBuiltins())
	check("special operator", lisp.DefaultSpecialOps())
	check("macro", lisp.DefaultMacros())
}
