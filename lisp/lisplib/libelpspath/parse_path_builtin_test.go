// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// callParsePath invokes the builtin the way the interpreter does.
func callParsePath(t *testing.T, arg *lisp.LVal) *lisp.LVal {
	t.Helper()
	env := lisp.NewEnv(nil)
	return BuiltinParsePath(env, lisp.SExpr([]*lisp.LVal{arg}))
}

// TestBuiltinParsePathRejectsBadSelector is the test the lisp suite could not
// be: it distinguishes RAISING from RETURNING AN EMPTY LIST.
//
// (ignore-errors (parse-path x)) yields () when x raises -- and () is also
// the successful result for the identity selector, so a lisp assertion on nil
// passes either way. The distinction is the whole safety property here. An
// empty step list is the IDENTITY path, so a builtin that swallowed the parse
// error would turn a malformed selector into "the whole document":
//
//	(apply ?set (concat 'list (list obj) (parse-path junk) (list v)))
//
// replaces obj entirely rather than failing. That is the worst outcome this
// function has, and it is what these assertions pin.
func TestBuiltinParsePathRejectsBadSelector(t *testing.T) {
	t.Parallel()
	for _, sel := range []string{
		"", "a", "..", ".[", ".]", ".[a]", ".[1:2:3]", `.["a`,
		".my-key", "not a path at all", " ", "[0]",
	} {
		got := callParsePath(t, lisp.String(sel))
		if got.Type != lisp.LError {
			t.Errorf("parse-path(%q) = %s (type %v), want an error -- an empty "+
				"step list is the IDENTITY path, so this would silently address "+
				"the whole document", sel, got, got.Type)
		}
	}
}

// TestBuiltinParsePathAcceptsIdentity is the other half: the identity
// selector must succeed with no steps, which is what makes the empty result
// ambiguous in lisp and is therefore worth pinning separately.
func TestBuiltinParsePathAcceptsIdentity(t *testing.T) {
	t.Parallel()
	got := callParsePath(t, lisp.String("."))
	if got.Type == lisp.LError {
		t.Fatalf(`parse-path(".") = %s, want an empty step list`, got)
	}
	if got.Type != lisp.LSExpr || len(got.Cells) != 0 {
		t.Fatalf(`parse-path(".") = %s (type %v), want an empty list`, got, got.Type)
	}
}

// TestBuiltinParsePathRequiresAString pins the type check.
//
// It is not cosmetic: lisp.LSymbol also carries .Str, and ".a" is a legal
// elps symbol, so without the check a quoted symbol is silently accepted as
// a selector and parses as though it were the string.
func TestBuiltinParsePathRequiresAString(t *testing.T) {
	t.Parallel()
	for _, arg := range []*lisp.LVal{
		lisp.Symbol(".a"),
		lisp.Symbol("."),
		lisp.Int(0),
		lisp.Float(1.5),
		lisp.Nil(),
		lisp.QExpr([]*lisp.LVal{lisp.String(".a")}),
		lisp.SortedMap(),
	} {
		got := callParsePath(t, arg)
		if got.Type != lisp.LError {
			t.Errorf("parse-path(%s of type %v) = %s, want an error",
				arg, arg.Type, got)
		}
	}
}

// TestBuiltinParsePathReportsTheParserMessage keeps the builtin's error text
// tied to the parser's, since the builtin re-emits it verbatim and a
// divergence would reach users unnoticed.
func TestBuiltinParsePathReportsTheParserMessage(t *testing.T) {
	t.Parallel()
	for _, sel := range []string{"", "a", ".[", ".[1:2:3]"} {
		_, err := SelectorSteps(sel)
		if err == nil {
			t.Fatalf("SelectorSteps(%q) unexpectedly succeeded", sel)
		}
		got := callParsePath(t, lisp.String(sel))
		if got.Type != lisp.LError {
			t.Fatalf("parse-path(%q) did not error", sel)
		}
		if !strings.Contains(got.String(), err.Error()) {
			t.Errorf("parse-path(%q) reported %s, which does not carry the "+
				"parser's message %q", sel, got, err)
		}
	}
}
