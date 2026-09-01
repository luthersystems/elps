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

// TestBuiltinParsePathRefusesASelectorSpanningLines pins the ONE place this
// builtin is deliberately stricter than ParseSelector.
//
// A bracket-led selector is cut at its first newline and the tail dropped in
// silence, so ".[0]\n.password" parses -- through the Go API -- as the single
// step 0. That is not a rejection with a confusing message; it is a SHORTER
// PATH THAT WORKS, and the pattern this builtin exists for writes through it:
//
//	(apply ?set (concat 'list (list obj) (parse-path sel) (list v)))
//
// with those steps replaces the whole of element 0. It is the empty-step-list
// hazard one notch along, and the same answer applies -- raise.
//
// The Go API keeps the wart for parity with the v1 jq-string builtins
// downstream, and the assertions below say so explicitly: the same selector
// must still parse through ParseSelector and SelectorSteps, or the strictness
// has been pushed into the shared grammar and FuzzParseSelector's agreement
// invariant is what would notice next.
func TestBuiltinParsePathRefusesASelectorSpanningLines(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		sel     string
		wantErr bool
	}{
		{".[0]\n.password", true},
		{".[0]\n.a", true},
		{".[\"a\"]\n.b", true},
		{".[0]\nJUNK", true},
		{" .[0]\n.a ", true},
		{".[]\n.x", true},
		{".[1:2]\n.x", true},
		{". \n [0]\n.a", true},
		// NOT cut: the tail after the newline is only whitespace, which
		// TrimSpace removes before the rule is applied. An over-eager check
		// would reject these, and a selector ending in a newline is what a
		// line-oriented caller hands over.
		{".[0]\n", false},
		{".[0]\n\t ", false},
		{". \n [0]", false},
		// NOT cut: the leading-bracket rule is the only thing that cuts, so
		// a dot-led selector keeps every step it names.
		{".items[0]\n.id", false},
		{".a\n.b", false},
	} {
		t.Run(tc.sel, func(t *testing.T) {
			t.Parallel()
			got := callParsePath(t, lisp.String(tc.sel))
			if !tc.wantErr {
				if got.Type == lisp.LError {
					t.Fatalf("parse-path(%q) = %s, but nothing would be discarded", tc.sel, got)
				}
				return
			}
			if got.Type != lisp.LError {
				t.Fatalf("parse-path(%q) = %s, want an error -- the tail after the "+
					"newline is discarded, so this is a live path to the wrong node",
					tc.sel, got)
			}
			if !strings.Contains(got.String(), "may not span lines") {
				t.Errorf("parse-path(%q) reported %s, which does not name the problem",
					tc.sel, got)
			}
		})
	}

	// The Go API is unchanged, which is the parity half of the contract.
	for _, sel := range []string{".[0]\n.password", ".[0]\nJUNK"} {
		if _, err := ParseSelector(sel); err != nil {
			t.Errorf("ParseSelector(%q) must keep the v1 behaviour, got %v", sel, err)
		}
		if _, err := SelectorSteps(sel); err != nil {
			t.Errorf("SelectorSteps(%q) must mirror ParseSelector, got %v", sel, err)
		}
	}

	// A newline in a selector that is NOT bracket-led is not cut at all, so
	// the builtin must not reject it: it parses to every step it names.
	got := callParsePath(t, lisp.String(".items[0]\n.id"))
	if got.Type == lisp.LError {
		t.Fatalf("parse-path(%q) = %s, but a dot-led selector is not cut", ".items[0]\n.id", got)
	}
	if want := `'("items" 0 "id")`; got.String() != want {
		t.Errorf("parse-path(%q) = %s, want %s", ".items[0]\n.id", got, want)
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
