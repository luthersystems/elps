// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/types"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

// TestNativePayloadAnalyzer runs the rule over testdata/src/nativepayload,
// which carries every construction spelling (Native, NativeOf inferred and
// explicitly instantiated, an aliased import, a parenthesised callee, the
// Value fallthrough, the field literal, the field write), the basic tier,
// the NativeCloner rule on both receiver kinds, the audited allowlist keyed
// on exact type, the interface-typed report, and the allow marker in every
// placement -- with and without a justification.  review.go adds the field
// reached through lisp.ErrorVal, a conversion, and embedding; the field's
// address; and multi-line literals with the marker on each candidate line.
//
// analysistest checks absence as strictly as presence: a construction with
// no want-expectation comment asserts NO diagnostic there, so the exemptions
// are pinned by this run as firmly as the reports.
func TestNativePayloadAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), nativePayloadAnalyzer, "nativepayload")
}

// TestAllowedPayloadTypesJustified guards the allowlist's shape: every row
// must carry a justification a reviewer can read, and every row's key must
// be spelled the way classifyPayload will look it up.  The rule cannot check
// that the words are TRUE -- that is what review is for -- but an empty or
// thin row is a classification nobody made, which is the thing the rule
// exists to prevent.
func TestAllowedPayloadTypesJustified(t *testing.T) {
	// The audited inventory.  A row added without a justification, or a key
	// that drifts from the type it names, fails here; a row deleted fails
	// here too, so that shrinking the map is a deliberate act.
	want := []string{
		"*github.com/luthersystems/elps/lisp.funData",
		"*[]byte",
		"*github.com/luthersystems/elps/lisp.MapData",
		"*github.com/luthersystems/elps/lisp.CallStack",
		"*regexp.Regexp",
		"time.Time",
		"error",
		"*github.com/luthersystems/elps/lisp/lisplib/libschema.validatorTag",
		"*github.com/luthersystems/elps/lisp/lisplib/libjson.ownMessage",
	}
	for _, key := range want {
		why, ok := allowedPayloadTypes[key]
		if !ok {
			t.Errorf("allowedPayloadTypes lost the audited row for %s;"+
				" this map may only shrink deliberately, and shrinking it means the type"+
				" is no longer used as a native payload", key)
			continue
		}
		if len(why) < 60 {
			t.Errorf("allowedPayloadTypes[%s] justification is too thin to audit: %q", key, why)
		}
	}
	if len(allowedPayloadTypes) != len(want) {
		t.Errorf("allowedPayloadTypes has %d rows, the audited inventory lists %d;"+
			" add the new row to this test with its justification reviewed",
			len(allowedPayloadTypes), len(want))
	}
	for key, why := range allowedPayloadTypes {
		if why == "" {
			t.Errorf("allowedPayloadTypes[%s] has no justification", key)
		}
	}
}

// TestClassifyPayloadUniverseError pins that the `error` row is reachable:
// the universe type has no package, and types.TypeString spells it bare,
// which is how the row is keyed.
func TestClassifyPayloadUniverseError(t *testing.T) {
	if got := classifyPayload(types.Universe.Lookup("error").Type()); got != payloadSafe {
		t.Errorf("classifyPayload(error) = %v, want payloadSafe via the allowlist row", got)
	}
	if got := classifyPayload(types.Universe.Lookup("any").Type()); got != payloadDynamic {
		t.Errorf("classifyPayload(any) = %v, want payloadDynamic", got)
	}
}

// TestJustifiedNativeAllow pins the justification requirement on the marker
// text itself, independent of placement: the rule's own marker, at least
// three words after it, and nothing that merely shares the prefix.
func TestJustifiedNativeAllow(t *testing.T) {
	cases := map[string]bool{
		"//elpsvet:allow-native the handle is immutable":   true,
		"// elpsvet:allow-native\tthe handle is immutable": true,
		"/*elpsvet:allow-native the handle is immutable*/": true,
		"//elpsvet:allow-native one two three":             true,
		"//elpsvet:allow-native":                           false,
		"//elpsvet:allow-native   ":                        false,
		"/*elpsvet:allow-native*/":                         false,
		"//elpsvet:allow-native .":                         false,
		"//elpsvet:allow-native one two":                   false,
		"//elpsvet:allow-natives by nobody at all":         false,
		"//elpsvet:allow-native-ish reason given here":     false,
		"//elpsvet:allow the ownership rule's own marker":  false,
		"//elps:mutates a different marker entirely":       false,
		"// plain comment with several words":              false,
	}
	for text, want := range cases {
		if got := justifiedNativeAllow(text); got != want {
			t.Errorf("justifiedNativeAllow(%q) = %v, want %v", text, got, want)
		}
	}
}

// TestOwnershipAllowStopsAtWordBoundary pins the other half of the marker
// separation: the ownership rule's bare //elpsvet:allow still suppresses,
// with or without a justification (that rule enforces none), but the native
// rule's //elpsvet:allow-native does not satisfy it -- otherwise one native
// justification on a package-level var would silence both rules.
func TestOwnershipAllowStopsAtWordBoundary(t *testing.T) {
	cases := map[string]bool{
		"//elpsvet:allow":                             true,
		"//elpsvet:allow guarded singleton":           true,
		"//elpsvet:allow\tsealed formals":             true,
		"//elpsvet:allow-native a native reason":      false,
		"//elpsvet:allowed by nobody":                 false,
		"// a plain comment mentioning elpsvet:allow": false,
	}
	for text, want := range cases {
		cg := &ast.CommentGroup{List: []*ast.Comment{{Text: text}}}
		if got := allowed(cg); got != want {
			t.Errorf("allowed(%q) = %v, want %v", text, got, want)
		}
	}
}
