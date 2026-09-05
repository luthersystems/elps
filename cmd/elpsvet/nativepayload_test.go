// Copyright © 2026 The ELPS authors

package main

import (
	"go/types"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

// TestNativePayloadAnalyzer runs the rule over testdata/src/nativepayload,
// which carries every construction spelling (Native, NativeOf inferred and
// explicitly instantiated, an aliased import, a parenthesised callee, the
// Value fallthrough, the LVal literal, the field write), the basic tier,
// the NativeCloner rule on both receiver kinds, the audited allowlist keyed
// on exact type, the interface-typed report, and the allow marker in every
// placement -- with and without a justification.
//
// analysistest checks absence as strictly as presence: a construction with
// no `// want` comment asserts NO diagnostic there, so the exemptions are
// pinned by this run as firmly as the reports.
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

// TestJustifiedAllow pins the justification requirement on the marker text
// itself, independent of placement.
func TestJustifiedAllow(t *testing.T) {
	cases := map[string]bool{
		"//elpsvet:allow the handle is immutable":   true,
		"// elpsvet:allow\tthe handle is immutable": true,
		"/*elpsvet:allow the handle is immutable*/": true,
		"//elpsvet:allow":                           false,
		"//elpsvet:allow   ":                        false,
		"/*elpsvet:allow*/":                         false,
		"//elpsvet:allowed by nobody":               false,
		"//elpsvet:allow-ish":                       false,
		"//elps:mutates a different marker":         false,
		"// plain comment":                          false,
	}
	for text, want := range cases {
		if got := justifiedAllow(text); got != want {
			t.Errorf("justifiedAllow(%q) = %v, want %v", text, got, want)
		}
	}
}
