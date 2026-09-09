// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/types"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

// TestNativePayloadAnalyzer runs the rule over three fixture packages.
//
// testdata/src/nativepayload carries every construction spelling (Native,
// NativeOf inferred and explicitly instantiated, an aliased import, a
// parenthesised callee, the Value fallthrough, the field literal, the field
// write), the basic tier, the kernel-slot allowlist, the diagnostic-stack
// ban, the interface-typed report, and the allow marker in every placement --
// with and without a justification.  review.go adds the field reached through
// lisp.ErrorVal, a conversion, and embedding; the field's address; and
// multi-line literals with the marker on each candidate line.
//
// testdata/src/github.com/luthersystems/elps/nativemarker carries the marker
// tier: the struct VALUE embedding templatepolicy.Marker that publication
// admits, the pointer to one that it does not, the method-name lookalike, and
// the NativeCloner-only type that is now a report rather than a tier.  It
// lives under the module path because Go's internal rule lets only packages
// there import internal/templatepolicy -- which is the same reason the tier
// is closed to downstream embedders.
//
// testdata/src/github.com/luthersystems/elps/nativepaired is run by
// TestNativePayloadAnalyzerMirrorsTemplateAdmission
// (nativepayload_runtime_test.go) rather than from here, because each of its
// constructions is paired with the same value put through a real
// lisp.NewTemplate.  That test is the only thing in this package that can
// check the rule's actual claim -- that it mirrors template admission -- as
// opposed to checking that it still says what its own fixtures expect.
//
// analysistest checks absence as strictly as presence: a construction with
// no want-expectation comment asserts NO diagnostic there, so the exemptions
// are pinned by this run as firmly as the reports.
func TestNativePayloadAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), nativePayloadAnalyzer,
		"nativepayload", "github.com/luthersystems/elps/nativemarker")
}

// TestAllowedPayloadTypesJustified guards the allowlist's shape: every row
// must carry a justification a reviewer can read, and every row's key must
// be spelled the way classifyPayload will look it up.  The rule cannot check
// that the words are TRUE -- that is what review is for -- but an empty or
// thin row is a classification nobody made, which is the thing the rule
// exists to prevent.
func TestAllowedPayloadTypesJustified(t *testing.T) {
	// The audited inventory, and it is deliberately only the kernel's own
	// representation slots: every other payload goes through the marker tier
	// (internal/templatepolicy), the embedder's TemplateWithNativePolicy, or
	// a site annotation.  A row added without a justification, or a key that
	// drifts from the type it names, fails here; a row deleted fails here
	// too, so that shrinking the map is a deliberate act.
	want := []string{
		"*github.com/luthersystems/elps/lisp.funData",
		"*[]byte",
		"*github.com/luthersystems/elps/lisp.MapData",
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

// TestAllowedPayloadTypesDroppedRows pins the rows the template re-audit
// removed, each refuted by code on main: a raw time.Time and a *regexp.Regexp
// are rejected by publication itself (lisp/lisplib/template_natives_test.go),
// a *lisp.CallStack is banned by (*templateInventory).checkDiagnosticPayload,
// and libjson's *ownMessage and libschema's *validatorTag are no longer
// spelled as rows -- the marked struct value passes on its own, the pointer
// carries a site annotation.  Re-adding any of them should fail here first.
func TestAllowedPayloadTypesDroppedRows(t *testing.T) {
	for _, key := range []string{
		"time.Time",
		"*regexp.Regexp",
		"*github.com/luthersystems/elps/lisp.CallStack",
		"error",
		"*github.com/luthersystems/elps/lisp/lisplib/libschema.validatorTag",
		"*github.com/luthersystems/elps/lisp/lisplib/libjson.ownMessage",
	} {
		if why, ok := allowedPayloadTypes[key]; ok {
			t.Errorf("allowedPayloadTypes re-admitted %s (%q); publication does not, so"+
				" the row would exempt a construction the runtime still refuses", key, why)
		}
	}
}

// TestClassifyPayloadUniverse pins the two universe types the rule sees most
// often at a constructor boundary.  `error` is an interface, so it is
// unclassifiable rather than safe -- publication reads a native's DYNAMIC
// type, which is why the port's `error` allowlist row was dropped.
func TestClassifyPayloadUniverse(t *testing.T) {
	for _, site := range []payloadSite{siteConstructor, siteKernelSlot} {
		if got := classifyPayload(types.Universe.Lookup("error").Type(), site); got != payloadDynamic {
			t.Errorf("classifyPayload(error, %v) = %v, want payloadDynamic", site, got)
		}
		if got := classifyPayload(types.Universe.Lookup("any").Type(), site); got != payloadDynamic {
			t.Errorf("classifyPayload(any, %v) = %v, want payloadDynamic", site, got)
		}
	}
}

// TestRuntimeScalarKindsMatchesTheRuntimesList pins the basic tier against
// the reflect.Kind list (*templateInventory).native admits, in BOTH
// directions.  The runtime's switch names its refused kinds explicitly too,
// which is what makes the comparison checkable at all: of the kinds that can
// reach the tier -- those with a *types.Basic underlying type -- uintptr and
// unsafe.Pointer are refused there, and every other one is admitted.  The
// mirror was wrong on uintptr, and the analyzer exempted a payload that
// publication rejects with "native uintptr has no template immutability
// declaration" (see TestNativePayloadAnalyzerMirrorsTemplateAdmission).
func TestRuntimeScalarKindsMatchesTheRuntimesList(t *testing.T) {
	refused := map[types.BasicKind]string{
		types.Uintptr:       "reflect.Uintptr is in templateInventory.native's refused arm",
		types.UnsafePointer: "reflect.UnsafePointer is in templateInventory.native's refused arm",
	}
	// Every basic kind go/types can produce, so a new one cannot be
	// forgotten into the tier by omission.
	all := []types.BasicKind{
		types.Bool, types.Int, types.Int8, types.Int16, types.Int32, types.Int64,
		types.Uint, types.Uint8, types.Uint16, types.Uint32, types.Uint64,
		types.Uintptr, types.Float32, types.Float64, types.Complex64, types.Complex128,
		types.String, types.UnsafePointer,
		types.UntypedBool, types.UntypedInt, types.UntypedRune, types.UntypedFloat,
		types.UntypedComplex, types.UntypedString, types.UntypedNil,
	}
	for _, kind := range all {
		why, isRefused := refused[kind]
		if got := runtimeScalarKinds[kind]; got == isRefused {
			if isRefused {
				t.Errorf("runtimeScalarKinds admits kind %d, but %s", kind, why)
			} else {
				t.Errorf("runtimeScalarKinds does not admit kind %d, which the runtime's scalar"+
					" arm does; an author would have to annotate what publication already admits", kind)
			}
		}
	}
	if len(runtimeScalarKinds) != len(all)-len(refused) {
		t.Errorf("runtimeScalarKinds has %d entries, want %d: a kind outside the enumerated"+
			" universe was added without deciding what the runtime does with it",
			len(runtimeScalarKinds), len(all)-len(refused))
	}
}

// TestKernelSlotRowsOnlyExemptKernelSpellings pins the third narrowing: an
// allowlist row is a claim about storage on a header
// (*templateInventory).val handles by its own arm, so it exempts a .Native
// field write and a keyed LVal{Native: ...} literal -- and nothing else.
// lisp.Native, lisp.NativeOf and a falling-through lisp.Value all build an
// LNative, whose payload val hands to native(), where every row type is
// refused (see TestNativePayloadAnalyzerMirrorsTemplateAdmission for the
// runtime half).
func TestKernelSlotRowsOnlyExemptKernelSpellings(t *testing.T) {
	// Universe's `byte` rather than Typ[Uint8]: go/types keeps them as
	// separate *types.Basic objects with separate names, source that says
	// []byte yields the former, and the allowlist is keyed on how
	// types.TypeString spells what the source said.
	bytePtr := types.NewPointer(types.NewSlice(types.Universe.Lookup("byte").Type()))
	if key := types.TypeString(bytePtr, nil); key != "*[]byte" {
		t.Fatalf("constructed key %q, want the allowlist's spelling *[]byte", key)
	}
	if got := classifyPayload(bytePtr, siteKernelSlot); got != payloadSafe {
		t.Errorf("classifyPayload(*[]byte, siteKernelSlot) = %v, want payloadSafe:"+
			" lisp.Bytes writes exactly this payload into an LBytes literal", got)
	}
	if got := classifyPayload(bytePtr, siteConstructor); got != payloadKernelSlotMisuse {
		t.Errorf("classifyPayload(*[]byte, siteConstructor) = %v, want payloadKernelSlotMisuse:"+
			" a constructor builds an LNative, and native() refuses a *[]byte", got)
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

// TestRegisteredAnalyzers pins the gate's rule set. `make elpsvet` runs
// whatever multichecker.Main is handed, so a rule dropped from the slice --
// or one added and never wired -- leaves a green gate checking less than the
// Makefile comment, the workflow comment and CLAUDE.md all claim it does.
// The payload rule is the fourth, and the reason this test exists.
func TestRegisteredAnalyzers(t *testing.T) {
	want := []string{"elpsownership", "elpsfreshness", "elpsescape", "elpsnativepayload"}
	got := make([]string, 0, len(analyzers))
	for _, a := range analyzers {
		got = append(got, a.Name)
	}
	if len(got) != len(want) {
		t.Fatalf("elpsvet registers %v, want %v", got, want)
	}
	for i, name := range want {
		if got[i] != name {
			t.Errorf("analyzers[%d] = %s, want %s", i, got[i], name)
		}
	}
}
