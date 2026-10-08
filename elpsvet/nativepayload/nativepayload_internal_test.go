// Copyright © 2026 The ELPS authors

package nativepayload

import (
	"go/types"
	"strings"
	"testing"
)

// elpsConfig is the configuration Analyzer runs with.
var elpsConfig = &config{Config: Config{AllowMarker: nativeAllowMarker, AllowMinWords: nativeAllowMinWords}}

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
	//
	// Each row is a claim about a HEADER, so the inventory pins the LType
	// constant alongside the payload type: a row whose headerType drifted
	// would exempt the kernel's literal for some other arm of
	// (*templateInventory).val, which is the failure the narrowing exists to
	// prevent.
	want := map[string]string{
		"*github.com/luthersystems/elps/lisp.funData": "LFun",
		"*[]byte": "LBytes",
		"*github.com/luthersystems/elps/lisp.MapData": "LSortMap",
	}
	for key, header := range want {
		row, ok := allowedPayloadTypes[key]
		if !ok {
			t.Errorf("allowedPayloadTypes lost the audited row for %s;"+
				" this map may only shrink deliberately, and shrinking it means the type"+
				" is no longer used as a native payload", key)
			continue
		}
		if len(row.reason) < 60 {
			t.Errorf("allowedPayloadTypes[%s] justification is too thin to audit: %q", key, row.reason)
		}
		if row.headerType != header {
			t.Errorf("allowedPayloadTypes[%s].headerType = %q, the audited inventory says %q;"+
				" the row exempts a kernel literal only when its Type key names this header,"+
				" so a drift here silently moves which literal is exempt",
				key, row.headerType, header)
		}
	}
	if len(allowedPayloadTypes) != len(want) {
		t.Errorf("allowedPayloadTypes has %d rows, the audited inventory lists %d;"+
			" add the new row to this test with its justification and its header type reviewed",
			len(allowedPayloadTypes), len(want))
	}
	for key, row := range allowedPayloadTypes {
		if row.reason == "" {
			t.Errorf("allowedPayloadTypes[%s] has no justification", key)
		}
		if row.headerType == "" {
			t.Errorf("allowedPayloadTypes[%s] names no header type, so the row would exempt"+
				" only a literal that sets no Type key -- which is exactly the shape the"+
				" narrowing reports", key)
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
		if row, ok := allowedPayloadTypes[key]; ok {
			t.Errorf("allowedPayloadTypes re-admitted %s (%q); publication does not, so"+
				" the row would exempt a construction the runtime still refuses", key, row.reason)
		}
	}
}

// TestClassifyPayloadUniverse pins the two universe types the rule sees most
// often at a constructor boundary.  `error` is an interface, so it is
// unclassifiable rather than safe -- publication reads a native's DYNAMIC
// type, which is why the port's `error` allowlist row was dropped.
func TestClassifyPayloadUniverse(t *testing.T) {
	for _, site := range everySite() {
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

// everySite enumerates the site shapes classifyPayload is asked about, so a
// test that means "at any site" cannot silently stop covering one.
func everySite() []payloadSite {
	var out []payloadSite
	for _, kind := range []siteKind{siteConstructor, siteHeaderLiteral, siteFieldWrite} {
		for _, inKernel := range []bool{false, true} {
			for _, header := range []string{"", "LBytes", "LNative"} {
				out = append(out, payloadSite{kind: kind, inKernel: inKernel, headerType: header})
			}
		}
	}
	return out
}

// TestKernelSlotRowsOnlyExemptTheKernelsOwnHeader pins the narrowing that
// closed the second review's remaining gap.  An allowlist row is a claim
// about storage on a header (*templateInventory).val handles by its own arm,
// so it exempts a keyed LVal literal INSIDE package lisp whose Type key
// names that header -- and nothing else.  Before the narrowing every
// kernel-slot SPELLING was exempt, so `LVal{Type: LNative, Native: &b}` and
// `v.Native = &b` in any package passed a gate the runtime then failed (see
// TestNativePayloadAnalyzerMirrorsTemplateAdmission for the runtime half).
func TestKernelSlotRowsOnlyExemptTheKernelsOwnHeader(t *testing.T) {
	// Universe's `byte` rather than Typ[Uint8]: go/types keeps them as
	// separate *types.Basic objects with separate names, source that says
	// []byte yields the former, and the allowlist is keyed on how
	// types.TypeString spells what the source said.
	bytePtr := types.NewPointer(types.NewSlice(types.Universe.Lookup("byte").Type()))
	if key := types.TypeString(bytePtr, nil); key != "*[]byte" {
		t.Fatalf("constructed key %q, want the allowlist's spelling *[]byte", key)
	}

	kernelLiteral := func(header string) payloadSite {
		return payloadSite{kind: siteHeaderLiteral, inKernel: true, headerType: header}
	}
	cases := []struct {
		name string
		site payloadSite
		want payloadVerdict
	}{{
		name: "the kernel's own LBytes literal, which is what lisp.Bytes writes",
		site: kernelLiteral("LBytes"),
		want: payloadSafe,
	}, {
		name: "an LNative literal in the kernel: exactly the header val hands to native()",
		site: kernelLiteral("LNative"),
		want: payloadKernelSlotMisuse,
	}, {
		name: "a literal with no Type key shows no header at all",
		site: kernelLiteral(""),
		want: payloadKernelSlotMisuse,
	}, {
		name: "a literal naming another real header is still the wrong arm",
		site: kernelLiteral("LSortMap"),
		want: payloadKernelSlotMisuse,
	}, {
		name: "the same LBytes literal outside package lisp",
		site: payloadSite{kind: siteHeaderLiteral, headerType: "LBytes"},
		want: payloadKernelSlotMisuse,
	}, {
		name: "a constructor builds an LNative, in the kernel as anywhere",
		site: payloadSite{kind: siteConstructor, inKernel: true},
		want: payloadKernelSlotMisuse,
	}, {
		name: "a field write inside the kernel: the documented residual",
		site: payloadSite{kind: siteFieldWrite, inKernel: true},
		want: payloadSafe,
	}, {
		name: "the same field write outside the kernel, which is where the bypass mattered",
		site: payloadSite{kind: siteFieldWrite},
		want: payloadKernelSlotMisuse,
	}}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := classifyPayload(bytePtr, tc.site); got != tc.want {
				t.Errorf("classifyPayload(*[]byte, %+v) = %v, want %v", tc.site, got, tc.want)
			}
		})
	}
}

// TestMisuseReasonNamesTheFailedCondition pins that the diagnostic says which
// of exemptsRow's conditions the site failed rather than reciting the rule --
// the difference between a message an author can act on and one they have to
// decode.
func TestMisuseReasonNamesTheFailedCondition(t *testing.T) {
	row := allowedPayloadTypes["*[]byte"]
	cases := []struct {
		site payloadSite
		want string
	}{
		{payloadSite{kind: siteHeaderLiteral, headerType: "LBytes"}, "outside package lisp"},
		{payloadSite{kind: siteConstructor, inKernel: true}, "constructor always builds an LNative"},
		{payloadSite{kind: siteHeaderLiteral, inKernel: true}, "sets no Type key"},
		{payloadSite{kind: siteHeaderLiteral, inKernel: true, headerType: "LNative"}, "is LNative, not LBytes"},
		// A Type key that resolved to nothing is a different thing to say
		// than no Type key at all: the author wrote a header and the rule
		// could not read it, which is what a shadowing local, an alias or a
		// computed expression looks like from here.
		{payloadSite{kind: siteHeaderLiteral, inKernel: true, hasTypeKey: true},
			"does not name the kernel's own header constant"},
	}
	for _, tc := range cases {
		if got := tc.site.misuseReason(row); !strings.Contains(got, tc.want) {
			t.Errorf("misuseReason(%+v) = %q, want it to mention %q", tc.site, got, tc.want)
		}
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
		if got := elpsConfig.justifiedAllow(text); got != want {
			t.Errorf("justifiedNativeAllow(%q) = %v, want %v", text, got, want)
		}
	}
}
