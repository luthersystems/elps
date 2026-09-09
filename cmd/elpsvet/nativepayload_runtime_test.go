// Copyright © 2026 The ELPS authors

package main

import (
	"path/filepath"
	"regexp"
	"testing"
	"time"

	"golang.org/x/tools/go/analysis/analysistest"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/luthersystems/elps/parser"
)

// This file is the NEGATIVE CONTROL for the elpsnativepayload rule: it runs
// the real analyzer and the real runtime over the same constructions and
// requires the two verdicts to agree.
//
// The rule's whole claim is that it MIRRORS template admission
// ((*templateInventory).native and .val, lisp/template.go).  Nothing in the
// rule's own tests could check that claim: analysistest only asks whether
// the analyzer said what the fixture's `// want` comments say it would, so a
// rule and its fixtures can drift away from publication together and stay
// green forever.  Five constructions had done exactly that -- a uintptr
// payload, a lisp.Value([]**LVal), a *[]byte through a constructor, and the
// same *[]byte put onto an LNative header through the .Native field or
// through a literal that names Type: LNative -- each silently exempt here
// and each rejected by NewTemplate at the first publication.
//
// So each case below carries both halves: a function in the nativepaired
// fixture (testdata/src/github.com/luthersystems/elps/nativepaired, whose
// header names this test) that the analyzer runs over, and a value built the
// same way that a real lisp.NewTemplate is asked to publish.  A case is
// consistent when both refuse it or both admit it; the two positive controls
// are what stop "tighten until nothing passes" from looking like a fix.

// pairedCase is one construction, spelled twice.
type pairedCase struct {
	// name is the case, and doubles as the subtest name.
	name string
	// fixture is the function in the nativepaired fixture package that
	// spells this construction in source, so a reader can find the line the
	// analyzer half is about.
	fixture string
	// build makes the same value with the REAL lisp package.  It is a
	// closure rather than a value so that the byte-header case can spell the
	// local variable a caller would write.
	build func() *lisp.LVal
	// wantDiagnostic matches the analyzer's message for this construction.
	// Empty means the analyzer must say nothing about the fixture function.
	wantDiagnostic string
	// wantPublishErr matches the error NewTemplate returns for this payload.
	// Empty means publication must ADMIT it.
	wantPublishErr string
	// payloadSpelling is how the analyzer would render this payload's type
	// in a diagnostic.  On a CONTROL it is the string no diagnostic may
	// contain -- asserting the analyzer's silence by what it would have had
	// to say, rather than by counting.
	payloadSpelling string
	// inKernel says the fixture function lives in the IN-KERNEL paired
	// package (testdata/nativepairedkernel, whose import path is the
	// kernel's) rather than in nativepaired.  The allowlist tier's first
	// condition is that the site is inside package lisp, so a case about
	// WHICH HEADER a kernel literal names can only be spelled there: from
	// any other path the tier stops at the package test and the header
	// condition is never reached.
	inKernel bool
}

func pairedCases() []pairedCase {
	return []pairedCase{{
		name:    "uintptr is not a scalar the runtime admits",
		fixture: "PairUintptr",
		build:   func() *lisp.LVal { return lisp.Native(uintptr(1)) }, //elpsvet:allow-native the negative control's own payload: this value is published only to assert that publication REFUSES it
		// reflect.Uintptr sits in the arm templateInventory.native refuses,
		// beside reflect.UnsafePointer: an address wearing a basic type's
		// clothes, which a VM can convert back and follow.
		wantDiagnostic: `lisp\.Native payload type uintptr is not a known-safe value type`,
		wantPublishErr: `native uintptr has no template immutability declaration`,
	}, {
		name:    "[]**LVal is not lisp.Value's []*LVal arm",
		fixture: "PairNestedLValSlice",
		build:   func() *lisp.LVal { return lisp.Value([]**lisp.LVal{}) },
		// Value's type switch has `case []*LVal` and nothing deeper, so this
		// falls through to Native and becomes an opaque payload.
		wantDiagnostic: `lisp\.Value payload type \[\]\*\*lisp\.LVal is not a known-safe value type`,
		wantPublishErr: `native \[\]\*\*lisp\.LVal has no template immutability declaration`,
	}, {
		name:    "*[]byte through a constructor is an LNative, not a kernel slot",
		fixture: "PairByteHeader",
		build: func() *lisp.LVal {
			b := []byte{1}
			return lisp.Native(&b) //elpsvet:allow-native the negative control's own payload: this value is published only to assert that publication REFUSES it
		},
		// The allowlist row for *[]byte is a claim about LBytes storage,
		// which templateInventory.val handles by its own byte-span arm.  A
		// constructor builds an LNative instead, and val hands an LNative's
		// payload to native(), which has no such arm.
		wantDiagnostic: `lisp\.Native payload type \*\[\]byte is a kernel representation slot`,
		wantPublishErr: `native \*\[\]uint8 has no template immutability declaration`,
	}, {
		name:    "*[]byte written onto an LNative header through the field",
		fixture: "PairByteFieldWrite",
		build: func() *lisp.LVal {
			b := []byte{1}
			v := lisp.Native(int64(0))
			v.Native = &b //elpsvet:allow-native the negative control's own payload: this value is published only to assert that publication REFUSES it
			return v
		},
		// A field write shows no header, so the row exempts one only inside
		// package lisp, where every such write is guarded by a check of the
		// header's own Type.  Here the header is an LNative, and val hands
		// an LNative's payload to native().
		wantDiagnostic: `LVal\.Native assignment payload type \*\[\]byte is a kernel representation slot`,
		wantPublishErr: `native \*\[\]uint8 has no template immutability declaration`,
	}, {
		name:    "*[]byte in a literal that names Type: LNative",
		fixture: "PairNativeHeaderLiteral",
		build: func() *lisp.LVal {
			b := []byte{1}
			return &lisp.LVal{Type: lisp.LNative, Native: &b} //elpsvet:allow-native the negative control's own payload: this value is published only to assert that publication REFUSES it
		},
		// The kernel's own literal SHAPE with the one Type key the row is
		// not about.  Exempting the shape rather than the header is what
		// this narrowing fixed.
		wantDiagnostic: `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`,
		wantPublishErr: `native \*\[\]uint8 has no template immutability declaration`,
	}, {
		name:     "in the kernel: a literal naming Type: LNative over a row payload",
		fixture:  "PairKernelNativeHeaderLiteral",
		inKernel: true,
		build: func() *lisp.LVal {
			b := []byte{1}
			return &lisp.LVal{Type: lisp.LNative, Native: &b} //elpsvet:allow-native the negative control's own payload: this value is published only to assert that publication REFUSES it
		},
		// The pair that makes the header condition checkable: this case and
		// the control below are the same literal in the same package over
		// the same payload, differing only in the LType constant the Type
		// key names.  Drop that condition and this case goes quiet while
		// publication keeps refusing the value.
		wantDiagnostic: `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`,
		wantPublishErr: `native \*\[\]uint8 has no template immutability declaration`,
	}, {
		name:     "control: the kernel's own LSortMap literal still publishes",
		fixture:  "PairKernelSortMapLiteral",
		inKernel: true,
		// Exactly what lisp.SortedMapFromData writes.  templateInventory.val
		// takes its mapData arm on an LSortMap header and the planner
		// rebuilds the backing per VM.  The payload type differs from the
		// reported case's because this control asserts silence by the
		// string a diagnostic about it would have had to contain.
		build:           func() *lisp.LVal { return lisp.SortedMap() },
		payloadSpelling: "*lisp.MapData",
	}, {
		name:            "control: a scalar payload still publishes",
		fixture:         "PairScalarControl",
		build:           func() *lisp.LVal { return lisp.Native(int64(1)) },
		payloadSpelling: "int64",
	}, {
		name:    "control: a marked struct value still publishes",
		fixture: "PairMarkerControl",
		// libtime.Time wraps the host's time.Time in ownedTime, a struct
		// value embedding templatepolicy.Marker.  The fixture spells its own
		// marked struct (nativepaired.ownedStamp) because libtime's type is
		// unexported and no fixture can name it; the tier reads only the
		// marker on a struct value, which both have.
		build:           func() *lisp.LVal { return libtime.Time(pairedEpoch) },
		payloadSpelling: "ownedStamp",
	}}
}

// TestNativePayloadAnalyzerMirrorsTemplateAdmission is the paired check.
func TestNativePayloadAnalyzerMirrorsTemplateAdmission(t *testing.T) {
	cases := pairedCases()

	// The analyzer half, over both paired fixtures.  analysistest.Run also
	// verifies each fixture's own `// want` comments, so a diagnostic that
	// moves or disappears fails twice: once against the fixture, once
	// against the table below.
	//
	// The two roots are kept apart rather than merged because each one's
	// diagnostic COUNT is an assertion: every report in a paired fixture
	// must belong to a case in the table, which is what catches a
	// construction that starts reporting for a reason nobody wrote down.
	messages := map[bool][]string{
		false: pairedDiagnostics(t, analysistest.TestData(), "github.com/luthersystems/elps/nativepaired"),
		true:  pairedDiagnostics(t, filepath.Join(analysistest.TestData(), "nativepairedkernel"), lispPkgPath),
	}

	wantReports := map[bool]int{}
	for _, tc := range cases {
		if tc.wantDiagnostic != "" {
			wantReports[tc.inKernel]++
		}
	}
	for _, inKernel := range []bool{false, true} {
		if len(messages[inKernel]) != wantReports[inKernel] {
			t.Errorf("the paired fixture (inKernel=%v) produced %d diagnostics, want %d"+
				" (one per reported case): %q",
				inKernel, len(messages[inKernel]), wantReports[inKernel], messages[inKernel])
		}
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			said := messages[tc.inKernel]
			// --- what the analyzer said about the source ---
			if tc.wantDiagnostic == "" {
				if matchesAny(t, regexp.QuoteMeta(tc.payloadSpelling), said) {
					t.Errorf("analyzer named %s in %q, and %s is a control the runtime ADMITS;"+
						" a rule tightened until it reports everything mirrors nothing",
						tc.payloadSpelling, said, tc.fixture)
				}
			} else if !matchesAny(t, tc.wantDiagnostic, said) {
				t.Errorf("analyzer said nothing matching %q for %s; it reported %q."+
					" The runtime refuses this construction, so a static gate that stays quiet"+
					" hands the failure to whatever request first publishes the value",
					tc.wantDiagnostic, tc.fixture, said)
			}

			// --- what publication said about the value ---
			env := pairedTemplateEnv(t)
			if rc := env.PutGlobal(lisp.Symbol("value"), tc.build()); rc.Type == lisp.LError {
				t.Fatalf("PutGlobal: %v", rc)
			}
			plan, err := lisp.NewTemplate(env,
				lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
			if tc.wantPublishErr == "" {
				if err != nil {
					t.Fatalf("NewTemplate refused the control %s: %v."+
						" The analyzer exempts this construction, so publication refusing it"+
						" leaves the gate quieter than admission for a shape authors legitimately"+
						" write, and the exemption is now the thing that is wrong",
						tc.fixture, err)
				}
				if plan == nil {
					t.Fatal("NewTemplate returned no error and no template")
				}
				return
			}
			if err == nil {
				t.Fatalf("NewTemplate ADMITTED %s; the analyzer reports it, so one of the two"+
					" halves has moved and the rule no longer mirrors admission", tc.fixture)
			}
			if !regexp.MustCompile(tc.wantPublishErr).MatchString(err.Error()) {
				t.Errorf("NewTemplate error = %q, want a match for %q", err, tc.wantPublishErr)
			}
			if plan != nil {
				t.Error("NewTemplate returned both an error and a template")
			}
		})
	}
}

// pairedDiagnostics runs the analyzer over one paired fixture package and
// returns what it said, so the two roots are read the same way.
func pairedDiagnostics(t *testing.T, dir, pkg string) []string {
	t.Helper()
	results := analysistest.Run(t, dir, nativePayloadAnalyzer, pkg)
	if len(results) != 1 {
		t.Fatalf("analysistest returned %d results for %s, want 1", len(results), pkg)
	}
	out := make([]string, 0, len(results[0].Diagnostics))
	for _, d := range results[0].Diagnostics {
		out = append(out, d.Message)
	}
	return out
}

// pairedEpoch is a fixed timestamp: libtime.Time detaches the host's
// *Location into the owned struct value, and a fixed instant keeps the
// control's publication independent of the clock.
var pairedEpoch = time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)

func matchesAny(t *testing.T, pattern string, messages []string) bool {
	t.Helper()
	re := regexp.MustCompile(pattern)
	for _, msg := range messages {
		if re.MatchString(msg) {
			return true
		}
	}
	return false
}

// pairedTemplateEnv builds the smallest environment NewTemplate will accept,
// matching lisp_test's templateTestEnv.
func pairedTemplateEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	return env
}
