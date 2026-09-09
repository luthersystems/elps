// Copyright © 2026 The ELPS authors

package main

import (
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
// green forever.  Three constructions had done exactly that -- a uintptr
// payload, a lisp.Value([]**LVal), and a *[]byte through a constructor --
// each silently exempt here and each rejected by NewTemplate at the first
// publication.
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

	// The analyzer half.  analysistest.Run also verifies the fixture's own
	// `// want` comments, so a diagnostic that moves or disappears fails
	// twice: once against the fixture, once against the table below.
	results := analysistest.Run(t, analysistest.TestData(), nativePayloadAnalyzer,
		"github.com/luthersystems/elps/nativepaired")
	if len(results) != 1 {
		t.Fatalf("analysistest returned %d results for the paired fixture, want 1", len(results))
	}
	messages := make([]string, 0, len(results[0].Diagnostics))
	for _, d := range results[0].Diagnostics {
		messages = append(messages, d.Message)
	}

	wantReports := 0
	for _, tc := range cases {
		if tc.wantDiagnostic != "" {
			wantReports++
		}
	}
	if len(messages) != wantReports {
		t.Errorf("the paired fixture produced %d diagnostics, want %d (one per reported case): %q",
			len(messages), wantReports, messages)
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// --- what the analyzer said about the source ---
			if tc.wantDiagnostic == "" {
				if matchesAny(t, regexp.QuoteMeta(tc.payloadSpelling), messages) {
					t.Errorf("analyzer named %s in %q, and %s is a control the runtime ADMITS;"+
						" a rule tightened until it reports everything mirrors nothing",
						tc.payloadSpelling, messages, tc.fixture)
				}
			} else if !matchesAny(t, tc.wantDiagnostic, messages) {
				t.Errorf("analyzer said nothing matching %q for %s; it reported %q."+
					" The runtime refuses this construction, so a static gate that stays quiet"+
					" hands the failure to whatever request first publishes the value",
					tc.wantDiagnostic, tc.fixture, messages)
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
