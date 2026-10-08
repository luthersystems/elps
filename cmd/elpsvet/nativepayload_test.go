// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/elpsvet/nativepayload"
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
	analysistest.Run(t, analysistest.TestData(), nativepayload.Analyzer,
		"nativepayload", "github.com/luthersystems/elps/nativemarker")
}

// TestNativePayloadAnalyzerInKernelPackage runs the rule over a fixture whose
// IMPORT PATH is github.com/luthersystems/elps/lisp, which is the only way to
// exercise the exempt side of the allowlist tier: a row describes the
// kernel's own representation storage, so the tier's first condition is that
// the package under analysis is the kernel itself.
//
// It needs a private testdata root because the kernel-path stub in
// testdata/src carries `// want` comments for the ESCAPE rule, and
// analysistest checks every expectation in a package against the one analyzer
// it is running -- sharing the package would fail both tests.  The ownership
// fixture already uses a private root for its own reason.
//
// The fixture pins both halves: the kernel's own literals (an LBytes header
// over a *[]byte, an LSortMap over a *MapData, an LFun over a *funData) are
// exempt, and the same rows on any other header -- LNative above all, and a
// literal that names no Type at all -- are reported. The reported half is
// the defect the narrowing closed: `LVal{Type: LNative, Native: &b}` was a
// kernel-slot SPELLING, so it was exempt statically, while
// (*templateInventory).val routes an LNative header's payload to native(),
// which refuses a *[]byte.
func TestNativePayloadAnalyzerInKernelPackage(t *testing.T) {
	analysistest.Run(t, filepath.Join(analysistest.TestData(), "nativelisp"),
		nativepayload.Analyzer, lispPkgPath)
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
// Makefile comment, the workflow comment and the elpsvet skill
// (.claude/skills/elpsvet/SKILL.md) all claim it does.
// The payload rule is fourth, builtin-state fifth, frozen-package sixth,
// lazy-read seventh, and own-package (elpsvet/ownpkg, importable) eighth.
// Exhaustive switches and value walkers are ninth and tenth, marker fields
// eleventh, and durable natives (elpsvet/nativepayload, importable) twelfth.
func TestRegisteredAnalyzers(t *testing.T) {
	want := []string{"elpsownership", "elpsfreshness", "elpsescape", "elpsnativepayload", "elpsbuiltinstate", "elpsfrozenpackage", "elpslazyread", "elpsownpkg", "elpsltypeswitch", "elpsvalwalker", "elpsmarkerfields", "elpsdurablenative"}
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
