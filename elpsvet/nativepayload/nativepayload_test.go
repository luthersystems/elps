// Copyright © 2026 The ELPS authors

package nativepayload_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsvet/nativepayload"
	"golang.org/x/tools/go/analysis/analysistest"
)

// embedderConfig is the configuration of a module other than elps, shaped
// like substrate's: an audited row of a third module, an exempting call,
// its own marker and fix text, and interface-typed payloads hidden.
var embedderConfig = nativepayload.Config{
	Name:         "embednativepayload",
	AllowMarker:  "embedvet:allow",
	AllowedTypes: map[string]string{"example.com/dec.Decimal": "immutable by API contract in this fixture"},
	ExemptCalls:  []string{"example.com/embed/probe.Capture"},
	Fix:          "declare the capture with probe.Capture, add an audited row, or annotate //embedvet:allow with a justification",
	HideDynamic:  true,
}

// TestEmbedderNativePayload runs an embedder's configuration.  A
// construction with no want comment asserts no diagnostic, so the row, the
// exempting call and the marker placements are pinned too.
func TestEmbedderNativePayload(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), nativepayload.New(embedderConfig), "example.com/embed/natives")
}

// TestEmbedderNativePayloadDynamic runs the same configuration with
// -anypayload set.
func TestEmbedderNativePayloadDynamic(t *testing.T) {
	a := nativepayload.New(embedderConfig)
	if err := a.Flags.Set("anypayload", "true"); err != nil {
		t.Fatal(err)
	}
	analysistest.Run(t, analysistest.TestData(), a, "example.com/embed/anypayload")
}

// TestEmbedderAllowMinWords runs a configuration that asks for five words
// after the allow marker, so a four-word justification is reported.
func TestEmbedderAllowMinWords(t *testing.T) {
	cfg := embedderConfig
	cfg.AllowMinWords = 5
	analysistest.Run(t, analysistest.TestData(), nativepayload.New(cfg), "example.com/embed/minwords")
}

// TestNewDefaults pins the names and the flag default of each
// configuration.
func TestNewDefaults(t *testing.T) {
	if got := nativepayload.Analyzer.Name; got != "elpsnativepayload" {
		t.Errorf("Analyzer.Name = %q", got)
	}
	if got := nativepayload.Analyzer.Flags.Lookup("anypayload").DefValue; got != "true" {
		t.Errorf("elps reports dynamic payloads by default; -anypayload default = %s", got)
	}
	a := nativepayload.New(embedderConfig)
	if a.Name != "embednativepayload" || a.Flags.Lookup("anypayload").DefValue != "false" {
		t.Errorf("embedder analyzer: name %q, -anypayload default %s", a.Name, a.Flags.Lookup("anypayload").DefValue)
	}
}

// TestNewRefusesMalformedExemptCalls pins that an ExemptCalls entry that
// does not name a package-level function fails at construction.
func TestNewRefusesMalformedExemptCalls(t *testing.T) {
	for _, entry := range []string{
		"Capture",                          // no package
		".Capture",                         // empty package
		"example.com/embed/probe.",         // empty function
		"example.com/embed/probe.T.Method", // a method
		"example.com/embed/probe.not-a-name",
		"example.com/embed/",
	} {
		t.Run(entry, func(t *testing.T) {
			defer func() {
				r := recover()
				if r == nil || !strings.Contains(fmt.Sprint(r), "ExemptCalls") {
					t.Fatalf("New with ExemptCalls %q recovered %v, want a panic naming ExemptCalls", entry, r)
				}
			}()
			nativepayload.New(nativepayload.Config{ExemptCalls: []string{entry}})
		})
	}
	// Well-formed entries, a dotted module path included.
	nativepayload.New(nativepayload.Config{ExemptCalls: []string{"example.com/embed/probe.Capture", "fmt.Println"}})
}
