// Copyright © 2026 The ELPS authors

package nativepayload_test

import (
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
