// Copyright © 2026 The ELPS authors

package nativepayload_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsvet/nativepayload"
	"golang.org/x/tools/go/analysis/analysistest"
)

// TestEmbedderDurableNative runs an embedder's elpsdurablenative over a
// package that declares codecs and builds natives, a package that builds
// the registry, and a package whose only registry call spreads a slice.
func TestEmbedderDurableNative(t *testing.T) {
	a := nativepayload.NewDurable(nativepayload.DurableConfig{
		Name:            "embeddurablenative",
		Module:          "example.com/embed",
		TransientMarker: "embedvet:transient",
	})
	analysistest.Run(t, analysistest.TestData(), a,
		"example.com/embed/durablecodecs", "example.com/embed/durableregistry", "example.com/embed/durablespread",
		"example.com/embed/durableforeign")
}

// TestElpsDurableNative runs elps's own configuration over a fixture and
// over the lisp stub, whose kernel slot literals and interface-typed
// constructor are not natives the rule classifies.
func TestElpsDurableNative(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), nativepayload.DurableAnalyzer,
		"github.com/luthersystems/elps/durableelps", "github.com/luthersystems/elps/lisp")
}

// TestDurableDefaults pins the name of elps's configuration.
func TestDurableDefaults(t *testing.T) {
	if got := nativepayload.DurableAnalyzer.Name; got != "elpsdurablenative" {
		t.Errorf("DurableAnalyzer.Name = %q", got)
	}
}

// TestElpsDurableNativeSkipsOtherModules runs elps's own configuration over
// a package of another module, as cmd/elpsvet does when an embedder runs it
// over its tree.  Nothing is reported.
func TestElpsDurableNativeSkipsOtherModules(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), nativepayload.DurableAnalyzer, "example.com/outside")
}

// TestNewDurableNeedsModule pins that a configuration without a module
// path fails at construction: the rule checks only that module.
func TestNewDurableNeedsModule(t *testing.T) {
	defer func() {
		r := recover()
		if r == nil || !strings.Contains(fmt.Sprint(r), "DurableConfig.Module") {
			t.Fatalf("NewDurable(DurableConfig{}) recovered %v, want a panic naming DurableConfig.Module", r)
		}
	}()
	nativepayload.NewDurable(nativepayload.DurableConfig{})
}
