// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// These former extension-contract cases now all fail at the ownership boundary.
// Even a well-behaved external factory is not part of the interpreter graph.
type templateContractMap struct {
	lisp.Map
	identity     any
	failure      string
	entriesCalls int
}

func newTemplateContractMap(failure string) *templateContractMap {
	m := &templateContractMap{Map: lisp.SortedMap().Map(), failure: failure}
	m.identity = m
	switch failure {
	case "nil-identity":
		m.identity = nil
	case "noncomparable-identity":
		m.identity = []int{1}
	}
	return m
}

func (m *templateContractMap) TemplateMapIdentity() any { return m.identity }

func (m *templateContractMap) NewTemplateMap() *templateContractMap {
	switch m.failure {
	case "nil-factory":
		return nil
	case "reused-factory":
		return m
	case "nonempty-factory":
		clone := newTemplateContractMap("")
		clone.Map.Set(lisp.String("unexpected"), lisp.Int(99))
		return clone
	case "set-error-factory":
		return newTemplateContractMap("set-error")
	case "nil-clone-identity":
		return newTemplateContractMap("nil-identity")
	case "noncomparable-clone-identity":
		return newTemplateContractMap("noncomparable-identity")
	default:
		return newTemplateContractMap("")
	}
}

func (m *templateContractMap) Set(key, value *lisp.LVal) *lisp.LVal {
	if m.failure == "set-error" {
		return lisp.Errorf("test map set failure")
	}
	return m.Map.Set(key, value)
}

func (m *templateContractMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	m.entriesCalls++
	if m.failure == "entries-error" || (m.failure == "late-entries-error" && m.entriesCalls > 1) {
		return lisp.Errorf("test map entries failure")
	}
	return m.Map.Entries(buf)
}

// This wrapper implements Map but intentionally declares no Template protocol.
type templateUnknownMap struct{ lisp.Map }

func TestTemplateMapContractFailuresRejectWithoutPublishing(t *testing.T) {
	for _, tc := range []struct {
		failure string
		want    string
	}{
		{"unknown-backing", "template: user:subject: map backing *lisp_test.templateUnknownMap is not interpreter-owned"},
		{"nil-identity", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
		{"noncomparable-identity", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
		{"nil-factory", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
		{"reused-factory", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
		{"nonempty-factory", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
		{"nil-clone-identity", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
		{"noncomparable-clone-identity", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
		{"set-error-factory", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
		{"entries-error", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
		{"late-entries-error", "template: user:subject: map backing *lisp_test.templateContractMap is not interpreter-owned"},
	} {
		t.Run(tc.failure, func(t *testing.T) {
			source := newTemplateContractMap(tc.failure)
			original := lisp.Int(7)
			if got := source.Map.Set(lisp.String("original"), original); !got.IsNil() {
				t.Fatal(got)
			}
			var backing lisp.Map = source
			if tc.failure == "unknown-backing" {
				backing = &templateUnknownMap{Map: source.Map}
			}
			subject := lisp.SortedMapFromData(lisp.NewMapData(backing))
			env := templateTestEnv(t)
			if got := env.PutGlobal(lisp.Symbol("subject"), subject); got.Type == lisp.LError {
				t.Fatal(got)
			}
			tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
			if tmpl != nil || err == nil || err.Error() != tc.want {
				t.Fatalf("template=%v error=%v; want nil template and %q", tmpl, err, tc.want)
			}
			if source.entriesCalls != 0 {
				t.Fatalf("rejection invoked opaque enumeration %d times", source.entriesCalls)
			}
			// Read through the actual source implementation, bypassing only the
			// test's injected Entries failure. Identity and content must survive.
			if got := env.Get(lisp.Symbol("subject")); got != subject || got.Map() != subject.Map() {
				t.Fatal("rejection replaced the source map")
			}
			got, found := source.Get(lisp.String("original"))
			if !found || got != original || got.Type != lisp.LInt || got.Int != 7 || source.Len() != 1 {
				t.Fatalf("rejection mutated the source map: original=%v, length=%d", got, source.Len())
			}
		})
	}
}
