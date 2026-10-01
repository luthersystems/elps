// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"io"
	"reflect"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// forkRuntimeFieldPolicy records, for every field of Runtime, what Template.NewVM
// is supposed to do with it. The table exists because construction builds the new
// Runtime FIELD BY FIELD from a literal, which is a construction that fails
// silently: a field nobody lists is simply left at its zero value, and no
// compiler, vet check or existing test notices.  Runtime.LoadCache was dropped
// that way for three review rounds (issue #536 round-three review, suspicious
// 1) — a forked environment reparsed every file its template had already
// parsed, with nothing failing to say so.
//
// Adding a field to Runtime now breaks this test until the field is
// classified, and classifying it means deciding what a fork should do with it.
var forkRuntimeFieldPolicy = map[string]string{
	// Shared with the template: process-wide or read-only state that a fork
	// is meant to reuse.  Each of these is also a row in docs/templates.md's
	// shared/copied table.
	"Stderr":    "shared",
	"Reader":    "shared",
	"Library":   "shared",
	"LoadCache": "shared",

	// Copied by value: the template's configured limits become the fork's.
	"MaxAlloc":               "copied",
	"MaxValueDepth":          "copied",
	"MaxMacroExpansionDepth": "copied",
	"MaxEvalNesting":         "copied",
	"MaxSleep":               "copied",
	"LegacyKeywordFormals":   "copied",
	"maxSteps":               "copied",

	// Rebuilt for the fork: fresh instance, seeded from the template where
	// the seeding is itself part of the contract (Stack's limits, Registry's
	// Lang, the two counters' continuity).
	"Registry": "rebuilt",
	"Stack":    "rebuilt",
	"Package":  "rebuilt",
	"numenv":   "rebuilt",
	"numsym":   "rebuilt",
	"settings": "copied",
	// Values are copied at publication and borrowed until a VM's first write.
	"values": "copied",
	// Each VM rebuilds the flag from the presence of a published map.
	"valuesShared": "rebuilt",

	// Deliberately NOT carried: an observer the embedder attaches itself, or
	// state about an evaluation/load in progress (the template is quiescent,
	// so all of these are zero on it anyway).
	"Profiler":        "not-carried",
	"Debugger":        "not-carried",
	"conditionStack":  "not-carried",
	"evalDepth":       "not-carried",
	"loadCacheActive": "not-carried",
	"evalNesting":     "not-carried",
	"steps":           "not-carried",
	"totalSteps":      "not-carried",
	// Per-evaluation companion of steps: a fresh VM has not overflowed.
	"stepsOverflowed": "not-carried",
	// Per-VM transaction state; VMWithStepBudget sets it on the new VM.
	"stepBudget":           "not-carried",
	"stepBudgetUsed":       "not-carried",
	"stepBudgetOverflowed": "not-carried",
	"macroExpSeq":          "not-carried",
	// closures is only ever compared against itself within one let* call
	// (opLetSeq reads it before and after each initializer), so its absolute
	// value carries no meaning across a fork.
	"closures": "not-carried",
	// Derived from the copied MaxEvalNesting setting on first eval. A zero
	// cache is valid for a fresh VM even when its configured cap is nonzero.
	"evalNestingSetting": "not-carried",
	"evalNestingLimit":   "not-carried",
}

// TestForkRuntimeFieldCoverage fails when Runtime grows a field nobody has
// decided a fork policy for.
func TestForkRuntimeFieldCoverage(t *testing.T) {
	typ := reflect.TypeOf(Runtime{})
	seen := map[string]bool{}
	for i := range typ.NumField() {
		name := typ.Field(i).Name
		seen[name] = true
		if _, ok := forkRuntimeFieldPolicy[name]; !ok {
			t.Errorf("Runtime.%s has no fork policy: add it to forkRuntimeFieldPolicy "+
				"and make Template.NewVM carry, rebuild or deliberately drop it "+
				"(a field left out of the runtime literal is silently zeroed)", name)
		}
	}
	for name := range forkRuntimeFieldPolicy {
		if !seen[name] {
			t.Errorf("forkRuntimeFieldPolicy names Runtime.%s, which no longer exists", name)
		}
	}
}

// TestForkCarriesSharedRuntimeFields pins the "shared" rows of the table
// against the real Fork, so the policy map cannot drift away from the code it
// documents.
func TestForkCarriesSharedRuntimeFields(t *testing.T) {
	var stderr bytes.Buffer
	cache := &fieldCoverageCache{}
	env := NewEnv(nil)
	env.Runtime.Stderr = &stderr
	env.Runtime.Reader = &fieldCoverageReader{}
	env.Runtime.Library = &fieldCoverageLibrary{}
	env.Runtime.LoadCache = cache
	env.Runtime.MaxSleep = 5 * time.Nanosecond
	env.Runtime.LegacyKeywordFormals = true

	fork, err := forkTestSnapshot(env)
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	if fork.Runtime.Stderr != env.Runtime.Stderr {
		t.Error("fork did not share Runtime.Stderr")
	}
	if fork.Runtime.Reader != env.Runtime.Reader {
		t.Error("fork did not share Runtime.Reader")
	}
	if fork.Runtime.Library != env.Runtime.Library {
		t.Error("fork did not share Runtime.Library")
	}
	if fork.Runtime.LoadCache != env.Runtime.LoadCache {
		t.Errorf("fork did not share Runtime.LoadCache: template %T, fork %T",
			env.Runtime.LoadCache, fork.Runtime.LoadCache)
	}
	if fork.Runtime.MaxSleep != env.Runtime.MaxSleep {
		t.Error("fork did not copy Runtime.MaxSleep")
	}
	if !fork.Runtime.LegacyKeywordFormals {
		t.Error("fork did not copy Runtime.LegacyKeywordFormals")
	}
	if fork.Runtime.Profiler != nil || fork.Runtime.Debugger != nil {
		t.Error("fork carried an observer it should not have")
	}
	if fork.Runtime.loadCacheActive {
		t.Error("fork carried the load-cache re-entrancy guard")
	}
}

func TestRuntimeSettingValueMapBorrow(t *testing.T) {
	source := NewEnv(nil)
	require.NoError(t, source.Runtime.SetSettingValue("published", "source"))
	require.NoError(t, source.Runtime.SetSettingValue("keep", "source"))
	tmpl, err := NewTemplate(source)
	require.NoError(t, err)
	published := reflect.ValueOf(tmpl.plan.runtime.values).UnsafePointer()
	require.NotNil(t, published)
	assert.NotEqual(t, published, reflect.ValueOf(source.Runtime.values).UnsafePointer())
	assert.False(t, source.Runtime.valuesShared)

	for _, action := range []string{"set", "overwrite", "delete"} {
		t.Run(action, func(t *testing.T) {
			vm, err := tmpl.NewVM()
			require.NoError(t, err)
			assert.True(t, vm.Runtime.valuesShared)
			assert.Equal(t, published, reflect.ValueOf(vm.Runtime.values).UnsafePointer())
			assertRuntimeSettingValue(t, vm.Runtime, "published", "source")
			vm.Runtime.DeleteSettingValue("missing")
			require.Error(t, vm.Runtime.SetSettingValue("published", new(int)))
			require.Error(t, vm.Runtime.SetSettingValue("missing", new(int)))
			assert.True(t, vm.Runtime.valuesShared)
			assert.Equal(t, published, reflect.ValueOf(vm.Runtime.values).UnsafePointer())
			assertRuntimeSettingValue(t, vm.Runtime, "published", "source")
			assertNoRuntimeSettingValue(t, vm.Runtime, "missing")

			switch action {
			case "set":
				require.NoError(t, vm.Runtime.SetSettingValue("new", "vm"))
				assertRuntimeSettingValue(t, vm.Runtime, "new", "vm")
			case "overwrite":
				require.NoError(t, vm.Runtime.SetSettingValue("published", "vm"))
				assertRuntimeSettingValue(t, vm.Runtime, "published", "vm")
			case "delete":
				vm.Runtime.DeleteSettingValue("published")
				assertNoRuntimeSettingValue(t, vm.Runtime, "published")
			}
			assert.False(t, vm.Runtime.valuesShared)
			private := reflect.ValueOf(vm.Runtime.values).UnsafePointer()
			assert.NotEqual(t, published, private)
			require.NoError(t, vm.Runtime.SetSettingValue("keep", "vm"))
			vm.Runtime.DeleteSettingValue("keep")
			assert.Equal(t, private, reflect.ValueOf(vm.Runtime.values).UnsafePointer())
			sibling, err := tmpl.NewVM()
			require.NoError(t, err)
			assert.True(t, sibling.Runtime.valuesShared)
			assert.Equal(t, published, reflect.ValueOf(sibling.Runtime.values).UnsafePointer())
			for _, rt := range []*Runtime{source.Runtime, sibling.Runtime} {
				assertRuntimeSettingValue(t, rt, "published", "source")
				assertRuntimeSettingValue(t, rt, "keep", "source")
				assertNoRuntimeSettingValue(t, rt, "new")
			}
		})
	}
}

func TestRuntimeSettingValueEmptyMapNotBorrowed(t *testing.T) {
	for _, initiallySet := range []bool{false, true} {
		t.Run(fmt.Sprintf("initially_set=%t", initiallySet), func(t *testing.T) {
			source := NewEnv(nil)
			if initiallySet {
				require.NoError(t, source.Runtime.SetSettingValue("removed", "source"))
				source.Runtime.DeleteSettingValue("removed")
			}
			tmpl, err := NewTemplate(source)
			require.NoError(t, err)
			assert.Nil(t, tmpl.plan.runtime.values)
			vm, err := tmpl.NewVM()
			require.NoError(t, err)
			assert.Nil(t, vm.Runtime.values)
			assert.False(t, vm.Runtime.valuesShared)
			vm.Runtime.DeleteSettingValue("missing")
			require.Error(t, vm.Runtime.SetSettingValue("missing", nil))
			assert.Nil(t, vm.Runtime.values)
			assert.False(t, vm.Runtime.valuesShared)
		})
	}
}

type fieldCoverageCache struct{}

func (c *fieldCoverageCache) Load(string) (*CachedSource, bool) { return nil, false }
func (c *fieldCoverageCache) Store(string, *CachedSource)       {}

type fieldCoverageReader struct{}

func (r *fieldCoverageReader) Read(string, io.Reader) ([]*LVal, error) { return nil, nil }

type fieldCoverageLibrary struct{}

func (l *fieldCoverageLibrary) LoadSource(SourceContext, string) (string, string, []byte, error) {
	return "", "", nil, nil
}
