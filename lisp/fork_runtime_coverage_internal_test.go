// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"io"
	"reflect"
	"testing"
	"time"
)

// forkRuntimeFieldPolicy records, for every field of Runtime, what (*LEnv).Fork
// is supposed to do with it.  The table exists because Fork builds the new
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
	// is meant to reuse.  Each of these is also a row in docs/fork.md's
	// shared/copied table.
	"Stderr":    "shared",
	"Reader":    "shared",
	"Library":   "shared",
	"LoadCache": "shared",

	// Copied by value: the template's configured limits become the fork's.
	"MaxAlloc":               "copied",
	"MaxMacroExpansionDepth": "copied",
	"MaxEvalNesting":         "copied",
	"MaxSleep":               "copied",
	"maxSteps":               "copied",

	// Rebuilt for the fork: fresh instance, seeded from the template where
	// the seeding is itself part of the contract (Stack's limits, Registry's
	// Lang, the two counters' continuity).
	"Registry": "rebuilt",
	"Stack":    "rebuilt",
	"Package":  "rebuilt",
	"numenv":   "rebuilt",
	"numsym":   "rebuilt",

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
	"macroExpSeq":     "not-carried",
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
				"and make (*LEnv).Fork carry, rebuild or deliberately drop it "+
				"(a field left out of Fork's literal is silently zeroed)", name)
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

	fork, err := env.Fork()
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
	if fork.Runtime.Profiler != nil || fork.Runtime.Debugger != nil {
		t.Error("fork carried an observer it should not have")
	}
	if fork.Runtime.loadCacheActive {
		t.Error("fork carried the load-cache re-entrancy guard")
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
