// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// A *token.Location is a mutable object, and until issues #362 and #366 the
// interpreter handed the same one to several owners at once.  The tests here
// pin the two ends of that: an error must not share a Location with the
// evaluator that raised it (#366), and the process-wide "<native code>"
// Location must be watched by the singleton guard (#362).

// TestErrorAssociateCopiesLocation is the regression test for issue #366.
//
// ErrorAssociate used to store env.Loc by reference.  env.Loc is a pointer
// the evaluator rebinds on every eval step, and it points at an AST node's
// Source -- or, for a natively constructed node, at the process-wide
// nativeSource singleton.  Storing the pointer left an already-raised error
// and the still-running evaluator sharing one Location, so an in-place write
// through either silently moved the position the error had already reported.
func TestErrorAssociateCopiesLocation(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)

	loc := &token.Location{File: "err.lisp", Path: "/tmp/err.lisp", Pos: 10, Line: 3, Col: 4}
	env.Loc = loc

	lerr := &LVal{Type: LError, Str: "test-error", Cells: []*LVal{String("boom")}}
	if res := env.ErrorAssociate(lerr); res != nil {
		t.Fatalf("ErrorAssociate failed: %v", res)
	}

	// 1. The error's location is pointer-independent of env.Loc.
	if lerr.Source == nil {
		t.Fatal("ErrorAssociate did not stamp a source location")
	}
	if lerr.Source == loc {
		t.Error("the error's Source aliases env.Loc; ErrorAssociate must store a copy (#366)")
	}
	if *lerr.Source != *loc {
		t.Errorf("the copied location differs from env.Loc: got %+v, want %+v", *lerr.Source, *loc)
	}

	// 2. Mutating env.Loc in place afterwards does not move the position the
	//    error already recorded.  This is the failure the aliasing produced:
	//    a position that changes after the fact, at a point unrelated to the
	//    write.
	before := lerr.Source.String()
	loc.Line = 99
	loc.Col = 42
	loc.Pos = 1234
	if got := lerr.Source.String(); got != before {
		t.Errorf("an in-place write through env.Loc moved the error's reported position: %s -> %s (#366)", before, got)
	}

	// 3. A nil env.Loc still yields a nil Source: the copy preserves nil, so
	//    the `Source == nil` convention ErrorAssociate itself tests for is
	//    unchanged.
	env.Loc = nil
	nilErr := &LVal{Type: LError, Str: "test-error", Cells: []*LVal{String("boom")}}
	if res := env.ErrorAssociate(nilErr); res != nil {
		t.Fatalf("ErrorAssociate failed: %v", res)
	}
	if nilErr.Source != nil {
		t.Errorf("a nil env.Loc must leave Source nil, got %+v", nilErr.Source)
	}
}

// TestErrorConstructorsCopyLocation covers the two sibling sites found by the
// audit for #366: ErrorCondition and ErrorConditionf built their LError with
// `Source: env.Loc`, the same aliasing statement ErrorAssociate had.  Fixing
// only ErrorAssociate would have left every error raised by Errorf aliased.
func TestErrorConstructorsCopyLocation(t *testing.T) {
	t.Parallel()
	env := initSafetyTestEnv(t)

	loc := &token.Location{File: "err.lisp", Pos: 10, Line: 3, Col: 4}
	env.Loc = loc

	for _, test := range []struct {
		name string
		fn   func() *LVal
	}{
		{"Errorf", func() *LVal { return env.Errorf("boom") }},
		{"ErrorConditionf", func() *LVal { return env.ErrorConditionf("test-error", "boom") }},
		{"ErrorCondition", func() *LVal { return env.ErrorCondition("test-error", "boom") }},
	} {
		lerr := test.fn()
		if lerr.Type != LError {
			t.Fatalf("%s: expected an error value, got %v", test.name, lerr.Type)
		}
		if lerr.Source == nil {
			t.Fatalf("%s: expected a source location", test.name)
		}
		if lerr.Source == loc {
			t.Errorf("%s: the error's Source aliases env.Loc (#366)", test.name)
		}
		if *lerr.Source != *loc {
			t.Errorf("%s: copied location differs: got %+v, want %+v", test.name, *lerr.Source, *loc)
		}
	}

	// A nil env.Loc is preserved rather than materialised into a zero
	// Location, which would print as ":0:0" instead of being omitted.
	env.Loc = nil
	if lerr := env.Errorf("boom"); lerr.Source != nil {
		t.Errorf("a nil env.Loc must leave Source nil, got %+v", lerr.Source)
	}
}

// TestSingletonSnapshotDetectsNativeLocationDrift is the guard added for
// issue #362.  defaultSourceLocation is a fourth shared mutable singleton
// alongside the three singleton LVals, and TakeSingletonSnapshot did not
// cover it: a stray `v.Source.Pos = 7` on a value from lisp.Int corrupted
// every value in the process and Verify reported nothing, so the failure
// landed later, elsewhere, in whatever test happened to read a position.
//
// This does not stop the write.  It converts it from action at a distance
// into a named offender at the next singleton check.
func TestSingletonSnapshotDetectsNativeLocationDrift(t *testing.T) {
	// This test writes to a shared singleton on purpose.  Pause the write
	// watchdog so the deliberate mutation is ordered against its reads and
	// does not surface as a data race under `make race`.
	defer pauseSingletonWatchdog()()

	orig := *defaultSourceLocation
	defer func() { *defaultSourceLocation = orig }()

	snap := TakeSingletonSnapshot()
	if drift := snap.Verify(); drift != "" {
		t.Fatalf("fresh snapshot should match current state, got drift %q", drift)
	}

	// Exactly the write from the issue: reach a shared Location through a
	// value a constructor handed out, and edit it in place.
	v := Int(1)
	v.Source.Pos = 7

	if drift := snap.Verify(); drift != "nativeSource()" {
		t.Errorf("Verify() = %q, want %q: a write through a constructed value's Source must be named (#362)", drift, "nativeSource()")
	}

	*defaultSourceLocation = orig
	if drift := snap.Verify(); drift != "" {
		t.Errorf("after restore Verify() = %q, want empty", drift)
	}
}

// TestNativeLocationIsValueTyped pins the shape that makes the fix hold:
// token.NativeLocation returns a VALUE, so a caller that needs a Location it
// may write to cannot accidentally get the shared one.
func TestNativeLocationIsValueTyped(t *testing.T) {
	t.Parallel()

	a := token.NativeLocation()
	b := token.NativeLocation()
	if a != b {
		t.Errorf("NativeLocation is not stable: %+v vs %+v", a, b)
	}
	if a != *defaultSourceLocation {
		t.Errorf("the shared native location has drifted from token.NativeLocation: %+v vs %+v", *defaultSourceLocation, a)
	}
	a.Pos = 7
	if b.Pos == 7 || defaultSourceLocation.Pos == 7 {
		t.Error("writing to a NativeLocation value must not reach any other copy")
	}
}
