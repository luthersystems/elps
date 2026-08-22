// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// A *token.Location is a mutable object, and until issues #362 and #366 the
// interpreter handed the same one to several owners at once.
//
// WHAT USED TO BE IN THIS FILE, and where it went.
//
// TestErrorAssociateCopiesLocation and TestErrorConstructorsCopyLocation
// (#366) now live in eval_safety_test.go, next to the rest of the
// error-construction safety suite, written against the unexported `source`
// field.  They are the same two properties -- the error's location is
// pointer-independent of env.loc, and an in-place write through env.loc does
// not move a position an error already reported -- with the same nil-preserved
// third case.  Two copies of one property is one copy too many, and the
// surviving pair is the one that can still see the field.
//
// TestSingletonSnapshotDetectsNativeLocationDrift (#362) is DELETED, and its
// deletion is the point rather than an omission.  It asserted that
// TakeSingletonSnapshot names "nativeSource()" after a write through a
// constructed value's Source.  There is no such write to make any more:
// defaultSourceLocation is gone, constructors leave source nil, and
// nativeLocation() synthesizes "<native code>" by value on demand.  A
// regression test for a singleton that does not exist passes by doing nothing,
// which is exactly the vacuous guard that family of tests exists to prevent.
// What replaced the guard is the absence of the object -- see the note on
// SingletonSnapshot in singleton.go, which records the same thing where a
// reader adding a fourth singleton would look.

// TestNativeLocationIsValueTyped pins the shape that makes issue #362's fix
// hold: token.NativeLocation returns a VALUE, so a caller that needs a
// Location it may write to cannot accidentally get a shared one -- there is no
// shared one to get.
func TestNativeLocationIsValueTyped(t *testing.T) {
	t.Parallel()

	a := token.NativeLocation()
	b := token.NativeLocation()
	if a != b {
		t.Errorf("NativeLocation is not stable: %+v vs %+v", a, b)
	}
	a.Pos = 7
	if b.Pos == 7 {
		t.Error("writing to a NativeLocation value must not reach any other copy")
	}
	if c := token.NativeLocation(); c.Pos == 7 {
		t.Error("writing to a NativeLocation value reached the next call's result: the location is shared after all (#362)")
	}
}

// TestNativeValuesCarryNoLocation is the other half, and it is what makes the
// test above more than a statement about a struct literal.  A value Go
// constructed must record NO location at all, so that no two such values can
// share one: Source() reports false and synthesizes the "<native code>"
// location by value for printing.
func TestNativeValuesCarryNoLocation(t *testing.T) {
	t.Parallel()

	for _, v := range []*LVal{Int(1), Float(1.5), String("s"), Symbol("sym"), Nil(), Bool(true)} {
		if v.source != nil {
			t.Errorf("%v carries a *token.Location (%+v); Go-constructed values must carry none (#362)", v.Type, v.source)
		}
		loc, ok := v.Source()
		if ok {
			t.Errorf("%v reports a recorded location", v.Type)
		}
		if loc != token.NativeLocation() {
			t.Errorf("%v reports %+v, want the synthetic native location %+v", v.Type, loc, token.NativeLocation())
		}
	}
}
