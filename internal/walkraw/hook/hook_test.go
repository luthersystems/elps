// Copyright © 2026 The ELPS authors

package hook

import "testing"

// The slot is write-once so that no in-module package can swap the walker
// the alias guard drives — the detach arm is the one that exists because a
// bug hid there for a week (issue #585).  This is the control for that:
// making SetDetach overwrite silently must fail here.
//
// The slot may or may not already hold a value: this test binary does not
// import lisp, so the first run finds it empty, but `go test -count=2`
// re-runs the function in the same process and would otherwise panic on
// its own first write and read as a real defect.  So claim the slot only
// if it is free, then assert that a further write panics either way.
func TestSetDetachIsWriteOnce(t *testing.T) {
	if Detach() == nil {
		SetDetach(func() {})
	}
	if Detach() == nil {
		t.Fatal("the accessor was not stored")
	}
	defer func() {
		if recover() == nil {
			t.Error("a second SetDetach did not panic.\n" +
				"The slot accepts an overwrite again, so an in-module package can swap the walker\n" +
				"the alias guard drives and blind the detach arm silently.")
		}
	}()
	SetDetach(func() {})
}
