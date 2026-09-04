// Copyright © 2026 The ELPS authors

package hook

import "testing"

// The slot is write-once so that no in-module package can swap the walker
// the alias guard drives — the detach arm is the one that exists because a
// bug hid there for a week (issue #585).  This is the control for that:
// making SetDetach overwrite silently must fail here.
//
// This test binary does not import lisp, so the slot starts empty and the
// first write is the test's own.
func TestSetDetachIsWriteOnce(t *testing.T) {
	first := func() {}
	SetDetach(first)
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
