// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestArray(t *testing.T) {
	tests := elpstest.TestSuite{
		{"vector", elpstest.TestSequence{
			{"(vector)", "(vector)", ""},
			{"(vector 1 2 3)", "(vector 1 2 3)", ""},
			{"(vector (vector 1 2 3))", "(vector (vector 1 2 3))", ""},
			{`(aref (vector 'a 'b 'c) 0)`, "'a", ""},
			{`(aref (vector 1 2 3) 2)`, "3", ""},
			{`(ignore-errors (aref (vector 1 2 3) 3))`, "()", ""},
			{`(ignore-errors (aref (vector 1 2 3) -1))`, "()", ""},
			{"(ignore-errors (nth (vector) -1))", "()", ""},
			{"(length (vector))", "0", ""},
			{"(nth (vector) 0)", "()", ""},
			{"(nth (vector) 1)", "()", ""},
			{"(nth (vector) 2)", "()", ""},
			{"(nth (vector 1) 0)", "1", ""},
			{"(nth (vector 1) 1)", "()", ""},
			{"(nth (vector 1) 2)", "()", ""},
			{"(first (vector))", "()", ""},
			{"(second (vector))", "()", ""},
			{"(rest (vector))", "()", ""},
			{"(first (vector 1))", "1", ""},
			{"(second (vector 1))", "()", ""},
			{"(rest (vector 1))", "()", ""},
			{"(first (vector 1 2))", "1", ""},
			{"(second (vector 1 2))", "2", ""},
			{"(rest (vector 1 2))", "'(2)", ""},
		}},
		{"append!", elpstest.TestSequence{
			{"(set 'v (vector))", "(vector)", ""},
			{"(append! v 1)", "(vector 1)", ""},
			{"(append! v 2)", "(vector 1 2)", ""},
			{"(append! v 3)", "(vector 1 2 3)", ""},
			{"v", "(vector 1 2 3)", ""},
		}},
		{"append 'vector", elpstest.TestSequence{
			{"(set 'v (vector))", "(vector)", ""},
			{"(set 'v1 (append 'vector v 1))", "(vector 1)", ""},
			{"(set 'v12 (append 'vector v1 2))", "(vector 1 2)", ""},
			{"(set 'v123 (append 'vector v12 3))", "(vector 1 2 3)", ""},
			{"(set 'v1234 (append 'vector v123 4))", "(vector 1 2 3 4)", ""},
			{"v", "(vector)", ""},
			{"v1", "(vector 1)", ""},
			{"v12", "(vector 1 2)", ""},
			{"v123", "(vector 1 2 3)", ""},
			{"v1234", "(vector 1 2 3 4)", ""},
			{"(set 'v1235 (append 'vector v123 5))", "(vector 1 2 3 5)", ""},
			// Two appends off the same source are independent (issue #373).
			//
			// This row used to assert (vector 1 2 3 5).  The append above
			// reused excess capacity left in v123 by the append that built
			// it, so it wrote through the shared backing array and rewrote
			// v1234 -- a value `append` had already returned.  The comment
			// here called that an "assumed performance benefit" and told
			// callers to use `append` "sparingly and with care".
			//
			// There was no care that helped: nothing about v1234 said it
			// was still aliased, and `append` is the non-mutating
			// constructor by contract and by docstring.  Producers now
			// clamp the capacity of every view they hand out and `append`
			// clamps its input, so an append needing room reallocates.
			// `append!` is the in-place accumulator and still grows in
			// amortised O(1).
			{"v1234", "(vector 1 2 3 4)", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
