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
			// The above append reuses excess vector capacity allocated in the
			// previous append to v12, creating v123.  Analogous to go slices,
			// this causes side effects in the value of v1234.  The assumed
			// performance benefit seems valuable but in general (append
			// 'vector ...) should be used sparingly and with care.  The
			// append! function will be easier to reason about.
			{"v1234", "(vector 1 2 3 5)", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
