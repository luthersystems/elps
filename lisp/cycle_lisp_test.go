// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// TestCyclicValues drives the guard from lisp, which is where issue #390 was
// reported: 32 bytes of lisp took the host process down with a stack overflow
// that recover() cannot catch.  The Go-level tests live in lisp/cycle_test.go;
// these pin what a program actually sees.
func TestCyclicValues(t *testing.T) {
	tests := elpstest.TestSuite{
		{"printing a value that contains itself", elpstest.TestSequence{
			// The repro from the issue.
			{`(set 'm (sorted-map))`, `(sorted-map)`, ``},
			{`(assoc! m "k" m)`, `(sorted-map "k" #<cycle>)`, ``},
			{`m`, `(sorted-map "k" #<cycle>)`, ``},
			{`(debug-print m)`, `()`, "(sorted-map \"k\" #<cycle>)\n"},
			{`(format-string "{}" m)`, `"(sorted-map \"k\" #<cycle>)"`, ``},
		}},
		{"printing a vector that contains itself", elpstest.TestSequence{
			{`(set 'v (vector 1))`, `(vector 1)`, ``},
			{`(append! v v)`, `(vector 1 #<cycle>)`, ``},
			// A cycle reached through more than one key or index is bounded
			// once per occurrence, not unrolled.
			{`(append! v v)`, `(vector 1 #<cycle> #<cycle>)`, ``},
		}},
		{"equality of values that contain themselves", elpstest.TestSequence{
			{`(set 'a (sorted-map))`, `(sorted-map)`, ``},
			{`(assoc! a "k" a)`, `(sorted-map "k" #<cycle>)`, ``},
			{`(set 'b (sorted-map))`, `(sorted-map)`, ``},
			{`(assoc! b "k" b)`, `(sorted-map "k" #<cycle>)`, ``},
			{`(equal? a a)`, `true`, ``},
			// a and b are distinct values with the same infinite unfolding.
			{`(equal? a b)`, `true`, ``},
			// A difference at a finite depth is still reported: termination is
			// not bought by assuming equality.
			{`(assoc! b "other" 1)`, `(sorted-map "k" #<cycle> "other" 1)`, ``},
			{`(equal? a b)`, `false`, ``},
			{`(equal? b a)`, `false`, ``},
		}},
		{"acyclic values are unaffected", elpstest.TestSequence{
			// Shared substructure is a DAG, not a cycle, and renders in full.
			{`(set 'x (sorted-map "a" 1))`, `(sorted-map "a" 1)`, ``},
			{`(list x x)`, `'((sorted-map "a" 1) (sorted-map "a" 1))`, ``},
			{`(equal? (list x x) (list x x))`, `true`, ``},
			{`(vector x x)`, `(vector (sorted-map "a" 1) (sorted-map "a" 1))`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
