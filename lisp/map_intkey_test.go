// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// TestSortedMapIntKeys pins the semantics of integer keys (#733):
//
//   - an int key is its own identity, distinct from the string and the
//     symbol that spell the same digits;
//   - int keys order first, numerically, then string and symbol keys in
//     their existing order;
//
// The JSON half of the decision is pinned in lisp/lisplib/libjson.
func TestSortedMapIntKeys(t *testing.T) {
	tests := elpstest.TestSuite{
		{"construct and order", elpstest.TestSequence{
			{`(sorted-map 2 "b" 1 "a")`, `(sorted-map 1 "a" 2 "b")`, ""},
			{`(sorted-map "b" 1 10 2 -3 3 'a 4 0 5)`, `(sorted-map -3 3 0 5 10 2 'a 4 "b" 1)`, ""},
			{`(keys (sorted-map "b" 1 10 2 -3 3 'a 4))`, `'(-3 10 'a "b")`, ""},
			{`(sorted-map 9 1 10 2 100 3)`, `(sorted-map 9 1 10 2 100 3)`, ""},
		}},
		{"identity", elpstest.TestSequence{
			{`(set 'm (sorted-map 1 "int" "1" "string"))`, `(sorted-map 1 "int" "1" "string")`, ""},
			{`(get m 1)`, `"int"`, ""},
			{`(get m "1")`, `"string"`, ""},
			{`(get (sorted-map 1 "int") "1")`, `()`, ""},
			{`(get (sorted-map "1" "string") 1)`, `()`, ""},
			{`(key? (sorted-map 1 2) 1)`, `true`, ""},
			{`(key? (sorted-map 1 2) "1")`, `false`, ""},
			{`(length m)`, `2`, ""},
			{`(sorted-map 1 "a" 1 "b")`, `(sorted-map 1 "b")`, ""},
		}},
		{"assoc dissoc", elpstest.TestSequence{
			{`(set 'm (sorted-map "a" 1))`, `(sorted-map "a" 1)`, ""},
			{`(assoc m 5 "five")`, `(sorted-map 5 "five" "a" 1)`, ""},
			{`m`, `(sorted-map "a" 1)`, ""},
			{`(assoc! m 7 "seven")`, `(sorted-map 7 "seven" "a" 1)`, ""},
			{`(assoc! m 7 "SEVEN")`, `(sorted-map 7 "SEVEN" "a" 1)`, ""},
			{`(dissoc m 7)`, `(sorted-map "a" 1)`, ""},
			{`(get m 7)`, `"SEVEN"`, ""},
			{`(dissoc! m 7)`, `(sorted-map "a" 1)`, ""},
			{`(dissoc! m 7)`, `(sorted-map "a" 1)`, ""},
			{`(assoc () 3 4)`, `(sorted-map 3 4)`, ""},
		}},
		{"equality and copy", elpstest.TestSequence{
			{`(equal? (sorted-map 1 2 "a" 3) (sorted-map "a" 3 1 2))`, `true`, ""},
			{`(equal? (sorted-map 1 2) (sorted-map "1" 2))`, `false`, ""},
			{`(equal? (sorted-map 1 2) (sorted-map 1 3))`, `false`, ""},
			{`(set 'm (sorted-map 1 (vector 1) "a" 2))`, `(sorted-map 1 (vector 1) "a" 2)`, ""},
			{`(set 'c (copy m))`, `(sorted-map 1 (vector 1) "a" 2)`, ""},
			{`(assoc! c 2 3)`, `(sorted-map 1 (vector 1) 2 3 "a" 2)`, ""},
			{`m`, `(sorted-map 1 (vector 1) "a" 2)`, ""},
			{`(equal? m (copy m))`, `true`, ""},
		}},
		{"iteration", elpstest.TestSequence{
			{`(map 'list (lambda (k) (get (sorted-map 3 "c" 1 "a" "z" "last") k)) (keys (sorted-map 3 "c" 1 "a" "z" "last")))`,
				`'("a" "c" "last")`, ""},
		}},
		{"unsupported keys", elpstest.TestSequence{
			{`(sorted-map 1.5 2)`, `test:1:1: unhashable type: float`, ""},
			{`(get (sorted-map) 1.5)`, `test:1:1: unhashable type: float`, ""},
			{`(assoc (sorted-map) 1.5 1)`, `test:1:1: lisp:assoc: <native code>: unhashable type: float`, ""},
			{`(assoc! (sorted-map) 1.5 1)`, `test:1:1: lisp:assoc!: <native code>: unhashable type: float`, ""},
			{`(key? (sorted-map 1 1) 1.0)`, `test:1:1: lisp:key?: unhashable type: float`, ""},
		}},
		{"string-keyed maps now answer int lookups", elpstest.TestSequence{
			{`(get (sorted-map "1" "x") 1)`, `()`, ""},
			{`(key? (sorted-map "1" "x") 1)`, `false`, ""},
			{`(dissoc (sorted-map "1" "x") 1)`, `(sorted-map "1" "x")`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
