// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestSort(t *testing.T) {
	tests := elpstest.TestSuite{
		{"sort lists", elpstest.TestSequence{
			// sorting primitive values.  The list is runtime-constructed:
			// stable-sort sorts in place, and a quoted literal input raises
			// modify-literal-error (issue #378; pinned by
			// TestSealedWriteRaisesCatchableCondition).
			{"(set 'lis (list 3 1 2 5 4))", "'(3 1 2 5 4)", ""},
			{"(stable-sort < lis)", "'(1 2 3 4 5)", ""},
			{"(stable-sort < lis identity)", "'(1 2 3 4 5)", ""},
			{"(stable-sort > lis)", "'(5 4 3 2 1)", ""},
			{"(stable-sort < lis -)", "'(5 4 3 2 1)", ""},
			{"(set 'sort-asc #^(stable-sort < %))", "(lambda (%) (stable-sort < %))", ""},
			{"(set 'sort-desc #^(stable-sort > %))", "(lambda (%) (stable-sort > %))", ""},
			{"(sort-asc lis)", "'(1 2 3 4 5)", ""},
			{"(sort-desc lis)", "'(5 4 3 2 1)", ""},
		}},
		{"sort complex lists", elpstest.TestSequence{
			// sorting structured values.  The outer list is
			// runtime-constructed (sorting only permutes its cells; the
			// quoted elements are never written).
			{"(set 'lis (list '(3 'c) '(1 'a) '(2 'b)))", "'('(3 'c) '(1 'a) '(2 'b))", ""},
			{"(stable-sort < lis first)", "'('(1 'a) '(2 'b) '(3 'c))", ""},
		}},
		{"insert-sorted", elpstest.TestSequence{
			// inserting into sorted lists
			{"(set 'lis '(1 2 3 4 5))", "'(1 2 3 4 5)", ""},
			{"(insert-sorted 'list lis < 2.5)", "'(1 2 2.5 3 4 5)", ""},
			{"(insert-sorted 'list lis < 2.5 identity)", "'(1 2 2.5 3 4 5)", ""},
			{"lis", "'(1 2 3 4 5)", ""},
			{"(set 'lis '('(1 'a) '(2 'b) '(3 'c)))", "'('(1 'a) '(2 'b) '(3 'c))", ""},
			{"(insert-sorted 'list lis < '(2.5 'ba) first)", "'('(1 'a) '(2 'b) '(2.5 'ba) '(3 'c))", ""},
		}},
		{"insert-sorted", elpstest.TestSequence{
			// inserting into sorted vectors
			{"(set 'vec (vector 1 2 3 4 5))", "(vector 1 2 3 4 5)", ""},
			{"(insert-sorted 'vector vec < 2.5)", "(vector 1 2 2.5 3 4 5)", ""},
			{"(insert-sorted 'vector vec < 2.5 identity)", "(vector 1 2 2.5 3 4 5)", ""},
			{"vec", "(vector 1 2 3 4 5)", ""},
			{"(set 'vec (vector '(1 'a) '(2 'b) '(3 'c)))", "(vector '(1 'a) '(2 'b) '(3 'c))", ""},
			{"(insert-sorted 'vector vec < '(2.5 'ba) first)", "(vector '(1 'a) '(2 'b) '(2.5 'ba) '(3 'c))", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// TestSortComparatorArgumentsAreTheElements pins that stable-sort's
// predicate and key function, and insert-sorted's predicate, receive the
// list's own elements -- the contract map, foldl and select already have.
// A write through an argument lands on the element in the list.
//
// Before #604 removed it, both builtins deep-copied their arguments on every
// comparison, so these writes landed on ephemeral copies and the elements
// came out untouched; every assertion below failed on that code.  Both
// arguments are marked in each predicate because which element sort.Stable
// and sort.Search pass first is an implementation detail.
func TestSortComparatorArgumentsAreTheElements(t *testing.T) {
	tests := elpstest.TestSuite{
		{"stable-sort predicate", elpstest.TestSequence{
			{`(set 'm1 (sorted-map 'key 1))`, `(sorted-map 'key 1)`, ""},
			{`(set 'm2 (sorted-map 'key 2))`, `(sorted-map 'key 2)`, ""},
			{`(set 'm3 (sorted-map 'key 3))`, `(sorted-map 'key 3)`, ""},
			{`(stable-sort
				(lambda (a b)
					(assoc! a 'seen true)
					(assoc! b 'seen true)
					(< (get a 'key) (get b 'key)))
				(list m3 m1 m2))`,
				`'((sorted-map 'key 1 'seen true) (sorted-map 'key 2 'seen true) (sorted-map 'key 3 'seen true))`, ""},
			{`(map 'list (lambda (m) (key? m 'seen)) (list m1 m2 m3))`, `'(true true true)`, ""},
		}},
		{"stable-sort key function", elpstest.TestSequence{
			{`(set 'm1 (sorted-map 'key 1))`, `(sorted-map 'key 1)`, ""},
			{`(set 'm2 (sorted-map 'key 2))`, `(sorted-map 'key 2)`, ""},
			// The key function sees every element it is asked to key.
			{`(stable-sort < (list m2 m1) (lambda (m) (assoc! m 'keyed true) (get m 'key)))`,
				`'((sorted-map 'key 1 'keyed true) (sorted-map 'key 2 'keyed true))`, ""},
			{`(map 'list (lambda (m) (key? m 'keyed)) (list m1 m2))`, `'(true true)`, ""},
		}},
		{"insert-sorted predicate", elpstest.TestSequence{
			{`(set 'm1 (sorted-map 'key 1))`, `(sorted-map 'key 1)`, ""},
			{`(set 'm2 (sorted-map 'key 2))`, `(sorted-map 'key 2)`, ""},
			// A one-element list is probed exactly once, so both the item
			// and the element are seen by the predicate.
			{`(insert-sorted 'list (list m1)
				(lambda (a b)
					(assoc! a 'seen true)
					(assoc! b 'seen true)
					(< (get a 'key) (get b 'key)))
				m2)`,
				`'((sorted-map 'key 1 'seen true) (sorted-map 'key 2 'seen true))`, ""},
			{`(key? m1 'seen)`, `true`, ""},
			{`(key? m2 'seen)`, `true`, ""},
		}},
		{"insert-sorted key function", elpstest.TestSequence{
			{`(set 'm1 (sorted-map 'key 1))`, `(sorted-map 'key 1)`, ""},
			{`(set 'm2 (sorted-map 'key 2))`, `(sorted-map 'key 2)`, ""},
			{`(insert-sorted 'vector (vector m1) < m2 (lambda (m) (assoc! m 'keyed true) (get m 'key)))`,
				`(vector (sorted-map 'key 1 'keyed true) (sorted-map 'key 2 'keyed true))`, ""},
			{`(key? m1 'keyed)`, `true`, ""},
			{`(key? m2 'keyed)`, `true`, ""},
		}},
		// The corollary: an element that is a sealed program literal is
		// handed over sealed, so a predicate or key function that writes
		// to it raises modify-literal-error and the sort aborts -- the
		// #378 policy (an error, not a silent write, and not a silent
		// copy).  The per-comparison copy cleared the seal, so on the
		// previous code each of these sorted silently, the write landing
		// on a throwaway copy.  handler-bind names the condition without
		// depending on the error's column.
		{"stable-sort predicate writing to a literal element", elpstest.TestSequence{
			{`(set 'xs (list '(2 1) '(1 0)))`, `'('(2 1) '(1 0))`, ""},
			{`(handler-bind ([modify-literal-error (lambda (c &rest args) 'caught)])
				(stable-sort (lambda (a b) (stable-sort < a) (< (first a) (first b))) xs))`, `'caught`, ""},
			// The literals are as written and the list is unsorted: the
			// call aborted at the first comparison.
			{`xs`, `'('(2 1) '(1 0))`, ""},
			// A predicate that only reads the literal elements sorts them.
			{`(stable-sort (lambda (a b) (< (first a) (first b))) xs)`, `'('(1 0) '(2 1))`, ""},
		}},
		{"stable-sort key function writing to a literal element", elpstest.TestSequence{
			{`(set 'xs (list '(2 1) '(1 0)))`, `'('(2 1) '(1 0))`, ""},
			{`(handler-bind ([modify-literal-error (lambda (c &rest args) 'caught)])
				(stable-sort < xs (lambda (a) (stable-sort < a) (first a))))`, `'caught`, ""},
			{`xs`, `'('(2 1) '(1 0))`, ""},
		}},
		{"insert-sorted predicate writing to a literal item", elpstest.TestSequence{
			{`(set 'xs (list (list 1 0)))`, `'('(1 0))`, ""},
			{`(handler-bind ([modify-literal-error (lambda (c &rest args) 'caught)])
				(insert-sorted 'list xs (lambda (a b) (stable-sort < a) (stable-sort < b) (< (first a) (first b))) '(2 1)))`, `'caught`, ""},
			{`xs`, `'('(1 0))`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
