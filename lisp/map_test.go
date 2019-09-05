// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestMaps(t *testing.T) {
	tests := elpstest.TestSuite{
		{"value association", elpstest.TestSequence{
			// Test that maps associate values property.  Furthermore, it must
			// be ensured that ``assoc!'' mutates its argument while ``assoc''
			// returns a copy.
			{"(set 'm (sorted-map 'b 1 'a 2))", "(sorted-map 'a 2 'b 1)", ""},
			{"(assoc! m 'a 0)", "(sorted-map 'a 0 'b 1)", ""},
			{"(assoc m 'b 2)", "(sorted-map 'a 0 'b 2)", ""},
			{"(get (assoc m 'b 2) 'b)", "2", ""},
			{"(get () 'a)", "()", ""},
			{`(get m "b")`, "1", ""},
			{`(assoc! m "abc" 2)`, `(sorted-map 'a 0 "abc" 2 'b 1)`, ""},
			{`(get m 'a)`, "0", ""},
			{`(get m 'abc)`, "2", ""},
			{`(get m 'b)`, "1", ""},
		}},
		{"value dissociation", elpstest.TestSequence{
			// Test that maps dissociate values property.  Furthermore, it must
			// be ensured that ``dissoc!'' mutates its argument while ``dissoc''
			// returns a copy.
			{"(set 'm (sorted-map 'b 1 'a 2))", "(sorted-map 'a 2 'b 1)", ""},
			{"(dissoc! m 'a)", "(sorted-map 'b 1)", ""},
			{"(dissoc! m 'a)", "(sorted-map 'b 1)", ""},
			{"(dissoc m 'b)", "(sorted-map)", ""},
			{`(get m 'b)`, "1", ""},
		}},
		{"order", elpstest.TestSequence{
			// Test that the default representation of a map always has sorted
			// keys and that the ``keys'' builtin returns a properly sorted
			// list.  That is, without numeric keys the expression (map
			// to-string (keys m)) will always be sorted list.
			{`(keys (sorted-map))`, `'()`, ""},
			{`(keys (sorted-map 'a 0))`, `'('a)`, ""},
			{`(keys (sorted-map "a" 0))`, `'("a")`, ""},
			{`(set 'm (sorted-map 'a 0))`, `(sorted-map 'a 0)`, ""},
			{`(assoc! m 'b 1)`, `(sorted-map 'a 0 'b 1)`, ""},
			{`(assoc! m "abc" 2)`, `(sorted-map 'a 0 "abc" 2 'b 1)`, ""},
			{"(keys m)", `'('a "abc" 'b)`, ""},
			{`(assoc! m "a" 3)`, `(sorted-map 'a 3 "abc" 2 'b 1)`, ""},
			{"(keys m)", `'('a "abc" 'b)`, ""},
			{`(dissoc! m "a")`, `(sorted-map "abc" 2 'b 1)`, ""},
			{"(keys m)", `'("abc" 'b)`, ""},
		}},
		{"key?", elpstest.TestSequence{
			// Test map membership.
			{`(key? (sorted-map) "a")`, `false`, ""},
			{`(key? (sorted-map 'a 0) 'a)`, `true`, ""},
			{`(key? (sorted-map 'a 0) "a")`, `true`, ""},
			{`(key? (sorted-map 'a 0) "b")`, `false`, ""},
		}},
		{"size", elpstest.TestSequence{
			{`(empty? (sorted-map))`, `true`, ""},
			{`(empty? (sorted-map 'a 1))`, `false`, ""},
			{`(length (sorted-map))`, `0`, ""},
			{`(length (sorted-map 'a 1))`, `1`, ""},
		}},
		{"get-default", elpstest.TestSequence{
			// Test the get-default macro.  Ensure that the macro performs a
			// lazy evaluation of the default expression.
			{`(get-default (sorted-map) "a" "default")`, `"default"`, ""},
			{`(get-default (sorted-map 'a 0) "a" "default")`, `0`, ""},
			{`(get-default (sorted-map 'b 1) "a" "default")`, `"default"`, ""},
			{`(set 'm (sorted-map))`, `(sorted-map)`, ""},
			{`(get-default m "a" (and (assoc! m "a" 0) "default"))`, `"default"`, ""},
			{`(get-default m "a" "default")`, `0`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
