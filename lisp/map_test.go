// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
)

func TestMapImpl(t *testing.T) {
	m := lisp.SortedMap().Map()
	m.Set(lisp.String("a"), lisp.Int(1))
	m.Set(lisp.Symbol("b"), lisp.Int(2))
	m.Set(lisp.String("c"), lisp.Int(3))
	elpstest.AssertSortedMap(t, m)
}

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

// TestSortedMapCopy verifies that LVal.Copy() on a sorted-map produces
// a structurally independent copy. Mutations via assoc!/dissoc! on
// either side must not affect the other.
func TestSortedMapCopy(t *testing.T) {
	original := lisp.SortedMap()
	original.Map().Set(lisp.Symbol("a"), lisp.Int(1))
	original.Map().Set(lisp.Symbol("b"), lisp.Int(2))

	cp := original.Copy()

	// Verify the copy has all the same contents.
	assert.Equal(t, original.Map().Len(), cp.Map().Len())
	vA, okA := cp.Map().Get(lisp.Symbol("a"))
	assert.True(t, okA)
	assert.Equal(t, 1, vA.Int)
	vB, okB := cp.Map().Get(lisp.Symbol("b"))
	assert.True(t, okB)
	assert.Equal(t, 2, vB.Int)

	// Mutate the COPY — the original must be unaffected.
	cp.Map().Set(lisp.Symbol("a"), lisp.Int(99))
	origVal, _ := original.Map().Get(lisp.Symbol("a"))
	assert.Equal(t, 1, origVal.Int,
		"mutating the copy should not affect the original")

	// Mutate the ORIGINAL — the copy must be unaffected.
	original.Map().Set(lisp.Symbol("b"), lisp.Int(777))
	cpVal, _ := cp.Map().Get(lisp.Symbol("b"))
	assert.Equal(t, 2, cpVal.Int,
		"mutating the original should not affect the copy")

	// Delete from the COPY — the original must retain the key.
	cp.Map().Del(lisp.Symbol("b"))
	assert.Equal(t, 2, original.Map().Len(),
		"deleting from the copy should not affect the original")
	origB, origBOk := original.Map().Get(lisp.Symbol("b"))
	assert.True(t, origBOk, "key 'b' should still exist in original")
	assert.Equal(t, 777, origB.Int)
}

// TestSortedMapEqual verifies that Equal() correctly compares sorted-maps
// by their entries.
func TestSortedMapEqual(t *testing.T) {
	m1 := lisp.SortedMap()
	m1.Map().Set(lisp.Symbol("a"), lisp.Int(1))
	m1.Map().Set(lisp.Symbol("b"), lisp.Int(2))

	m2 := lisp.SortedMap()
	m2.Map().Set(lisp.Symbol("a"), lisp.Int(1))
	m2.Map().Set(lisp.Symbol("b"), lisp.Int(2))

	// Empty maps.
	assert.True(t, lisp.True(lisp.SortedMap().Equal(lisp.SortedMap())),
		"empty sorted-maps should be equal")

	// Self-equality (reflexivity).
	assert.True(t, lisp.True(m1.Equal(m1)),
		"a map should be equal to itself")

	// Identical contents.
	assert.True(t, lisp.True(m1.Equal(m2)),
		"identical sorted-maps should be equal")

	// Different values.
	m3 := lisp.SortedMap()
	m3.Map().Set(lisp.Symbol("a"), lisp.Int(999))
	m3.Map().Set(lisp.Symbol("b"), lisp.Int(2))
	assert.True(t, lisp.Not(m1.Equal(m3)),
		"maps with different values should not be equal")

	// Different keys.
	m4 := lisp.SortedMap()
	m4.Map().Set(lisp.Symbol("x"), lisp.Int(1))
	m4.Map().Set(lisp.Symbol("b"), lisp.Int(2))
	assert.True(t, lisp.Not(m1.Equal(m4)),
		"maps with different keys should not be equal")

	// Same keys, different value types.
	m5 := lisp.SortedMap()
	m5.Map().Set(lisp.Symbol("a"), lisp.String("1"))
	m5.Map().Set(lisp.Symbol("b"), lisp.Int(2))
	assert.True(t, lisp.Not(m1.Equal(m5)),
		"maps with same keys but different value types should not be equal")

	// Cross-type: map vs non-map.
	assert.True(t, lisp.Not(m1.Equal(lisp.Nil())),
		"map should not be equal to nil")
	assert.True(t, lisp.Not(m1.Equal(lisp.Int(42))),
		"map should not be equal to an integer")
}

// TestSortedMapCopyViaSort verifies that Copy() produces an independent map
// when used internally by stable-sort. The comparison function mutates its
// Copy()'d argument via assoc!, which must NOT bleed through to the original.
func TestSortedMapCopyViaSort(t *testing.T) {
	tests := elpstest.TestSuite{
		{"sort copy isolation", elpstest.TestSequence{
			{`(set 'm1 (sorted-map 'key 1))`, `(sorted-map 'key 1)`, ""},
			{`(set 'm2 (sorted-map 'key 2))`, `(sorted-map 'key 2)`, ""},

			// The comparison function mutates its first argument (a
			// Copy()'d element). This mutation must stay on the ephemeral
			// copy and not affect the originals.
			{`(stable-sort
				(lambda (a b)
					(assoc! a 'poisoned true)
					(< (get a 'key) (get b 'key)))
				(list m1 m2))`, `'((sorted-map 'key 1) (sorted-map 'key 2))`, ""},

			// Originals must be unmodified.
			{`(key? m1 'poisoned)`, `false`, ""},
			{`(key? m2 'poisoned)`, `false`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// TestSortedMapEqualELPS verifies sorted-map equality from the ELPS level.
func TestSortedMapEqualELPS(t *testing.T) {
	tests := elpstest.TestSuite{
		{"sorted-map equal?", elpstest.TestSequence{
			// Empty maps.
			{`(equal? (sorted-map) (sorted-map))`, `true`, ""},

			// Identical non-empty maps.
			{`(equal? (sorted-map 'a 1) (sorted-map 'a 1))`, `true`, ""},
			{`(equal? (sorted-map 'a 1 'b 2) (sorted-map 'a 1 'b 2))`, `true`, ""},

			// Different values.
			{`(equal? (sorted-map 'a 1) (sorted-map 'a 2))`, `false`, ""},
			// Different keys.
			{`(equal? (sorted-map 'a 1) (sorted-map 'b 1))`, `false`, ""},
			// Different lengths.
			{`(equal? (sorted-map 'a 1) (sorted-map 'a 1 'b 2))`, `false`, ""},

			// Nested map equality.
			{`(equal? (sorted-map 'a (sorted-map 'x 1))
			         (sorted-map 'a (sorted-map 'x 1)))`, `true`, ""},
			{`(equal? (sorted-map 'a (sorted-map 'x 1))
			         (sorted-map 'a (sorted-map 'x 2)))`, `false`, ""},

			// Cross-type: map vs non-map.
			{`(equal? (sorted-map) ())`, `false`, ""},
			{`(equal? (sorted-map 'a 1) 42)`, `false`, ""},
			{`(equal? (sorted-map 'a 1) "hello")`, `false`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
