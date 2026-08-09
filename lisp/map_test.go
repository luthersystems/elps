// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"sort"
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

// TestSortedMapEqualIgnoresKeySpelling pins that sorted-map equality follows
// the same key identity as lookup.
//
// docs/lang.md documents that a sorted-map key may be written as a string or
// a symbol and that "looking up values by key can be done with either a
// string or a symbol, regardless which type was used to insert/set the value
// originally" — get, key?, assoc and dissoc all honour that. Equal() used to
// compare the reconstructed key LVals with LVal.Equal, which is type
// sensitive, so it disagreed with every one of those accessors.
//
// The map's remembered key spelling is also sticky: Set with a string key
// does not clear a symbol spelling recorded by an earlier Set. That made
// equality depend on a map's construction *history* rather than its
// contents — two maps holding identical entries compared unequal purely
// because one of them had once been keyed with a symbol.
func TestSortedMapEqualIgnoresKeySpelling(t *testing.T) {
	strKeyed := lisp.SortedMap()
	strKeyed.Map().Set(lisp.String("a"), lisp.Int(1))

	symKeyed := lisp.SortedMap()
	symKeyed.Map().Set(lisp.Symbol("a"), lisp.Int(1))

	// Every documented accessor agrees these hold the same entry.
	for _, key := range []*lisp.LVal{lisp.String("a"), lisp.Symbol("a")} {
		s, sok := strKeyed.Map().Get(key)
		y, yok := symKeyed.Map().Get(key)
		assert.True(t, sok && yok, "both maps must contain key %v", key)
		assert.True(t, lisp.True(s.Equal(y)), "both maps must map %v to the same value", key)
	}
	assert.Equal(t, strKeyed.Map().Len(), symKeyed.Map().Len())

	assert.True(t, lisp.True(strKeyed.Equal(symKeyed)),
		`(sorted-map "a" 1) and (sorted-map 'a 1) must be equal?`)
	assert.True(t, lisp.True(symKeyed.Equal(strKeyed)),
		"equality must be symmetric")

	// Sticky key spelling: overwriting a symbol-keyed entry with a string
	// key leaves the symbol spelling in place. Equality must not see it.
	historyDependent := lisp.SortedMap()
	historyDependent.Map().Set(lisp.Symbol("a"), lisp.Int(0))
	historyDependent.Map().Set(lisp.String("a"), lisp.Int(1))
	assert.True(t, lisp.True(historyDependent.Equal(strKeyed)),
		"equality must depend on contents, not on construction history")

	// Different key names still differ.
	other := lisp.SortedMap()
	other.Map().Set(lisp.Symbol("b"), lisp.Int(1))
	assert.True(t, lisp.Not(strKeyed.Equal(other)),
		"keys with different names must not be equal")
}

// TestSortedMapKeySpellingIsStickyOnSymbol pins ACCEPTED behaviour, not a bug.
// It exists so the next reader does not rediscover it as one.
//
// A sorted-map remembers whether each key was last *seen* as a symbol, and
// that memory is monotonic within an entry's lifetime: any symbol write turns
// it on, a string write never turns it off, and only deleting the entry
// clears it. Measured:
//
//	(sorted-map "a" 1)                            => (sorted-map "a" 1)
//	(sorted-map 'a 0)  + (assoc! m "a" 1)         => (sorted-map 'a 1)   <- string write does NOT clear
//	(sorted-map "a" 0) + (assoc! m 'a 1)          => (sorted-map 'a 1)   <- symbol write sets it
//	(sorted-map 'a 0)  + dissoc + (assoc! "a" 1)  => (sorted-map "a" 1)  <- delete clears it
//
// So the displayed spelling reflects the entry's history rather than its last
// write. That is arbitrary rather than designed, but it is only ever
// COSMETIC: it reaches `keys` and printing, and nothing else. Lookup has
// always ignored key spelling (docs/lang.md), and as of
// TestSortedMapEqualIgnoresKeySpelling so does equality -- which is the bug
// that was actually fixed. Nothing in the language depends on which spelling
// comes back.
//
// Changing this would alter `keys` and printed output for every existing
// program, which is a far larger blast radius than the cosmetic inconsistency
// justifies. If that trade is ever revisited, this test is the record of what
// the behaviour was and why it was left alone.
func TestSortedMapKeySpellingIsStickyOnSymbol(t *testing.T) {
	symbolKey := lisp.Symbol("a")
	stringKey := lisp.String("a")

	// spelling reports the type of the single key returned by Keys().
	spelling := func(m *lisp.LVal) lisp.LType {
		keys := m.Map().Keys()
		if len(keys.Cells) != 1 {
			t.Fatalf("expected exactly one key, got %d", len(keys.Cells))
		}
		return keys.Cells[0].Type
	}

	neverSymbol := lisp.SortedMap()
	neverSymbol.Map().Set(stringKey, lisp.Int(1))
	assert.Equal(t, lisp.LString, spelling(neverSymbol),
		"a key only ever written as a string stays a string")

	stringWriteDoesNotClear := lisp.SortedMap()
	stringWriteDoesNotClear.Map().Set(symbolKey, lisp.Int(0))
	stringWriteDoesNotClear.Map().Set(stringKey, lisp.Int(1))
	assert.Equal(t, lisp.LSymbol, spelling(stringWriteDoesNotClear),
		"overwriting with a string key does NOT clear the remembered symbol spelling")

	symbolWriteSets := lisp.SortedMap()
	symbolWriteSets.Map().Set(stringKey, lisp.Int(0))
	symbolWriteSets.Map().Set(symbolKey, lisp.Int(1))
	assert.Equal(t, lisp.LSymbol, spelling(symbolWriteSets),
		"a symbol write sets the remembered spelling")

	deleteClears := lisp.SortedMap()
	deleteClears.Map().Set(symbolKey, lisp.Int(0))
	deleteClears.Map().Del(stringKey)
	deleteClears.Map().Set(stringKey, lisp.Int(1))
	assert.Equal(t, lisp.LString, spelling(deleteClears),
		"deleting the entry clears the remembered spelling")

	// The whole point: none of the above is observable through equality.
	// Every map here holding value 1 must be equal? to every other, however
	// its key was spelled along the way.
	for _, m := range []*lisp.LVal{stringWriteDoesNotClear, symbolWriteSets, deleteClears} {
		assert.True(t, lisp.True(neverSymbol.Equal(m)),
			"key spelling must not be observable through equality")
	}
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

			// Key spelling is not part of key identity: get/key?/assoc all
			// accept either spelling for the same entry, so equal? must
			// too. See TestSortedMapEqualIgnoresKeySpelling.
			{`(equal? (sorted-map "a" 1) (sorted-map 'a 1))`, `true`, ""},
			{`(equal? (sorted-map "a" 1 'b 2) (sorted-map 'a 1 "b" 2))`, `true`, ""},
			{`(equal? (sorted-map "a" 1) (sorted-map 'a 2))`, `false`, ""},
			{`(equal? (sorted-map "a" 1) (sorted-map 'b 1))`, `false`, ""},
			// Equality follows contents, not construction history.
			{`(set 'mh (sorted-map 'a 0))`, `(sorted-map 'a 0)`, ""},
			{`(assoc! mh "a" 1)`, `(sorted-map 'a 1)`, ""},
			{`(equal? mh (sorted-map "a" 1))`, `true`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// intKeyedMap is a minimal custom lisp.Map backed by integer keys. Map is an
// exported interface and SortedMapFromData an exported extension point, so an
// embedder can legitimately build one of these — the stock sortedmap's
// string/symbol-only key restriction is not part of the Map contract, and
// elpstest.AssertSortedMap does not test for it.
type intKeyedMap struct{ m map[int]*lisp.LVal }

func newIntKeyedMap(pairs map[int]string) *lisp.LVal {
	im := &intKeyedMap{m: make(map[int]*lisp.LVal, len(pairs))}
	for k, v := range pairs {
		im.m[k] = lisp.String(v)
	}
	return lisp.SortedMapFromData(&lisp.MapData{Map: im})
}

func (im *intKeyedMap) Len() int { return len(im.m) }

func (im *intKeyedMap) Get(k *lisp.LVal) (*lisp.LVal, bool) {
	v, ok := im.m[k.Int]
	if !ok {
		return lisp.Nil(), false
	}
	return v, true
}

func (im *intKeyedMap) Set(k, v *lisp.LVal) *lisp.LVal {
	im.m[k.Int] = v
	return lisp.Nil()
}

func (im *intKeyedMap) Del(k *lisp.LVal) *lisp.LVal {
	delete(im.m, k.Int)
	return lisp.Nil()
}

func (im *intKeyedMap) Keys() *lisp.LVal {
	ks := make([]int, 0, len(im.m))
	for k := range im.m {
		ks = append(ks, k)
	}
	sort.Ints(ks)
	cells := make([]*lisp.LVal, len(ks))
	for i, k := range ks {
		cells[i] = lisp.Int(k)
	}
	return lisp.QExpr(cells)
}

func (im *intKeyedMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	keys := im.Keys()
	for i, k := range keys.Cells {
		v, _ := im.Get(k)
		buf[i] = lisp.QExpr([]*lisp.LVal{k, v})
	}
	return lisp.Int(len(im.m))
}

// TestSortedMapEqualNonStringKeys pins that the key-name comparison used for
// string-like keys is NOT applied to key types that carry no name.
//
// Comparing keys by .Str unconditionally is safe for the stock sortedmap,
// which rejects anything but LString/LSymbol. But a custom lisp.Map may be
// keyed by integers or tuples, whose .Str is always "" — so every key would
// compare equal to every other, and two structurally different maps would
// report as equal?. That is a silent wrong answer in a public API: an
// embedder comparing snapshots for change detection would see "no change"
// and skip a write.
func TestSortedMapEqualNonStringKeys(t *testing.T) {
	a := newIntKeyedMap(map[int]string{1: "x", 2: "y"})
	b := newIntKeyedMap(map[int]string{7: "x", 9: "y"})

	// The maps are plainly different through their keys.
	assert.Equal(t, `'(1 2)`, a.Map().Keys().String())
	assert.Equal(t, `'(7 9)`, b.Map().Keys().String())

	assert.True(t, lisp.Not(a.Equal(b)),
		"integer-keyed maps with different keys must not be equal")
	assert.True(t, lisp.Not(b.Equal(a)), "inequality must be symmetric")

	// Same keys and values still compare equal.
	assert.True(t, lisp.True(a.Equal(newIntKeyedMap(map[int]string{1: "x", 2: "y"}))),
		"integer-keyed maps with identical entries must be equal")

	// Same keys, different values.
	assert.True(t, lisp.Not(a.Equal(newIntKeyedMap(map[int]string{1: "x", 2: "z"}))),
		"differing values must not be equal")

	// And the string-like rule still applies where it was reasoned about.
	strKeyed := lisp.SortedMap()
	strKeyed.Map().Set(lisp.String("a"), lisp.Int(1))
	symKeyed := lisp.SortedMap()
	symKeyed.Map().Set(lisp.Symbol("a"), lisp.Int(1))
	assert.True(t, lisp.True(strKeyed.Equal(symKeyed)),
		"string-like keys must still compare by name")
}
