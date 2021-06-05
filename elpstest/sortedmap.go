package elpstest

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
)

// AssertSortedMap runs tests to ensure that m satisfies constraints required
// for sorted maps.  The following properties are tested by AssertSortedMap:
//
//		The m.Keys and m.Entries produce lists with the expected length,
//		m.Len()
//
//		Repeated calls to m.Entries() return equal lists of pairs
//
//		Repeated calls to m.Keys() return equal lists
//
//		The lists returned by m.Keys() and m.Entries() have consistent elements
//		and order.
//
//		Calling m.Get() with a key from m.Entries() returns a value consistent
//		with that entry.
//
// AssertSortedMap does not test any of the following properties:
//
//		Success/Failure of insertions or deletions -- m must already be
//		populated with values.
//
//		Restrictions the implementation places on the types of keys/values.
//
//		Any measure of correctness in the ordering of entries/keys.  The only
//		requirement is that the order be fixed for a given set of key-value
//		pairs.
func AssertSortedMap(t *testing.T, m lisp.Map) bool {
	t.Helper()
	if !assert.NotEqual(t, 0, m.Len(), "Cannot test an empty sorted-map") {
		return false
	}
	if !testFixed(t, "Entries", 3, func() (*lisp.LVal, error) { return mapEntries(m) }) {
		return false
	}
	if !testFixed(t, "Keys", 3, func() (*lisp.LVal, error) { return mapKeys(m) }) {
		return false
	}
	if !testEntryKeyOrder(t, m) {
		return false
	}
	if !testGetEntries(t, m) {
		return false
	}
	return true
}

func testFixed(t *testing.T, method string, n int, fn func() (*lisp.LVal, error)) bool {
	t.Helper()
	expect, err := fn()
	if !assert.NoError(t, err) {
		return false
	}
	for i := 0; i < n; i++ {
		v, err := fn()
		if !assert.NoError(t, err) {
			return false
		}
		res := expect.Equal(v)
		err = lisp.GoError(res)
		if !assert.NoError(t, err) {
			return false
		}
		ok := lisp.True(res)
		if !assert.True(t, ok, "%s got: %v expected: %v", method, v, expect) {
			return false
		}
	}
	return true
}

func testEntryKeyOrder(t *testing.T, m lisp.Map) bool {
	t.Helper()
	entries, err := mapEntries(m)
	if !assert.NoError(t, err) {
		return false
	}
	keys, err := mapKeys(m)
	if !assert.NoError(t, err) {
		return false
	}
	if !assert.Equal(t, entries.Len(), keys.Len()) {
		// already tested indirectly so this is just paranoia
		return false
	}
	for i := range entries.Cells {
		key := entries.Cells[i].Cells[0]
		eq := key.Equal(keys.Cells[i])
		err := lisp.GoError(eq)
		if !assert.NoError(t, err, "Keys and Entries cannot be tested at index %d -- expect: %v got: %v", i, key, keys.Cells[i]) {
			return false
		}
		if !assert.True(t, lisp.True(eq), "Keys and Entries not consistent at index %d -- expect: %v got: %v", i, key, keys.Cells[i]) {
			return false
		}
	}
	return true
}

func testGetEntries(t *testing.T, m lisp.Map) bool {
	t.Helper()
	entries, err := mapEntries(m)
	if !assert.NoError(t, err) {
		return false
	}
	for i := range entries.Cells {
		pair := entries.Cells[i]
		key, val := pair.Cells[0], pair.Cells[1]
		v, ok := m.Get(key)
		if !assert.True(t, ok, "Entries %d was not found in map: %v", i, key) {
		}
		eq := val.Equal(v)
		err := lisp.GoError(eq)
		if !assert.NoError(t, err, "Entry for key %v cannot be tested at index %d -- expected: %v got: %v", key, i, val, v) {
			return false
		}
		if !assert.True(t, lisp.True(eq), "Entry for key %v not consistent at index %d -- expected: %v got: %v", key, i, val, v) {
			return false
		}
	}
	return true
}

func mapKeys(m lisp.Map) (*lisp.LVal, error) {
	v := m.Keys()
	err := lisp.GoError(v)
	if err != nil {
		return nil, err
	}
	if v.Type != lisp.LSExpr {
		return nil, fmt.Errorf("Keys did not return a list")
	}
	if v.Len() != m.Len() {
		return nil, fmt.Errorf("Keys has an invalid length")
	}
	return v, nil
}

func mapEntries(m lisp.Map) (*lisp.LVal, error) {
	cells := make([]*lisp.LVal, m.Len())
	res := m.Entries(cells)
	err := lisp.GoError(res)
	if err != nil {
		return nil, err
	}
	if res.Type != lisp.LInt || res.Int != len(cells) {
		err := fmt.Errorf("Entries returned %v (expected %d)", res, len(cells))
		return nil, err
	}
	for i := range cells {
		if cells[i].Type != lisp.LSExpr || cells[i].Len() != 2 {
			return nil, fmt.Errorf("Entries index %d is not valid : %v", i, cells[i])
		}
	}
	return lisp.QExpr(cells), nil
}
