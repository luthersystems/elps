// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestLazyTable checks a paged table against a flat slice at the sizes
// where the page arithmetic can go wrong: empty, one slot, one slot either
// side of a page boundary, and several pages with a short last page.
func TestLazyTable(t *testing.T) {
	for _, bits := range []uint8{lazyPageBits1, lazyPageBits2, lazyPageBits3} {
		page := 1 << bits
		for _, n := range []int{0, 1, page - 1, page, page + 1, 3*page - 1, 3 * page, 3*page + 1, 10*page + 7} {
			var table lazyTable[int]
			table.init(n, bits)
			flat := make([]int, n)
			for i := range n {
				require.Zero(t, table.get(i), "bits %d, n %d: slot %d before any store", bits, n, i)
			}
			// Store out of order, so that a page is allocated by a store
			// into its middle and into its last slot.
			for k := range n {
				i := (k*7 + 3) % n
				if k%3 == 0 {
					continue
				}
				flat[i] = i + 1
				table.set(i, i+1)
			}
			for i := range n {
				require.Equal(t, flat[i], table.get(i), "bits %d, n %d: slot %d", bits, n, i)
			}
			slots := 0
			for _, p := range table.pages {
				assert.LessOrEqual(t, len(p), page)
				slots += len(p)
			}
			assert.LessOrEqual(t, slots, n, "bits %d, n %d: the pages hold more slots than the plan has", bits, n)
		}
	}
}

// TestLazyTablePagesFollowUse pins what the paging is for: a VM that reads
// one binding of a large plan allocates the pages that binding lands in,
// not a slot for each object of the plan.
func TestLazyTablePagesFollowUse(t *testing.T) {
	vm, err := lazyFixture(t).NewVM()
	require.NoError(t, err)
	inst := lazyInstanceOf(vm)
	require.NotNil(t, inst)
	allocated := func() int {
		n := 0
		for _, p := range inst.values.pages {
			if p != nil {
				n++
			}
		}
		return n
	}
	require.Zero(t, allocated(), "a new VM has built no value")
	got := vm.Eval(Symbol("lib:v5"))
	require.Equal(t, LSExpr, got.Type, "%v", got)
	assert.Positive(t, allocated())
	if len(inst.values.pages) > 2 {
		assert.Less(t, allocated(), len(inst.values.pages), "one read allocated every page")
	}
}
