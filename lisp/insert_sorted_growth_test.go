// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// resultCells returns the storage a sequence result actually holds, which is
// the array's backing for a vector and the cells themselves for a list.  It
// deliberately does not go through Len(): a vector's cardinality is recorded
// in its dimensions, and the defect under test is precisely a result whose
// dimensions and backing disagree.
func resultCells(t *testing.T, v *lisp.LVal) []*lisp.LVal {
	t.Helper()
	if v.Type == lisp.LArray {
		require.Len(t, v.Cells, 2, "an array is dimensions and backing: %v", v.Type)
		return v.Cells[1].Cells
	}
	require.Equal(t, lisp.LSExpr, v.Type, "unexpected result type: %v", v.Type)
	return v.Cells
}

// TestInsertSortedSizesResultFromItsInputSnapshot pins that insert-sorted
// builds a result sized from the input it actually read, not from whatever
// the input has become by the time the callbacks are done with it.
//
// insert-sorted snapshots the input's cells before the binary search and
// copies out of that snapshot afterwards.  It used to size the result from
// the LIVE sequence (list.Len()), read after every comparator and key call
// had run.  A callback that appends to the sequence being inserted into --
// which nothing forbids, and which the callback can reach through any binding
// -- therefore left trailing slots of the result with nothing copied into
// them: Go-nil *LVal cells for 'list, which panic the interpreter on first
// use and surface as an uncatchable internal-panic, and spurious () cells for
// 'vector, whose dimensions then disagree with the elements it holds.
func TestInsertSortedSizesResultFromItsInputSnapshot(t *testing.T) {
	const size = 8
	for _, typespec := range []string{"list", "vector"} {
		for _, route := range []struct {
			name  string
			setup string
			call  string
		}{
			{
				name:  "comparator",
				setup: `(defun grow-less (a b) (append! v 99) (< a b))`,
				call:  `(insert-sorted '%s v grow-less 0)`,
			},
			{
				name:  "key-function",
				setup: `(defun grow-key (x) (append! v 99) x)`,
				call:  `(insert-sorted '%s v < 0 grow-key)`,
			},
		} {
			t.Run(typespec+"/"+route.name, func(t *testing.T) {
				env := newPredicateValuesEnv(t)
				require.NoError(t, lisp.GoError(env.LoadString("insert-sorted-growth.lisp",
					`(set 'v (vector 1 2 3 4 5 6 7 8))`)))
				require.NoError(t, lisp.GoError(env.LoadString("insert-sorted-growth.lisp", route.setup)))

				got := env.LoadString("insert-sorted-growth.lisp", fmt.Sprintf(route.call, typespec))
				require.False(t, lisp.IsInternalPanic(got), "%v", got)
				require.NoError(t, lisp.GoError(got))

				cells := resultCells(t, got)
				assert.Len(t, cells, size+1, "the result is sized from the input snapshot plus the inserted item")
				for i, c := range cells {
					require.NotNil(t, c, "result cell %d was never written", i)
					assert.NotEqual(t, "()", c.String(), "result cell %d holds a placeholder, not an element", i)
				}
				assert.Equal(t, size+1, got.Len(), "the reported length must match the storage")
				// Rendering walks every cell: a Go-nil cell panics here.
				assert.NotEmpty(t, got.String())
			})
		}
	}
}

// TestGrowingCallbacksDoNotUnderfillOtherHigherOrderResults is the companion
// control: map, select and reject all read their input's cells once, before
// the first callback, and size or grow their result from that same read, so a
// callback that appends to the input cannot leave a result slot unwritten.
// stable-sort has no separate result to size -- it sorts the sequence in
// place and hands the same value back -- so the assertion there is only that
// its storage and its reported length still agree afterwards.
func TestGrowingCallbacksDoNotUnderfillOtherHigherOrderResults(t *testing.T) {
	const size = 8
	for _, tc := range []struct {
		name  string
		setup string
		expr  string
		// want is the number of cells the result must hold, or 0 for an
		// in-place operation whose result is the (grown) input itself.
		want int
	}{
		{"map list", `(defun grow-id (x) (append! v 99) x)`, `(map 'list grow-id v)`, size},
		{"map vector", `(defun grow-id (x) (append! v 99) x)`, `(map 'vector grow-id v)`, size},
		{"select list", `(defun grow-true (x) (append! v 99) true)`, `(select 'list grow-true v)`, size},
		{"select vector", `(defun grow-true (x) (append! v 99) true)`, `(select 'vector grow-true v)`, size},
		{"reject list", `(defun grow-false (x) (append! v 99) false)`, `(reject 'list grow-false v)`, size},
		{"reject vector", `(defun grow-false (x) (append! v 99) false)`, `(reject 'vector grow-false v)`, size},
		{"stable-sort", `(defun grow-less (a b) (append! v 99) (< a b))`, `(stable-sort grow-less v)`, 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newPredicateValuesEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("insert-sorted-growth.lisp",
				`(set 'v (vector 1 2 3 4 5 6 7 8))`)))
			require.NoError(t, lisp.GoError(env.LoadString("insert-sorted-growth.lisp", tc.setup)))

			got := env.LoadString("insert-sorted-growth.lisp", tc.expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.NoError(t, lisp.GoError(got))

			cells := resultCells(t, got)
			if tc.want > 0 {
				assert.Len(t, cells, tc.want, "the result is sized from the input as it was read")
			}
			for i, c := range cells {
				require.NotNil(t, c, "result cell %d was never written", i)
			}
			assert.Equal(t, len(cells), got.Len(), "the reported length must match the storage")
			assert.NotEmpty(t, got.String())
		})
	}
}
