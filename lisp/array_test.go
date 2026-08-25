// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
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
			// Two appends off the same source are independent (issue #373).
			//
			// This row used to assert (vector 1 2 3 5).  The append above
			// reused excess capacity left in v123 by the append that built
			// it, so it wrote through the shared backing array and rewrote
			// v1234 -- a value `append` had already returned.  The comment
			// here called that an "assumed performance benefit" and told
			// callers to use `append` "sparingly and with care".
			//
			// There was no care that helped: nothing about v1234 said it
			// was still aliased, and `append` is the non-mutating
			// constructor by contract and by docstring.  Producers now
			// clamp the capacity of every view they hand out and `append`
			// clamps its input, so an append needing room reallocates.
			// `append!` is the in-place accumulator and still grows in
			// amortised O(1).
			{"v1234", "(vector 1 2 3 4)", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// TestArrayDoesNotAliasCallerDims pins the constraint that decides which
// Array calls copy their dims.  An array rewrites its own cardinality in
// place -- builtinSelect and builtinReject size a vector's dims to the
// element count once the predicate has run -- so a caller-supplied dims list
// must be copied or that write lands in a value the caller still holds.  Dims
// Array constructs for itself (the dims == nil entry) are reachable from
// nothing else, so they are stored directly and cost no copy.
func TestArrayDoesNotAliasCallerDims(t *testing.T) {
	dims := lisp.QExpr([]*lisp.LVal{lisp.Int(3)})
	arr := lisp.Array(dims, []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	require.NotEqual(t, lisp.LError, arr.Type, "constructing the array: %v", arr)
	require.NotSame(t, dims, arr.Cells[0], "the array stored the caller's dims list")
	arr.Cells[0].Cells[0].Int = 2 // the resize select/reject perform
	require.Equal(t, 3, dims.Cells[0].Int, "resizing the array rewrote the caller's dims")

	// The vector path reports the dims it derived from the cells it was
	// given, and each array owns its own dims list.
	v1 := lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	v2 := lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	require.Equal(t, 2, v1.Cells[0].Cells[0].Int)
	require.NotSame(t, v1.Cells[0], v2.Cells[0], "two vectors share one dims list")
}

// TestArrayWithoutBackingStorageIsReadable pins the fix for issue #367.
//
// lisp.Array documents that cells may be nil, and every internal caller uses
// that form and then fills the storage itself.  An embedder following the
// documentation does not fill it, and before this fix the array it got held
// the slice's zero value -- a Go nil *LVal -- in every element.  A nil *LVal
// is not a value the interpreter can hold, so the first lisp expression to
// read one dereferenced it and took the host process down.  The evaluator
// reported that as `internal-panic`, which handler-bind is documented NOT to
// catch, so the embedder had no way to contain it either.
//
// The unit here is deliberately the CONSTRUCTOR, not any one reader: `aref`
// was merely the first expression to touch the element.  `equal?`, `nth` and
// printing reached the same nil by other routes, and a per-reader nil check
// would have been one guard per reader against a value that should never have
// existed.
func TestArrayWithoutBackingStorageIsReadable(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	require.NotEqual(t, lisp.LError, rc.Type, "initialize-user-env: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.NotEqual(t, lisp.LError, rc.Type, "in-package: %v", rc)

	for _, test := range []struct {
		name string
		arr  *lisp.LVal
	}{
		{"vector", lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(3)}), nil)},
		{"empty", lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(0)}), nil)},
		// Zero dimensions is one element, not zero: the product of an empty
		// list of sizes is 1.  It is the smallest array that has an element
		// nobody supplied.
		{"zero-dimensional", lisp.Array(lisp.QExpr(nil), nil)},
	} {
		t.Run(test.name, func(t *testing.T) {
			require.NotEqual(t, lisp.LError, test.arr.Type, "constructing the array: %v", test.arr)
			for _, cell := range test.arr.Cells[1].Cells {
				require.NotNil(t, cell,
					"Array left an element as a Go nil; every unset element must be Nil()")
				require.True(t, cell.IsNil(), "an unset array element must be nil, got %v", cell)
			}
			env.PutGlobal(lisp.Symbol("a"), test.arr)
			// Reading the array must produce values, not an internal-panic --
			// and it must not produce an error either: an unset element is
			// nil, which is an answer.
			for _, expr := range []string{`(equal? a a)`, `(to-string (nth a 0))`} {
				res := env.LoadString(test.name, expr)
				require.False(t, lisp.IsInternalPanic(res),
					"%s over an array with no backing storage panicked the host: %v", expr, res)
			}
			if test.name == "vector" {
				res := env.LoadString(test.name, `(aref a 0)`)
				require.False(t, lisp.IsInternalPanic(res), "(aref a 0) panicked the host: %v", res)
				require.True(t, res.IsNil(), "an unset element must read as nil, got %v", res)
			}
		})
	}
}
