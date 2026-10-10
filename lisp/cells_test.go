// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func cellsOf(xs ...int) lisp.Cells {
	out := make(lisp.Cells, len(xs))
	for i, x := range xs {
		out[i] = lisp.Int(x)
	}
	return out
}

func ints(c lisp.Cells) []int {
	out := make([]int, len(c))
	for i, x := range c {
		out[i] = x.Int
	}
	return out
}

func double(x *lisp.LVal) *lisp.LVal { return lisp.Int(2 * x.Int) }

func identity(x *lisp.LVal) *lisp.LVal { return x }

func TestCellsMap(t *testing.T) {
	assert.Nil(t, lisp.Cells(nil).Map(double))
	empty := lisp.Cells{}.Map(double)
	assert.NotNil(t, empty)
	assert.Empty(t, empty)

	in := make(lisp.Cells, 3, 8)
	copy(in, cellsOf(1, 2, 3))
	out := in.Map(double)
	assert.Equal(t, []int{2, 4, 6}, ints(out))
	assert.Equal(t, 3, cap(out))
	assert.Equal(t, []int{1, 2, 3}, ints(in))

	// The result never aliases the input, even for the identity.
	same := in.Map(identity)
	same[0] = lisp.Int(9)
	assert.Equal(t, 1, in[0].Int)
}

func TestCellsMapIfChanged(t *testing.T) {
	out, changed := lisp.Cells(nil).MapIfChanged(double)
	assert.Nil(t, out)
	assert.False(t, changed)

	in := cellsOf(1, 2, 3)
	out, changed = in.MapIfChanged(identity)
	assert.False(t, changed)
	require.Len(t, out, 3)
	assert.Same(t, &in[0], &out[0], "an unchanged map returns the original slice")

	// A change in the middle keeps the prefix and maps the rest.
	calls := 0
	f := func(x *lisp.LVal) *lisp.LVal {
		calls++
		if x.Int == 2 {
			return lisp.Int(20)
		}
		return x
	}
	out, changed = in.MapIfChanged(f)
	assert.True(t, changed)
	assert.Equal(t, 3, calls, "f is called once per cell")
	assert.Equal(t, []int{1, 20, 3}, ints(out))
	assert.Equal(t, 3, cap(out))
	assert.Same(t, in[0], out[0])
	assert.Same(t, in[2], out[2])
	assert.Equal(t, []int{1, 2, 3}, ints(in), "the input is not written")
	out[0] = lisp.Int(7)
	assert.Equal(t, 1, in[0].Int)
}

func TestCellsClone(t *testing.T) {
	assert.Nil(t, lisp.Cells(nil).Clone())
	empty := lisp.Cells{}.Clone()
	assert.NotNil(t, empty)
	assert.Empty(t, empty)

	in := make(lisp.Cells, 5, 9)
	copy(in, cellsOf(1, 2, 3, 4, 5))
	out := in.Clone()
	assert.Len(t, out, 5)
	assert.Equal(t, 5, cap(out))
	out[0] = lisp.Int(9)
	assert.Equal(t, 1, in[0].Int)
	assert.Same(t, in[1], out[1], "Clone copies pointers, not values")
}

func TestCellsAppend(t *testing.T) {
	out := lisp.Cells(nil).Append()
	assert.NotNil(t, out)
	assert.Empty(t, out)

	in := make(lisp.Cells, 2, 8)
	copy(in, cellsOf(1, 2))
	out = in.Append(lisp.Int(3), lisp.Int(4))
	assert.Equal(t, []int{1, 2, 3, 4}, ints(out))
	assert.Equal(t, 4, cap(out))
	// in had spare capacity; Append must not have written it.
	spare := in[len(in):cap(in)]
	assert.Nil(t, spare[0])
	out[0] = lisp.Int(9)
	assert.Equal(t, 1, in[0].Int)

	assert.Equal(t, []int{1, 2}, ints(in.Append()))
}

func TestCellsChain(t *testing.T) {
	v := cellsOf(1, 2).Append(lisp.Int(3)).Map(double).List()
	assert.Equal(t, "'(2 4 6)", v.String())
}

var sinkCells lisp.Cells

func TestCellsAllocs(t *testing.T) {
	in := cellsOf(1, 2, 3, 4)
	x := lisp.Int(5)
	cases := []struct {
		name string
		want float64
		f    func()
	}{
		{"Map", 1, func() { sinkCells = in.Map(identity) }},
		{"MapIfChanged unchanged", 0, func() { sinkCells, _ = in.MapIfChanged(identity) }},
		{"MapIfChanged changed", 1, func() {
			sinkCells, _ = in.MapIfChanged(func(v *lisp.LVal) *lisp.LVal {
				if v == in[3] {
					return x
				}
				return v
			})
		}},
		{"Clone", 1, func() { sinkCells = in.Clone() }},
		{"Append", 1, func() { sinkCells = in.Append(x) }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			assert.InDelta(t, tc.want, testing.AllocsPerRun(100, tc.f), 0)
		})
	}
}

func BenchmarkCellsMap(b *testing.B) {
	in := cellsOf(1, 2, 3, 4, 5, 6, 7, 8)
	b.Run("method", func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			sinkCells = in.Map(identity)
		}
	})
	b.Run("loop", func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			out := make(lisp.Cells, len(in))
			for i, c := range in {
				out[i] = identity(c)
			}
			sinkCells = out
		}
	})
}
