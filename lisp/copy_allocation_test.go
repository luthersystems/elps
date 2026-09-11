// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestCopyAllocationLimit(t *testing.T) {
	const limit = 8
	for _, kind := range []string{"list", "vector", "bytes", "map"} {
		for _, size := range []int{limit, limit + 1} {
			for _, nested := range []bool{false, true} {
				t.Run(fmt.Sprintf("%s/%d/nested=%t", kind, size, nested), func(t *testing.T) {
					source := copyAllocationValue(t, kind, size)
					if nested {
						outer := lisp.SortedMap()
						require.NoError(t, lisp.GoError(outer.Map().Set(lisp.String("nested"), source)))
						source = lisp.QExpr([]*lisp.LVal{outer})
					}
					before := source.String()
					env := copyTestEnv(t)
					env.Runtime.MaxAlloc = limit
					require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("source"), source)))
					got := env.LoadString("copy-limit.lisp", `(copy source)`)
					require.False(t, lisp.IsInternalPanic(got), "%v", got)
					require.Equal(t, before, source.String(), "copy refusal and success must preserve the source")
					if size > limit {
						require.Equal(t, lisp.LError, got.Type, "%v", got)
						require.Contains(t, got.String(), "allocation size 9 exceeds maximum (8)")
						return
					}
					require.NoError(t, lisp.GoError(got))
					require.Equal(t, before, got.String())
					require.NotSame(t, source, got, "a successful copy must still rebuild mutable data")
				})
			}
		}
	}
}

func copyAllocationValue(t *testing.T, kind string, size int) *lisp.LVal {
	t.Helper()
	cells := make([]*lisp.LVal, size)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	switch kind {
	case "list":
		return lisp.QExpr(cells)
	case "vector":
		return lisp.Vector(cells)
	case "bytes":
		data := make([]byte, size)
		for i := range data {
			data[i] = byte(i)
		}
		return lisp.Bytes(data)
	case "map":
		m := lisp.SortedMap()
		for i := range size {
			require.NoError(t, lisp.GoError(m.Map().Set(lisp.String(strconv.Itoa(i)), lisp.Int(i))))
		}
		return m
	default:
		t.Fatalf("unknown data kind %q", kind)
		return nil
	}
}

func TestCopyAllocationLimitIsPerBackingContainer(t *testing.T) {
	env := copyTestEnv(t)
	env.Runtime.MaxAlloc = 8
	// Distinct headers over one byte payload must still share exactly one
	// copied payload. Total graph storage exceeds eight, but no container does.
	data := lisp.Bytes([]byte("12345678"))
	other := *data
	source := lisp.QExpr([]*lisp.LVal{data, &other, copyAllocationValue(t, "list", 8)})
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("source"), source)))
	got := env.LoadString("copy-limit.lisp", `(copy source)`)
	require.NoError(t, lisp.GoError(got))
	require.Len(t, got.Cells, 3)
	got.Cells[0].Bytes()[0] = 'X'
	require.Equal(t, "X2345678", string(got.Cells[1].Bytes()), "payload aliases survive copying")
	require.Equal(t, "12345678", string(data.Bytes()), "the copied payload is independent")
	require.Equal(t, source.Cells[2].String(), got.Cells[2].String())
	require.NotSame(t, source.Cells[2], got.Cells[2])
}

func TestCopyAllocationLimitPreservesCyclesAndSharedStrings(t *testing.T) {
	env := copyTestEnv(t)
	env.Runtime.MaxAlloc = 8
	cycle := lisp.SortedMap()
	require.NoError(t, lisp.GoError(cycle.Map().Set(lisp.String("self"), cycle)))
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("source"), cycle)))
	got := env.LoadString("copy-limit.lisp", `(copy source)`)
	require.NoError(t, lisp.GoError(got))
	require.NotSame(t, cycle, got)
	self, found := got.Map().Get(lisp.String("self"))
	require.True(t, found)
	require.Same(t, got, self, "the cycle must close onto the copied map")
	require.NoError(t, lisp.GoError(got.Map().Set(lisp.String("new"), lisp.Int(1))))
	require.Equal(t, 1, cycle.Len())
	require.Equal(t, 2, got.Len())

	// Copy reuses immutable string bytes, so their length is not an allocation.
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("source"), lisp.String("longer than eight"))))
	got = env.LoadString("copy-limit.lisp", `(copy source)`)
	require.NoError(t, lisp.GoError(got))
	require.Equal(t, "longer than eight", got.Str)
}

type copyAllocationNativeProbe struct{ calls *int }

func (p *copyAllocationNativeProbe) CloneNative() interface{} {
	*p.calls++
	return &copyAllocationNativeProbe{calls: p.calls}
}

type copyAllocationMapProbe struct {
	lisp.Map
	entries int
}

func (p *copyAllocationMapProbe) Entries(buf []*lisp.LVal) *lisp.LVal {
	p.entries++
	return p.Map.Entries(buf)
}

func TestCopyAllocationLimitChecksBeforeCloning(t *testing.T) {
	env := copyTestEnv(t)
	env.Runtime.MaxAlloc = 8
	calls := 0
	source := copyAllocationValue(t, "list", 9)
	source.Cells[0] = lisp.Native(&copyAllocationNativeProbe{calls: &calls})
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("source"), source)))
	got := env.LoadString("copy-limit.lisp", `(copy source)`)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	require.Contains(t, got.String(), "allocation size 9 exceeds maximum (8)")
	require.Zero(t, calls, "an oversized cell span must fail before cloning its children")

	// Entries allocates pair storage in the stock map. The cap must be
	// checked before enumeration, not after an entire map has been copied.
	probe := &copyAllocationMapProbe{Map: copyAllocationValue(t, "map", 9).Map()}
	source = lisp.SortedMapFromData(lisp.NewMapData(probe))
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("source"), source)))
	got = env.LoadString("copy-limit.lisp", `(copy source)`)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	require.Contains(t, got.String(), "allocation size 9 exceeds maximum (8)")
	require.Zero(t, probe.entries, "an oversized map must fail before allocating entries")
}
