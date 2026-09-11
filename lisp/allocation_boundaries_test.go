// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func assertAllocationBoundary(t *testing.T, got, want *lisp.LVal, limit int) {
	t.Helper()
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	if want.Len() > limit {
		require.Equal(t, lisp.LError, got.Type, "output length %d exceeds limit %d: %v", want.Len(), limit, got)
		assert.Contains(t, got.String(), "allocation size")
		assert.Contains(t, got.String(), fmt.Sprintf("exceeds maximum (%d)", limit))
		return
	}
	require.Equal(t, want.Type, got.Type, "%v", got)
	if want.Type == lisp.LBytes {
		assert.Equal(t, want.Bytes(), got.Bytes())
		return
	}
	assert.True(t, lisp.True(got.Equal(want)), "got %v, want %v", got, want)
}

func allocationSequence(kind string, cells []*lisp.LVal) *lisp.LVal {
	if kind == "vector" {
		return lisp.Vector(cells)
	}
	return lisp.QExpr(cells)
}

func allocationByteSource(kind, text string) *lisp.LVal {
	switch kind {
	case "string":
		return lisp.String(text)
	case "bytes":
		return lisp.Bytes([]byte(text))
	default:
		cells := make([]*lisp.LVal, len(text))
		for i := range cells {
			cells[i] = lisp.Int(int(text[i]))
		}
		return allocationSequence(kind, cells)
	}
}

func TestAllocationBoundariesConversions(t *testing.T) {
	const limit = 8
	for _, kind := range []string{"string to bytes", "bytes to string", "int to string", "float to string"} {
		for _, size := range []int{limit - 1, limit, limit + 1} {
			t.Run(fmt.Sprintf("%s/%d", kind, size), func(t *testing.T) {
				env := newPredicateValuesEnv(t)
				env.Runtime.MaxAlloc = limit
				text := "ééé" + strings.Repeat("a", size-6)
				var source, want *lisp.LVal
				expr := `(to-string source)`
				switch kind {
				case "string to bytes":
					source, want = lisp.String(text), lisp.Bytes([]byte(text))
					expr = `(to-bytes source)`
				case "bytes to string":
					source, want = lisp.Bytes([]byte(text)), lisp.String(text)
				case "int to string":
					text = "123456789"[:size]
					n, err := strconv.Atoi(text)
					require.NoError(t, err)
					source, want = lisp.Int(n), lisp.String(text)
				case "float to string":
					text = "1.2345678"[:size]
					x, err := strconv.ParseFloat(text, 64)
					require.NoError(t, err)
					source, want = lisp.Float(x), lisp.String(text)
				}
				env.PutGlobal(lisp.Symbol("source"), source)
				before := source.String()
				got := env.LoadString("allocation-boundary.lisp", expr)
				assert.Equal(t, before, source.String(), "conversion must preserve its source on success and failure")
				assertAllocationBoundary(t, got, want, limit)
			})
		}
	}
}

func TestAllocationBoundariesAllocatingSlices(t *testing.T) {
	const limit = 8
	for _, pair := range [][2]string{
		{"string", "bytes"}, {"string", "list"}, {"string", "vector"},
		{"bytes", "string"}, {"bytes", "list"}, {"bytes", "vector"},
		{"list", "string"}, {"list", "bytes"},
		{"vector", "string"}, {"vector", "bytes"},
	} {
		for _, size := range []int{limit - 1, limit, limit + 1} {
			t.Run(fmt.Sprintf("%s to %s/%d", pair[0], pair[1], size), func(t *testing.T) {
				env := newPredicateValuesEnv(t)
				env.Runtime.MaxAlloc = limit
				window := "ééé" + strings.Repeat("a", size-6)
				source := allocationByteSource(pair[0], "!"+window+"?")
				env.PutGlobal(lisp.Symbol("source"), source)
				before := source.String()
				got := env.LoadString("allocation-boundary.lisp", fmt.Sprintf(`(slice '%s source 1 %d)`, pair[1], size+1))
				assert.Equal(t, before, source.String(), "a slice conversion cannot modify the input")
				assertAllocationBoundary(t, got, allocationByteSource(pair[1], window), limit)
			})
		}
	}
}

func TestAllocationBoundariesPreserveNonAllocatingPaths(t *testing.T) {
	for _, tc := range []struct {
		name   string
		source string
		expr   string
		result string
	}{
		{"bytes identity", "bytes", `(to-bytes source)`, "bytes"},
		{"string identity", "string", `(to-string source)`, "string"},
		{"literal format", "string", `(format-string source)`, "string"},
		{"string view", "string", `(slice 'string source 0 9)`, "string"},
		{"bytes view", "bytes", `(slice 'bytes source 0 9)`, "bytes"},
		{"list view", "list", `(slice 'list source 0 9)`, "list"},
		{"vector view", "vector", `(slice 'vector source 0 9)`, "vector"},
		{"list to vector view", "list", `(slice 'vector source 0 9)`, "vector"},
		{"vector to list view", "vector", `(slice 'list source 0 9)`, "list"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newPredicateValuesEnv(t)
			env.Runtime.MaxAlloc = 8
			source := allocationByteSource(tc.source, "123456789")
			env.PutGlobal(lisp.Symbol("source"), source)
			got := env.LoadString("allocation-boundary.lisp", tc.expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.NotEqual(t, lisp.LError, got.Type, "an existing buffer/view requires no element allocation: %v", got)
			want := allocationByteSource(tc.result, "123456789")
			require.Equal(t, want.Type, got.Type)
			if got.Type == lisp.LBytes {
				assert.Equal(t, want.Bytes(), got.Bytes())
			} else {
				assert.True(t, lisp.True(got.Equal(want)), "got %v, want %v", got, want)
			}
			if tc.name == "bytes identity" {
				assert.Same(t, source, got, "to-bytes must preserve its existing bytes passthrough")
			}
		})
	}
}

func TestAllocationBoundariesInsertion(t *testing.T) {
	const limit = 8
	for _, tc := range []struct {
		name   string
		expr   string
		result string
	}{
		{"cons", `(cons 0 source)`, "list"},
		{"index list", `(insert-index 'list source 0 0)`, "list"},
		{"index vector", `(insert-index 'vector source 0 0)`, "vector"},
		{"sorted list", `(insert-sorted 'list source less 0)`, "list"},
		{"sorted vector", `(insert-sorted 'vector source less 0)`, "vector"},
		{"sorted list key", `(insert-sorted 'list source less 0 key)`, "list"},
		{"sorted vector key", `(insert-sorted 'vector source less 0 key)`, "vector"},
	} {
		for _, size := range []int{limit - 1, limit, limit + 1} {
			t.Run(fmt.Sprintf("%s/%d", tc.name, size), func(t *testing.T) {
				env := newPredicateValuesEnv(t)
				env.Runtime.MaxAlloc = limit
				cells := make([]*lisp.LVal, size-1)
				for i := range cells {
					cells[i] = lisp.Int(i + 1)
				}
				source := lisp.QExpr(cells)
				env.PutGlobal(lisp.Symbol("source"), source)
				comparisons, keys := 0, 0
				env.AddBuiltins(true,
					elpsutil.Function("less", lisp.Formals("a", "b"), func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
						comparisons++
						return lisp.Bool(args.Cells[0].Int < args.Cells[1].Int)
					}),
					elpsutil.Function("key", lisp.Formals("x"), func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
						keys++
						return args.Cells[0]
					}))
				before := source.String()
				got := env.LoadString("allocation-boundary.lisp", tc.expr)
				assert.Equal(t, before, source.String())
				if size > limit {
					assert.Zero(t, comparisons, "known oversized insertion must fail before invoking its comparator")
					assert.Zero(t, keys, "known oversized insertion must fail before extracting keys")
				} else if strings.HasPrefix(tc.name, "sorted") {
					assert.Positive(t, comparisons, "successful insertion must actually use the comparator")
				}
				wantCells := append([]*lisp.LVal{lisp.Int(0)}, cells...)
				assertAllocationBoundary(t, got, allocationSequence(tc.result, wantCells), limit)
			})
		}
	}
}

func TestAllocationBoundariesFilters(t *testing.T) {
	const limit = 8
	for _, op := range []string{"select", "reject"} {
		for _, resultKind := range []string{"list", "vector"} {
			for _, keep := range []int{0, limit - 1, limit, limit + 1} {
				t.Run(fmt.Sprintf("%s %s/%d", op, resultKind, keep), func(t *testing.T) {
					env := newPredicateValuesEnv(t)
					env.Runtime.MaxAlloc = limit
					cells := make([]*lisp.LVal, limit+2)
					for i := range cells {
						cells[i] = lisp.Int(i + 1)
					}
					source := lisp.Vector(cells)
					env.PutGlobal(lisp.Symbol("source"), source)
					calls := 0
					env.AddBuiltins(true, elpsutil.Function("predicate", lisp.Formals("x"), func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
						calls++
						match := args.Cells[0].Int <= keep
						if op == "reject" {
							match = !match
						}
						return lisp.Bool(match)
					}))
					before := source.String()
					got := env.LoadString("allocation-boundary.lisp", fmt.Sprintf(`(%s '%s predicate source)`, op, resultKind))
					assert.Equal(t, before, source.String())
					if keep > limit {
						assert.Equal(t, limit+1, calls, "the first oversized output must stop filtering before the next callback")
					} else {
						assert.Equal(t, len(cells), calls, "large input is valid when the selected output fits")
					}
					assertAllocationBoundary(t, got, allocationSequence(resultKind, cells[:keep]), limit)
				})
			}
		}
	}
}

func TestAllocationBoundariesFormatString(t *testing.T) {
	for _, tc := range []struct {
		name string
		expr string
		want string
	}{
		{"sequential", `(format-string "{}{}" "ab" "cde")`, "abcde"},
		{"repeated positional", `(format-string "{0}{0}" "abcde")`, "abcdeabcde"},
		{"literal text", `(format-string "x{}y" "abc")`, "xabcy"},
		{"escaped braces and UTF8", `(format-string "{{{}}}" "é")`, "{é}"},
		{"quoted UTF8 string", `(format-string "{}" (quote "é"))`, `"é"`},
		{"nested list", `(format-string "{}" '((1 2) (3)))`, "'((1 2) (3))"},
		{"vector", `(format-string "{}" (vector 1 2))`, "(vector 1 2)"},
		{"bytes", `(format-string "{}" (to-bytes "A"))`, "#<bytes 65>"},
		{"map", `(format-string "{}" (sorted-map "a" 1))`, `(sorted-map "a" 1)`},
		{"integer", `(format-string "{}" 123456789)`, "123456789"},
	} {
		for _, limit := range []int{len(tc.want) - 1, len(tc.want), len(tc.want) + 1} {
			t.Run(fmt.Sprintf("%s/%d", tc.name, limit), func(t *testing.T) {
				env := newPredicateValuesEnv(t)
				env.Runtime.MaxAlloc = limit
				got := env.LoadString("allocation-boundary.lisp", tc.expr)
				assertAllocationBoundary(t, got, lisp.String(tc.want), limit)
			})
		}
	}
}

type allocationRenderMap struct {
	lisp.Map
	reads *int
}

func (m allocationRenderMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	*m.reads++
	return m.Map.Entries(buf)
}

func TestAllocationBoundariesFormatStopsRenderingAtLimit(t *testing.T) {
	env := newPredicateValuesEnv(t)
	env.Runtime.MaxAlloc = 8
	reads := 0
	backing := lisp.SortedMap()
	require.NoError(t, lisp.GoError(backing.Map().Set(lisp.String("a"), lisp.Int(1))))
	last := lisp.SortedMapFromData(lisp.NewMapData(allocationRenderMap{Map: backing.Map(), reads: &reads}))
	source := lisp.QExpr([]*lisp.LVal{lisp.String("123456789"), last})
	env.PutGlobal(lisp.Symbol("source"), source)
	got := env.LoadString("allocation-boundary.lisp", `(format-string "{}" source)`)
	// A final-size check after source.String() has already walked the whole
	// graph and allocated it. The bounded renderer must stop at the first
	// oversized string, before even enumerating the following map.
	assert.Zero(t, reads, "formatting must stop traversing once the output exceeds its cap")
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	assert.Contains(t, got.String(), "allocation size")
	assert.Equal(t, "123456789", source.Cells[0].Str)
}
