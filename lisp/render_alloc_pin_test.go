// Copyright © 2026 The ELPS authors

// Allocation pins run in ordinary builds, following sort_alloc_pin_test.go.
// Race instrumentation and elpscheck ownership bookkeeping may add work;
// rendering, exact-limit, cycle and source-integrity tests remain enabled
// for those builds in render_bounded_test.go and the walker oracle.

//go:build !race && !elpscheck

package lisp

import (
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var renderAllocationString string

func TestBoundedStringScalarAllocations(t *testing.T) {
	for _, v := range []*LVal{Int(30), Float(99.5), Symbol("true"), Quote(Symbol("receipt"))} {
		t.Run(v.String(), func(t *testing.T) {
			want := v.String()
			baseline := testing.AllocsPerRun(20, func() { renderAllocationString = v.String() })
			bounded := testing.AllocsPerRun(20, func() {
				var ok bool
				renderAllocationString, ok = v.boundedString(len(want))
				if !ok {
					t.Fatal("exact limit rejected")
				}
			})
			assert.Equal(t, want, renderAllocationString)
			assert.LessOrEqual(t, bounded, baseline, "bounding a scalar must not add an intermediate output buffer")
		})
	}
}

func TestStringWideAllocationBudget(t *testing.T) {
	cells := make([]*LVal, 64)
	parts := make([]string, len(cells))
	for i := range cells {
		cells[i] = Int(i)
		parts[i] = strconv.Itoa(i)
	}
	v := SExpr(cells)
	allocations := testing.AllocsPerRun(20, func() { renderAllocationString = v.String() })
	assert.Equal(t, "("+strings.Join(parts, " ")+")", renderAllocationString)
	// BenchmarkString/wide used four allocations before streaming. Avoid
	// regressing that baseline through repeated tiny builder growth.
	assert.LessOrEqual(t, allocations, float64(4))
}

func TestFormatStringAllocationBudget(t *testing.T) {
	env := NewEnv(nil)
	for _, tc := range []struct {
		name, want string
		args       []*LVal
		allocs     float64
	}{
		{"simple", "hello world", []*LVal{String("hello {}"), String("world")}, 2},
		{"mixed", "name: Alice, age: 30, score: 99.5", []*LVal{String("name: {}, age: {}, score: {}"), String("Alice"), Int(30), Float(99.5)}, 3},
	} {
		t.Run(tc.name, func(t *testing.T) {
			args := SExpr(tc.args)
			var result *LVal
			allocations := testing.AllocsPerRun(20, func() { result = builtinFormatString(env, args) })
			require.Equal(t, LString, result.Type)
			assert.Equal(t, tc.want, result.Str)
			// One final value and one reserved output buffer, plus the
			// float's temporary numeric text in the mixed case.
			assert.LessOrEqual(t, allocations, tc.allocs)
		})
	}
}
