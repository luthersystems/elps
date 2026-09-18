// Copyright © 2026 The ELPS authors

package lisp

import (
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestRenderLeafSequenceBudgets(t *testing.T) {
	cells := make([]*LVal, 64)
	parts := make([]string, len(cells))
	for i := range cells {
		cells[i], parts[i] = Int(i), strconv.Itoa(i)
	}
	v := SExpr(cells)
	want := "(" + strings.Join(parts, " ") + ")"
	for _, tc := range []struct {
		bytes, steps int
		ok           bool
	}{{len(want), 65, true}, {len(want) - 1, 65, false}, {len(want), 64, false}} {
		budget := renderBudget{remaining: tc.steps}
		got, ok := v.boundedRender(tc.bytes, &budget, false)
		assert.Equal(t, tc.ok, ok)
		if ok {
			assert.Equal(t, want, got)
		}
	}
}
