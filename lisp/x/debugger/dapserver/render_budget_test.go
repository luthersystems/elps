// Copyright © 2026 The ELPS authors

package dapserver

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/stretchr/testify/require"
)

func TestVariablesResponseRenderBudget(t *testing.T) {
	v := lisp.Symbol(strings.Repeat("x", lisp.DefaultMaxAlloc/2-32))
	bindings := []debugger.ScopeBinding{{Name: "a", Value: v}, {Name: "b", Value: v}, {Name: "c", Value: v}, {Name: "d", Value: v}}
	noRef := func(*lisp.LVal) int { return 0 }
	for _, expand := range []bool{false, true} {
		vars := translateVariables(bindings, noRef, nil)
		if expand {
			vars = expandVariable(lisp.SExpr([]*lisp.LVal{v, v, v, v}), noRef, nil, nil)
		}
		total := 0
		for _, variable := range vars {
			total += len(variable.Value)
		}
		require.LessOrEqual(t, total, lisp.DefaultMaxAlloc)
		require.Len(t, vars, 3)
		require.Equal(t, "#<truncated>", vars[2].Value)
	}
}
