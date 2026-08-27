// Copyright © 2018 The ELPS authors

package lsp

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// specialOpsDriftExempt lists special operators deliberately absent from
// specialOps.  It is empty: every special operator is a keyword for
// highlighting purposes, and there is no known reason for one not to be.
//
// It exists anyway so that a future exception is recorded with a reason
// instead of being made by deleting an assertion.
var specialOpsDriftExempt = map[string]string{}

// TestSpecialOpsCoversTheOpTable pins specialOps against
// lisp.DefaultSpecialOps().
//
// specialOps is one of several hand-maintained mirrors of the operator table
// (see internal/formsync for the grammar pair and the full inventory).  It
// drifted silently until this test existed: function, expr,
// qualified-symbol, macrolet and assert were all special operators the
// language server classified as ordinary function calls, so they rendered
// without keyword highlighting in every LSP client.
func TestSpecialOpsCoversTheOpTable(t *testing.T) {
	t.Parallel()
	ops := lisp.DefaultSpecialOps()
	require.NotEmpty(t, ops, "lisp.DefaultSpecialOps() returned nothing")

	for _, def := range ops {
		name := def.Name()
		if reason, ok := specialOpsDriftExempt[name]; ok {
			require.NotEmpty(t, reason, "exemption for %q must carry a reason", name)
			continue
		}
		require.True(t, specialOps[name],
			"special operator %q is missing from specialOps, so the language"+
				" server will not highlight it as a keyword; add it, or add it"+
				" to specialOpsDriftExempt with a reason", name)
	}
}
