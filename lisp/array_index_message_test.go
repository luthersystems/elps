// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// An error message is program output.
//
// (*LVal).ArrayIndex used to report a dimension-count mismatch with %#v on the
// dimensions LVal, which prints the Go struct literal -- including the Source
// and Cells POINTERS.  The message therefore embedded heap addresses and
// differed on every evaluation of the same source (elps#427), reachable from
// lisp with a 14-byte expression.
//
// Downstream a phylum runs as Fabric chaincode, where every endorsing peer
// must produce identical output for a transaction.  A phylum that returns,
// logs or hashes a caught condition's message would produce divergent proposal
// responses, and the endorsement policy would fail for a reason nothing in the
// phylum explains.  So the assertion is determinism, not wording.
func TestArrayIndexMessageIsDeterministic(t *testing.T) {
	t.Parallel()
	const src = `(aref (vector))`
	const runs = 3

	msgs := make([]string, 0, runs)
	for range runs {
		v := loadStringFresh(t, src)
		require.Equal(t, lisp.LError, v.Type, "%s must be an error", src)
		msgs = append(msgs, v.String())
	}
	for i := 1; i < len(msgs); i++ {
		assert.Equal(t, msgs[0], msgs[i],
			"evaluating %s twice produced different error text; an error message must not depend on"+
				" where a value happens to be allocated", src)
	}

	// The specific regression: no Go struct dump, and no pointer.
	assert.NotContains(t, msgs[0], "lisp.LVal{",
		"the message renders the Go struct literal for an LVal")
	assert.NotContains(t, msgs[0], "0xc0",
		"the message contains a heap address")
	assert.Contains(t, msgs[0], "dimensions",
		"the message should name the dimensions")
}

// The multi-dimensional spelling reaches the same branch.
func TestArrayIndexMessageIsDeterministicMultiDim(t *testing.T) {
	t.Parallel()
	const src = `(aref (vector 1 2) 0 0)`
	a := loadStringFresh(t, src)
	b := loadStringFresh(t, src)
	require.Equal(t, lisp.LError, a.Type)
	assert.Equal(t, a.String(), b.String())
	assert.NotContains(t, a.String(), "lisp.LVal{")
}

func loadStringFresh(t *testing.T, src string) *lisp.LVal {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	require.True(t, lisplib.LoadLibrary(env).IsNil())
	require.True(t, env.InPackage(lisp.String(lisp.DefaultUserPackage)).IsNil())
	v := env.LoadString("test", src)
	require.NotNil(t, v)
	return v
}
