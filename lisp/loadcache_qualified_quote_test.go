// Regression test for the WITHHELD-discount direction of the quote
// heuristic.  (user:quote X) is real quoting at eval time — every package
// that use-packages lisp resolves pkg:quote to the quote special form — so
// a spelling whitelist cannot enumerate quote's names.  When the discount
// was withheld from a qualified spelling, the walk charged eval = raw over
// a big shared datum, saturated, and errReaderTreeUnbounded HARD-FAILED a
// load that completes in O(1) with no cache installed — violating the
// contract that installing a cache never makes a load worse than not
// installing one.  loaderQuotingPayload now matches any ":quote" suffix;
// the false-discount direction is parity-safe (raw stays quote-blind, so
// storage and every unfolding walk stay bounded either way).
package lisp_test

import (
	"io"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

type qualifiedQuoteReader struct{ exprs []*lisp.LVal }

func (r qualifiedQuoteReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	return r.exprs, nil
}
func (r qualifiedQuoteReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

func qualifiedQuoteEnv(t *testing.T, reader lisp.Reader, cache lisp.LoadCache) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = reader
	env.Runtime.LoadCache = cache
	rc := lisp.InitializeUserEnv(env)
	require.NotEqual(t, lisp.LError, rc.Type, "init: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.NotEqual(t, lisp.LError, rc.Type)
	return env
}

func r5bLoad(t *testing.T, env *lisp.LEnv, d time.Duration) *lisp.LVal {
	t.Helper()
	done := make(chan *lisp.LVal, 1)
	go func() { done <- env.Load("r5b.lisp", strings.NewReader("x")) }()
	select {
	case v := <-done:
		return v
	case <-time.After(d):
		t.Fatal("load did not terminate")
		return nil
	}
}

// TestLoadCacheQualifiedQuoteSpellingParity: (user:quote DAG) over a 41-deep shared
// datum.  Real quoting at eval (O(1) with no cache); the discount matcher does
// not recognise the spelling, so the walk counts 2^41 evals.
func TestLoadCacheQualifiedQuoteSpellingParity(t *testing.T) {
	t.Parallel()
	build := func() []*lisp.LVal {
		node := lisp.Int(7)
		for range 40 {
			node = lisp.SExpr([]*lisp.LVal{node, node})
		}
		q := lisp.SExpr([]*lisp.LVal{lisp.Symbol("user:quote"), node})
		q.SealAST()
		return []*lisp.LVal{q, sealedValue(5)}
	}

	off := r5bLoad(t, qualifiedQuoteEnv(t, qualifiedQuoteReader{exprs: build()}, nil), 30*time.Second)
	require.NotEqual(t, lisp.LError, off.Type,
		"control: (user:quote DAG) must load fine with no cache: %.200v", off)
	require.Equal(t, "5", off.String())

	cache := newTestLoadCache()
	on := r5bLoad(t, qualifiedQuoteEnv(t, qualifiedQuoteReader{exprs: build()}, cache), 30*time.Second)
	if on.Type == lisp.LError {
		t.Fatalf("installing a cache hard-failed a terminating program spelled user:quote: %.300v", on)
	}
	require.Equal(t, "5", on.String())
}
