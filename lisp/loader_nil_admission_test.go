// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// A nil *LVal in reader output is refused AT ADMISSION, on every path.
//
// It used to panic there — the walk dereferenced v.Type unconditionally — and
// a nil guard added at the head of the walk turned the loud, immediate,
// on-the-producing-goroutine failure into silence: the nil was admitted,
// firstUnsealed(nil) answered "nothing unsealed here", and a tree containing
// a nil node was stored in a PROCESS-WIDE cache, from which every later load
// laundered it into a catchable internal-panic (issue #536 round-three
// review, suspicious 2).
//
// The refusal is an ordinary one, not one of the two tree sentinels, so a
// cached load falls back to an uncached parse and the nil reaches the
// evaluator exactly as it does with no cache installed — the nil-cache path
// stays byte-identical.  What must never happen is the STORE.

func TestAdmissionRefusesNilRoot(t *testing.T) {
	t.Parallel()
	rd := graphReader{tree: nil}

	_, err := lisp.ReadProgram(rd, "nil.lisp", strings.NewReader("x"))
	require.Error(t, err, "ReadProgram must refuse a nil root")
	assert.Contains(t, err.Error(), "nil expression")

	_, err = lisp.TextLoader(rd, "nil.lisp", strings.NewReader("x"))
	require.Error(t, err, "TextLoader must refuse a nil root")
	assert.Contains(t, err.Error(), "nil expression")

	cache := newTestLoadCache()
	env := readerEnv(t, rd, cache)
	_ = env.Load("nil.lisp", strings.NewReader("x"))
	assert.Zero(t, cache.stores, "a tree containing a nil node must never be stored")
}

func TestAdmissionRefusesNilCell(t *testing.T) {
	t.Parallel()
	mk := func() *lisp.LVal {
		return lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), nil})
	}
	rd := graphReader{tree: mk()}

	_, err := lisp.ReadProgram(rd, "nil.lisp", strings.NewReader("x"))
	require.Error(t, err, "ReadProgram must refuse a nil cell")
	assert.Contains(t, err.Error(), "nil expression")

	_, err = lisp.TextLoader(rd, "nil.lisp", strings.NewReader("x"))
	require.Error(t, err, "TextLoader must refuse a nil cell")

	cache := newTestLoadCache()
	env := readerEnv(t, graphReader{tree: mk()}, cache)
	_ = env.Load("nil.lisp", strings.NewReader("x"))
	assert.Zero(t, cache.stores, "a tree containing a nil cell must never be stored")
}

// A well-formed parse is unaffected: the guard is about nil, not about
// admission generally.
func TestAdmissionAdmitsOrdinaryParse(t *testing.T) {
	t.Parallel()
	p, err := lisp.ReadProgram(parser.NewReader(), "ok.lisp", strings.NewReader("(+ 1 2)\n(+ 3 4)\n"))
	require.NoError(t, err)
	assert.Equal(t, 2, p.Len())
}
