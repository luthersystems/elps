// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Repeated loads of byte-identical source reuse the digest instead of
// re-hashing (luthersystems/substrate#548).
func TestLoadCacheKeyMemoAvoidsRehash(t *testing.T) {
	resetLoadCacheKeyMemo()
	src := []byte(strings.Repeat("(set 'x 1)\n", 1000))
	before := loadCacheKeyDigests.Load()
	k1 := memoLoadCacheKey("f.lisp", "f.lisp", "rid", true, append([]byte(nil), src...))
	k2 := memoLoadCacheKey("f.lisp", "f.lisp", "rid", true, append([]byte(nil), src...))
	assert.Equal(t, int64(1), loadCacheKeyDigests.Load()-before, "second load must not re-hash")
	assert.Equal(t, k1, k2)
	assert.Equal(t, loadCacheKey("f.lisp", "f.lisp", "rid", true, src), k1)
}

// Same identity, same length, different content: never the same key.  The
// memo compares full bytes, so a caller mutating its slice cannot poison it.
func TestLoadCacheKeyMemoSameLengthDifferentContent(t *testing.T) {
	resetLoadCacheKeyMemo()
	a := []byte("(set 'x 1)")
	b := []byte("(set 'x 2)")
	require.Equal(t, len(a), len(b))
	ka := memoLoadCacheKey("f.lisp", "f.lisp", "rid", false, a)
	a[len(a)-2] = '2' // caller mutates its slice after the load
	kb := memoLoadCacheKey("f.lisp", "f.lisp", "rid", false, b)
	assert.NotEqual(t, ka, kb)
	assert.Equal(t, loadCacheKey("f.lisp", "f.lisp", "rid", false, b), kb)
	// Only the last byte differs: still a distinct, correct key.
	c := bytes.Clone(b)
	c[0] = '['
	assert.Equal(t, loadCacheKey("f.lisp", "f.lisp", "rid", false, c),
		memoLoadCacheKey("f.lisp", "f.lisp", "rid", false, c))
	// Every identity component still separates entries.
	for _, tc := range []struct {
		name, loc, rid string
		byLoc          bool
	}{{"g", "f.lisp", "rid", false}, {"f.lisp", "g", "rid", false}, {"f.lisp", "f.lisp", "r2", false}, {"f.lisp", "f.lisp", "rid", true}} {
		assert.Equal(t, loadCacheKey(tc.name, tc.loc, tc.rid, tc.byLoc, b), memoLoadCacheKey(tc.name, tc.loc, tc.rid, tc.byLoc, b))
	}
}

func BenchmarkLoadCacheKeyRepeat(b *testing.B) {
	src := []byte(strings.Repeat("(defun f (x) (+ x 1))\n", 20000)) // ~430 KB
	resetLoadCacheKeyMemo()
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	for range b.N {
		memoLoadCacheKey("bench.lisp", "bench.lisp", "rid", true, src)
	}
}

func BenchmarkLoadCacheKeyNoMemo(b *testing.B) {
	src := []byte(strings.Repeat("(defun f (x) (+ x 1))\n", 20000))
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	for range b.N {
		loadCacheKey("bench.lisp", "bench.lisp", "rid", true, src)
	}
}

// Through the funnel: a second environment loading the same bytes hits the
// cache without re-hashing, and a same-length edit under the same name is a
// miss that reparses (never the stale program).
func TestReadCachedReusesDigestAcrossEnvs(t *testing.T) {
	resetLoadCacheKeyMemo()
	cache := &mapLoadCache{}
	var parsed []string
	parse := func(r io.Reader) ([]*LVal, error) {
		b, err := io.ReadAll(r)
		if err != nil {
			return nil, err
		}
		parsed = append(parsed, string(b))
		e := SExpr([]*LVal{Symbol("quote"), String(string(b))})
		e.SealAST()
		return []*LVal{e}, nil
	}
	load := func(src string) []*LVal {
		env := NewEnv(nil)
		env.Runtime.LoadCache = cache
		exprs, err := env.readCached("f.lisp", "f.lisp", true, strings.NewReader(src), parse)
		require.NoError(t, err)
		return exprs
	}
	before := loadCacheKeyDigests.Load()
	a1 := load("(set 'x 1)")
	a2 := load("(set 'x 1)")
	assert.Equal(t, int64(1), loadCacheKeyDigests.Load()-before)
	assert.Same(t, a1[0], a2[0])
	b := load("(set 'x 2)")
	assert.Equal(t, int64(2), loadCacheKeyDigests.Load()-before)
	assert.Equal(t, "(set 'x 2)", b[0].Cells[1].Str)
	assert.Equal(t, []string{"(set 'x 1)", "(set 'x 2)"}, parsed)
}
