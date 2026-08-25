// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"io"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// --- round-three blocker 2: ordinary sharing is not "unbounded" ---

// internCompositeReader parses with the real parser and then makes the two
// occurrences of one repeated subexpression ONE node — a textbook
// constant-interning pass, and the shape a Reader with an interning table
// produces.
type internCompositeReader struct{ inner lisp.Reader }

func (r *internCompositeReader) Read(name string, in io.Reader) ([]*lisp.LVal, error) {
	exprs, err := r.inner.Read(name, in)
	if err != nil {
		return nil, err
	}
	return internAdditions(exprs), nil
}

func (r *internCompositeReader) ReadLocation(name, loc string, in io.Reader) ([]*lisp.LVal, error) {
	rd, ok := r.inner.(lisp.LocationReader)
	if !ok {
		return r.Read(name, in)
	}
	exprs, err := rd.ReadLocation(name, loc, in)
	if err != nil {
		return nil, err
	}
	return internAdditions(exprs), nil
}

// internAdditions collapses every `(+ ...)` form that renders identically to
// the first one onto that same pointer.
//
// It works on PRIVATE COPIES and re-seals, never on the parser's sealed tree:
// writing through a sealed node is the corruption the seal exists to forbid,
// and a test Reader that did it would be indicting itself rather than the
// cache (the same discipline internReaderOutput follows in the fuzz target).
func internAdditions(parsed []*lisp.LVal) []*lisp.LVal {
	exprs := make([]*lisp.LVal, len(parsed))
	for i, e := range parsed {
		exprs[i] = e.Copy()
	}
	var proto *lisp.LVal
	var walk func(v *lisp.LVal)
	walk = func(v *lisp.LVal) {
		if v == nil {
			return
		}
		for i, c := range v.Cells {
			if c == nil {
				continue
			}
			if c.Type == lisp.LSExpr && len(c.Cells) == 3 && c.Cells[0].Type == lisp.LSymbol && c.Cells[0].Str == "+" {
				switch {
				case proto == nil:
					proto = c
				case c != proto && c.String() == proto.String():
					v.Cells[i] = proto
					continue
				}
			}
			walk(c)
		}
	}
	for _, e := range exprs {
		walk(e)
		// Re-sealed as the standard parser seals its own output, so the two
		// arms differ only in whether a cache is installed.
		e.SealAST()
	}
	return exprs
}

const internedSharingFixture = `(in-package 'user)
(set 'a (+ 1 2))
(set 'b (+ 1 2))
`

// A composite node reached twice is ORDINARY SHARING, and a cache must not
// turn it into a failure.  The rule this replaces ("a composite reached twice
// is errReaderTreeUnbounded, which is a hard load error") was justified by
// "an interned subtree evaluates once per path, exponentially" — an argument
// about NESTED sharing.  One small subexpression reached twice is linear, and
// with no cache installed this file loads in microseconds (issue #536
// round-three review, blocker 2).
func TestLoadCacheAdmitsShallowInternedComposite(t *testing.T) {
	t.Parallel()
	run := func(cache lisp.LoadCache) (string, string) {
		env := readerEnv(t, &internCompositeReader{inner: parser.NewReader()}, cache)
		res := env.LoadLocation("i.lisp", "/i.lisp", strings.NewReader(internedSharingFixture))
		return res.String(), env.GetGlobal(lisp.Symbol("b")).String()
	}
	offLoad, offB := run(nil)
	cache := newTestLoadCache()
	onLoad, onB := run(cache)

	assert.Equal(t, offLoad, onLoad, "installing a cache changed the result of an interned parse")
	assert.Equal(t, offB, onB, "installing a cache changed a binding of an interned parse")
	assert.Equal(t, "3", onB)
	assert.Equal(t, 1, cache.stores, "an interned parse of ordinary size is cacheable")
}

// The other half of the same rule: sharing whose UNFOLDED size is
// astronomical is still refused outright, because nothing that terminates
// looks like it.  TestLoadCacheInternedSubtreeIsBounded is the timing proof;
// this one pins the sentinel the refusal uses, so the two halves cannot drift
// into each other.
func TestLoadCacheRefusesExponentialSharing(t *testing.T) {
	t.Parallel()
	node := sealedValue(0)
	for range 40 {
		parent := lisp.SExpr(nil)
		parent.Cells = []*lisp.LVal{node, node}
		node = parent
	}
	cache := newTestLoadCache()
	env := readerEnv(t, graphReader{tree: node}, cache)
	v := env.Load("dag.lisp", strings.NewReader("x"))
	require.Equal(t, lisp.LError, v.Type)
	assert.Contains(t, v.String(), "not a finite tree")
	assert.Zero(t, cache.stores)
}

// --- round-three minor 1: admission order must not defeat the cycle rule ---

// cycleAfterBigReader returns an over-budget (but legal) expression FIRST and
// a cyclic one second.
type cycleAfterBigReader struct{}

func (r cycleAfterBigReader) out() []*lisp.LVal {
	big := make([]*lisp.LVal, 0, (1<<20)+2)
	big = append(big, lisp.Symbol("quote"))
	inner := make([]*lisp.LVal, (1<<20)+1)
	for i := range inner {
		inner[i] = lisp.Int(1)
	}
	big = append(big, lisp.SExpr(inner))
	root := lisp.SExpr(big)
	root.SealAST()

	cycInner := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn")})
	cyc := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), cycInner})
	cycInner.Cells = append(cycInner.Cells, cyc)
	return []*lisp.LVal{root, cyc}
}

func (r cycleAfterBigReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	return r.out(), nil
}
func (r cycleAfterBigReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

// A cycle must be refused wherever it sits in the stream.  The admission walk
// returns on the first refusal and the node budget belongs to the whole
// stream, so an over-budget expression in FRONT of a cyclic one used to make
// the budget sentinel win — and readCached turns that one into an uncached
// fall-back, which hands the cycle to the evaluator, exactly what splitting
// the two sentinels was for (issue #536 round-three review, minor 1).
func TestLoadCacheCycleOutranksBudgetAcrossStream(t *testing.T) {
	t.Parallel()
	cache := newTestLoadCache()
	env := readerEnv(t, cycleAfterBigReader{}, cache)

	done := make(chan *lisp.LVal, 1)
	go func() { done <- env.Load("mixed.lisp", strings.NewReader("x")) }()
	select {
	case v := <-done:
		require.Equal(t, lisp.LError, v.Type, "a cycle behind a big expression must still be refused")
		assert.Contains(t, v.String(), "not a finite tree",
			"the budget sentinel outranked the cycle sentinel")
	case <-time.After(30 * time.Second):
		t.Fatal("admission did not terminate")
	}
	assert.Zero(t, cache.stores)
}

// --- round-three suspicious 3: the Native rule is not TextLoader's ---

// nativeAnnotationReader is an embedder Reader that annotates nodes.  In the
// LVal struct source, meta and macroExpansion are all unexported, so Native
// is the only exported per-node slot such a Reader has.
type nativeAnnotationReader struct{}

func (r nativeAnnotationReader) out() []*lisp.LVal {
	lit := lisp.String("hello")
	lit.Native = map[string]int{"annotation": 1}
	return []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), lit})}
}

func (r nativeAnnotationReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	return r.out(), nil
}
func (r nativeAnnotationReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

// TextLoader tolerates it: every load it serves gets expr.Copy(), so no two
// loads share the node, and Copy shallow-copying Native was already true
// before this hook.
func TestTextLoaderAdmitsNativeAnnotation(t *testing.T) {
	t.Parallel()
	loader, err := lisp.TextLoader(nativeAnnotationReader{}, "n.lisp", strings.NewReader("x"))
	require.NoError(t, err, "TextLoader must not refuse an embedder's Native annotation")
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	v := loader(env)
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	assert.Equal(t, `"hello"`, v.String())
}

// The Program constructors and the cache still refuse it: their trees ARE
// aliased between environments, and nothing downstream can vouch for what an
// embedder hung off Native.  The message says "admit", not "cache" — these
// callers never asked for a cache.
func TestProgramRefusesNativeAnnotation(t *testing.T) {
	t.Parallel()
	_, err := lisp.ReadProgram(nativeAnnotationReader{}, "n.lisp", strings.NewReader("x"))
	require.Error(t, err)
	assert.Contains(t, err.Error(), "carrying a native payload")
	assert.NotContains(t, err.Error(), "cannot cache",
		"ReadProgram has no cache; the message must not blame one")

	cache := newTestLoadCache()
	env := readerEnv(t, nativeAnnotationReader{}, cache)
	v := env.Load("n.lisp", strings.NewReader("x"))
	require.NotEqual(t, lisp.LError, v.Type, "the cache path falls back uncached: %v", v)
	assert.Zero(t, cache.stores, "a Native payload must never be stored")
}
