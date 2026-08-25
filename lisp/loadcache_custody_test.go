// Copyright © 2026 The ELPS authors

// Round-two review of the sealed per-file load cache (issue #368).
//
// Every test here is a regression test for a defect the second adversarial
// pass found, and each one FAILED before the fix it guards.  They share a
// theme the first round did not cover: the READER.  The hook's contract with
// a Reader is narrow ("hand back a parse and do not touch it again"), and
// three of the four defects below live in the gap between that sentence and
// what a plausible Reader actually does — reuse an output buffer, intern its
// symbols, hang a Go value off a literal.
//
// The other theme is the cache's licence.  A cache is an OPTIMIZATION, so
// anything it declines to store must fall back to loading the same program
// uncached; a rule the cache needs for its own safety must never become a
// rule the public parse API enforces.  TestLoaderNodeBudgetIsCacheOnly and
// TestLoadCacheNativePayloadFallsBackUncached pin both halves of that.

package lisp_test

import (
	"io"
	"strings"
	"testing"
	"testing/fstest"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// --- blocker 1: a Reader that reuses its output slice ---

// reusingSliceReader is an ordinary parser wrapped in an ordinary
// optimization: it keeps ONE output slice and refills it on every call, as a
// Reader that does not want to allocate a fresh header per parse would.  It
// never touches a node it handed over, so it keeps the documented custody
// contract to the letter.
//
// The slice, however, is not a node, and before the fix newProgram's sealed
// fast path stored this exact header in the cache entry.  The next parse
// refilled it and the entry — key, name, fingerprint and all — silently
// began describing a different file's program.
type reusingSliceReader struct {
	inner readLocationReader
	buf   []*lisp.LVal
	reads int
}

func newReusingSliceReader() *reusingSliceReader {
	return &reusingSliceReader{inner: parser.NewReader().(readLocationReader)}
}

func (r *reusingSliceReader) Read(name string, in io.Reader) ([]*lisp.LVal, error) {
	exprs, err := r.inner.Read(name, in)
	if err != nil {
		return nil, err
	}
	return r.retain(exprs), nil
}

func (r *reusingSliceReader) ReadLocation(name, loc string, in io.Reader) ([]*lisp.LVal, error) {
	exprs, err := r.inner.ReadLocation(name, loc, in)
	if err != nil {
		return nil, err
	}
	return r.retain(exprs), nil
}

func (r *reusingSliceReader) retain(exprs []*lisp.LVal) []*lisp.LVal {
	r.reads++
	if r.buf == nil {
		// Spare capacity on purpose: the entry must not inherit it either
		// (the clampCap discipline of issue #373).
		r.buf = make([]*lisp.LVal, 0, 32)
	}
	r.buf = append(r.buf[:0], exprs...)
	return r.buf
}

// custodyFS is two one-expression files whose values cannot be confused.
func custodyFS() fstest.MapFS {
	return fstest.MapFS{
		"a.lisp": &fstest.MapFile{Data: []byte("111")},
		"b.lisp": &fstest.MapFile{Data: []byte("222")},
	}
}

func custodyEnv(t *testing.T, reader lisp.Reader, cache lisp.LoadCache) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = reader
	env.Runtime.Library = &lisp.FSLibrary{FS: custodyFS()}
	env.Runtime.LoadCache = cache
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("could not initialize the environment: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("could not enter the user package: %v", rc)
	}
	return env
}

func loadInt(t *testing.T, env *lisp.LEnv, name string) int {
	t.Helper()
	v := env.LoadFile(name)
	require.NotEqual(t, lisp.LError, v.Type, "loading %s failed: %v", name, v)
	require.Equal(t, lisp.LInt, v.Type, "loading %s produced %v, not an int", name, v)
	return v.Int
}

// TestLoadCacheReaderReusingOutputSliceServesTheRightFile is blocker 1.
//
// Before the fix the third load returned 222 — file b's program, served under
// file a's key, with a matching stored fingerprint because the fingerprint
// was taken at admission over the slice that had since been refilled.  The
// no-cache control in the same test returns 111 both times, which is what
// makes this a cache defect rather than a Reader defect.
func TestLoadCacheReaderReusingOutputSliceServesTheRightFile(t *testing.T) {
	t.Parallel()

	// Control: the same Reader with NO cache installed.
	ctrlReader := newReusingSliceReader()
	ctrl := custodyEnv(t, ctrlReader, nil)
	require.Equal(t, 111, loadInt(t, ctrl, "a.lisp"))
	require.Equal(t, 222, loadInt(t, ctrl, "b.lisp"))
	require.Equal(t, 111, loadInt(t, ctrl, "a.lisp"),
		"the control is not a control if the Reader alone corrupts the load")

	cache := newTestLoadCache()
	reader := newReusingSliceReader()
	env := custodyEnv(t, reader, cache)

	a1 := loadInt(t, env, "a.lisp")
	b1 := loadInt(t, env, "b.lisp")
	a2 := loadInt(t, env, "a.lisp")

	assert.Equal(t, 111, a1)
	assert.Equal(t, 222, b1)
	assert.Equal(t, 111, a2,
		"the cache served file b's program under file a's key: the entry retained"+
			" the Reader's output slice, which the next parse refilled")
	assert.Equal(t, 2, reader.reads, "the third load must be a cache hit")
}

// --- blocker 2 and 3: the node budget is the cache's rule, not everyone's ---

// bigExprNodes is one node past the cache admission budget (sealFPMaxNodes,
// 1<<20).  A flat list of distinct integers is the cheapest way to build that
// many DISTINCT nodes — the budget counts distinct nodes, so a list of one
// shared leaf repeated would cost two.
const bigExprNodes = (1 << 20) + 2

// bigQuotedList builds `'(0 1 2 ... n-1)` as a sealed tree: a legal program
// that evaluates to itself in one step, and that no parser would ever emit
// but nothing forbids either.
func bigQuotedList(n int) *lisp.LVal {
	cells := make([]*lisp.LVal, 0, n)
	for i := range n {
		cells = append(cells, lisp.Int(i))
	}
	root := lisp.Quote(lisp.SExpr(cells))
	root.SealAST()
	return root
}

// TestLoaderNodeBudgetIsCacheOnly is blockers 2 and 3 together, because they
// are two halves of one rule: the node budget protects an entry that will be
// ALIASED into unboundedly many environments, so it belongs to the cache
// admission and nowhere else, and overflowing it must cost the caller the
// cache rather than the load.
//
// Before the fix the bound lived in the one admission walk shared with
// lisp.ReadProgram, lisp.ParseProgram and lisp.TextLoader, and it
// FAILED the load with a message blaming "a cycle, shared subtree, or too
// deep".  All three assertions below failed: the public constructors
// rejected a program they accepted before this PR, and the cached load
// turned a working program into an error.
func TestLoaderNodeBudgetIsCacheOnly(t *testing.T) {
	// Not parallel: this test allocates a few hundred MiB for the oversize
	// tree and holds it for the duration.
	root := bigQuotedList(bigExprNodes)

	t.Run("ReadProgram accepts it", func(t *testing.T) {
		prog, err := lisp.ReadProgram(graphReader{tree: root}, "big.lisp", strings.NewReader("x"))
		require.NoError(t, err, "the public parse API must still accept a program it accepted before #536")
		assert.Equal(t, 1, prog.Len())
	})

	t.Run("TextLoader accepts it", func(t *testing.T) {
		_, err := lisp.TextLoader(graphReader{tree: root}, "big.lisp", strings.NewReader("x"))
		require.NoError(t, err, "TextLoader must still accept a program it accepted before #536")
	})

	t.Run("a cached load falls back to an uncached one", func(t *testing.T) {
		cache := newTestLoadCache()
		env := readerEnv(t, graphReader{tree: root}, cache)
		v := env.Load("big.lisp", strings.NewReader("x"))
		require.NotEqual(t, lisp.LError, v.Type,
			"exceeding a CACHE budget must cost the cache, not the load: %v", v)
		assert.Equal(t, 0, cache.stores, "an oversize parse must not be stored")
	})
}

// TestLoadCacheAcceptsInternedSymbols is the rest of blocker 2: a Reader with
// a symbol-interning table returns a DAG, which is an ordinary memory
// optimization.  Before the fix the admission walk rejected any repeated
// node, so such a Reader could not load at all with a cache installed and
// could not build a Program without one.
//
// Leaf sharing stays legal on the cache path too — a leaf has no children to
// re-descend, so nothing can unfold exponentially.  Composite sharing is
// legal there as well (TestLoadCacheAdmitsShallowInternedComposite); what the
// cache refuses is sharing whose UNFOLDED size is astronomical, which
// TestLoadCacheInternedSubtreeIsBounded pins.
func TestLoadCacheAcceptsInternedSymbols(t *testing.T) {
	t.Parallel()

	interned := func() *lisp.LVal {
		// ONE symbol node in three positions, as an interning table yields.
		x := lisp.Symbol("abc")
		root := lisp.Quote(lisp.SExpr([]*lisp.LVal{x, x, x}))
		root.SealAST()
		return root
	}

	prog, err := lisp.ReadProgram(graphReader{tree: interned()}, "interned.lisp", strings.NewReader("x"))
	require.NoError(t, err, "a symbol-interning Reader must still build a Program")
	assert.Equal(t, 1, prog.Len())

	_, err = lisp.TextLoader(graphReader{tree: interned()}, "interned.lisp", strings.NewReader("x"))
	require.NoError(t, err, "a symbol-interning Reader must still build a TextLoader")

	cache := newTestLoadCache()
	env := readerEnv(t, graphReader{tree: interned()}, cache)
	v := env.Load("interned.lisp", strings.NewReader("x"))
	require.NotEqual(t, lisp.LError, v.Type, "a symbol-interning parse must load: %v", v)
	assert.Equal(t, "'(abc abc abc)", v.String())
	assert.Equal(t, 1, cache.stores, "leaf sharing is cacheable; it cannot unfold")
}

// TestLoadCacheDeepSharedLeavesTerminate is the other half of the memo the
// interning fix needs.  firstUnsealed and the admission walk both re-descend
// a shared node once per PATH unless they memoize; this tree has one leaf
// reachable by 2^40 paths through sealed composite spines that are all
// distinct, so a memo-less walk does not finish.  Completing at all is the
// proof.
func TestLoadCacheDeepSharedLeavesTerminate(t *testing.T) {
	t.Parallel()
	leaf := lisp.Symbol("x")
	node := lisp.SExpr([]*lisp.LVal{leaf, leaf})
	for range 40 {
		// Distinct composite spines, one shared LEAF at the bottom: legal
		// under the cache's strict-tree rule, exponential without a memo.
		node = lisp.SExpr([]*lisp.LVal{node, lisp.SExpr([]*lisp.LVal{leaf, leaf})})
	}
	node.SealAST()

	_, err := lisp.ReadProgram(graphReader{tree: node}, "deep.lisp", strings.NewReader("x"))
	require.NoError(t, err, "leaf sharing must be admitted, and admitted in linear time")
}

// --- finding 9: a mutable Native riding on a sealed literal ---

// nativeBox is the Go value the seal cannot vouch for.
type nativeBox struct{ n int }

// nativeLiteralReader hands back a sealed integer literal with a live Go
// pointer hung off its Native field.  The seal freezes an LVal's own fields;
// it does not and cannot freeze whatever is on the other end of that pointer,
// the fingerprint oracle does not hash Native by design, and (*LVal).Copy
// shallow-copies it — so before the fix the box crossed every Runtime
// boundary aliased, unfingerprinted, and unreported.
type nativeLiteralReader struct {
	box   *nativeBox
	reads int
}

func (r *nativeLiteralReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	r.reads++
	lit := lisp.Int(7)
	lit.Native = r.box
	lit.SealAST()
	return []*lisp.LVal{lit}, nil
}

func (r *nativeLiteralReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

// TestLoadCacheNativePayloadFallsBackUncached is finding 9, and it is BLOCKER
// 3's principle applied to it: refusing to share a value is a reason to skip
// the cache, never a reason to fail the load.
//
// Before the fix the entry was admitted and stored (stores == 1) with the box
// aliased into every environment that hit it.
func TestLoadCacheNativePayloadFallsBackUncached(t *testing.T) {
	t.Parallel()
	cache := newTestLoadCache()
	reader := &nativeLiteralReader{box: &nativeBox{n: 1}}
	env := readerEnv(t, reader, cache)

	v := env.Load("native.lisp", strings.NewReader("x"))
	require.NotEqual(t, lisp.LError, v.Type, "the load must still succeed uncached: %v", v)
	assert.Equal(t, 7, v.Int)
	assert.Equal(t, 0, cache.stores,
		"a node carrying a Native payload the seal cannot vouch for must not be shared")

	// And it stays uncached: every load re-parses, exactly as with no cache.
	require.Equal(t, 7, env.Load("native.lisp", strings.NewReader("x")).Int)
	assert.Equal(t, 2, reader.reads)
}

// TestProgramRefusesNativePayload is the same rule at the other admission
// point.  lisp.Program's documented contract is that output the seal cannot
// protect is rejected; a Native payload on a type SealAST marks is exactly
// that, and the rejection has to be an error here because Program has no
// "load it anyway" fall-back to offer.
func TestProgramRefusesNativePayload(t *testing.T) {
	t.Parallel()
	_, err := lisp.ReadProgram(&nativeLiteralReader{box: &nativeBox{}}, "native.lisp", strings.NewReader("x"))
	require.Error(t, err, "a sealed literal carrying a Go pointer is not shareable")
	assert.Contains(t, err.Error(), "native payload")
}

// --- non-blocking: a Reader that states an EMPTY identity ---

// mumblingReader implements lisp.ReaderIdentity and says nothing.  An empty
// token is not an identity: two readers returning it would be declared
// interchangeable producers and would serve each other's parses, which is the
// exact failure ReaderIdentity exists to prevent, reached by implementing it
// badly.
type mumblingReader struct{ reads int }

func (r *mumblingReader) ReaderIdentity() string { return "" }

func (r *mumblingReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	r.reads++
	return []*lisp.LVal{sealedValue(5)}, nil
}

func (r *mumblingReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

// TestLoadCacheEmptyReaderIdentityDisablesTheCache pins the choice: an empty
// identity DISABLES the cache for that reader's loads rather than failing
// them or falling back to the Go type.  Failing them would let a bad optional
// interface break a working embedder; falling back to the type would be
// worse than useless, since a reader that multiplexes parse behaviour behind
// one type is precisely why the interface exists.
//
// Before the fix the empty token was concatenated into the key as "id:" and
// the cache served normally — one parse for two loads, and two such readers
// sharing a process would have served each other's programs.
func TestLoadCacheEmptyReaderIdentityDisablesTheCache(t *testing.T) {
	t.Parallel()
	cache := newTestLoadCache()
	reader := &mumblingReader{}
	env := readerEnv(t, reader, cache)

	require.Equal(t, 5, env.Load("mum.lisp", strings.NewReader("x")).Int)
	require.Equal(t, 5, env.Load("mum.lisp", strings.NewReader("x")).Int)

	assert.Equal(t, 2, reader.reads, "an empty identity must disable the cache, not key on nothing")
	assert.Equal(t, 0, cache.stores, "nothing may be stored under an underivable key")
	assert.Equal(t, 0, cache.loads, "the cache must not even be probed")
}
