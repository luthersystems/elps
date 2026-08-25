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

// --- round-three minor 2: an embedder hook must not panic out of Load* ---

// panickingCache is an embedder cache with a bug in it.
type panickingCache struct {
	entries    map[string]*lisp.CachedSource
	panicLoad  bool
	panicStore bool
	stores     int
}

func (c *panickingCache) Load(key string) (*lisp.CachedSource, bool) {
	if c.panicLoad {
		panic("cache Load blew up")
	}
	v, ok := c.entries[key]
	return v, ok
}

func (c *panickingCache) Store(key string, src *lisp.CachedSource) {
	if c.panicStore {
		panic("cache Store blew up")
	}
	c.stores++
	c.entries[key] = src
}

// (*LEnv).Load* is total: it returns an *LVal, an LError at worst.  readCached
// runs before the evaluator's recover, though, so a panicking hook escaped as
// a raw Go panic through an API that never panics.  A panic in either hook now
// degrades to "the cache did not help", like every other cache-implementation
// mistake on this path.
func TestLoadCacheHookPanicDoesNotEscape(t *testing.T) {
	t.Parallel()
	const src = `(in-package 'user)(set 'who "A")`
	for _, tc := range []struct{ name, which string }{{"Store", "store"}, {"Load", "load"}} {
		t.Run(tc.name, func(t *testing.T) {
			cache := &panickingCache{entries: map[string]*lisp.CachedSource{}}
			cache.panicStore = tc.which == "store"
			cache.panicLoad = tc.which == "load"
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
			require.NotEqual(t, lisp.LError, env.InPackage(lisp.String(lisp.DefaultUserPackage)).Type)
			env.Runtime.LoadCache = cache

			var v *lisp.LVal
			require.NotPanics(t, func() {
				v = env.LoadLocation("a.lisp", "/a.lisp", strings.NewReader(src))
			}, "a panicking %s escaped (*LEnv).LoadLocation", tc.name)
			require.NotEqual(t, lisp.LError, v.Type, "the load must still succeed: %v", v)
			assert.Equal(t, `"A"`, env.GetGlobal(lisp.Symbol("who")).String())

			// And the cache is not left disabled: with the bug removed the
			// next load caches normally.
			cache.panicStore, cache.panicLoad = false, false
			require.NotEqual(t, lisp.LError,
				env.LoadLocation("a.lisp", "/a.lisp", strings.NewReader(src)).Type)
			assert.Equal(t, 1, cache.stores, "the re-entrancy guard was left set")
		})
	}
}

// --- the on-path cycle guard, on the paths with no cache installed ---

// cycleTree returns one expression that reaches itself after n links, so the
// same shape can be tested both far inside and far outside the depth at which
// the non-strict walk starts recording its on-path set.
func cycleTree(n int) *lisp.LVal {
	root := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn")})
	node := root
	for range n {
		next := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn")})
		node.Cells = append(node.Cells, next)
		node = next
	}
	node.Cells = append(node.Cells, root)
	return root
}

// The non-strict walk allocates no memo, so its cycle guard records only the
// nodes on the current path and only past loaderWalkPathRecordDepth.  That is
// exact — a cycle is unbounded in depth by construction, so it always passes
// the recording depth and repeats afterwards — but it is exact for a reason
// that is easy to break, so both a cycle far shorter than the recording depth
// and one far longer are pinned here, on the two public constructors that
// have no cache installed.
func TestPublicAdmissionRefusesCycle(t *testing.T) {
	t.Parallel()
	for _, links := range []int{1, 3, 200} {
		done := make(chan [2]error, 1)
		go func() {
			_, e1 := lisp.ReadProgram(graphReader{tree: cycleTree(links)}, "c.lisp", strings.NewReader("x"))
			_, e2 := lisp.TextLoader(graphReader{tree: cycleTree(links)}, "c.lisp", strings.NewReader("x"))
			done <- [2]error{e1, e2}
		}()
		select {
		case errs := <-done:
			require.Error(t, errs[0], "ReadProgram admitted a %d-link cycle", links)
			assert.Contains(t, errs[0].Error(), "not a finite tree")
			require.Error(t, errs[1], "TextLoader admitted a %d-link cycle", links)
			assert.Contains(t, errs[1].Error(), "not a finite tree")
		case <-time.After(30 * time.Second):
			t.Fatalf("admission of a %d-link cycle did not terminate", links)
		}
	}
}

// --- round-four blocker: quoted data is not code, and the size rules know it ---

// streamReader hands back a caller-built expression stream unchanged.  Unlike
// graphReader it returns more than one top-level expression, which is what the
// stream-level rules (the budget, the cycle guard, the on-path unwind) are
// about.
type streamReader struct{ exprs []*lisp.LVal }

func (r streamReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	return r.exprs, nil
}
func (r streamReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

// doublingDAG returns a depth-n doubling DAG: n+1 distinct nodes whose
// unfolded size is 2^(n+1)-1.  Nothing about it is large; everything about it
// is shared.
func doublingDAG(n int) *lisp.LVal {
	node := sealedValue(7)
	for range n {
		node = lisp.SExpr([]*lisp.LVal{node, node})
	}
	return node
}

// loadWithin runs one load on its own goroutine so a rule that stops bounding
// the walk shows up as a test failure rather than as a suite that never ends.
func loadWithin(t *testing.T, env *lisp.LEnv, name string, d time.Duration) *lisp.LVal {
	t.Helper()
	done := make(chan *lisp.LVal, 1)
	go func() { done <- env.Load(name, strings.NewReader("x")) }()
	select {
	case v := <-done:
		return v
	case <-time.After(d):
		t.Fatalf("%s: load did not terminate within %v", name, d)
		return nil
	}
}

// TestLoadCacheAdmitsSharedQuotedData is the round-four blocker.
//
// The Unbounded sentinel's justification is "nothing that terminates looks
// like 4.3e9 node evaluations".  That is a claim about CODE.  (*LEnv).eval
// returns a quoted node without descending into it and opQuote hands its
// argument back unwalked, so a quoted 41-node DAG whose unfolded size is 2^41
// evaluates in O(1) — it loads instantly with no cache installed, and the
// unfolded-size rule hard-failed it the moment a cache was installed.
//
// Both spellings are covered because the walk recognises them by different
// means: the PARSER's form is a node with the quoted flag set (rdparser's
// ParseQuote calls lisp.Quote), and a hand-built Reader's is an explicit
// (quote X) s-expression.
//
// The load is not CACHED — the quote discount deliberately does not apply to
// the TooLarge budget, so this lands there — and that is the point: TooLarge
// falls back to an uncached load, which is byte-identical to having no cache.
func TestLoadCacheAdmitsSharedQuotedData(t *testing.T) {
	t.Parallel()
	forms := map[string]func() *lisp.LVal{
		"quote-form": func() *lisp.LVal {
			root := lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), doublingDAG(40)})
			root.SealAST()
			return root
		},
		"quoted-flag": func() *lisp.LVal {
			root := lisp.Quote(doublingDAG(40))
			root.SealAST()
			return root
		},
	}
	for name, build := range forms {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			exprs := []*lisp.LVal{build(), sealedValue(5)}
			rd := streamReader{exprs: exprs}

			off := loadWithin(t, readerEnv(t, rd, nil), "dag.lisp", 30*time.Second)
			require.NotEqual(t, lisp.LError, off.Type,
				"control: a quoted DAG loads fine with no cache installed")
			assert.Equal(t, "5", off.String())

			cache := newTestLoadCache()
			on := loadWithin(t, readerEnv(t, rd, cache), "dag.lisp", 30*time.Second)
			if on.Type == lisp.LError {
				t.Fatalf("installing a cache hard-failed a terminating program: %.200v", on)
			}
			assert.Equal(t, "5", on.String(), "installing a cache changed the result")
			assert.Zero(t, cache.stores,
				"quoted sharing still unfolds for Copy/firstUnsealed/fingerprint, so it must not be STORED")
		})
	}
}

// The discriminator has to be the quoting, not the sharing: the same DAG with
// the quote taken off is 2^41 node EVALUATIONS and is still refused outright.
// TestLoadCacheRefusesExponentialSharing pins that from the other side; this
// pins the two against each other so the discount cannot quietly widen.
func TestLoadCacheQuoteDiscountIsWhatSeparatesThem(t *testing.T) {
	t.Parallel()
	dag := doublingDAG(40)

	quoted := lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), dag})
	quoted.SealAST()
	cacheQ := newTestLoadCache()
	vq := loadWithin(t, readerEnv(t, streamReader{exprs: []*lisp.LVal{quoted}}, cacheQ), "q.lisp", 30*time.Second)
	assert.NotEqual(t, lisp.LError, vq.Type, "quoted: data, admitted uncached")

	bare := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), dag})
	bare.SealAST()
	cacheB := newTestLoadCache()
	vb := loadWithin(t, readerEnv(t, streamReader{exprs: []*lisp.LVal{bare}}, cacheB), "b.lisp", 30*time.Second)
	require.Equal(t, lisp.LError, vb.Type, "unquoted: 2^41 evaluations, still refused")
	assert.Contains(t, vb.String(), "not a finite tree")

	assert.Zero(t, cacheQ.stores)
	assert.Zero(t, cacheB.stores)
}

// A quoted DAG SMALL enough to be cached is stored and served aliased, and
// that has to stay bounded: the walks downstream of admission — (*LVal).Copy,
// firstUnsealed, SealedASTFingerprint and, under -tags elpscheck, the entry
// re-verification on every hit — all unfold sharing and none of them stops at
// a quote.  They are bounded because the TooLarge budget is taken against the
// QUOTE-BLIND count, so anything that reaches the store has an unfolded size
// under loaderWalkMaxNodes by construction.  This is that case exercised end
// to end: stored once, served from the cache, still correct.
func TestLoadCacheServesModestQuotedDAG(t *testing.T) {
	t.Parallel()
	// 18 distinct nodes, 2^18 unfolded: shared hard, comfortably under budget.
	root := lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), doublingDAG(17)})
	root.SealAST()
	rd := streamReader{exprs: []*lisp.LVal{root, sealedValue(11)}}
	cache := newTestLoadCache()

	first := loadWithin(t, readerEnv(t, rd, cache), "small.lisp", 30*time.Second)
	require.NotEqual(t, lisp.LError, first.Type)
	require.Equal(t, 1, cache.stores, "a modest quoted DAG is cacheable")

	second := loadWithin(t, readerEnv(t, rd, cache), "small.lisp", 30*time.Second)
	require.NotEqual(t, lisp.LError, second.Type)
	assert.Equal(t, first.String(), second.String(), "the served entry ran a different program")
	assert.Equal(t, 1, cache.stores, "the second load must be a hit, not a re-store")
	assert.Equal(t, 1, cache.hits)
}

// Quasiquote gets NO discount, and the reason is mechanical rather than
// cautious: opQuasiquote calls findAndUnquote, which descends the entire
// payload — through quote levels included — looking for unquote forms.  Its
// cost is the quote-blind unfolded size, so a quasiquoted sharing bomb really
// does not terminate and really is refused.
func TestLoadCacheRefusesQuasiquotedSharing(t *testing.T) {
	t.Parallel()
	for _, head := range []string{"quasiquote", "lisp:quasiquote"} {
		t.Run(head, func(t *testing.T) {
			t.Parallel()
			// The payload is itself quoted, which is the shape that would
			// slip through if the discount were applied by node rather than
			// by enclosing form.
			root := lisp.SExpr([]*lisp.LVal{lisp.Symbol(head), lisp.Quote(doublingDAG(40))})
			root.SealAST()
			cache := newTestLoadCache()
			v := loadWithin(t, readerEnv(t, streamReader{exprs: []*lisp.LVal{root}}, cache), "qq.lisp", 30*time.Second)
			require.Equal(t, lisp.LError, v.Type)
			assert.Contains(t, v.String(), "not a finite tree")
			assert.Zero(t, cache.stores)
		})
	}
}

// --- round-four minor 2: the depth cap must survive a memo hit ---

// TestLoadCacheDepthCapSurvivesMemoHit pins the fix for a memo that answered
// with a subtree's SIZE and nothing about its interior DEPTH.  Two 60k-deep
// chains whose second ends at the first's head are 120k deep in total — past
// loaderWalkMaxDepth — but the second chain's walk reached the shared head at
// depth 60k and took the memo's word for it, so the stream was admitted and
// STORED.  newProgram's promise that the walks after admission see
// depth-bounded output was false for exactly that entry.
func TestLoadCacheDepthCapSurvivesMemoHit(t *testing.T) {
	t.Parallel()
	chain := func(depth int, tail *lisp.LVal) *lisp.LVal {
		node := tail
		if node == nil {
			node = sealedValue(1)
		}
		for range depth {
			node = lisp.SExpr([]*lisp.LVal{node})
		}
		return node
	}
	chainA := chain(60000, nil)
	exprA := lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), chainA})
	exprA.SealAST()
	exprB := lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), chain(60000, chainA)})
	exprB.SealAST()

	cache := newTestLoadCache()
	env := readerEnv(t, streamReader{exprs: []*lisp.LVal{exprA, exprB, sealedValue(3)}}, cache)
	v := loadWithin(t, env, "deep.lisp", 30*time.Second)

	assert.Zero(t, cache.stores,
		"a stream whose real depth is 120k must not be admitted, memo hit or no memo hit")
	// Depth is the Unbounded class on every path (it is not a budget: the
	// alternative to refusing is a Go stack overflow), so the load fails
	// rather than falling back.  Asserted so the classification cannot drift
	// silently.
	require.Equal(t, lisp.LError, v.Type)
	assert.Contains(t, v.String(), "not a finite tree")
}
