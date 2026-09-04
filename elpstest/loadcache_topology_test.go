// Copyright © 2026 The ELPS authors

// The deployed topology, run through the isolation and parity oracles.
//
// substrate's production shape is ONE preheated template, N forks (one per
// transaction), and ONE warm lisp.LoadCache shared by all of them: the cache
// travels across Fork (lisp/fork.go, the LoadCache line of the forked
// Runtime), and docs/fork.md states the sharing is safe because every entry
// is immutable and sealed throughout.  The mechanism has been verified by
// hand, and every load-cache test on commit 74e4ac8 — FuzzLoadCacheMultiEnv,
// FuzzLoadCacheHostileReader, the custody and sharing tests — exercises it
// over INDEPENDENT environments.  No TransactionCheck installed a cache, so
// the shape an embedder actually ships had never been run through an
// isolation oracle.  These tests close that (issue #600, gap 6).
//
// The property, in the owner's words: for two VMs instantiated from the
// same source, ELPS code should work identically to two forks from the same
// template.  Fork is a performance optimisation.  Parity with cold VMs is
// the spec; isolation and fidelity follow from it.
//
// What is asserted, and by which test:
//
//   - TestLoadCacheTopology_Isolation: elpstest.CheckTransactions over a
//     template whose cache is shared by the template, its forks and the
//     concurrent arm's second template.  Every isolation property, with the
//     cache installed, sequentially and concurrently (the concurrent arm is
//     the -race gate).
//
//   - TestLoadCacheTopology_Parity: elpstest.RunForkCheck, whose cold arm
//     builds a fresh environment per transaction.  With a fresh cache per
//     NewEnv call, the fork arm HITS the entries the template and earlier
//     forks minted while the cold arm MISSES in its own cache — parity
//     across cache warmth, on the oracle that already exists on this base.
//
//   - TestLoadCacheTopology_SharedEntryIdentity: the documented design,
//     asserted explicitly.  Two forks that hit one key hold the SAME *LVal;
//     it is sealed; the in-place mutation is refused identically on every
//     fork; and each fork's fingerprint equals a cold environment's that
//     used its own fresh cache — from lisp's point of view a shared sealed
//     entry is indistinguishable from a private one.
//
//   - TestLoadCacheTopology_PrivateCopyControl: control (b).  Each fork gets
//     a PRIVATE cache, so no two forks share a node.  Every assertion above
//     still passes EXCEPT the pointer-identity one: over-copying is not a
//     leak, and the identity assertion is the only one in this file that
//     can tell sharing from copying.
//
//   - TestLoadCacheTopology_MixedWarmth: control (c).  Half the forks carry
//     a private cache that diverges from the template's (miss on the fork,
//     hit on the template).  No transaction's result or state changes.
//
//   - TestLoadCacheTopology_NativeAnnotationIsReported: the hostile-reader
//     angle.  A Native annotation on a sealed non-LNative header reaches
//     every fork by reference, and the native oracle reports it — the
//     closed-gap negative control for commit aa0dbe4.  See its comment.
//
//   - loadcache_topology_unsealed_test.go: control (a), the shape the seal
//     exists to forbid — one unsealed parse handed to every fork — in a
//     !elpscheck file because the ownership checker refuses it first.
//
//   - TestLoadCacheTopology_CheckParity: the same topology through
//     elpstest.CheckParity, the headline oracle — n forks sharing the
//     template's warm cache against n cold environments each on a fresh
//     cache of its own, per-transaction results and post-run state, over
//     every schedule and hop count the oracle offers.
//
// The identity test's cold arm predates CheckParity on this branch and is
// written directly over FingerprintEnv; it stays as it is, and
// TestLoadCacheTopology_CheckParity is the additional assertion through
// the oracle proper.
package elpstest_test

import (
	"fmt"
	"io"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// ---------------------------------------------------------------------------
// The cache double
// ---------------------------------------------------------------------------

// topologyCache is the minimal LoadCache the interface's doc comment
// describes — a map behind a mutex — with counters so a test can assert
// the hook is doing its job (a shared cache parses each source once) rather
// than silently asserting nothing about it, which is FuzzLoadCacheMultiEnv's
// assertion 4 applied here.
type topologyCache struct {
	mu      sync.Mutex
	entries map[string]*lisp.CachedSource
	byName  map[string]int // stores per entry Name(): a second store of one name is a concurrent double miss
	loads   int
	hits    int
	stores  int
}

func newTopologyCache() *topologyCache {
	return &topologyCache{entries: make(map[string]*lisp.CachedSource), byName: make(map[string]int)}
}

func (c *topologyCache) Load(key string) (*lisp.CachedSource, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.loads++
	src, ok := c.entries[key]
	if ok {
		c.hits++
	}
	return src, ok
}

func (c *topologyCache) Store(key string, src *lisp.CachedSource) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.stores++
	c.byName[src.Name()]++
	c.entries[key] = src
}

// storesNamed reports how many entries were stored under stream name.
func (c *topologyCache) storesNamed(name string) int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.byName[name]
}

func (c *topologyCache) counts() (loads, hits, stores int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.loads, c.hits, c.stores
}

// newTopologyEnv builds the template the way NewForkCheckEnv does, with
// reader and cache installed BEFORE the library loads so the whole
// preheat runs under the cache, as an embedder's does.
func newTopologyEnv(reader lisp.Reader, cache lisp.LoadCache) (*lisp.LEnv, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = reader
	env.Runtime.LoadCache = cache
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return env, nil
}

// ---------------------------------------------------------------------------
// The program and the transactions
// ---------------------------------------------------------------------------

// topologyProgram is the template's program.  Its own load-string mints a
// cache entry DURING template load (the "pre-populated" half of the warm
// cache); the forks' transactions mint and hit the rest.
const topologyProgram = `
(set 'counter 0)
(set 'log (vector))
(defun bump () (set 'counter (+ counter 1)))
(load-string "(set 'tmpl-lit '(5 4 6))" :name "tmpl.lisp")
`

// topologySharedSource is loaded by EVERY fork after it is taken: the first
// fork to load it mints the entry (a miss), every later fork hits it, and
// the template never loads it at all — so a node two forks hold in common
// under 'shared-lit can only have come through the cache, never through
// Fork's own sealed-node sharing.
const topologySharedSource = `(set 'shared-lit '(3 1 2))`

// topologyLateSource is loaded only by forks 1 and 3: a fork minting an
// entry a LATER fork hits, with forks that never touched the key in
// between.
const topologyLateSource = `(set 'late-lit '(9 7 8))`

// topologyForks is the fork count.  Four is the minimum the task set; six
// gives every role two occupants (minter, hitter, late minter, late hitter,
// and two that never load the late key).
const topologyForks = 6

// topologyTx is transaction i.  Every fork: loads the shared source
// (miss on fork 0, hit after), loads the late source on the odd forks,
// reads through the cached values, mutates its own state (a different
// number of bumps per fork, so the forks are distinguishable), and attempts
// an in-place mutation of BOTH cached literals under a handler that turns
// the refusal into a value the test can compare across forks — the
// transaction itself must not error, or the oracle stops.
func topologyTx(i int) string {
	late := ""
	if i%2 == 1 {
		late = fmt.Sprintf("(load-string %q :name \"late.lisp\")\n(set 'late-seen (foldl + 0 late-lit))\n", topologyLateSource)
	}
	return fmt.Sprintf(`
(load-string %q :name "shared.lisp")
%s(set 'seen (list (foldl + 0 shared-lit) (first shared-lit) (length tmpl-lit)))
(dotimes (n %d) (bump))
(append! log %d)
(set 'sort-result
  (handler-bind ((modify-literal-error (lambda (c &rest _) (list 'refused c))))
    (stable-sort < shared-lit)))
(set 'tmpl-sort-result
  (handler-bind ((modify-literal-error (lambda (c &rest _) (list 'refused c))))
    (stable-sort < tmpl-lit)))
(set 'after (list shared-lit tmpl-lit (stable-sort < (copy shared-lit))))
`, topologySharedSource, late, i+1, i)
}

func topologyTxs() []string {
	txs := make([]string, topologyForks)
	for i := range txs {
		txs[i] = topologyTx(i)
	}
	return txs
}

// topologyOpts is the fingerprint configuration CheckTransactions uses for
// template-level comparisons (elpstest.templateOpts, which is unexported).
var topologyOpts = elpstest.FingerprintOptions{Seal: true, PackageMetadata: true}

// ---------------------------------------------------------------------------
// The harness the explicit assertions run over
// ---------------------------------------------------------------------------

// forkRun is what one transaction left behind on one environment.
type forkRun struct {
	name       string
	env        *lisp.LEnv
	result     string
	fp         *elpstest.Fingerprint
	sharedLit  *lisp.LVal // the node bound to shared-lit, by identity
	sortResult string
	tmplSort   string
	after      string
	seen       string
}

// topologyRun runs the deployed topology by hand — template, forks, one
// transaction per fork — and, for every fork, a COLD environment on its
// own fresh cache running the same transaction.  forkCache, when non-nil,
// substitutes the cache a fork carries (nil keeps the template's).
func topologyRun(t *testing.T, shared *topologyCache, forkCache func(i int) lisp.LoadCache, concurrent bool) (forks, colds []forkRun) {
	t.Helper()
	tmpl, err := newTopologyEnv(parser.NewReader(), shared)
	if err != nil {
		t.Fatalf("template: %v", err)
	}
	if rc := tmpl.LoadString("program.lisp", topologyProgram); rc.Type == lisp.LError {
		t.Fatalf("program: %v", rc)
	}
	forks = make([]forkRun, topologyForks)
	for i := range forks {
		f, err := tmpl.Fork()
		if err != nil {
			t.Fatalf("fork %d: %v", i, err)
		}
		if forkCache != nil {
			if c := forkCache(i); c != nil {
				f.Runtime.LoadCache = c
			}
		}
		forks[i] = forkRun{name: fmt.Sprintf("fork %d", i), env: f}
	}
	if concurrent {
		var wg sync.WaitGroup
		for i := range forks {
			wg.Add(1)
			go func(i int) {
				defer wg.Done()
				forks[i].result = renderTx(forks[i].env, i)
			}(i)
		}
		wg.Wait()
	} else {
		for i := range forks {
			forks[i].result = renderTx(forks[i].env, i)
		}
	}
	for i := range forks {
		forks[i].read(t)
	}
	// The template never loaded the shared source, so the node the forks
	// hold in common under shared-lit came through the cache and not
	// through Fork's sealed-node sharing (topologySharedSource's comment).
	if v := tmpl.GetGlobal(lisp.Symbol("shared-lit")); v.Type != lisp.LError {
		t.Fatalf("the template binds shared-lit (%v); the forks' sharing would then be Fork's, not the cache's", v)
	}
	colds = make([]forkRun, topologyForks)
	for i := range colds {
		env, err := newTopologyEnv(parser.NewReader(), newTopologyCache())
		if err != nil {
			t.Fatalf("cold %d: %v", i, err)
		}
		if rc := env.LoadString("program.lisp", topologyProgram); rc.Type == lisp.LError {
			t.Fatalf("cold %d program: %v", i, rc)
		}
		colds[i] = forkRun{name: fmt.Sprintf("cold %d", i), env: env}
		colds[i].result = renderTx(env, i)
		colds[i].read(t)
	}
	return forks, colds
}

func renderTx(env *lisp.LEnv, i int) string {
	rc := env.LoadString(fmt.Sprintf("tx%d.lisp", i), topologyTx(i))
	if rc.Type == lisp.LError {
		return "error: " + rc.String()
	}
	return rc.Type.String() + " " + rc.String()
}

func (r *forkRun) read(t *testing.T) {
	t.Helper()
	if strings.HasPrefix(r.result, "error:") {
		t.Fatalf("%s: transaction failed: %s", r.name, r.result)
	}
	r.fp = elpstest.FingerprintEnv(r.env, topologyOpts)
	r.sharedLit = r.env.GetGlobal(lisp.Symbol("shared-lit"))
	if r.sharedLit == nil || r.sharedLit.Type == lisp.LError {
		t.Fatalf("%s: shared-lit is not bound: %v", r.name, r.sharedLit)
	}
	r.sortResult = renderGlobal(r.env, "sort-result")
	r.tmplSort = renderGlobal(r.env, "tmpl-sort-result")
	r.after = renderGlobal(r.env, "after")
	r.seen = renderGlobal(r.env, "seen")
}

func renderGlobal(env *lisp.LEnv, name string) string {
	v := env.GetGlobal(lisp.Symbol(name))
	if v == nil {
		return "<nil>"
	}
	return v.Type.String() + " " + v.String()
}

// assertForkParity holds everything that must be true of a fork's
// transaction REGARDLESS of whether its cache shared nodes with the other
// forks: the refusal is identical everywhere, the cached values read the
// same, and the fork is fingerprint-identical to a cold environment that
// ran the same transaction on its own fresh cache.
func assertForkParity(t *testing.T, forks, colds []forkRun) {
	t.Helper()
	// The handler's value, rendered: the in-place sort of a sealed cached
	// literal raises modify-literal-error (lisp/builtins.go, builtinSortStable's
	// sealed branch) and the handler turns it into this list.
	const wantSort = "list '('refused 'modify-literal-error)"
	for i, f := range forks {
		c := colds[i]
		if f.sortResult != wantSort {
			t.Errorf("%s: in-place sort of the cached literal: got %s, want %s", f.name, f.sortResult, wantSort)
		}
		if f.tmplSort != wantSort {
			t.Errorf("%s: in-place sort of the template's cached literal: got %s, want %s", f.name, f.tmplSort, wantSort)
		}
		if f.sortResult != c.sortResult || f.tmplSort != c.tmplSort {
			t.Errorf("%s: refusal differs from the cold run\n  fork: %s / %s\n  cold: %s / %s", f.name, f.sortResult, f.tmplSort, c.sortResult, c.tmplSort)
		}
		if f.after != c.after || f.seen != c.seen {
			t.Errorf("%s: reads through the cached literal differ from the cold run\n  fork: %s %s\n  cold: %s %s", f.name, f.after, f.seen, c.after, c.seen)
		}
		if f.result != c.result {
			t.Errorf("%s: transaction result differs from the cold run\n  fork: %s\n  cold: %s", f.name, f.result, c.result)
		}
		if !f.fp.Equal(c.fp) {
			t.Errorf("%s: reachable state differs from the cold run on its own fresh cache\n%s", f.name, c.fp.Diff(f.fp))
		}
		if !f.sharedLit.IsSealed() {
			t.Errorf("%s: shared-lit is not sealed", f.name)
		}
		if got := f.sharedLit.String(); got != "'(3 1 2)" {
			t.Errorf("%s: shared-lit reads %s after every fork's refused sort; want '(3 1 2)", f.name, got)
		}
	}
	// The forks are distinguishable from one another (a different bump
	// count each), or the parity comparison above would pass between any
	// two of them for free.
	for i := 1; i < len(forks); i++ {
		if forks[i].fp.Equal(forks[0].fp) {
			t.Errorf("%s and %s are fingerprint-identical; the transactions do not distinguish the forks", forks[i].name, forks[0].name)
		}
	}
}

// ---------------------------------------------------------------------------
// 1. The isolation oracle, with the cache installed
// ---------------------------------------------------------------------------

// TestLoadCacheTopology_Isolation runs CheckTransactions over the deployed
// topology.  ONE cache is shared by everything NewEnv builds — the
// template, its forks (the cache travels across Fork), and the concurrent
// arm's second template, which therefore HITS the program entry the first
// template minted.  The oracle's every property is asserted with the cache
// in place; the concurrent arm doubles as the -race gate over the cache's
// own locking and over the hit path serving one sealed tree to several
// goroutines at once.
func TestLoadCacheTopology_Isolation(t *testing.T) {
	t.Parallel()
	shared := newTopologyCache()
	got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		NewEnv:                func() (*lisp.LEnv, error) { return newTopologyEnv(parser.NewReader(), shared) },
		Program:               topologyProgram,
		Tx:                    topologyTxs(),
		ExpectNoSharedNatives: true,
		Repro:                 "elpstest/loadcache_topology_test.go: one template, six forks, one shared LoadCache",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	for _, w := range got {
		t.Errorf("%s", w)
	}
	loads, hits, stores := shared.counts()
	t.Logf("shared cache after CheckTransactions: loads=%d hits=%d stores=%d", loads, hits, stores)
	// The cache did its job.  CheckTransactions on this base runs its
	// parity channel FIRST (aliasguard_parity.go, commit f3538f1): one more
	// template plus one cold environment per transaction, all built by
	// NewEnv and so all on the shared cache, which mints the six
	// env<i>-tx0.lisp entries (same sources, different names — the key
	// binds both, lisp/loadcache.go loadCacheKey).  The sweep then mints
	// program.lisp, tmpl.lisp, shared.lisp, late.lisp and six tx<i>.lisp
	// (10), and property 5 runs tx 0 ON THE TEMPLATE as tmpl-tx0.lisp
	// (templateToForkWitnesses) — an 11th, and only one, because the first
	// transaction moves the template and that sweep stops.  17 in all; the
	// concurrent arm re-runs names and sources already minted and must hit
	// every one.  A store count above 17 means the shared cache parsed
	// something twice — the property FuzzLoadCacheMultiEnv asserts over
	// independent environments, asserted here over forks.  Measured on the
	// rebased tree (c684e7b + this file): loads=80 hits=63 stores=17; on
	// 74e4ac8, before the parity channel, it was loads=36 hits=25 stores=11.
	if stores != 17 {
		t.Errorf("the shared cache stored %d entries; the oracle over this topology mints exactly 17 distinct (name, source) pairs", stores)
	}
	if hits == 0 || loads-hits != stores {
		t.Errorf("loads=%d hits=%d stores=%d: every load that was not a store must have been a hit", loads, hits, stores)
	}
}

// ---------------------------------------------------------------------------
// 2. The parity oracle that exists on this base
// ---------------------------------------------------------------------------

// TestLoadCacheTopology_Parity is RunForkCheck — fork, fork-of-fork and a
// COLD environment per transaction, compared on result, reachable state
// and alias structure — with a FRESH cache per NewEnv call.  The template's
// cache is warm from the template's own load and from every earlier
// transaction's fork (so the fork arms hit shared.lisp from tx[1] on, and
// the fork-of-fork arm hits it on tx[0] already), while each cold arm
// misses in a cache of its own.  Parity across cache warmth, on the oracle
// this base already has.
func TestLoadCacheTopology_Parity(t *testing.T) {
	t.Parallel()
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		NewEnv:  func() (*lisp.LEnv, error) { return newTopologyEnv(parser.NewReader(), newTopologyCache()) },
		Program: topologyProgram,
		Tx:      topologyTxs(),
	})
}

// ---------------------------------------------------------------------------
// 3. Cross-fork identity, asserted explicitly
// ---------------------------------------------------------------------------

// TestLoadCacheTopology_SharedEntryIdentity asserts the documented design
// rather than only its consequences: two forks that hit one key hold the
// SAME *LVal (docs/fork.md, "LoadCache: shared"), that value is sealed, no
// fork can write through it, and — the parity assertion for the cache —
// each fork is fingerprint-identical to a cold environment that used its
// own private cache.  A shared sealed entry is indistinguishable from a
// private one from lisp's point of view.
//
// Run sequentially and concurrently.  The sequential arm states the strict
// form: one store of shared.lisp, five hits, ONE node held by all six
// forks.  The concurrent arm is the -race gate over the hit path, and its
// counts are deliberately NOT the sequential ones: the six forks first-touch
// shared.lisp at once, the LoadCache contract (lisp/loadcache.go) promises
// no single-flight — "elps makes no assumption that a Stored entry is later
// Loadable" — so two or more forks can miss together, each parse its own
// tree and each Store it, last write winning.  Measured on commit 74e4ac8
// over 60 runs of this arm: 46 stored every entry once; shared.lisp was
// stored twice in 9 runs and three times in 2, late.lisp twice in 5 and
// three times in 2.  That is a repeated parse, not a leak —
// assertForkParity holds on every run — and the identity assertion for
// that arm is the exact contract: the forks hold as many distinct
// shared-lit nodes as the cache stored shared.lisp entries, never more,
// and never a cold environment's.
func TestLoadCacheTopology_SharedEntryIdentity(t *testing.T) {
	t.Parallel()
	for _, concurrent := range []bool{false, true} {
		name := "sequential"
		if concurrent {
			name = "concurrent"
		}
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			shared := newTopologyCache()
			forks, colds := topologyRun(t, shared, nil, concurrent)
			assertForkParity(t, forks, colds)
			loads, hits, stores := shared.counts()
			sharedStores := shared.storesNamed("shared.lisp")
			lateStores := shared.storesNamed("late.lisp")
			t.Logf("shared cache: loads=%d hits=%d stores=%d (shared.lisp stored %d times, late.lisp %d)", loads, hits, stores, sharedStores, lateStores)
			assertSharedIdentity(t, forks, colds, sharedStores)
			// Every load is a hit or a store; nothing is served from
			// nowhere and nothing is parsed without being offered.
			if loads != hits+stores {
				t.Errorf("loads=%d hits=%d stores=%d: every load is either a hit or a store", loads, hits, stores)
			}
			// program.lisp, tmpl.lisp, shared.lisp, late.lisp, six
			// tx<i>.lisp: ten distinct (name, source) pairs, plus one
			// store per concurrent double miss on the two keys the forks
			// race for.
			if want := 10 + (sharedStores - 1) + (lateStores - 1); stores != want {
				t.Errorf("the shared cache stored %d entries, want %d: ten distinct sources plus the double misses", stores, want)
			}
			if concurrent {
				return
			}
			// The strict form, sequentially: shared.lisp is parsed once and
			// hit by five forks; late.lisp is parsed once and hit by two of
			// the three odd forks.
			if sharedStores != 1 || lateStores != 1 {
				t.Errorf("sequential forks stored shared.lisp %d times and late.lisp %d times; want once each", sharedStores, lateStores)
			}
			if hits != 7 {
				t.Errorf("the shared cache served %d hits; five forks hit shared.lisp and two hit late.lisp", hits)
			}
		})
	}
}

// assertSharedIdentity is the ONE assertion in this file that can tell a
// shared entry from a private copy: the forks hold exactly `classes`
// distinct *LVal nodes under shared-lit — one per shared.lisp entry the
// cache stored, so ONE in the sequential arm — and no cold environment
// holds any of them.
func assertSharedIdentity(t *testing.T, forks, colds []forkRun, classes int) {
	t.Helper()
	distinct := map[*lisp.LVal][]string{}
	for _, f := range forks {
		distinct[f.sharedLit] = append(distinct[f.sharedLit], f.name)
	}
	if len(distinct) != classes {
		var groups []string
		for node, names := range distinct {
			groups = append(groups, fmt.Sprintf("%p held by %s", node, strings.Join(names, ", ")))
		}
		t.Errorf("the forks hold %d distinct nodes under shared-lit, want %d (one per stored shared.lisp entry; a shared cache serves one sealed tree per store):\n  %s", len(distinct), classes, strings.Join(groups, "\n  "))
	}
	for _, c := range colds {
		if _, held := distinct[c.sharedLit]; held {
			t.Errorf("%s holds a fork's node under shared-lit; a cold environment on its own cache must have parsed its own", c.name)
		}
	}
}

// ---------------------------------------------------------------------------
// Control (b): a cache that gives every environment a private copy
// ---------------------------------------------------------------------------

// TestLoadCacheTopology_PrivateCopyControl is control (b).  CachedSource is
// opaque — only lisp.newCachedSource mints one, and no exported member
// yields its nodes — so a LoadCache cannot hand out a COPY of an entry; the
// nearest shape an embedder can build is a private cache per environment,
// which is what an over-copying cache degenerates to from lisp's point of
// view: every environment holds its own sealed nodes for every entry, and
// hits only its own.  Under it every assertion in assertForkParity still
// passes, and assertSharedIdentity is the ONLY thing that notices — stated
// here as the point of the control: over-copying is not a leak, and the
// identity assertion is the one assertion that distinguishes sharing from
// copying.  Red-on-weakening for the identity assertion is this test's
// inverse: measured on commit 74e4ac8, running the sequential identity arm
// over private fork caches fails it ("the forks hold 6 distinct nodes under
// shared-lit, want 0" — the shared cache never stored shared.lisp at all).
func TestLoadCacheTopology_PrivateCopyControl(t *testing.T) {
	t.Parallel()
	shared := newTopologyCache()
	forks, colds := topologyRun(t, shared, func(int) lisp.LoadCache { return newTopologyCache() }, false)
	assertForkParity(t, forks, colds)
	for i := 1; i < len(forks); i++ {
		if forks[i].sharedLit == forks[0].sharedLit {
			t.Errorf("%s and %s hold the same node under private caches; the control is not private", forks[i].name, forks[0].name)
		}
	}
	// The template's cache saw only the template's own loads: nothing a
	// fork did reached it, so every fork parsed shared.lisp itself.
	if loads, hits, stores := shared.counts(); stores != 2 || hits != 0 {
		t.Errorf("template cache: loads=%d hits=%d stores=%d; with private fork caches it holds program.lisp and tmpl.lisp only, unhit", loads, hits, stores)
	}
	// And the oracle agrees that copying is not a leak.
	got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		NewEnv:  func() (*lisp.LEnv, error) { return newTopologyEnv(parser.NewReader(), newTopologyCache()) },
		Program: topologyProgram,
		Tx:      topologyTxs(),
		Fork: func(tmpl *lisp.LEnv) (*lisp.LEnv, error) {
			f, err := tmpl.Fork()
			if err != nil {
				return nil, err
			}
			f.Runtime.LoadCache = newTopologyCache()
			return f, nil
		},
		ExpectNoSharedNatives: true,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	for _, w := range got {
		t.Errorf("%s", w)
	}
}

// ---------------------------------------------------------------------------
// Control (c): forks whose cache warmth diverges from the template's
// ---------------------------------------------------------------------------

// TestLoadCacheTopology_MixedWarmth is control (c).  The shared cache is
// pre-warmed by a throw-away environment so the TEMPLATE hits every entry
// it loads; the odd forks then carry a fresh PRIVATE cache (every load a
// miss) while the even forks keep the shared one (every load a hit).
// Parity across cache warmth: every fork matches its cold reference, the
// odd forks match the even ones on everything but the fork index, and
// CheckTransactions with the same mixed fork walker reports nothing.
func TestLoadCacheTopology_MixedWarmth(t *testing.T) {
	t.Parallel()
	shared := newTopologyCache()
	warm, err := newTopologyEnv(parser.NewReader(), shared)
	if err != nil {
		t.Fatalf("warmer: %v", err)
	}
	if rc := warm.LoadString("program.lisp", topologyProgram); rc.Type == lisp.LError {
		t.Fatalf("warmer program: %v", rc)
	}
	for i := range topologyForks {
		if rc := warm.LoadString(fmt.Sprintf("tx%d.lisp", i), topologyTx(i)); rc.Type == lisp.LError {
			t.Fatalf("warmer tx %d: %v", i, rc)
		}
	}
	_, _, warmed := shared.counts()
	mixed := func(i int) lisp.LoadCache {
		if i%2 == 1 {
			return newTopologyCache()
		}
		return nil
	}
	forks, colds := topologyRun(t, shared, mixed, false)
	assertForkParity(t, forks, colds)
	loads, hits, stores := shared.counts()
	t.Logf("shared cache: warmed=%d then loads=%d hits=%d stores=%d", warmed, loads, hits, stores)
	if stores != warmed {
		t.Errorf("the template and the even forks stored %d new entries over a fully warm cache; every load of theirs must hit", stores-warmed)
	}
	// The even forks' shared-lit is the warmer's node, served by the
	// cache; the odd forks parsed their own.  Both read identically —
	// assertForkParity said so — which is the control's whole claim.
	for i := 2; i < len(forks); i += 2 {
		if forks[i].sharedLit != forks[0].sharedLit {
			t.Errorf("%s and fork 0 both hit the warm cache yet hold different nodes", forks[i].name)
		}
	}
	for i := 1; i < len(forks); i += 2 {
		if forks[i].sharedLit == forks[0].sharedLit {
			t.Errorf("%s carries a private cache yet holds the shared cache's node", forks[i].name)
		}
	}

	got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		NewEnv:  func() (*lisp.LEnv, error) { return newTopologyEnv(parser.NewReader(), shared) },
		Program: topologyProgram,
		Tx:      topologyTxs(),
		Fork: func() func(*lisp.LEnv) (*lisp.LEnv, error) {
			var mu sync.Mutex
			n := 0
			return func(tmpl *lisp.LEnv) (*lisp.LEnv, error) {
				f, err := tmpl.Fork()
				if err != nil {
					return nil, err
				}
				mu.Lock()
				i := n
				n++
				mu.Unlock()
				if c := mixed(i); c != nil {
					f.Runtime.LoadCache = c
				}
				return f, nil
			}
		}(),
		ExpectNoSharedNatives: true,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	for _, w := range got {
		t.Errorf("%s", w)
	}
}

// ---------------------------------------------------------------------------
// 4. The hostile-reader angle: a Native annotation on a sealed header
// ---------------------------------------------------------------------------

// annotation is the embedder-side payload an annotating Reader hangs on a
// node.  It is a pointer so it has identity to share, and it declares
// NativeCloner so that, IF an oracle saw it shared, the finding would be
// unconditional (aliasguard_isolation.go, sharedNativeWitnesses: a payload
// that declares a protocol and is shared anyway is always a finding).
type annotation struct{ hits int }

func (a *annotation) CloneNative() interface{} { return &annotation{hits: a.hits} }

// annotatingReader wraps the standard parser and, on every quoted list it
// returns, sets LVal.Native — "the ONLY exported per-node slot an
// embedder's Reader has for annotation" (lisp/loader.go, newTextLoaderWalk).
// The header type stays LSExpr; only the payload slot is used.
type annotatingReader struct {
	inner   lisp.Reader
	payload *annotation
	tagged  int
}

func (r *annotatingReader) ReaderIdentity() string { return "annotating-reader" }

func (r *annotatingReader) Read(name string, in io.Reader) ([]*lisp.LVal, error) {
	exprs, err := r.inner.Read(name, in)
	if err != nil {
		return nil, err
	}
	for _, e := range exprs {
		r.tag(e)
	}
	return exprs, nil
}

func (r *annotatingReader) tag(v *lisp.LVal) {
	if v == nil {
		return
	}
	if v.Type == lisp.LSExpr && v.IsSealed() && len(v.Cells) > 0 && v.Cells[0].Type == lisp.LInt && v.Native == nil && isQuotedSExpr(v) {
		v.Native = r.payload
		r.tagged++
	}
	for _, c := range v.Cells {
		r.tag(c)
	}
}

// isQuotedSExpr reports whether v renders as a quoted list literal.
func isQuotedSExpr(v *lisp.LVal) bool {
	return strings.HasPrefix(v.String(), "'(")
}

// TestLoadCacheTopology_NativeAnnotationIsReported is the hostile-reader
// case, forked: a Reader annotates a quoted literal through LVal.Native on
// an LSExpr header, the template loads it under the shared cache, and the
// template is forked N times.  It is the negative control for the native
// channel of CheckTransactions over this topology: the annotation IS
// shared, and the oracle says so.
//
// What holds, and is asserted:
//
//  1. The cache REFUSES the annotated parse — newProgramForCache's strict
//     walk rejects a Native payload on a sealable node, so the load runs
//     uncached (TestLoadCacheNativePayloadFallsBackUncached is the same
//     rule over one environment).  "Cached once and forked N times" is
//     therefore impossible through this hook: the annotation reaches the
//     forks through Fork's own sealed-node sharing (lisp/fork.go, "sealed
//     values: SHARED"), and it is shared BY REFERENCE across every fork
//     exactly as docs/fork.md says natives are — its NativeCloner is not
//     consulted, because the node it rides on is sealed and shared
//     wholesale.
//  2. CheckTransactions with ExpectNoSharedNatives REPORTS it: one witness
//     on the native property per PAIR of environments — the template with
//     each fork and each fork with each other, ten for four forks — every
//     one at user:lit, naming the payload type and "[declares NativeCloner
//     and is shared anyway]" (the unconditional form, since the payload
//     declared a protocol and was shared regardless), and no witness of
//     any other kind.  Measured on fcc08b3 + this conversion: 10 witnesses.
//
// History, because this test was born as the opposite assertion.  On
// commit 74e4ac8 reachableNatives (aliasguard_isolation.go) recorded a
// payload only under an LNative header, so the annotation above was
// invisible to the oracle and this test pinned that as a known gap, in the
// TestForkParity_ViewSortGapStillOpen pattern: it required ZERO native
// witnesses and was measured red when the oracle was widened by hand.
// Commit aa0dbe4 (on the #599 branch) keyed "has a payload" on the payload
// rather than on the header's type, excluding kernel-owned payloads; once
// it sat under this branch the pinned form went red with exactly the
// witness required here, and the arm was converted.  If this test fails
// again with no witness, the oracle has been narrowed back to the header
// type.
func TestLoadCacheTopology_NativeAnnotationIsReported(t *testing.T) {
	t.Parallel()
	const program = `(set 'lit '(1 2 3))` + "\n" + `(set 'counter 0)`
	shared := newTopologyCache()
	payload := &annotation{}
	newEnv := func() (*lisp.LEnv, error) {
		return newTopologyEnv(&annotatingReader{inner: parser.NewReader(), payload: payload}, shared)
	}

	tmpl, err := newEnv()
	if err != nil {
		t.Fatalf("template: %v", err)
	}
	_, _, before := shared.counts()
	if rc := tmpl.LoadString("program.lisp", program); rc.Type == lisp.LError {
		t.Fatalf("program: %v", rc)
	}
	if _, _, after := shared.counts(); after != before {
		t.Fatalf("the cache stored the annotated parse (%d new entries); newProgramForCache refuses a Native payload on a sealable node", after-before)
	}
	lit := tmpl.GetGlobal(lisp.Symbol("lit"))
	if lit.Native != payload {
		t.Fatalf("the template's literal does not carry the annotation: %T", lit.Native)
	}
	if !lit.IsSealed() || lit.Type != lisp.LSExpr {
		t.Fatalf("the annotated node is %v sealed=%t; the case is a Native payload on a SEALED non-LNative header", lit.Type, lit.IsSealed())
	}
	const forks = 4
	for i := range forks {
		f, err := tmpl.Fork()
		if err != nil {
			t.Fatalf("fork %d: %v", i, err)
		}
		got := f.GetGlobal(lisp.Symbol("lit"))
		if got != lit {
			t.Errorf("fork %d holds its own copy of the sealed literal (%p vs %p); Fork shares sealed nodes", i, got, lit)
		}
		if got.Native != payload {
			t.Errorf("fork %d: annotation is %p, want the template's %p (shared by reference; NativeCloner not consulted on a sealed node)", i, got.Native, payload)
		}
	}

	// The oracle sees what the pointer comparison above saw.
	wits, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		NewEnv:                newEnv,
		Program:               program,
		Tx:                    []string{`(set 'counter 1)`, `(set 'counter 2)`, `(set 'counter 3)`, `(set 'counter 4)`},
		ExpectNoSharedNatives: true,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	reported := 0
	for _, w := range wits {
		if !strings.Contains(w.Property, "native payload") {
			t.Errorf("unexpected witness: %s", w)
			continue
		}
		t.Logf("%s", w)
		if w.Leak == "user:lit" &&
			strings.Contains(w.Detail, "*elpstest_test.annotation") &&
			strings.Contains(w.Detail, "[declares NativeCloner and is shared anyway]") {
			reported++
		}
	}
	// The template and forks 0..3 are five environments sharing one
	// payload: every pair is a witness.
	const pairs = (forks + 1) * forks / 2
	if reported != pairs {
		t.Fatalf("the annotation is shared by reference across %d forks (asserted above) and CheckTransactions reported it at user:lit as a NativeCloner shared anyway for %d environment pairs, want %d (%d witnesses in all); the native oracle has been narrowed back to the header type (see commit aa0dbe4)", forks, reported, pairs, len(wits))
	}
}

// ---------------------------------------------------------------------------
// 5. The headline oracle: CheckParity over the shared warm cache
// ---------------------------------------------------------------------------

// topologyParitySeq is environment i's transaction sequence for
// CheckParity, the same work as topologyTx(i) split into steps so every
// step's RESULT is compared cold-vs-fork, plus one step that is
// deliberately not handled: an in-place sort of the cached literal that
// must RAISE identically on both arms (CheckParity compares an error value
// like any other, and exactly one arm raising is its own witness).
func topologyParitySeq(i int) []string {
	seq := []string{
		fmt.Sprintf("(load-string %q :name \"shared.lisp\")", topologySharedSource),
	}
	if i%2 == 1 {
		seq = append(seq, fmt.Sprintf("(load-string %q :name \"late.lisp\")\n(set 'late-seen (foldl + 0 late-lit))", topologyLateSource))
	}
	return append(seq,
		"(set 'seen (list (foldl + 0 shared-lit) (first shared-lit) (length tmpl-lit)))",
		fmt.Sprintf("(dotimes (n %d) (bump))\n(append! log %d)", i+1, i),
		"(stable-sort < shared-lit)",
		"(stable-sort < tmpl-lit)",
		`(set 'sort-result (handler-bind ((modify-literal-error (lambda (c &rest _) (list 'refused c)))) (stable-sort < shared-lit)))`,
		"(set 'after (list shared-lit tmpl-lit (stable-sort < (copy shared-lit))))",
	)
}

// TestLoadCacheTopology_CheckParity runs the shared-warm-cache topology
// through elpstest.CheckParity: one template whose forks all share its
// cache (the cache travels across Fork), against one cold environment per
// sequence, each cold environment on a FRESH cache of its own.  NewEnv
// hands out a new topologyCache on every call and records it, so the
// warmth is asserted by role afterwards rather than assumed: exactly one
// cache served hits — the template's, to its forks — every cold cache
// served none, and every cache stored shared.lisp exactly once.  Run over
// both schedules (sequential: each fork taken after the previous finished;
// interleaved: every fork live while every other writes) and both hop
// counts.  No witness on any arm.
func TestLoadCacheTopology_CheckParity(t *testing.T) {
	t.Parallel()
	tx := make([][]string, topologyForks)
	for i := range tx {
		tx[i] = topologyParitySeq(i)
	}
	for _, interleave := range []bool{false, true} {
		for _, hops := range []int{1, 2} {
			name := fmt.Sprintf("interleave=%t/hops=%d", interleave, hops)
			t.Run(name, func(t *testing.T) {
				t.Parallel()
				var mu sync.Mutex
				var caches []*topologyCache
				got, err := elpstest.CheckParity(elpstest.ParityCheck{
					NewEnv: func() (*lisp.LEnv, error) {
						c := newTopologyCache()
						mu.Lock()
						caches = append(caches, c)
						mu.Unlock()
						return newTopologyEnv(parser.NewReader(), c)
					},
					Program:    topologyProgram,
					Tx:         tx,
					Interleave: interleave,
					Hops:       hops,
					Repro:      "elpstest/loadcache_topology_test.go: forks on one warm LoadCache vs cold environments on fresh ones",
				})
				if err != nil {
					t.Fatalf("harness error: %v", err)
				}
				for _, w := range got {
					t.Errorf("%s", w)
				}
				if len(caches) != 1+topologyForks {
					t.Fatalf("NewEnv was called %d times; want the template plus %d cold environments", len(caches), topologyForks)
				}
				warm := 0
				for _, c := range caches {
					loads, hits, stores := c.counts()
					if loads != hits+stores {
						t.Errorf("a cache saw loads=%d hits=%d stores=%d; every load is a hit or a store", loads, hits, stores)
					}
					if n := c.storesNamed("shared.lisp"); n != 1 {
						t.Errorf("a cache stored shared.lisp %d times; the forks share one parse and each cold environment makes its own", n)
					}
					if hits > 0 {
						warm++
						t.Logf("the template's cache: loads=%d hits=%d stores=%d", loads, hits, stores)
						// Five forks hit shared.lisp and two hit late.lisp;
						// the schedule runs on one goroutine, so unlike the
						// concurrent identity arm this count is exact.
						// Measured on commit ed2eb02's tree: loads=56
						// hits=7 stores=49 on every arm.
						if hits != 7 {
							t.Errorf("the template's cache served %d hits; five forks hit shared.lisp and two hit late.lisp", hits)
						}
					}
				}
				// The template's cache is the one the forks warmed: the
				// only one with a hit.  A cold environment on a fresh cache
				// cannot hit anything, so the cold arm really is cold.
				if warm != 1 {
					t.Errorf("%d caches served hits; want exactly one (the template's, from its forks), with every cold environment missing in its own", warm)
				}
			})
		}
	}
}
