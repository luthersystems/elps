// Copyright © 2026 The ELPS authors

package lisp

// Per-callsite macro-expansion caching (issue #381) — EXPERIMENTAL / POC.
//
// Sealed AST nodes have stable pointer identity and immutable content
// (lisp/seal.go), which makes them valid cache keys: the first time a macro
// call at a sealed callsite is expanded, the expansion tree can be sealed
// and memoized, and every later evaluation of the same callsite can reuse
// the tree instead of re-running the macro.  Macro-heavy request paths
// re-expand the same callsites on every call; this cache attacks that cost.
//
// # Safety model
//
// Reusing an expansion TREE is observationally identical to re-expansion
// only for macros whose expansion is a pure structural rewrite of the
// callsite's argument nodes.  Three whitelist tiers enforce that:
//
//  1. Native kernel/lisplib macros hand-audited as pure rewrites are
//     whitelisted by qualified name (pureNativeMacros).  Notably NOT
//     whitelisted: defun/defmacro/deftype, whose expansions embed a closure
//     allocated at expansion time (env.Lambda) — caching one would share a
//     closure bound to the first expander's environment.
//  2. User macros (defmacro) are admitted only when proveUserMacroPure can
//     verify, from the macro function's own (sealed, shared) body, that the
//     expansion is a pure template instantiation: quasiquote templates whose
//     unquotes are bare formals or gensym-bound locals, glued together by a
//     tiny allowlist of structural operators.  Anything else — free symbol
//     reads, arbitrary computation, nested quasiquote, a gensym leaked under
//     quote (fresh-symbol-as-data semantics) — is rejected and the callsite
//     simply bypasses the cache.  See macrocache_purity.go.
//  3. Everything else bypasses: unsealed callsites (runtime-constructed
//     code), debugger-attached runtimes (MacroExpansionInfo IDs must stay
//     unique per expansion), and macros that fail 1 and 2.
//
// # Gensym reuse
//
// A cached expansion fixes the gensym names minted by its first expansion.
// That is safe for admitted macros because their gensyms are always BOUND
// within the expansion itself (let/let*/lambda/labels binding positions) and
// never escape as data: evaluation creates fresh bindings for the fixed name
// on every call, exactly like a hand-written local variable, and shadowing
// an identical name in a nested cached expansion is harmless because no
// admitted expansion references a gensym it did not bind.  The purity prover
// rejects the one shape where identity matters — a gensym under quote —
// and native whitelisted macros were audited for the same property.
//
// # Invalidation
//
// Every cache entry records the identity of the macro that produced it:
// package+FID for native macros, the (sealed, parse-shared) formals node
// pointer for user macros.  A lookup validates the entry against the macro
// the callsite resolved to RIGHT NOW; redefinition changes the resolved
// function (defmacro/set create a fresh function object with a fresh
// formals node), so a stale entry can never be returned — it is
// overwritten by the next expansion.  There is no epoch counter to bump
// because the identity comparison is exact.
//
// # Sharing scope (per-runtime vs process-shared)
//
// Both are implemented, selected by MacroCacheMode:
//
//   - MacroCacheRuntime keys a plain map on the Runtime (single-threaded by
//     the Runtime contract).  Memory is duplicated per runtime and a fresh
//     runtime starts cold.
//   - MacroCacheShared uses one process-wide table (sync.Map, or an LRU
//     under SetMacroCacheCap).  Cached expansions are sealed before they
//     are published, mirroring the sealed-formals precedent
//     (registrationFormals): immutable content, copy-on-write protected.
//     Cross-runtime hits require cross-runtime-shared sealed callsites,
//     which is exactly substrate's production parse-cache aliasing
//     (substrate#375).  Under -tags elpscheck the ownership checker forbids
//     cross-runtime AST sharing outright (substrate detaches cache handoffs
//     under the tag), so a cross-runtime cache hit is unreachable in checked
//     builds: a shared callsite would panic at eval entry before macro
//     dispatch.  Within one runtime the checker is satisfied trivially.
//
// The POC default is MacroCacheOff; nothing changes unless a host opts in
// via SetMacroCacheMode or the ELPS_MACRO_CACHE environment variable
// (values: off, runtime, shared; ELPS_MACRO_CACHE_CAP bounds the shared
// table).

import (
	"container/list"
	"context"
	"os"
	"strconv"
	"sync"
	"sync/atomic"
)

// MacroCacheMode selects the macro-expansion cache implementation.
type MacroCacheMode int32

// Macro-expansion cache modes.
const (
	// MacroCacheOff disables expansion caching entirely (default).
	MacroCacheOff MacroCacheMode = iota
	// MacroCacheRuntime caches per Runtime in an unsynchronized map.
	MacroCacheRuntime
	// MacroCacheShared caches in one process-wide synchronized table.
	MacroCacheShared
)

//elpsvet:allow process-wide cache configuration; scalar atomics, no LVals
var macroCacheConfig struct {
	mode atomic.Int32
	cap  atomic.Int64 // shared-table entry cap; <=0 = unbounded
}

// SetMacroCacheMode selects the macro-expansion cache mode for the process.
// Safe to call at any time; existing cached entries are retained (they
// remain valid — entries are validated against the resolved macro on every
// hit) but ignored while the mode is MacroCacheOff.
func SetMacroCacheMode(mode MacroCacheMode) {
	macroCacheConfig.mode.Store(int32(mode))
}

// GetMacroCacheMode reports the current macro-expansion cache mode.
func GetMacroCacheMode() MacroCacheMode {
	return MacroCacheMode(macroCacheConfig.mode.Load())
}

// SetMacroCacheCap bounds the process-shared cache to n entries with LRU
// eviction.  n <= 0 removes the bound.  Takes effect for subsequent stores;
// an over-capacity table evicts down as new entries arrive.
func SetMacroCacheCap(n int) {
	macroCacheConfig.cap.Store(int64(n))
}

// ResetMacroCache drops every entry in the process-shared cache and resets
// hit/miss statistics.  Per-runtime caches are dropped when their Runtime
// is garbage; tests can simply build a fresh runtime.
func ResetMacroCache() {
	sharedMacroCache.reset()
	macroCacheStats.reset()
}

func init() {
	for _, qual := range pureNativeMacroNames {
		RegisterPureNativeMacro(qual)
	}
	switch os.Getenv("ELPS_MACRO_CACHE") {
	case "runtime":
		SetMacroCacheMode(MacroCacheRuntime)
	case "shared":
		SetMacroCacheMode(MacroCacheShared)
	}
	if s := os.Getenv("ELPS_MACRO_CACHE_CAP"); s != "" {
		if n, err := strconv.Atoi(s); err == nil {
			SetMacroCacheCap(n)
		}
	}
	initMacroStats()
}

// pureNativeMacros whitelists builtin (Go-native) macros whose expansions
// are hand-audited pure structural rewrites of their argument nodes.  Keyed
// by package-qualified registration name.
//
// Audit notes (lisp/macro.go, lisp/lisplib/libtesting):
//   - lisp:get-default, lisp:trace: quasiquote-style template with gensyms
//     bound by the expansion's own let; args spliced by reference.
//   - lisp:curry-function: builds (lambda (&rest g) (apply ...)) source; the
//     closure is created at EVAL time, per call, so tree reuse is safe.
//   - testing:* assertions and test-let/test-let*/benchmark-simple: pure
//     list construction from arg nodes (plus String() renderings of them,
//     deterministic for sealed nodes); gensyms bound by the expansion.
//
// Deliberately EXCLUDED: lisp:defmacro, lisp:defun, lisp:deftype (expansion
// embeds an env.Lambda closure allocated at expansion time — impure) and
// lisp:defconst (pure, but top-level-only; zero request-path value).
//
// pureNativeMacroNames lists the audited macros as "pkg:name"; the lookup
// table used on the hot path is keyed by pkg+FID (the FID embeds the
// registration name — see AddMacros) and built in init below.
//
//elpsvet:allow static whitelist table; string slice, no LVals
var pureNativeMacroNames = []string{
	"lisp:get-default",
	"lisp:trace",
	"lisp:curry-function",
	"testing:assert=",
	"testing:assert-string=",
	"testing:assert-equal",
	"testing:assert-nil",
	"testing:assert-not-nil",
	"testing:assert-not",
	"testing:test-let",
	"testing:test-let*",
	"testing:benchmark-simple",
}

//elpsvet:allow whitelist lookup table built once in init and extended only via RegisterPureNativeMacro; string keys, no LVals
var pureNativeMacroFIDs sync.Map // "pkg\x00fid" -> bool

func pureNativeMacroKey(pkg, fid string) string {
	return pkg + "\x00" + fid
}

// builtinMacroFID mirrors the FID format AddMacros assigns to builtin
// macros; a test pins the two against each other.
func builtinMacroFID(name string) string {
	return "<builtin-macro ``" + name + "''>"
}

// RegisterPureNativeMacro whitelists a builtin macro, identified by its
// package-qualified registration name (e.g. "mypkg:my-macro"), as a pure
// structural rewrite whose expansions may be cached per callsite.  The
// caller asserts the macro's expansion depends only on its (unevaluated)
// argument nodes and mints gensyms solely into binding positions.  Intended
// for embedders that register macros via RegisterDefaultMacro/AddMacros.
func RegisterPureNativeMacro(qualifiedName string) {
	pkg, name, ok := splitQualified(qualifiedName)
	if !ok {
		return
	}
	pureNativeMacroFIDs.Store(pureNativeMacroKey(pkg, builtinMacroFID(name)), true)
}

func splitQualified(qualifiedName string) (pkg, name string, ok bool) {
	for i := range len(qualifiedName) {
		if qualifiedName[i] == ':' {
			return qualifiedName[:i], qualifiedName[i+1:], i > 0 && i < len(qualifiedName)-1
		}
	}
	return "", "", false
}

func isPureNativeMacro(pkg, fid string) bool {
	_, ok := pureNativeMacroFIDs.Load(pureNativeMacroKey(pkg, fid))
	return ok
}

// macroIdentity identifies the macro function that produced a cached
// expansion, for exact invalidation on redefinition.
//
// Native macros: pkg + FID strings (process-stable per registration name;
// identical across environments registering the same builtin table).
// User macros: the pointer identity of the function's sealed formals node —
// unique per defmacro source form and shared by every environment that
// evaluates the same parse, so identical source yields cross-env hits while
// any redefinition (a fresh parse or a fresh defmacro) yields a fresh node
// and therefore a miss.
type macroIdentity struct {
	formals *LVal // user macros only; nil for native
	pkg     string
	fid     string
}

// macroCacheEntry is one cached expansion: the identity of the macro that
// produced it and the sealed expansion tree.
type macroCacheEntry struct {
	exp *LVal
	id  macroIdentity
}

// macroCacheIdentity classifies fun for caching.  ok reports whether fun is
// admissible at all (pure); the returned identity is meaningful only when
// ok.  Purity verdicts for user macros are memoized process-wide keyed by
// the (sealed, shared) formals node.
func macroCacheIdentity(fun *LVal) (macroIdentity, bool) {
	if fun.Type != LFun {
		return macroIdentity{}, false
	}
	fd := fun.funData()
	if fd == nil {
		return macroIdentity{}, false
	}
	if fd.Builtin != nil {
		if !isPureNativeMacro(fd.Package, fd.FID) {
			return macroIdentity{}, false
		}
		return macroIdentity{pkg: fd.Package, fid: fd.FID}, true
	}
	// User macro: cells are [formals, body...].
	if len(fun.Cells) < 2 {
		return macroIdentity{}, false
	}
	formals := fun.Cells[0]
	if formals == nil || !formals.sealed {
		// Runtime-constructed macro (or unsealed source); no stable shared
		// identity and no purity guarantee.  Bypass.
		return macroIdentity{}, false
	}
	if !userMacroPurity(fun, formals) {
		return macroIdentity{}, false
	}
	return macroIdentity{formals: formals}, true
}

//elpsvet:allow process-wide purity-verdict memo keyed by sealed formals node pointers; stores bools, never mutates the keys
var userMacroPurityMemo sync.Map // *LVal (formals node) -> bool

func userMacroPurity(fun, formals *LVal) bool {
	if v, ok := userMacroPurityMemo.Load(formals); ok {
		return v.(bool)
	}
	verdict := proveUserMacroPure(fun)
	userMacroPurityMemo.Store(formals, verdict)
	return verdict
}

// sharedMacroCache is the process-wide expansion table (MacroCacheShared).
//
//elpsvet:allow process-shared cache of SEALED expansion trees keyed by sealed callsite nodes; mirrors the sealed-formals sharing precedent (registrationFormals); entries are immutable once published
var sharedMacroCache sharedCache

// sharedCache is an unbounded sync.Map with an optional LRU bound engaged
// by SetMacroCacheCap.  The unbounded fast path takes no locks on lookup.
type sharedCache struct {
	// LRU bookkeeping, engaged only when a cap is set.
	order *list.List              // front = most recent; values are *LVal keys
	elem  map[*LVal]*list.Element // callsite -> order element

	m sync.Map // *LVal (callsite) -> *macroCacheEntry

	count atomic.Int64
	mu    sync.Mutex
}

func (c *sharedCache) lookup(callsite *LVal) (*macroCacheEntry, bool) {
	v, ok := c.m.Load(callsite)
	if !ok {
		return nil, false
	}
	if macroCacheConfig.cap.Load() > 0 {
		c.touch(callsite)
	}
	return v.(*macroCacheEntry), true
}

func (c *sharedCache) touch(callsite *LVal) {
	c.mu.Lock()
	if c.elem != nil {
		if e, ok := c.elem[callsite]; ok {
			c.order.MoveToFront(e)
		}
	}
	c.mu.Unlock()
}

func (c *sharedCache) store(callsite *LVal, entry *macroCacheEntry) {
	_, loaded := c.m.Swap(callsite, entry)
	if loaded {
		if macroCacheConfig.cap.Load() > 0 {
			c.touch(callsite)
		}
		return
	}
	c.count.Add(1)
	limit := macroCacheConfig.cap.Load()
	if limit <= 0 {
		return
	}
	c.mu.Lock()
	if c.order == nil {
		c.order = list.New()
		c.elem = make(map[*LVal]*list.Element)
	}
	c.elem[callsite] = c.order.PushFront(callsite)
	// Entries published while the table was UNBOUNDED carry no LRU
	// bookkeeping, so eviction cannot reach them.  Without adoption the cap
	// would evict each freshly stored entry immediately (it is the only
	// element on the list) while the untracked backlog stayed pinned
	// forever: the table neither honours the bound nor serves any new hit.
	// Adopt them as the oldest entries the first time the bound sees drift.
	if int64(len(c.elem)) < c.count.Load() {
		c.adoptUntrackedLocked()
	}
	for int64(c.order.Len()) > limit {
		back := c.order.Back()
		victim := back.Value.(*LVal)
		c.order.Remove(back)
		delete(c.elem, victim)
		c.m.Delete(victim)
		c.count.Add(-1)
		macroCacheStats.evictions.Add(1)
	}
	c.mu.Unlock()
}

// adoptUntrackedLocked gives LRU bookkeeping to every table entry that does
// not have any, pushing them to the back (oldest) so they are the first
// evicted.  c.mu must be held and c.order/c.elem must be initialized.
func (c *sharedCache) adoptUntrackedLocked() {
	c.m.Range(func(k, _ any) bool {
		callsite := k.(*LVal)
		if _, ok := c.elem[callsite]; !ok {
			c.elem[callsite] = c.order.PushBack(callsite)
		}
		return true
	})
	c.count.Store(int64(c.order.Len()))
}

func (c *sharedCache) reset() {
	c.mu.Lock()
	c.order = nil
	c.elem = nil
	c.mu.Unlock()
	c.m.Range(func(k, _ any) bool {
		c.m.Delete(k)
		return true
	})
	c.count.Store(0)
}

// runtimeMacroCache returns the per-runtime cache map, allocating it
// lazily.  Runtime is single-threaded by contract; no locking.
func runtimeMacroCache(rt *Runtime) map[*LVal]*macroCacheEntry {
	if rt.macroCache == nil {
		rt.macroCache = make(map[*LVal]*macroCacheEntry)
	}
	return rt.macroCache
}

// macroCallCached wraps macroCall with per-callsite expansion caching (and
// optional instrumentation).  callsite is the original s-expression node
// being evaluated; fun/args are exactly what macroCall would receive.
func (env *LEnv) macroCallCached(ctx context.Context, callsite, fun, args *LVal) *LVal {
	mode := GetMacroCacheMode()
	recordMacroExpandEvent(callsite, fun)
	if mode == MacroCacheOff {
		return env.macroCall(ctx, fun, args)
	}
	if !callsite.sealed || env.Runtime.Debugger != nil {
		macroCacheStats.bypassUnsealed.Add(1)
		return env.macroCall(ctx, fun, args)
	}
	id, ok := macroCacheIdentity(fun)
	if !ok {
		macroCacheStats.bypassImpure.Add(1)
		return env.macroCall(ctx, fun, args)
	}
	// Native identity needs the resolved local name; user identity is the
	// formals pointer.  Compare against any existing entry.
	if mode == MacroCacheShared {
		if e, hit := sharedMacroCache.lookup(callsite); hit && e.id == id {
			macroCacheStats.hits.Add(1)
			return markMacExpand(e.exp)
		}
	} else {
		if e, hit := runtimeMacroCache(env.Runtime)[callsite]; hit && e.id == id {
			macroCacheStats.hits.Add(1)
			return markMacExpand(e.exp)
		}
	}
	r := env.macroCall(ctx, fun, args)
	if r == nil || r.Type != LMarkMacExpand {
		return r
	}
	exp := r.Cells[0]
	if !sealExpansion(exp) {
		// The expansion contains nodes that cannot be sealed (runtime
		// values smuggled into the tree).  The whitelist should make this
		// unreachable; refuse to cache rather than share mutable state.
		macroCacheStats.unsealable.Add(1)
		return r
	}
	entry := &macroCacheEntry{id: id, exp: exp}
	if mode == MacroCacheShared {
		sharedMacroCache.store(callsite, entry)
	} else {
		runtimeMacroCache(env.Runtime)[callsite] = entry
	}
	macroCacheStats.stores.Add(1)
	return r
}

// sealExpansion seals the expansion tree rooted at v and reports whether
// every reachable node is now sealed (or a shared singleton).  A false
// return means some node is not a parser-producible shape — a runtime value
// embedded in the expansion — and the tree must not be shared.
func sealExpansion(v *LVal) bool {
	v.SealAST()
	return expansionFullySealed(v)
}

func expansionFullySealed(v *LVal) bool {
	if v == nil || isSingleton(v) {
		return true
	}
	if !v.sealed {
		return false
	}
	for _, c := range v.Cells {
		if !expansionFullySealed(c) {
			return false
		}
	}
	return true
}
