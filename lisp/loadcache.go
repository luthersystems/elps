// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"fmt"
	"io"
)

// Sealed per-file load cache — an elps-owned parse cache hook (issue #368).
//
// # The seam this closes
//
// Reader and LocationReader (lisp/loader.go) are the only interfaces an
// embedder can implement to influence parsing, and both return a raw
// []*LVal.  An embedder that wants a parse cache therefore has exactly one
// place to put it, and that place requires it to take custody of AST nodes
// and hand the same nodes to every environment.  That is not a misuse of the
// API; it is the only shape the API offers, and luthersystems/substrate does
// precisely it.
//
// lisp.Program (lisp/program.go) closed the half of the seam an embedder
// drives directly — parse once, load many, through an opaque handle.  It
// does not cover Runtime.Reader, which is the seam `load-file` drives, and
// `load-file` is how a phylum actually loads its own sources.  This file
// covers that half.
//
// # What it buys, measured
//
// The hazard half of the story is already closed elsewhere: the rdparser
// seals every top-level expression at parse time and the kernel's mutation
// sites refuse a sealed input, so an embedder cache that aliases parser
// output is no longer the corruption vector it was.  What remains is a
// COST.  An embedder cannot see the seal's guarantees from outside the
// module — it holds []*LVal, not a promise — so the only way it can reach
// safety on its own terms is to deep-copy on every cache hit.  substrate's
// parse cache does exactly that, and measures it: an aliased hit costs
// 0.29 ns and 0 B, the same hit plus the copy costs ~22 ms and 26.5 MiB,
// once per environment load, multiplied by however many environments a
// deployment preheats.
//
// elps can hand out the alias safely because elps owns the AST type: it can
// establish the seal at admission and know that nothing it evaluates will
// write the tree.  That is the whole point of moving the cache inside the
// module.  See lisp/loadcache_bench_test.go for the elps-side numbers.
//
// # The shape
//
// The embedder supplies POLICY (what to key, what to keep, what to evict)
// and never touches DATA:
//
//	type LoadCache interface {
//	        Load(key string) (*CachedSource, bool)
//	        Store(key string, src *CachedSource)
//	}
//
// A *CachedSource is opaque: only elps mints one, it has no exported member
// that yields a *LVal, and there is no exported constructor.  An embedder
// stores handles in whatever map, LRU or size-bounded structure it likes,
// and the decision of whether a hit is served by reference or by copy stays
// inside this module, where it can change without an embedder migration.
//
// # Why aliasing a hit is legal
//
// Every clause is an existing rail, not a new one:
//
//   - ADMISSION.  newCachedSource routes the parse through newProgram, the
//     single admission point Program's constructors already use (issue
//     #394): reference types are rejected, output already sealed throughout
//     is admitted as-is, anything else is privately copied and sealed, and
//     what cannot be sealed is rejected.  A rejected parse is simply not
//     cached — the load proceeds exactly as it would with no cache at all.
//   - SHARING.  A sealed node may be reached by more than one Runtime by
//     design; the checked-mode ownership table exempts sealed nodes for
//     exactly this topology, and names substrate's warm parse cache as the
//     first of the sanctioned ones (lisp/ownership_check_elpscheck.go,
//     allowlist entry 2).  No new admission class is needed here: a cached
//     tree is sealed throughout by construction, so it is already inside
//     the existing exemption.
//   - MUTATION.  Lisp-level writes through the shared tree raise
//     modify-literal-error at the guarded kernel sites (lisp/seal.go), and
//     the evaluator's own metadata writes — stampMacroExpansion, SetSource
//     — skip sealed nodes.  That last one is what makes a debugger safe on
//     this path: attaching a debugger stamps macroExpansionInfo onto
//     macro-expanded nodes, but stampGuarded returns at the first sealed
//     node, so the stamp reaches only nodes the macro CREATED, never the
//     cached ones spliced into the expansion.  Debug mode therefore needs
//     no private copy of a cached tree.  Two tests pin that, and the
//     difference between them matters: TestLoadCacheDebuggerAliasIntact
//     is the end-to-end property over an ORDINARY parse, where rdparser's
//     real locations independently keep the stamp away, and
//     TestLoadCacheDebuggerDoesNotStampSharedNodes removes that second
//     guard by caching a Reader's location-less node, so the sealed skip
//     is the only thing left — deleting it makes that one fail.
//   - VERIFICATION.  A cached tree's roots are recorded at SealAST and
//     re-verified after every load through (*LEnv).load, so in checked
//     builds a corrupted cache entry is reported at the load that corrupted
//     it (lisp/seal_check_elpscheck.go).
//
// # Keying
//
// The key is derived by elps, from the source bytes AND the stream's name
// and location.  substrate's cacheReader keys on a hash of the content
// alone, which is safe for its topology (a file's name and location are
// stable across loads) but is not safe in general: two files with identical
// content would share an entry, and the served tree carries the FIRST
// file's parse locations, so every stack trace and error from the second
// would name the wrong file.  Including name and location costs nothing on
// the topology that motivated this and removes a silent misattribution
// everywhere else.

// CachedSource is an opaque, immutable, sealed parse result.  Only elps
// mints one — an embedder holds and returns handles but cannot construct
// one, open one, or reach the AST nodes inside it.
//
// A *CachedSource is safe to share: its expressions are sealed throughout
// (see newCachedSource), so it may be held by a process-wide cache and
// served to any number of Runtimes, concurrently.  Its own fields are
// written once at construction and never again.
//
// The zero value is not usable; obtain one only from a LoadCache's Load.
type CachedSource struct {
	key  string
	name string
	loc  string
	prog Program
	fp   uint64
}

// Key returns the cache key elps derived for this entry.  It is the key the
// entry was Stored under, and a cache that hands back an entry under a
// different key is treated as a miss (see (*LEnv).readCached).
func (s *CachedSource) Key() string {
	if s == nil {
		return ""
	}
	return s.key
}

// Name returns the stream name the source was parsed under.
func (s *CachedSource) Name() string {
	if s == nil {
		return ""
	}
	return s.name
}

// Location returns the physical location the source was parsed under, or
// the empty string when the parse had no location (the Reader, rather than
// LocationReader, path).
func (s *CachedSource) Location() string {
	if s == nil {
		return ""
	}
	return s.loc
}

// Len returns the number of top-level expressions in the cached parse.
func (s *CachedSource) Len() int {
	if s == nil {
		return 0
	}
	return s.prog.Len()
}

// Fingerprint returns the canonical structural digest of the sealed parse
// (lisp/sealfp.go), taken at admission.  It is a value, not a reference:
// exposing it lets a host log or deduplicate entries without any way to
// reach the nodes it summarises.
func (s *CachedSource) Fingerprint() uint64 {
	if s == nil {
		return 0
	}
	return s.fp
}

// String returns a short debugging description.  Like Program.String it
// deliberately does not render the cached expressions.
func (s *CachedSource) String() string {
	if s == nil {
		return "<cached-source nil>"
	}
	return fmt.Sprintf("<cached-source %s %d exprs fp=%016x>", s.name, s.prog.Len(), s.fp)
}

// LoadCache is an embedder-supplied cache of parsed source files.  Install
// one on Runtime.LoadCache and every Load* entry point — which is to say
// `load-file`, and the Go-side Load/LoadString/LoadLocation family — will
// consult it before parsing.
//
// The interface is deliberately policy-only.  Implementations decide what
// to keep, how much, and for how long; they never see, construct or copy a
// *LVal.  A minimal implementation is a map guarded by a mutex; a real one
// is usually size- or age-bounded.
//
// Contract:
//
//   - Load returns the entry previously Stored under key, or ok == false.
//     Returning an entry Stored under a DIFFERENT key is treated as a miss
//     rather than trusted, so an implementation that mixes up its own keys
//     degrades to "no cache" instead of running the wrong program.
//   - Store may drop the entry, keep it, or evict something else; elps
//     makes no assumption that a Stored entry is later Loadable.
//   - Both methods must be safe for concurrent use when the cache is
//     shared by Runtimes on more than one goroutine.  The entries
//     themselves are immutable, so no locking is needed around the values
//     — only around the implementation's own bookkeeping.
//
// A nil Runtime.LoadCache disables the hook entirely: the load path is then
// byte-identical to what it was before this hook existed, with no hashing,
// no buffering and no extra allocation (TestLoadCacheNilPathUnchanged).
//
// Note the breadth: the hook sits at the read funnel, so it sees LoadString
// and Load as well as LoadFile.  A host that evaluates many distinct
// one-off strings through one unbounded cache will accumulate an entry per
// distinct string — retention is the implementation's job, and an
// implementation with no bound has no bound.  (The embedder-side caches
// this hook replaces intercept Reader.Read and have exactly the same
// reach.)
type LoadCache interface {
	// Load returns the cached parse stored under key.
	Load(key string) (*CachedSource, bool)
	// Store records src under key.
	Store(key string, src *CachedSource)
}

// newCachedSource mints the cache entry for a fresh parse, or reports why
// the parse cannot be cached.
//
// The admission is newProgram's, unchanged and deliberately reused rather
// than reimplemented: it is the one place in this package that decides what
// may be shared between environments, and a second copy of that decision is
// how the two drift apart.  A parse it refuses — a Reader that returned a
// reference type, or a node no seal can cover — is not cacheable at all,
// and the caller falls back to handing the raw parse to this one load.
func newCachedSource(key, name, loc string, exprs []*LVal) (*CachedSource, error) {
	prog, err := newProgram(exprs)
	if err != nil {
		return nil, err
	}
	return &CachedSource{
		prog: prog,
		key:  key,
		name: name,
		loc:  loc,
		fp:   SealedASTFingerprint(prog.exprs),
	}, nil
}

// loadCacheKey derives the cache key for a source stream.  The digest
// covers the bytes and both identity strings, each length-prefixed so that
// no concatenation of one tuple can collide with another (a plain
// name+loc+src concatenation makes ("ab", "c") and ("a", "bc")
// indistinguishable).
//
// SHA-256 rather than a fast non-cryptographic hash: the key decides WHICH
// PROGRAM RUNS, and on a collision the wrong program runs silently.  The
// cost is one pass over a source file per load — nanoseconds per kilobyte,
// against the milliseconds of parsing it replaces.
func loadCacheKey(name, loc string, src []byte) string {
	h := sha256.New()
	var n [8]byte
	write := func(b []byte) {
		binary.LittleEndian.PutUint64(n[:], uint64(len(b)))
		_, _ = h.Write(n[:])
		_, _ = h.Write(b)
	}
	write([]byte(name))
	write([]byte(loc))
	write(src)
	return hex.EncodeToString(h.Sum(nil))
}

// readCached is the single point where every Load* entry point turns a byte
// stream into expressions.  With no cache installed it is exactly the
// reader call it replaced — parse is invoked on the caller's io.Reader and
// its result is returned untouched, so nothing about the historical path
// changes, not even an allocation.
//
// With a cache installed:
//
//	miss: read the stream, derive the key, parse, admit (seal), Store, and
//	      hand the SEALED tree to this load.  The first loader gets the
//	      cached nodes too — keeping a pristine copy back would mean the
//	      cache never serves what it stored, which is the copy substrate
//	      pays for and the reason this hook exists.
//	hit:  hand the cached sealed tree to the load BY REFERENCE.  No copy,
//	      no allocation, no walk.  See this file's header for why that is
//	      legal.
//
// Two defensive falls back to an ordinary parse, both silent because both
// are "the cache did not help", not "the load failed":
//
//   - An entry whose Key does not match the key elps derived is not
//     trusted.  A cache that mixes up its own keys then degrades to no
//     cache instead of running the wrong file's program.
//   - A parse newProgram refuses to admit — a Reader that returned a
//     reference type, or a node no seal can cover — is not cacheable, so
//     it is handed to this one load and never stored.  Correctness first:
//     the alternative (store it and copy on every load) would put an
//     unsealed tree in a process-wide cache, which is the topology this
//     hook exists to make impossible.
func (env *LEnv) readCached(name, loc string, r io.Reader, parse func(io.Reader) ([]*LVal, error)) ([]*LVal, error) {
	cache := env.Runtime.LoadCache
	if cache == nil {
		return parse(r)
	}
	src, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}
	key := loadCacheKey(name, loc, src)
	if entry, ok := cache.Load(key); ok && entry != nil && entry.key == key {
		return entry.prog.exprs, nil
	}
	exprs, err := parse(bytes.NewReader(src))
	if err != nil {
		return nil, err
	}
	entry, cacheable := admitCachedSource(key, name, loc, exprs)
	if !cacheable {
		// Not an error for THIS load: the parse is fine, it simply cannot
		// be shared, so it is handed over uncached (see the doc comment).
		return exprs, nil
	}
	cache.Store(key, entry)
	return entry.prog.exprs, nil
}

// admitCachedSource is newCachedSource with the admission's refusal folded
// into a boolean, because at this call site a refusal is not an error to
// report but a decision not to cache.  Keeping the error-returning
// constructor as the primitive means the reason stays available to any
// future caller that wants to surface it.
func admitCachedSource(key, name, loc string, exprs []*LVal) (*CachedSource, bool) {
	entry, err := newCachedSource(key, name, loc, exprs)
	if err != nil {
		return nil, false
	}
	return entry, true
}
