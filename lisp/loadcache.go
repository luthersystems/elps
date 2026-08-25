// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"reflect"
	"strings"
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
//     What that verification covers, precisely, is one sentence: THE SEALED
//     BYTES NEVER CHANGE after a tree is admitted (lisp/sealfp.go states the
//     same).  So -tags elpscheck proves that no load rewrites a cached node,
//     and — with the ownership checker's admission fix — that a node whose
//     seal flag is set but whose type is not sealable is NOT exempt and so
//     still trips the cross-runtime gate.  What it does NOT prove: that the
//     RIGHT program was served.  Serving one file's parse for another
//     (wrong-program-served) leaves the served bytes internally stable, so
//     the fingerprint check is blind to it; that class is closed by the key
//     (name, location, reader identity, and Read-vs-ReadLocation method) and
//     by the flag-vs-type admission conjunction, not by the checked build.
//
// # Keying
//
// The key is derived by elps, from the source bytes AND the stream's name,
// location, the identity of the reader that parses them, and which reader
// method (Read vs ReadLocation) is in use — so the key binds the ENTRY's
// producer, not merely its input (see loadCacheKey).  substrate's cacheReader
// keys on a hash of the content alone, which is safe for its topology (a
// file's name, location and reader are stable across loads) but is not safe
// in general: two files with identical content would share an entry, and the
// served tree carries the FIRST file's parse locations, so every stack trace
// and error from the second would name the wrong file; and two different
// readers, or the two reader methods, would serve each other's parses.
// Including all four costs nothing on the topology that motivated this and
// removes a silent misattribution — and a silent wrong-program serve —
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
//   - Load and Store must NOT re-enter the load path (they must not call
//     Load*/LoadFile, directly or through a warmer).  A re-entrant load is
//     defended against — the guard treats it as a miss and parses without
//     the cache (see (*LEnv).readCached) — but relying on that means giving
//     up caching for the re-entrant load; do the warming outside the hook.
//
// # Reader custody on the fast path
//
// When a Reader hands back output already sealed throughout (the standard
// parser's path), admission stores the Reader's OWN nodes — it does not copy
// them.  This is deliberate: it is what makes a cache miss zero-copy, and the
// standard and format-preserving parsers do not retain what they return.  The
// contract that keeps it safe is therefore on the Reader: a Reader whose
// output feeds a LoadCache MUST NOT retain and later mutate the nodes it
// returned.  This is the same residual the seal design already carries for all
// embedder Go code (seal.go: the flag cannot stop a direct v.Cells[0] = x),
// and its enforcement is the same — checked builds (-tags elpscheck) record
// each cached tree's fingerprint and re-verify it after every load, so a
// Reader that rewrites a stored tree is caught at the load that observed the
// change; production builds do not check and would be silently corrupted.
// (Copying on the fast path would close it unconditionally but would add a
// deep copy to every miss even though no compliant Reader needs it — the cost
// this hook exists to remove — so the contract is stated instead.)
//
// # Behaviour changes a cache can introduce
//
//   - Installing a cache can change lisp SEMANTICS for a non-sealing Reader.
//     Admission's copy path runs SealAST, so a guarded mutation site
//     ((stable-sort < …), (append 'vector …), (slice 'vector …)) that
//     succeeded cache-less against a Reader that did not seal begins raising
//     modify-literal-error once a cache is installed.  This is a migration
//     hazard, not a doc nit — code that mutated program literals in place
//     stops doing so.  (The standard parser already seals, so its callers see
//     no change; format-preserving and hand-written Readers are the ones
//     affected.)  A wrapping Reader that synthesizes even ONE node forces the
//     whole file down the copy-and-seal path, so the zero-copy hit is
//     conditional on the Reader sealing its entire output.
//   - Installing a cache drains the stream with io.ReadAll before parsing, so
//     a streaming Reader that delivers a full program then a non-EOF error
//     succeeds cache-less but fails with a cache (see (*LEnv).readCached).
//
// A nil Runtime.LoadCache disables the hook entirely: the load path is then
// byte-identical to what it was before this hook existed, with no hashing,
// no buffering and no extra allocation (TestLoadCacheNilPathUnchanged).
//
// Note the breadth: the hook sits at the read funnel, so it sees LoadString
// and Load as well as LoadFile — and, because `load-string`/`load-bytes` are
// builtins, GUEST lisp source can mint entries too (a phylum that load-strings
// N distinct programs adds N entries).  A host that evaluates many distinct
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
	prog, err := newProgramForCache(exprs)
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

// ReaderIdentity is an optional interface a Runtime.Reader may implement to
// tell the load cache which parses it produces.
//
// The cache key binds every entry to the reader that produced it, not just to
// the source bytes (see loadCacheKey): two readers that parse the same bytes
// into DIFFERENT trees must never serve each other's entries.  By default that
// binding is the reader's fully-qualified Go type, which is right for elps's
// own readers — the standard parser and the format-preserving parser are
// distinct types — and stable across instances, so many Runtimes each holding
// their own reader of the same type still share cache entries (the motivating
// warm-cache topology).
//
// A reader whose parse output depends on configuration NOT reflected in its Go
// type — the same struct in two modes — is indistinguishable by type alone.
// Such a reader implements ReaderIdentity to return a token that differs
// whenever its parse would; the cache folds that token into the key instead of
// the type.  Implementing it is optional: the default is safe without any
// embedder cooperation, and this is only for readers that multiplex parse
// behaviours behind one Go type.
type ReaderIdentity interface {
	// ReaderIdentity returns a stable token that differs between readers whose
	// parse output differs.  Two readers returning the same token are treated
	// as interchangeable producers by the cache.
	//
	// The EMPTY string is not a valid token: it states nothing, and two
	// readers returning it would be declared interchangeable.  A reader that
	// returns it disables the cache for its own loads (they parse every time)
	// rather than risking a wrong-program serve.
	ReaderIdentity() string
}

// readerIdentity derives the cache-key component that binds an entry to the
// reader that produced it.  A reader states its own identity through
// ReaderIdentity when it implements it; otherwise the reader's
// fully-qualified Go type is used — stable across instances of one type,
// distinct across types.  The "id:"/"go:" prefixes keep a crafted identity
// token from colliding with a type path.
func readerIdentity(r Reader) (string, bool) {
	if r == nil {
		return "<nil>", true
	}
	if id, ok := r.(ReaderIdentity); ok {
		tok := id.ReaderIdentity()
		if tok == "" {
			// An EMPTY token states nothing.  Two readers that both return it
			// would be declared interchangeable producers and would serve each
			// other's parses — the very failure ReaderIdentity exists to
			// prevent, reached by implementing it badly.  Falling back to the
			// Go type would be no better (a reader multiplexing parse
			// behaviours behind one type is exactly why it implements this),
			// so no key is derivable and the load runs uncached.
			return "", false
		}
		return "id:" + tok, true
	}
	t := reflect.TypeOf(r)
	stars := 0
	for t.Kind() == reflect.Pointer {
		stars++
		t = t.Elem()
	}
	star := strings.Repeat("*", stars)
	if pkg := t.PkgPath(); pkg != "" {
		return "go:" + star + pkg + "." + t.Name(), true
	}
	return "go:" + star + t.String(), true
}

// loadCacheKey derives the cache key for a source stream.  The digest covers
// the bytes, both identity strings, the identity of the READER that will parse
// them, and which reader METHOD (Read vs ReadLocation) is in use — each
// length-prefixed so that no concatenation of one tuple can collide with
// another (a plain name+loc+src concatenation makes ("ab", "c") and
// ("a", "bc") indistinguishable).
//
// Reader identity and method are in the key because the key binds the ENTRY's
// producer, not only its input.  Without them: two Runtimes with different
// Readers sharing one cache served each other's parses; swapping
// Runtime.Reader between two loads re-served the stale parse; and elps's own
// Load (Read) and LoadLocation (ReadLocation) collided on the same
// (name, "", src) tuple even though ReadLocation assigns locations Read does
// not.  Folding both in makes each of those degrade to a correct reparse
// instead of a wrong-program serve, and it is derived entirely by elps from
// the installed reader — no embedder cooperation required (see readerIdentity
// for the one optional hook).
//
// SHA-256 rather than a fast non-cryptographic hash: the key decides WHICH
// PROGRAM RUNS, and on a collision the wrong program runs silently.  The
// cost is one pass over a source file per load — nanoseconds per kilobyte,
// against the milliseconds of parsing it replaces.
func loadCacheKey(name, loc, readerID string, byLoc bool, src []byte) string {
	h := sha256.New()
	var n [8]byte
	write := func(b []byte) {
		binary.LittleEndian.PutUint64(n[:], uint64(len(b)))
		_, _ = h.Write(n[:])
		_, _ = h.Write(b)
	}
	write([]byte(name))
	write([]byte(loc))
	write([]byte(readerID))
	if byLoc {
		_, _ = h.Write([]byte{1})
	} else {
		_, _ = h.Write([]byte{0})
	}
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
// One behavioural difference is cache-gated and worth naming: a cache
// installed makes this drain the whole stream with io.ReadAll up front (the
// key is a digest of the bytes), whereas the nil-cache path streams straight
// into the reader.  For an io.Reader that fails partway — one that delivers a
// complete program and THEN a non-EOF error — the two paths disagree: with no
// cache the reader may have finished before the error surfaces (the load
// succeeds); with a cache io.ReadAll surfaces the error first (the load
// fails).  The divergence is inherent to needing the bytes before parsing;
// it is documented on the LoadCache interface as well.
//
// Fall-backs to an ordinary parse, silent because they are "the cache did
// not help", not "the load failed":
//
//   - An entry whose Key does not match the key elps derived is not
//     trusted.  A cache that mixes up its own keys then degrades to no
//     cache instead of running the wrong file's program.
//   - A Reader that declined to state an identity (an empty ReaderIdentity
//     token).  No key can bind an entry to a producer that names itself
//     nothing, so the cache disables itself for that reader's loads rather
//     than key on a token two readers could both return.
//   - A parse newProgramForCache refuses to admit as cacheable — a Reader
//     that returned a reference type, a nil node, a node no seal can cover,
//     or a node carrying a Native payload the seal cannot vouch for — is not
//     shareable, so it is handed to this one load and never stored.
//     Correctness first: the alternative (store it and copy on every load)
//     would put an unsealed tree in a process-wide cache, which is the
//     topology this hook exists to make impossible.
//   - A parse that is legal but larger than the cache admission's node
//     budget (errReaderTreeTooLarge) — in distinct nodes, in unfolded size,
//     or both.  A node COUNT is not a safety property: the program is fine,
//     it is merely too big to be worth aliasing process-wide, so the load
//     runs uncached.  The budget belongs to cache admission alone —
//     ReadProgram, ParseProgram and TextLoader impose none — precisely so
//     that installing a cache cannot reject a program that always loaded.
//     A heavily interned very large source lands here, not in the class
//     below.
//
// One case is NOT a silent fall-back but a hard load error: reader output
// that is not a finite tree — a cycle, nesting past the depth cap, or sharing
// whose UNFOLDED size is past loaderWalkUnfoldedCap (errReaderTreeUnbounded).
// Such output is not merely un-cacheable; it is unsafe to evaluate: a cycle
// only stops at the eval nesting cap, and 4.3e9 node evaluations do not stop
// at all.  So the load fails here rather than falling back to an uncached
// eval of it.
//
// ORDINARY SHARING IS NOT IN THAT CLASS.  A repeated leaf never was; a
// repeated composite used to be, and that rule broke a program that loaded
// fine without a cache — one small subexpression reached twice is linear, is
// what a constant-interning Reader produces, and evaluates in microseconds.
// The discriminator is now the unfolded size the memo computes exactly, not
// the presence of sharing (see loaderWalk.verdict).
//
// # Re-entrancy
//
// A cache whose Load or Store warms itself by loading another file re-enters
// this funnel.  Left alone that is a stack overflow (endless re-entry) or a
// deadlock (a non-reentrant embedder mutex re-locked).  Every other embedder
// mistake here degrades to "no cache"; this one kills the process, so it gets
// a guard: the OUTERMOST readCached owns the cache for the duration of one
// load-admission, and any load re-entered from inside it bypasses the cache
// and parses directly.  Ordinary nested loads do NOT trip it — a load-file
// whose file load-files another re-enters only AFTER this function has
// returned and (*LEnv).load begins evaluating, by which point the guard is
// already cleared — so nested loads still cache normally.
func (env *LEnv) readCached(name, loc string, byLoc bool, r io.Reader, parse func(io.Reader) ([]*LVal, error)) ([]*LVal, error) {
	cache := env.Runtime.LoadCache
	if cache == nil || env.Runtime.loadCacheActive {
		return parse(r)
	}
	readerID, ok := readerIdentity(env.Runtime.Reader)
	if !ok {
		// The reader declined to state an identity (an empty ReaderIdentity
		// token), so no key can bind this entry to its producer.  Parse
		// uncached rather than key on something that could collide.
		return parse(r)
	}
	env.Runtime.loadCacheActive = true
	defer func() { env.Runtime.loadCacheActive = false }()

	src, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}
	key := loadCacheKey(name, loc, readerID, byLoc, src)
	if entry, ok := cache.Load(key); ok && entry != nil && entry.key == key {
		// Checked builds re-verify the entry against the fingerprint taken at
		// ADMISSION, not against a per-root seal-time record: the entry
		// already carries exactly the value that catches a Reader which
		// rewrote what it handed over, and a per-root check cannot (each root
		// is compared to its own seal-time fingerprint, and a substituted
		// root is legitimately sealed).  No-op in production builds.
		verifyCachedSourceOnHit(entry)
		return entry.prog.exprs, nil
	}
	exprs, err := parse(bytes.NewReader(src))
	if err != nil {
		return nil, err
	}
	entry, err := newCachedSource(key, name, loc, exprs)
	if err != nil {
		if errors.Is(err, errReaderTreeUnbounded) {
			// Not safe to evaluate either — fail the load (see above).
			return nil, err
		}
		// Not an error for THIS load: the parse is fine (errReaderTreeTooLarge
		// says so explicitly — it is merely bigger than the cache budget), it
		// simply cannot be shared, so it is handed over uncached (see the doc
		// comment).
		return exprs, nil
	}
	cache.Store(key, entry)
	return entry.prog.exprs, nil
}
