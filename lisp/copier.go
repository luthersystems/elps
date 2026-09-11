// Copyright © 2026 The ELPS authors

package lisp

import (
	"cmp"
	"errors"
	"fmt"
	"reflect"
	"slices"
)

// copier is the walk behind (*LVal).Copy: the fifth value-rebuilding walker
// in this package, and until this file the only one with no memo at all.
//
// # The defect
//
// Copy rebuilt a value per *LVal HEADER.  Two headers over one payload --
// what `(quasiquote (unquote a))`, Quote, Splice and FunRef produce, since
// they copy the struct and keep its Native -- therefore came out of Copy in
// two ways, both wrong:
//
//   - a sorted map was rebuilt once per header (copyMapData allocates a
//     fresh map every call), so the copy held TWO maps where the source
//     held one, and a write through one name was invisible through the
//     other.  This is the #576 / #585 class: the fork walker had it (fixed
//     by the memo in the fork walker, now lisp/template*.go), the detach
//     walker had it (742598b), and
//     Copy had it for the whole time both guards were green, because Copy
//     was in neither registry.
//   - a bytes value was not rebuilt at all: `*cp = *v` kept the *[]byte, so
//     the copy and the source wrote one buffer.  A "deep copy" that shares
//     bytes is not a deep copy, and on the isolation boundary Copy serves
//     (lisp.TextLoader hands each evaluation "a PRIVATE tree", loader.go)
//     it is one load writing another's data.
//
// A sorted map's VALUES were also shared by pointer (copyMapData's contract,
// which assoc and dissoc build on and which is unchanged for them), so a
// container parked inside a map was reachable from the copy as the source's
// own header and payload.  Copy already walked a list's cells; it now walks
// a map's values the same way, so the copy's containers are all its own.
//
// # The fix
//
// The detacher's memo pattern (lisp/detach.go), applied verbatim: one memo
// per payload kind -- *MapData, *[]byte, and a NativeCloner payload held by
// pointer -- so two headers over one payload get ONE copied payload; plus
// the header memo every walker carries, which is what makes a value
// reachable twice copy once and a cycle close onto the copy rather than
// recurse without bound (Copy used not to terminate on a self-containing
// value; lisp/package_admit.go classifies a cycle before it copies for
// that reason, and still does).  Each memo is seeded BEFORE the payload's
// contents are walked, so a map that reaches itself through a second
// header closes onto its own copy (issue #585's shape).
//
// lisp/copier_test.go drives the walk over the historical aliasing shape
// and pins that the copy holds none of the source's headers or payloads
// while preserving the source's internal sharing.
//
// # What is deliberately unchanged
//
//   - The seal is cleared on every copied node (the sanctioned way to get a
//     mutable version of a program literal; lisp/seal.go).
//   - Macro-expansion debug metadata is dropped, as Fork and detach drop
//     it: its shared context points at the source tree's nodes (see the
//     comment at the assignment).
//   - A function value keeps its environment by reference: Copy shares
//     closures, as `copy` does.
//   - A list's cells backing array is NOT preserved across the copy
//     (TestCopyDoesNotPreserveBackingArraySharing).
//   - An LError's *CallStack is shared.  It is immutable by construction:
//     CallStack.Copy allocates exact-length Frames at every capture site
//     and nothing writes a captured stack, so sharing it shares nothing a
//     writer can reach.
//   - A native payload that is not a NativeCloner is shared by reference,
//     as it is by `copy` and by Fork; one that is a NativeCloner is cloned
//     exactly as the detacher clones it in copy mode -- once per pointer
//     payload, no runtime-affinity check, since the copy stays in the
//     runtime it was made in.
//
// # The LArray arm
//
// An LArray is walked like a list.  Copy used to keep the struct-copied
// Cells for an array outright ("reference semantics"), so the copy's vector
// held the SOURCE's dims and data-list headers -- and through them every
// element header, so a copy reached the source's map through a vector and a
// write through the copy landed in the source.  CI's fuzzer found that
// within seconds of (*LVal).Copy being driven through the alias guard
// (FuzzAliasGuard, #604).  The dims header and the data-list header now go
// through the header memo, exactly as they do in the fork walker, which has
// no LArray arm: two vector headers over one data
// list -- `(quasiquote (unquote v))` -- copy to two headers over ONE copied
// list, and the copy reaches no source header.  TestCopyRebuildsArrayBacking
// is the control; it was TestCopyAliasesArrayBacking, pinning the sharing,
// until then.
//
// # Cost, and why the memo is not simply a map
//
// Copy is a per-call primitive on small values: lambda creation copies its
// formals, make-sequence copies each number it emits, and
// TestCopyLeafAllocatesLikeAStructCopy pins the leaf cost as an equality.
// (The sort comparators, once the heaviest per-call callers, no longer copy
// at all -- stable-sort and insert-sorted pass their elements by reference,
// #604.)  A heap-allocated map per walk would be several allocations on
// every one of those calls.  So the header memo is an inline array of
// copierSmallMemo pairs that spills to the `seen` map only when a walk
// outgrows it, the copier lives on Copy's stack (no closure captures it --
// map entries are walked by loop, not through sortedmap.clone's callback,
// for exactly that reason).  A leaf therefore costs its header and nothing
// else.  TestCopyLeafAllocatesLikeAStructCopy and
// TestCopyMemoSpillsPastTheInlineArray pin both ends.
//
// A caller that knows the walk is large can say so: copyWithHint reserves
// the map at the count of headers the walk will memoise -- the containers
// and payload headers, not the leaves -- before the walk starts, which spares the load
// path (lisp.TextLoader copies a cached tree on every load, and counts it
// once at admission) the map's growth through every doubling.  The walk is
// the same either way; only where the memo lives differs.
//
// # Failure is whole-walk, not per-node
//
// Every memo in this walker is SEEDED before the thing it describes is
// finished: the header memo takes cp right after `*cp = *v`, and the
// payload memo takes an empty *MapData before the map's entries are walked,
// both so a value that reaches itself closes onto its own copy rather than
// recursing without bound.  A failure part-way through therefore does not
// have one damaged node; it has however many nodes the walk had already
// published over unfinished storage.  A map whose copy fails after it has
// been reached through a SECOND header leaves that header memoised over a
// *MapData whose backing was never assigned -- a sorted-map that panics on
// the first write -- and repairing only the header the arm failed on left
// that one parked in the enclosing container's cell, in a list Copy
// returned as a success.
//
// So the walk is fail-stop: the first arm to fail records the error in
// c.failed, every subsequent `copy` returns it without descending, the cell
// loop stops as soon as a child sets it, and Copy hands back c.failed
// rather than the container it was building.  Nothing the abandoned walk
// built is reachable from what the caller gets, which is the only statement
// that stays true whatever a walk had published before it failed.
// TestCopyFailedMapCopyLeavesNoSourceBackedHeader drives the shapes.
type copier struct {
	// small and n are the header memo until the walk outgrows them.
	small [copierSmallMemo]copyPair
	// seen is the header memo past that point; lookups check both.
	seen    map[*LVal]*LVal
	maps    map[*MapData]*MapData
	bytes   map[*[]byte]*[]byte
	natives map[interface{}]interface{}
	// runtime optionally limits each copied data backing allocation.
	// Ordinary Go Copy calls retain their existing unlimited behavior.
	runtime *Runtime
	// failed is the error the walk stopped on, and is NOT a memo: it holds
	// one value for the whole walk, it is never looked up by a source
	// pointer, and it is what Copy returns once it is set.
	failed *LVal
	n      int
}

// copierSmallMemo is how many headers a walk memoises before it allocates.
// A cond test, a formals list or a small literal fits; a program tree or a
// data structure spills to the map, where the per-node allocation is
// amortised over a copy that already allocates per node.
const copierSmallMemo = 16

// copierMemoHintCap bounds the memo a hint may reserve up front.  A hint is
// advisory: it is taken on trust from a caller inside the package, but it
// reserves memory before a single node has been copied, so a wrong one -- an
// interning Reader whose unfolded visit count is far above its distinct
// node count -- must not become an unbounded reservation.  Past the cap the
// memo grows from the cap exactly as an unhinted walk grows from empty.  It
// mirrors the cache admission's node budget, sealFPMaxNodes: orders of
// magnitude beyond any top-level expression a parser emits.
const copierMemoHintCap = sealFPMaxNodes

type copyPair struct{ src, dst *LVal }

func (c *copier) lookup(v *LVal) (*LVal, bool) {
	for i := range c.n {
		if c.small[i].src == v {
			return c.small[i].dst, true
		}
	}
	if c.seen != nil {
		cp, ok := c.seen[v]
		return cp, ok
	}
	return nil, false
}

func (c *copier) remember(v, cp *LVal) {
	if c.seen == nil {
		if c.n < len(c.small) {
			c.small[c.n] = copyPair{v, cp}
			c.n++
			return
		}
		c.seen = make(map[*LVal]*LVal, 2*copierSmallMemo)
	}
	c.seen[v] = cp
}

// presize reserves the header memo for a walk of about n headers, so a
// large tree is memoised into a map built once at its final size instead of
// one grown from empty through every doubling.  A hint that fits the inline
// array is ignored -- the walk then costs exactly what an unhinted one
// costs -- and a hint past copierMemoHintCap is clamped to it.  Once the map
// exists remember skips the inline array, so the lookup path is the map
// alone (c.n stays 0).  A hint smaller than the tree is harmless: the map
// grows from the hint as it would from empty.
func (c *copier) presize(n int) {
	if n <= copierSmallMemo {
		return
	}
	if n > copierMemoHintCap {
		n = copierMemoHintCap
	}
	c.seen = make(map[*LVal]*LVal, n)
}

// copyWithHint is Copy for a caller that knows about how many headers the
// walk will memoise: the same walk, the same memos, the same output -- one
// header per source header, the same aliasing, a cycle closing onto the copy
// -- with the header memo reserved at n up front (see presize).  It exists
// for lisp.TextLoader, which copies a cached parse tree on EVERY load and
// has already counted the tree's nodes at admission: on a library-sized
// source the map the unhinted walk grows from empty is thousands of entries
// through every doubling, and that growth's garbage and rehashing is a
// measurable share of the per-load cost (#604's benchmark gate,
// BenchmarkTextLoaderLoad: sec/op +15.5 %, B/op +8.9 %, allocs/op +2.5 % --
// the signature of map growth, not of per-node allocation).  Copy itself is
// unchanged for every other caller, and a hint of 0 IS Copy.
func (v *LVal) copyWithHint(n int) *LVal {
	if v == nil {
		return nil
	}
	var c copier
	c.presize(n)
	return c.copy(v)
}

// copyWithRuntime keeps Copy's within-runtime sharing rules, with allocation
// checks for interpreter calls. Failure is separate because a successfully
// copied condition is itself an LError.
func (v *LVal) copyWithRuntime(runtime *Runtime) (*LVal, *LVal) {
	c := copier{runtime: runtime}
	cp := c.copy(v)
	return cp, c.failed
}

func (c *copier) checkAlloc(n int) error {
	if c.runtime != nil {
		if msg := c.runtime.CheckAlloc(n); msg != "" {
			if c.failed == nil {
				c.failed = Errorf("%s", msg)
			}
			return errors.New(msg)
		}
	}
	return nil
}

// copy is the walk's only entry point, and the fail-stop's.  Once an arm
// has recorded a failure in c.failed every level returns it without
// descending -- the OUTERMOST level included, which is what makes
// (*LVal).Copy and copyWithHint hand the caller the error rather than the
// container the abandoned walk was building, with whatever it had already
// published over unfinished storage still hanging off it.  See the type
// comment for why repairing the failing node alone is not enough.
func (c *copier) copy(v *LVal) *LVal {
	if v == nil {
		return nil
	}
	if c.failed != nil {
		return c.failed
	}
	cp := c.copyNode(v)
	if c.failed != nil {
		return c.failed
	}
	return cp
}

// copyNode copies one node and its children.  Callers go through copy,
// which is where the fail-stop lives; nothing here has to check c.failed
// except the cell loop, which stops early rather than copying the error
// into every remaining cell of a container that is about to be discarded.
func (c *copier) copyNode(v *LVal) *LVal {
	// Only a node that can be reached twice in a way the copy could observe
	// is memoised: one with cell storage (a container, or a header over
	// hidden capacity) or a payload.  A leaf -- a number, string, symbol, an
	// empty list with no capacity -- has neither: it cannot close a cycle,
	// it carries no storage two copied headers could share, and Lisp cannot
	// observe whether two slots hold one leaf header or two (values compare
	// by value; leaves are immutable), which is also exactly what Copy
	// produced before it memoised at all.  Skipping leaves keeps the memo
	// proportional to the containers in a tree rather than to its size,
	// which on a parse tree -- mostly symbols and literals -- is the
	// difference between a memo the size of the tree and one a third of
	// it (the load-path benchmarks, TextLoaderLoad and the reader-cache-copy
	// arm of LoadIntoEnv, measure the bytes).
	memoise := cap(v.Cells) > 0 || v.Native != nil
	if memoise {
		if cp, ok := c.lookup(v); ok {
			return cp
		}
	}
	// Array/quote/tag Cells are fixed representation headers; their child
	// lists carry the variable data spans. Check these spans before any
	// child clone hook or backing allocation can run.
	if v.Type == LSExpr || v.Type == LError || v.Type == LFun {
		if c.checkAlloc(len(v.Cells)) != nil {
			return c.failed
		}
	}
	// Constructed here and written here, in one function: cmd/elpsvet's
	// rule (issues #333 and #334) is that a field write lands on a value
	// the writer built, so the per-node privatising below is inlined rather
	// than factored into a helper that would write on a value it did not
	// construct.
	cp := &LVal{}
	*cp = *v // shallow copy of all fields, including Native
	// The copy owns fresh storage, so the sealed constraint on v does not
	// apply to it.  Every fresh node the walk creates has the flag cleared,
	// so copying a sealed tree yields a fully unsealed, fully private tree
	// — the sanctioned way to obtain a mutable version of a program literal
	// (lisp/seal.go).
	cp.sealed = false
	// source rides along in the struct assignment above, so without this the
	// copy and the original hold ONE mutable *token.Location, at every depth
	// -- Cells are deep-copied just below, positions were not.  That is issue
	// #446, and lisp.TextLoader is what it defeats: TextLoader's entire
	// purpose is to hand each evaluation a PRIVATE tree (it is the entry
	// point an embedder is pointed at for a reusable parse cache; the Load*
	// entry points do not copy), and every one of those "private" trees
	// reported its positions through the retained cache's own objects.
	//
	// Sealing makes this MORE load-bearing, not less.  Copy is the sanctioned
	// way to obtain a mutable version of a sealed program literal, and it
	// clears the flag just above -- so SetSource, which is a no-op on the
	// sealed original, is live on the copy.  Sharing the pointer here would
	// let a write through the unsealed copy move a position in the sealed
	// tree every environment in the process is evaluating.
	//
	// One Location per NODE here, where issue #431 needed only one per macro
	// CALL, because what has to be separated is different.  There the N
	// stamped nodes genuinely sit at one position, so a single object owned
	// by the expansion separated the two owners.  Here each node has a
	// position of its own, so N nodes need N objects.
	//
	// The exception main carried for nativeSource's process-wide singleton is
	// gone with the singleton: values Go constructs now leave source nil and
	// synthesize the "<native code>" location by value in the accessor (issue
	// #362), so the nil check below is also the fast path this used to buy --
	// no allocation on the interpreter's hot path, where most values are ones
	// Go built.
	if v.source != nil {
		cp.source = v.source.Copy()
	}
	// meta rides along in the struct assignment above for the same reason
	// source did (issue #466).  It is PER-NODE mutable state -- fmtmeta.Meta
	// is what the parser writes and hoistOperandComments moves between
	// nodes -- so sharing it makes a "deep copy" a second writer on one
	// object, and reopens #446 one level down: the *token.Location on every
	// comment token is reachable from both trees.  It is nil outside
	// format-preserving parsing, so on the hot path this is a nil check.
	cp.meta = detachMeta(v.meta)
	// macroExpansion is DROPPED, as the fork walker and detach
	// (lisp/detach.go) drop it.  Its shared half, the per-expansion
	// context, records the macro call's unevaluated arguments as pointers to
	// the SOURCE tree's nodes, and on this fixture some of those are
	// unsealed -- so a copy that kept the record was a private tree with a
	// back-pointer into the tree it was copied from, the exact aliasing
	// hole this walker exists to close, on the one path (TextLoader) whose
	// purpose is a private tree.  Remapping the record to the copied nodes
	// would need a per-walk memo of contexts and a pass after the walk;
	// dropping it is what the other two walkers already do.  What is lost:
	// the metadata is stamped during evaluation, and the load paths copy a
	// tree BEFORE evaluating it, so nothing is lost there; a tree that was
	// expanded, retained and then copied loses the debugger's macro
	// attribution on the copy, as it already does on every fork.
	// TestCopyDropsMacroExpansionMetadata is the control.
	cp.macroExpansion = nil
	// Seeded before anything below descends: a child that reaches v again
	// gets cp, so a shared subtree is copied once and a cycle closes onto
	// the copy.  A leaf is not seeded (see memoise above).
	if memoise {
		c.remember(v, cp)
	}
	switch v.Type {
	case LSortMap:
		// Sorted-maps store data in Native (*MapData) which contains Go
		// maps.  A shallow struct copy would alias the underlying maps,
		// causing assoc!/dissoc! on the copy to mutate the original.  One
		// copied map per source map, however many headers reach it.
		md, err := c.mapData(v.Map())
		if err != nil {
			// cp is ALREADY in the header memo (seeded above so a cycle
			// closes onto it), and `*cp = *v` left it carrying the
			// SOURCE's *MapData: returning a fresh error value here and
			// walking away left that unfinished cp memoised, so a second
			// encounter of v -- `(list a a)`, a quasiquoted second header,
			// anything that reaches one header twice -- got a sorted-map
			// sharing the source's map, and a write through the copy
			// landed in the source.  A failed copy must never leave a
			// source-backed destination behind.
			//
			// Overwritten in place rather than deleted from the memo: cp
			// may already be referenced by a cell copied during the failed
			// traversal (the map reaching itself is the shape this walker
			// exists for), so the entry has to stay and its VALUE has to
			// stop being source-backed.  Every encounter of v then yields
			// this same error header.  cp is a value this function
			// constructed, which is what cmd/elpsvet's write rule requires.
			//
			// The overwrite alone is not the fix, only its local half: a
			// DIFFERENT header over the same *MapData, copied earlier in
			// this same failing walk, is memoised over the half-built
			// payload and is not reachable from here.  c.failed is what
			// stops the walk and keeps every such header off the result
			// (see the type comment).
			e := Errorf("copy sorted-map: %v", err)
			*cp = *e
			if c.failed == nil {
				c.failed = cp
			}
			return cp
		}
		cp.Native = md
		return cp
	case LBytes:
		if b, ok := v.Native.(*[]byte); ok && b != nil {
			cp.Native = c.byteSlice(b)
		}
	case LNative:
		if cl, ok := v.Native.(NativeCloner); ok {
			cp.Native = c.cloneNative(v.Native, cl) //elpsvet:allow-native a NativeCloner clone stored by the walker that invoked it: Copy is a within-runtime value copy, not template admission, and publication classifies the clone on its own dynamic type if the copy is ever published
		}
	default:
		// Every other type carries its payload in the struct copy above —
		// an LError's *CallStack included, shared by design (see the type
		// comment) — and its children in Cells, walked below.  An LArray
		// is one of them: its dims and data-list headers are children,
		// memoised like any other (see "What changed" in the type comment).
	}
	cp.Cells = c.cells(v)
	return cp
}

func (c *copier) cells(v *LVal) []*LVal {
	if len(v.Cells) == 0 {
		return nil
	}
	cells := make([]*LVal, len(v.Cells))
	for i := range cells {
		cells[i] = c.copy(v.Cells[i])
		if c.failed != nil {
			// Fail-stop: the container being built is discarded by Copy,
			// so there is nothing to finish and every further cell would
			// only be another copy of the error.
			return nil
		}
	}
	return cells
}

// mapData rebuilds md once per source map, seeding the memo BEFORE the
// entries are walked so a map that reaches itself through a second header
// closes onto its own copy.  Keys are shared (Set stores them by string);
// values are walked, so a container held as a value is the copy's own.
func (c *copier) mapData(md *MapData) (*MapData, error) {
	if md == nil {
		return nil, nil
	}
	if cp, ok := c.maps[md]; ok {
		if cp == nil {
			// The nil sentinel failMap leaves behind: an earlier encounter
			// of md failed part-way, so the seeded nm it published is
			// half-built (its backing may never have been set at all).  A
			// later header must fail too rather than pick that up as a
			// finished copy.
			return nil, errCopyMapFailed
		}
		return cp, nil
	}
	if md.mapBacking != nil {
		if err := c.checkAlloc(md.Len()); err != nil {
			return nil, err
		}
	}
	if c.maps == nil {
		c.maps = make(map[*MapData]*MapData)
	}
	nm := &MapData{}
	c.maps[md] = nm
	switch m0 := md.mapBacking.(type) {
	case nil:
		// Degenerate MapData with no implementation (possible via
		// SortedMapFromData(NewMapData(nil))): a fresh struct that shares
		// nothing, with the nil Map preserved.
		return nm, nil
	case sortedmap:
		// By loop rather than m0.clone(c.copy): a method value capturing c
		// would send the copier to the heap on every Copy call.
		//
		// In sorted KEY order, not Go map order: c.copy can call a host
		// hook per value -- NativeCloner.CloneNative, written by the
		// embedder, which may draw on state outside the value (an id
		// allocator, a sequence, an rng) -- so walking the entries in the
		// randomised order Go's map iteration hands out made two copies of
		// IDENTICAL input assign different clones to the same key.  Copy
		// is a value primitive on a runtime whose output must not depend
		// on a map's internal layout (the same reason ArrayIndex's error
		// message is %v rather than %#v, #427).
		//
		// The order is not free -- one []string and the sort per map,
		// measured at +52 % on a 64-entry copy and +73 % at 512 -- so it
		// is paid only where it can be observed: when no value in the map
		// can run host code, the walk order is unobservable and the sort
		// is skipped (copierValueOrderMatters, an O(n) scan that allocates
		// nothing, against a sort that allocates a []string).  A map of
		// scalars, which is the common shape, therefore copies for less
		// than it did before the order existed.
		//
		// The keys are collected and sorted alone, and the values read
		// back by lookup: a []string sorted by slices.Sort measured
		// cheaper than a []{key,value} sorted by slices.SortFunc, whose
		// comparison closure costs more than the hash lookups it saves
		// (+74 % against +52 % on the 64-entry copy).  A map of fewer
		// than two entries is already ordered and collects nothing.
		sm := m0.emptyLike()
		if len(m0.m) < 2 || !copierValueOrderMatters(m0.m) {
			for k, v := range m0.m {
				sm.m[k] = c.copy(v)
			}
		} else {
			for _, k := range copierSortedKeys(m0.m) {
				sm.m[k] = c.copy(m0.m[k])
			}
		}
		for k, t := range m0.tm {
			sm.tm[k] = t
		}
		nm.mapBacking = sm
		return nm, nil
	}
	if r, ok := md.mapBacking.(StringKeyRanger); ok {
		// Collect first, copy after: the callback must not capture c (see
		// the sortedmap case).
		type stringKV struct {
			v *LVal
			k string
		}
		pairs := make([]stringKV, 0, md.Len())
		if err := r.RangeStringKeys(func(k string, v *LVal) {
			pairs = append(pairs, stringKV{v: v, k: k})
		}); err != nil {
			return c.failMap(md, fmt.Errorf("failed to copy map: %w", err))
		}
		// Sorted before a single value is copied, for the reason the
		// sortedmap arm sorts: RangeStringKeys yields "in unspecified
		// order" by contract, and the host clone hook c.copy may call must
		// not see one order on one copy and another on the next -- and
		// skipped when no value can reach that hook, for the reason the
		// sortedmap arm skips it.
		order := false
		for _, p := range pairs {
			if !copierLeafValue(p.v) {
				order = true
				break
			}
		}
		if order {
			slices.SortFunc(pairs, func(a, b stringKV) int { return cmp.Compare(a.k, b.k) })
		}
		sm := emptyForStringKeys(len(pairs))
		for _, p := range pairs {
			sm.m[p.k] = c.copy(p.v)
		}
		nm.mapBacking = sm
		return nm, nil
	}
	m := &MapData{newmap()}
	entries := sortedMapEntries(md)
	if entries.Type == LError {
		// Entries reported a failure; its Cells hold the message, not
		// pairs, so this has to be checked before they are indexed.
		return c.failMap(md, fmt.Errorf("failed to copy map: %v", entries))
	}
	// Sorted before a single value is copied, for the reason the two arms
	// above sort, and NOT because Entries arrives ordered.  The Map
	// interface (lisp/maps.go) documents Keys as returning a sorted list
	// and says nothing whatever about the order of Entries, so an
	// embedder's implementation over a Go map yields whatever permutation
	// it gets -- and this arm calls the host's CloneNative once per value
	// in exactly that order.  The claim that "the generic Entries arm needs
	// nothing, sorted by contract" was a claim about the STOCK map's
	// Entries, which does sort; this arm is the one the stock map never
	// reaches.
	//
	// By (Str, Type) rather than Str alone, so the order is total over the
	// key kinds Str does not separate: an LString and an LSymbol that spell
	// the same thing sort the same way on every copy.  Stable, so any pair
	// the comparison still cannot separate keeps the order Entries gave it
	// rather than moving under the sort.
	//
	// ALWAYS, unlike the two arms above, which skip the ordering when no
	// value in the map can reach a host hook.  Their fast path rests on "no
	// host hook can run, so the walk order is unobservable", and that is
	// sound for them because their keys are unique Go strings by
	// construction: whatever order they walk in, the destination ends up
	// holding the same entries.  Here the keys arrive from the host as whole
	// LVals, and the destination is the stock map, whose Set (lisp/maps.go)
	// keys on Str for LString and LSymbol alike.  Two entries sharing a Str
	// -- "a" and 'a, or one key twice -- therefore collapse into one, and
	// with the ordering skipped WHICH of them survived was whichever the
	// host happened to yield last: two copies of one map with nothing but
	// scalar values, and no hook anywhere, came out with different
	// CONTENTS.  Order is observable on this arm through the destination
	// itself, not only through a hook, so the scan that decides whether to
	// sort cannot answer the question.  The sort costs little here in any
	// case, next to the per-entry boxing Entries has already done to hand
	// these pairs over.
	//elps:mutates reorders backing this call owns outright: sortedMapEntries allocates the cells slice for this call and wraps it in a QExpr held only by the local `entries`, so nothing outside this function can observe the permutation
	slices.SortStableFunc(entries.Cells, func(a, b *LVal) int {
		if r := cmp.Compare(a.Cells[0].Str, b.Cells[0].Str); r != 0 {
			return r
		}
		return cmp.Compare(a.Cells[0].Type, b.Cells[0].Type)
	})
	// Ordering makes the copy deterministic; it does not make it right.  Two
	// entries the destination cannot hold apart are now ADJACENT, so they can
	// be found -- and they are refused rather than silently resolved, because
	// there is no answer to pick: keeping one is picking the host's order by
	// another name, and the copy would claim to be a copy of a map whose
	// entries it does not hold.  Through failMap, so the walk fail-stops and
	// Copy returns the error at the top rather than parking a map that
	// quietly lost an entry in a cell (c79cec5).
	//
	// Checked against the previous key inside the copy loop rather than in a
	// pass of its own: the loop has that key in hand for Set either way, so
	// the guarantee costs a comparison per entry instead of a second walk of
	// the same pointer chain.
	//
	// Only a pair of string-like keys is judged.  Any other key kind is
	// unrepresentable outright, whatever it sits next to, and is rejected
	// with Set's message ("unhashable type"); an LInt and an LFloat both
	// carry Str "" and would otherwise be reported as sharing a key that
	// neither of them has.
	var prev *LVal
	for _, pair := range entries.Cells {
		key := pair.Cells[0]
		// Validate before copying the value: Go evaluates c.copy before Set
		// can reject its key. Unsupported keys may tie under (Str, Type),
		// so invoking their hooks would expose the host's Entries order (#643).
		if !isStringLike(key) {
			return c.failMap(md, fmt.Errorf("failed to copy map: %v", Errorf("unhashable type: %s", key.Type)))
		}
		if prev != nil && prev.Str == key.Str {
			return c.failMap(md, fmt.Errorf("failed to copy map: entries collide on key %q (%s and %s):"+
				" the destination map cannot hold them apart", key.Str, prev.Type, key.Type))
		}
		prev = key
		if lerr := m.Set(key, c.copy(pair.Cells[1])); lerr.Type == LError {
			return c.failMap(md, fmt.Errorf("failed to copy map: %v", lerr))
		}
	}
	nm.mapBacking = m.mapBacking
	return nm, nil
}

// copierLeafValue reports whether copying v can run no host code.  A value
// with no cell storage and no payload has nothing to descend into and no
// NativeCloner to invoke, so WHERE it sits in the walk order is
// unobservable: the copy is the same value whenever it is made.
//
// Deliberately the same predicate copyNode uses to decide what to memoise,
// and deliberately no weaker.  "Not itself a native" would be cheaper to
// state and wrong: a value with cell capacity is a subtree, and a
// NativeCloner several levels down it is reached by the same walk in the
// same order.  A nil value copies to nil and is a leaf.
// TestCopyMapWithANestedNativeStillCopiesInKeyOrder is the control.
func copierLeafValue(v *LVal) bool {
	return v == nil || (cap(v.Cells) == 0 && v.Native == nil)
}

// copierValueOrderMatters reports whether the order m's values are copied
// in is observable -- whether any of them can reach a host hook.  It is one
// O(n) scan that allocates nothing, standing in front of a sort that
// allocates; for the common map of numbers and strings it is the whole cost
// of the ordering guarantee.
func copierValueOrderMatters(m map[string]*LVal) bool {
	for _, v := range m {
		if !copierLeafValue(v) {
			return true
		}
	}
	return false
}

// copierSortedKeys returns m's keys in sorted order, so the walk over a
// map's values -- and with it every host CloneNative call the walk makes --
// runs in an order that depends only on the map's contents.  The other two
// arms order their own way: the ranger arm sorts the pairs it collected,
// and the generic Entries arm sorts the entry list in place (by key Str
// then key Type, since its keys need not be strings).
func copierSortedKeys(m map[string]*LVal) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	return keys
}

// errCopyMapFailed is what a SECOND encounter of a map whose copy already
// failed gets: the first encounter's error is long returned, and the only
// thing this one has to be is a failure rather than the half-built copy the
// first encounter published.
var errCopyMapFailed = errors.New("failed to copy map: an earlier copy of this map failed")

// failMap poisons md's payload memo and returns err unchanged, for the
// caller to return.  The entry seeded before the entries were walked (nm,
// which lets a map reaching itself close onto its own copy) is half-built
// once the walk fails -- on two of the three arms its backing was never
// assigned at all -- so leaving it in c.maps would hand a later header over
// md a "finished" copy that is empty.  The nil sentinel keeps the failure
// on the payload where a second lookup finds it, without a second map.
func (c *copier) failMap(md *MapData, err error) (*MapData, error) {
	c.maps[md] = nil
	return nil, err
}

// byteSlice copies a bytes payload once per source buffer.
func (c *copier) byteSlice(b *[]byte) *[]byte {
	if cp, ok := c.bytes[b]; ok {
		return cp
	}
	if c.checkAlloc(len(*b)) != nil {
		return nil
	}
	nb := make([]byte, len(*b))
	copy(nb, *b)
	if c.bytes == nil {
		c.bytes = make(map[*[]byte]*[]byte)
	}
	c.bytes[b] = &nb
	return &nb
}

// cloneNative resolves one NativeCloner payload once per pointer payload,
// exactly as the detacher does in copy mode (detacher.cloneNative with
// shareOpaque set): a payload held by value has no identity to preserve and
// is cloned per header.
func (c *copier) cloneNative(payload interface{}, cl NativeCloner) interface{} {
	memo := reflect.TypeOf(payload).Kind() == reflect.Pointer
	if memo {
		if clone, ok := c.natives[payload]; ok {
			return clone
		}
	}
	clone := cl.CloneNative()
	if memo {
		if c.natives == nil {
			c.natives = make(map[interface{}]interface{})
		}
		c.natives[payload] = clone
	}
	return clone
}
