// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"reflect"
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
//     by the memo in lisp/fork.go), the detach walker had it (742598b), and
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
// The walker is registered in lisp/walkers.go ("copier") so the drift
// guard holds its memo set to the same kinds the fork and detach walkers
// memoise, and lisp/copier_test.go drives it through the alias guard's
// CheckWalker.
//
// # What is deliberately unchanged
//
//   - The seal is cleared on every copied node (the sanctioned way to get a
//     mutable version of a program literal; lisp/seal.go).
//   - A function value keeps its environment by reference: Copy shares
//     closures, as `copy` does.
//   - An LArray's Cells backing is shared (reference semantics;
//     TestCopyAliasesArrayBacking), and a list's cells backing array is
//     NOT preserved across the copy (TestCopyDoesNotPreserveBackingArraySharing).
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
// # Cost, and why the memo is not simply a map
//
// Copy is a per-call primitive on small values: insert-sorted copies leaf
// integers inside a binary search, `cond` copies its test expression on
// every evaluation, lambda creation copies its formals, and
// TestVectorBuiltinAllocations pins those allocation counts as equalities.
// A heap-allocated map per walk would be several allocations on every one
// of those calls.  So the header memo is an inline array of
// copierSmallMemo pairs that spills to the `seen` map only when a walk
// outgrows it, the copier lives on Copy's stack (no closure captures it --
// map entries are walked by loop, not through sortedmap.clone's callback,
// for exactly that reason).  A leaf therefore costs its header and nothing
// else.  TestCopyLeafAllocatesLikeAStructCopy and
// TestCopyMemoSpillsPastTheInlineArray pin both ends.
type copier struct {
	// small and n are the header memo until the walk outgrows them.
	small [copierSmallMemo]copyPair
	// seen is the header memo past that point; lookups check both.
	seen    map[*LVal]*LVal
	maps    map[*MapData]*MapData
	bytes   map[*[]byte]*[]byte
	natives map[interface{}]interface{}
	n       int
}

// copierSmallMemo is how many headers a walk memoises before it allocates.
// A cond test, a formals list or a small literal fits; a program tree or a
// data structure spills to the map, where the per-node allocation is
// amortised over a copy that already allocates per node.
const copierSmallMemo = 16

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
	if c.n < len(c.small) {
		c.small[c.n] = copyPair{v, cp}
		c.n++
		return
	}
	if c.seen == nil {
		c.seen = make(map[*LVal]*LVal, 2*copierSmallMemo)
	}
	c.seen[v] = cp
}

func (c *copier) copy(v *LVal) *LVal {
	if v == nil {
		return nil
	}
	if cp, ok := c.lookup(v); ok {
		return cp
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
	// (lisp/seal.go).  (Values that share storage with v — an LArray's
	// backing — are never sealed: SealAST marks parser-producible types
	// only.)
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
	// meta and macroExpansion ride along in the struct assignment above for
	// the same reason source did, and issue #466 is that they still do.  Both
	// are PER-NODE mutable state -- fmtmeta.Meta is what the parser writes and
	// hoistOperandComments moves between nodes; macroExpansionInfo is the
	// per-node half of an expansion record whose shared half is the context
	// it embeds.  Sharing them makes a "deep copy" a second writer on one
	// object, and in meta's case it also reopens #446 one level down: the
	// *token.Location on every comment token is reachable from both trees.
	//
	// The cost argument is the opposite of source's.  meta is nil outside
	// format-preserving parsing and macroExpansion is nil unless a debugger
	// is attached, so on the interpreter's hot path this is two nil checks
	// and no allocation, and it allocates only on paths already doing
	// per-node formatting or debug work.
	cp.meta = detachMeta(v.meta)
	cp.macroExpansion = v.macroExpansion.Copy()
	// Seeded before anything below descends: a child that reaches v again
	// gets cp, so a shared subtree is copied once and a cycle closes onto
	// the copy.
	c.remember(v, cp)
	switch v.Type {
	case LArray:
		// Arrays are memory references but use Cells as backing storage.
		// We preserve the shared backing array (reference semantics).
		return cp
	case LSortMap:
		// Sorted-maps store data in Native (*MapData) which contains Go
		// maps.  A shallow struct copy would alias the underlying maps,
		// causing assoc!/dissoc! on the copy to mutate the original.  One
		// copied map per source map, however many headers reach it.
		md, err := c.mapData(v.Map())
		if err != nil {
			return Errorf("copy sorted-map: %v", err)
		}
		cp.Native = md
		return cp
	case LBytes:
		if b, ok := v.Native.(*[]byte); ok && b != nil {
			cp.Native = c.byteSlice(b)
		}
	case LNative:
		if cl, ok := v.Native.(NativeCloner); ok {
			cp.Native = c.cloneNative(v.Native, cl)
		}
	default:
		// Every other type carries its payload in the struct copy above —
		// an LError's *CallStack included, shared by design (see the type
		// comment) — and its children in Cells, walked below.
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
		return cp, nil
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
		sm := m0.emptyLike()
		for k, v := range m0.m {
			sm.m[k] = c.copy(v)
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
			return nil, fmt.Errorf("failed to copy map: %w", err)
		}
		sm := emptyForStringKeys(len(pairs))
		for _, p := range pairs {
			sm.m[p.k] = c.copy(p.v)
		}
		nm.mapBacking = sm
		return nm, nil
	}
	m := &MapData{newmap()}
	for _, pair := range sortedMapEntries(md).Cells {
		if lerr := m.Set(pair.Cells[0], c.copy(pair.Cells[1])); lerr.Type == LError {
			return nil, fmt.Errorf("failed to copy map: %v", lerr)
		}
	}
	nm.mapBacking = m.mapBacking
	return nm, nil
}

// byteSlice copies a bytes payload once per source buffer.
func (c *copier) byteSlice(b *[]byte) *[]byte {
	if cp, ok := c.bytes[b]; ok {
		return cp
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
