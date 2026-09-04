// Copyright © 2026 The ELPS authors

package elpstest

import (
	"fmt"
	"hash/fnv"
	"reflect"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
)

// The sharing-encoding fingerprint.
//
// # What it is for
//
// elps has five primitives that rebuild a value graph — Fork, the `copy`
// builtin, detach, the macro-expansion stamp, and the per-fork native
// protocol — and every one of them has shipped a bug in which two names for
// ONE piece of storage came out of the rebuild as two pieces of storage
// (issues #576, #579, #585) or in which the rebuild wrote into storage it
// did not own (#582, #583).  The product guarantee underneath all of them is
// transaction isolation: an embedder loads a program once into a template
// and runs every transaction on a fresh fork, so a de-aliased payload is a
// transaction reading state that is not its own, and a write into shared
// storage is one transaction's data appearing in another's.
//
// A fingerprint that hashed only VALUES could not see any of that: the
// aliased graph and the de-aliased graph hold equal values.  So sharing is
// part of the encoding.
//
// # How sharing gets into the stream
//
// An ordinal table is keyed on PAYLOAD POINTER.  The first arrival at a
// payload emits `KIND#n{contents…}` and assigns ordinal n; every later
// arrival at the same pointer emits `KIND#n` alone.  Two headers over one
// sorted map therefore emit the same ordinal twice, while two headers over
// two equal sorted maps emit different ordinals — the two graphs are
// `equal?` and fingerprint differently, which is exactly the distinction
// #576 turned on.  The table also makes the walk cycle-safe and keeps a
// diamond-shaped graph linear: a shared subtree's contents are written once,
// under its first ordinal.
//
// The stream is a TOKEN LIST, not just a digest.  Hash() is the convenience
// digest; Diff() reports the first divergence with the path that reached it,
// because a failing guard has to tell an operator WHERE two graphs differ,
// not merely that they do.
//
// # Canonical order
//
// Every enumeration is sorted before it is walked: package names, symbol
// names within a package, an environment's bindings, a package's exports.
// Sorted-map entries are walked in the map's own key order, which is already
// deterministic.  A Go map is never ranged directly.
//
// # Substitutions where Go is opaque
//
// Two things cannot be compared by content, so they are compared by identity
// and by a stable description instead:
//
//   - A builtin's Go function pointer is not comparable, so a function is
//     identified by its FID and package name (the substitution
//     elpstest/forkcheck.go already made).
//   - A native payload is an opaque interface{}, so it renders as its Go
//     type name plus an identity ordinal.  The ordinal is the load-bearing
//     half: it says whether two headers hold ONE payload, which is
//     observable even when the payload's contents are not.
//
// # Bounds
//
// The environment walk stops at the global/root environment and emits a
// sentinel there.  Without that bound every probe would fingerprint the
// whole standard library, since a top-level function captures the root
// environment directly.  The root's own bindings are covered by the package
// channel.
//
// # What is deliberately NOT in the value channel
//
//   - LEnv.evalCtx — a per-evaluation register that a fork never inherits;
//     it differs between a template and a fork by construction.
//   - LEnv.ID — an identifier minted from a per-Runtime counter; a cold
//     environment and a fork number theirs on independent counters.
//   - The Runtime pointer — a fork lives on a fresh Runtime by contract, so
//     comparing it would report the thing Fork exists to do.
//   - LEnv.loc — the evaluator's location register.  It is not excluded
//     because it does not matter; it has its OWN channel, because a fork
//     legitimately drops it (see aliasguard_location.go) and comparing it
//     directly would report that drop as a defect.
//   - The seal bit, unless FingerprintOptions.Seal is set.  It reports
//     program-text provenance rather than value or sharing, and `copy`
//     clears it on every node of its result by contract (lisp/copy.go), so
//     including it unconditionally would report a documented behaviour as a
//     divergence.  The template-level checks DO set it: there the seal is
//     stable, so a transaction that unsealed something the template holds
//     moves the fingerprint.  (The load cache's own sealing invariant is
//     NOT re-tested here — it is enforced by the sealed-AST mechanism and
//     already covered by lisp/loadcache_*_test.go and its two fuzz targets;
//     see docs/sealed-ast.md.)
//   - A *CallStack's identity behind an LError.  It is deep-copied per
//     header rather than memoised per payload, deliberately (see the
//     shrink-only exemption list in lisp/walkers.go), so its identity is not
//     an alias channel and encoding it would report the exemption as a bug.
//     Its frame count and frame locations are still in the stream.
//   - LVal.spliced and LVal.meta.  Both are UNEXPORTED and neither has an
//     accessor, so encoding them needs new lisp API rather than a line here;
//     that is a decision for a reviewer, not a side effect of this guard.
//     `spliced` is a value property like `quoted` (which IS now encoded,
//     because IsQuoted is exported) and belongs in the stream if an accessor
//     ever lands.  `meta` is format-preserving metadata, populated only on
//     that path and shared read-only after parse per docs/fork.md, so it is
//     the weaker candidate of the two.  Recorded rather than left silent:
//     until round 9 all three were absent from BOTH the stream and this
//     list, so the list read as complete and was not.  Relevant to #466 and
//     #333/#334, and tracked in #600.

// FingerprintOptions selects the optional channels of a fingerprint.  The
// zero value is what a value-copying walker is compared under: values and
// sharing only.
type FingerprintOptions struct {
	// Seal records each node's seal bit.  Set it for template-level
	// comparisons, where the seal is stable, so a transaction that unseals
	// something the template holds moves the fingerprint; leave it clear
	// when comparing a value against a copy produced by `copy` or detach,
	// which clear the seal by contract (lisp/copy.go).
	Seal bool
	// SkipCapturedEnvironments stops the walk at a function value: its
	// identity is recorded, the environment it captured is not.
	//
	// Set it when comparing a value against a copy produced by a walker
	// that SHARES closures rather than copying them — `copy` and detach.
	// Such a copy holds the ORIGINAL closure, which still reads and writes
	// the containers its defining scope holds, so the copy's own data and
	// what its methods see legitimately come apart:
	//
	//	(set 'o (let ([s (vector 0)]) (sorted-map "bump" (lambda () (append! s 1)) "state" s)))
	//	((get (copy o) "bump"))   ; writes what the ORIGINAL closes over
	//
	// That is stated in lisp/copy.go's doc comment, warned about in
	// docs/func.md and pinned by
	// TestCopySharedClosureKeepsTheOriginalsBindings.  Walking into a
	// shared closure's captured environment would report that documented
	// behaviour as a de-aliasing defect — which is exactly what the fuzz
	// target reported on its first run, before this option existed.
	SkipCapturedEnvironments bool
	// MacroExpansion records each node's macro-expansion debug metadata:
	// whether it carries any, the qualified macro name, the per-node ID,
	// and the IDENTITY of each recorded call-site argument.
	//
	// OPT-IN, AND NOT SET BY ANY WALKER'S DEFAULT OPTIONS -- deliberately,
	// because the three walkers disagree and averaging them would make the
	// token wrong for two of the three:
	//
	//	Fork          DROPS it (lisp/fork.go, cp.macroExpansion = nil)
	//	detach        DROPS it (lisp/detach.go)
	//	(*LVal).Copy  COPIES it (the per-node struct is copied, the
	//	              context shared, and the ID rides across unchanged)
	//
	// So a token in the default fingerprint would fail property 2 on a
	// correct Fork and the copy-vs-source comparison on a correct detach.
	// The behaviours are encoded as three assertions instead --
	// TestMacroExpansionBehaviourPerWalker -- and this option is what lets
	// that test see the field at all.
	//
	// The ARGUMENT IDENTITIES are in the token, not just presence, because
	// the harm this channel carries is a pointer: a copy that kept the
	// metadata hands out the SOURCE's nodes. See macroExpansionLeaks.
	MacroExpansion bool
	// PackageMetadata records the per-package tables that live beside the
	// symbol table: exports, symbol docs and the FID→name index.  Fork
	// copies all three rather than sharing them (lisp/fork.go, issue #397),
	// and nothing compared them until this channel existed, so a regression
	// that started sharing one would have gone unnoticed.
	PackageMetadata bool
}

// Fingerprint is a canonical token stream over everything reachable from a
// value or an environment, in which sharing is part of the encoding.  See
// the file comment for the encoding and for what is deliberately excluded.
type Fingerprint struct {
	tokens []string
	// paths[i] indexes nodes; it is the path that reached tokens[i].  Paths
	// are stored as a tree rather than as joined strings so that recording
	// one per token costs an int rather than a string concatenation.
	paths []int32
	nodes []pathNode
}

type pathNode struct {
	seg    string
	parent int32
}

// Tokens returns the token stream.  The slice is the Fingerprint's own; do
// not modify it.
func (f *Fingerprint) Tokens() []string {
	if f == nil {
		return nil
	}
	return f.tokens
}

// String renders the whole stream, space separated.  It is the form to print
// when a diff is not enough.
func (f *Fingerprint) String() string {
	if f == nil {
		return ""
	}
	return strings.Join(f.tokens, " ")
}

// Hash returns a digest of the token stream, for the callers that want to
// compare or store a fixed-size value.  Equal streams hash equal; unequal
// streams almost certainly do not, but Equal is the authority.
func (f *Fingerprint) Hash() string {
	h := fnv.New64a()
	if f != nil {
		for _, t := range f.tokens {
			_, _ = h.Write([]byte(t))
			_, _ = h.Write([]byte{0})
		}
	}
	return strconv.FormatUint(h.Sum64(), 16)
}

// Equal reports whether two fingerprints are the same token stream.
func (f *Fingerprint) Equal(g *Fingerprint) bool {
	if f == nil || g == nil {
		return f == nil && g == nil
	}
	if len(f.tokens) != len(g.tokens) {
		return false
	}
	for i := range f.tokens {
		if f.tokens[i] != g.tokens[i] {
			return false
		}
	}
	return true
}

// Diff renders the first place two fingerprints diverge: the path that
// reached the divergent token, and the token each stream carries there.  A
// guard that only said "these differ" would leave the operator to find the
// binding themselves, which on a fully loaded environment is thousands of
// tokens of searching.
func (f *Fingerprint) Diff(g *Fingerprint) string {
	if f.Equal(g) {
		return "  (identical)"
	}
	if f == nil || g == nil {
		return "  one side is absent"
	}
	n := len(f.tokens)
	if len(g.tokens) < n {
		n = len(g.tokens)
	}
	for i := range n {
		if f.tokens[i] != g.tokens[i] {
			return fmt.Sprintf("  at %s (token %d of %d/%d)\n    want: %s\n    got:  %s",
				f.pathOf(i), i, len(f.tokens), len(g.tokens),
				clip(f.tokens[i]), clip(g.tokens[i]))
		}
	}
	long, short, which := f, g, "want"
	if len(g.tokens) > len(f.tokens) {
		long, short, which = g, f, "got"
	}
	return fmt.Sprintf("  streams agree for %d tokens; %s continues at %s with %s",
		len(short.tokens), which, long.pathOf(n), clip(long.tokens[n]))
}

func (f *Fingerprint) pathOf(i int) string {
	if i >= len(f.paths) {
		return "<end>"
	}
	var segs []string
	for n := f.paths[i]; n >= 0; n = f.nodes[n].parent {
		segs = append(segs, f.nodes[n].seg)
	}
	for l, r := 0, len(segs)-1; l < r; l, r = l+1, r-1 {
		segs[l], segs[r] = segs[r], segs[l]
	}
	return strings.Join(segs, "/")
}

// FingerprintValue fingerprints everything reachable from v, including the
// environments any closure under v captured (bounded at the global
// environment).
func FingerprintValue(v *lisp.LVal, opts FingerprintOptions) *Fingerprint {
	w := newFingerprinter(opts)
	w.push("value")
	w.value(v)
	return w.done()
}

// FingerprintEnv fingerprints everything reachable from env's package
// registry — every package binding, and through a closure the environments
// it captured — plus env's own lexical chain up to the global boundary.  It
// is the template-level fingerprint: what an embedder's transactions can
// see and change.
func FingerprintEnv(env *lisp.LEnv, opts FingerprintOptions) *Fingerprint {
	w := newFingerprinter(opts)
	roots(env, func(pkg, name string, v *lisp.LVal) {
		w.push(pkg + ":" + name)
		w.value(v)
		w.pop()
	})
	if opts.PackageMetadata {
		w.packageMetadata(env)
	}
	w.push("<env>")
	w.env(env)
	w.pop()
	return w.done()
}

type fingerprinter struct {
	opts  FingerprintOptions
	out   []string
	paths []int32
	nodes []pathNode
	cur   int32
	// ord numbers a payload on its first arrival.  Keyed on the payload
	// pointer, which is what "same object" means; see the file comment.
	ord  map[any]int
	seen map[any]bool
}

func newFingerprinter(opts FingerprintOptions) *fingerprinter {
	return &fingerprinter{
		opts: opts,
		cur:  -1,
		ord:  make(map[any]int, 512),
		seen: make(map[any]bool, 512),
	}
}

func (w *fingerprinter) done() *Fingerprint {
	return &Fingerprint{tokens: w.out, paths: w.paths, nodes: w.nodes}
}

func (w *fingerprinter) push(seg string) {
	w.nodes = append(w.nodes, pathNode{seg: seg, parent: w.cur})
	// The path tree has one node per push, bounded by the walk, which is
	// bounded by the reachable graph; int32 is not a real ceiling here.
	w.cur = int32(len(w.nodes) - 1) //nolint:gosec // see above
}

func (w *fingerprinter) pop() {
	if w.cur >= 0 {
		w.cur = w.nodes[w.cur].parent
	}
}

func (w *fingerprinter) emit(tok string) {
	w.out = append(w.out, tok)
	w.paths = append(w.paths, w.cur)
}

func (w *fingerprinter) emitf(format string, args ...any) {
	w.emit(fmt.Sprintf(format, args...))
}

// id numbers a payload identity on first sight and reports whether this is
// that first sight.
func (w *fingerprinter) id(key any) (int, bool) {
	if n, ok := w.ord[key]; ok {
		return n, false
	}
	n := len(w.ord)
	w.ord[key] = n
	return n, true
}

// funIDPattern matches the environment-derived part of a lambda's name.  A
// FID is "_fun<envID>", and environment IDs are minted from a per-Runtime
// counter, so a cold environment and a fork number theirs independently.
// Function identity within one walk is carried by the header ordinal and by
// the captured environment, so normalising the number costs the stream
// nothing and lets a cold arm be compared against a fork.
var funIDPattern = regexp.MustCompile(`_fun\d+`)

func normalizeFunIDs(s string) string {
	return funIDPattern.ReplaceAllString(s, "_fun#")
}

// isPointerPayload reports whether a native payload is held by pointer, and
// therefore has an identity worth recording.  A payload held by value (an
// int, a struct) has none: two copies of it are the same payload in every
// sense the language can observe, and the kernel's own memos skip it for
// the same reason (forker.native, detacher.cloneNative).
// isCellViewLink reports whether v's Native is a KERNEL-INTERNAL cell-view
// link rather than an embedder payload.
//
// A cell view (cdr, rest, slice, `(append 'vector seq)` with no values) is a
// header whose Cells is a window into another value's backing array, and it
// records that relationship by holding the root *lisp.LVal in Native and the
// element offset in Int. Both fields were unused on an LSExpr before, which
// is what made them available.
//
// The distinction matters to every surface that asks "does this header carry
// a payload":
//
//   - the census would report a pointer payload on EVERY view. Within one
//     environment that pointer is the intended root; across forks the root
//     differs per fork, so it is noise -- and a fork that FAILED to re-point
//     would be reported as a shared native rather than as a de-aliased cell,
//     which is the wrong witness for the wrong bug;
//   - the fingerprint would emit an identity ordinal for every view, changing
//     the fingerprint of every program containing one;
//   - a probe-site walk that treated it as an opaque payload would STOP at
//     the view and never walk its Cells.
//
// So a link is a REFERENCE, never a payload. The root is reachable state and
// wants walking as such; the link itself gets no ordinal.
//
// This was written on #599 as a local check over the exported fields, with
// the two swaps below specified for the commit on #602 that would sit on
// top. Both have now been made, on #602's tree, where the API exists:
//
// SWAP 1 -- this function's body is `return v.IsCellView()`. #602 exports
// IsCellView as the cheap, UNVALIDATED predicate ("does this header carry a
// link") for exactly this use: the type-switch arms keep a view's Native
// out of the payload arm whether the link is current or stale, and do not
// pay for validation to decide that. Every control in
// aliasguard_payloadkey_test.go is unchanged across the swap: it asked the
// same question of the same fields.
//
// SWAP 2 -- the reference walk. A view's root is reachable state and the
// guard walks it as such, through the VALIDATED resolver
// (`root, off, ok := v.CellView()`): walkReachable (aliasguard_isolation.go)
// follows a live root as an ordinary reachable value under path ".../root",
// with no identity ordinal, and keeps walking the view's own Cells; a stale
// link (ok false) is walked as ordinary structure, its root not followed,
// no witness -- Fork copies such a header privately by the same call, so the
// guard and Fork agree by construction. Slot identity is NOT re-derived
// anywhere in this package: CellView is the one rule (the convention on
// lisp.cellsView).
//
// THE PROPERTY, asserted by cellViewWitnesses (aliasguard_cellview.go) on
// every fresh fork and on the pristine successor:
//
//	a fork's view shares its slots with the fork's own root exactly as
//	the template's view shares them with the template's root
//
// i.e. for every binding that is a live view in the template, the fork's
// binding at the same path is a live view whose root is a fork-side value
// (not a template value), and vice versa. Both halves are needed: the
// first alone passes on a fork that re-pointed a view at the TEMPLATE's
// root, the second alone on a fork whose views were all rebuilt privately.
// The failure reads "a fork's view no longer shares slots with its own
// root", the de-aliasing #600 gap 3 measured from pure ELPS (`(set 'tail
// (cdr l))` then `(stable-sort < l)` diverging between template and fork).
// With that, the Cells row of the walker-contract table (aliasguard.go,
// BackingRebuilt) is an asserted contract for Fork; it remains an exception
// for copy and detach, which do not preserve slot aliasing.
func isCellViewLink(v *lisp.LVal) bool {
	return v.IsCellView()
}

// annotation encodes a pointer payload carried on a header whose TYPE arm
// does not already encode one.
//
// The gap this closes: the fingerprint, the probe walk and the cross-fork
// census all keyed "has a payload" on `v.Type == LNative`. But Native is
// shared storage -- LBytes holds a *[]byte there, LSortMap a *MapData, LFun a
// *funData -- and an EMBEDDER can put a payload on an ordinary node too. A
// Reader that annotates an LSExpr is the measured case (#603,
// TestLoadCacheTopology_NativeAnnotationIsReported, which pinned the gap as
// open until this closed it): the LoadCache refuses to cache such a parse,
// but Fork still shares the annotation with every fork by reference, because
// a SEALED node is shared outright before the native policy runs -- so its
// NativeCloner is never consulted, and nothing here saw it.
//
// The types whose arms own Native are excluded because those arms already
// encode its identity (map#, bytes#, native#) and doing it twice would double
// every ordinal. LFun and LError are excluded for a different reason, stated
// rather than assumed: their payloads are the function and the captured
// stack, which the closure walk and errorValue already govern, and giving
// them identity ordinals here would make every fingerprint of a program
// containing a function depend on funData identity -- which a fork
// legitimately changes.
func (w *fingerprinter) annotation(v *lisp.LVal) {
	switch v.Type {
	case lisp.LSortMap, lisp.LBytes, lisp.LNative, lisp.LFun, lisp.LError:
		// These arms own this header's Native; see the doc above.
		return
	default:
		// Every other type can carry an embedder annotation.
	}
	if !isPointerPayload(v.Native) || isCellViewLink(v) {
		return
	}
	n, first := w.id(v.Native)
	if !first {
		w.emitf("annot#%d", n)
		return
	}
	w.emitf("annot#%d(%T)", n, v.Native)
}

// kernelOwnedPayload reports whether a header's Native belongs to the
// KERNEL rather than to an embedder, and is therefore governed by something
// other than the native policy.
//
// The cross-fork census asks "is this payload shared between two forks".
// That question is only meaningful for payloads whose per-fork privacy the
// native policy is responsible for. Three kinds are not:
//
//   - LFun's *funData IS the function. Every builtin in every package is
//     one, and packages are SHARED between forks by design, so counting them
//     turns a census of embedder payloads into a census of the standard
//     library -- measured at 142 entries for a graph with three real
//     payloads in it. The part a fork must copy is the closure ENVIRONMENT,
//     which the LFun arm walks separately.
//   - LError's *CallStack is a captured stack. Its identity carries no
//     observable state -- CallStack.Copy allocates an exact-length Frames
//     slice at every capture site and the only mutators run on the live
//     evaluator stack -- which is the same reason walkers.go exempts it from
//     the memo registry, measured there by
//     TestCopyAliasesCallStackAcrossHeaders.
//   - a cell-view link is a reference to a root, not a payload at all.
//
// Note what is NOT here: LBytes's *[]byte and LSortMap's *MapData. Those are
// embedder-visible mutable storage whose per-fork privacy is exactly the
// native policy's job, and keying the census on `v.Type == LNative` was
// hiding both.
func kernelOwnedPayload(v *lisp.LVal) bool {
	switch v.Type {
	case lisp.LFun, lisp.LError:
		return true
	default:
		// Fall through to the link check.
	}
	return isCellViewLink(v)
}

func isPointerPayload(payload any) bool {
	if payload == nil {
		return false
	}
	rv := reflect.ValueOf(payload)
	return rv.Kind() == reflect.Pointer && !rv.IsNil()
}

func (w *fingerprinter) value(v *lisp.LVal) {
	if v == nil {
		w.emit("nil-lval")
		return
	}
	n, first := w.id(v)
	if !first {
		w.emitf("hdr#%d", n)
		return
	}
	w.emitf("hdr#%d(%s)", n, v.Type)
	if w.opts.Seal && v.IsSealed() {
		w.emit("sealed")
	}
	// The quote flag is a VALUE property, not a sharing one, and a walker
	// that dropped it would produce a value lisp can tell apart from its
	// source.  It was unencoded until PR #599's round 9: '(1) and (1)
	// fingerprinted identically, so the whole class was invisible.  Cheap to
	// encode because IsQuoted is exported; `spliced` and `meta` are not, and
	// are excluded with reasons in the block above.
	if v.IsQuoted() {
		w.emit("quoted")
	}
	if loc, ok := v.Source(); ok {
		// The location's VALUES, not its pointer: detach copies a location
		// and Fork shares it, and both are correct.  A macro stamp that
		// wrote a call site into a binding (issue #582) changes the values,
		// which is what this records.
		w.emitf("at(%s:%d:%d)", loc.File, loc.Line, loc.Col)
	} else {
		w.emit("at(-)")
	}
	w.annotation(v)
	if w.opts.MacroExpansion {
		if m, ok := v.MacroExpansion(); ok {
			w.emitf("mexp(%s,%d)", m.Name, m.ID)
			for i, a := range m.Args {
				n, _ := w.id(a)
				w.emitf("mexparg%d#%d", i, n)
			}
		} else {
			w.emit("mexp(-)")
		}
	}
	switch v.Type {
	case lisp.LSortMap:
		w.sortedMap(v)
	case lisp.LBytes:
		w.bytes(v)
	case lisp.LNative:
		w.native(v.Native)
	case lisp.LFun:
		w.fun(v)
	case lisp.LError:
		w.errorValue(v)
	case lisp.LInt:
		w.emitf("int(%d)", v.Int)
	case lisp.LFloat:
		w.emitf("float(%v)", v.Float)
	case lisp.LString, lisp.LSymbol, lisp.LQSymbol, lisp.LTaggedVal:
		w.emitf("str(%q)", v.Str)
		w.cells(v)
	default:
		if v.Str != "" {
			w.emitf("str(%q)", v.Str)
		}
		w.cells(v)
	}
}

func (w *fingerprinter) cells(v *lisp.LVal) {
	if len(v.Cells) == 0 {
		w.emit("cells0")
		return
	}
	w.emitf("cells%d[", len(v.Cells))
	for i, c := range v.Cells {
		w.push(strconv.Itoa(i))
		w.value(c)
		w.pop()
	}
	w.emit("]")
}

func (w *fingerprinter) sortedMap(v *lisp.LVal) {
	md := v.Map()
	if md == nil {
		w.emit("map(nil)")
		return
	}
	n, first := w.id(md)
	if !first {
		w.emitf("map#%d", n)
		return
	}
	w.emitf("map#%d{", n)
	keys := md.Keys()
	for _, k := range keys.Cells {
		w.emitf("key(%s)", k.String())
		val, _ := md.Get(k)
		w.push(k.String())
		w.value(val)
		w.pop()
	}
	w.emit("}")
}

func (w *fingerprinter) bytes(v *lisp.LVal) {
	p, ok := v.Native.(*[]byte)
	if !ok || p == nil {
		w.emit("bytes(nil)")
		return
	}
	n, first := w.id(p)
	if !first {
		w.emitf("bytes#%d", n)
		return
	}
	w.emitf("bytes#%d(%q)", n, *p)
}

// native renders an opaque payload: its Go type, plus an identity ordinal
// when it is held by pointer.  The ordinal is what makes payload sharing
// observable through a value the walker cannot look inside.
func (w *fingerprinter) native(payload any) {
	if payload == nil {
		w.emit("native(nil)")
		return
	}
	if !isPointerPayload(payload) {
		w.emitf("native(%T,by-value)", payload)
		return
	}
	n, first := w.id(payload)
	if !first {
		w.emitf("native#%d", n)
		return
	}
	w.emitf("native#%d(%T)", n, payload)
}

func (w *fingerprinter) fun(v *lisp.LVal) {
	// FID and package name rather than the Go function pointer, which is
	// not comparable.  The FID's environment number is normalised so a cold
	// environment can be compared against a fork.
	w.emitf("fun(fid=%s,pkg=%s,builtin=%t,type=%d)",
		normalizeFunIDs(v.FID()), v.Package(), v.Builtin() != nil, v.FunType)
	if w.opts.SkipCapturedEnvironments {
		w.emit("env(skipped)")
	} else {
		w.push("env")
		w.env(funraw.Env(v))
		w.pop()
	}
	w.cells(v)
}

func (w *fingerprinter) errorValue(v *lisp.LVal) {
	w.emitf("str(%q)", v.Str)
	if st := v.CallStack(); st != nil {
		// Structure, not identity: see the *CallStack note in the file
		// comment.
		w.emitf("stack(%d){", len(st.Frames))
		for _, fr := range st.Frames {
			if fr.Source != nil {
				w.emitf("frame(%s,%s:%d:%d)", fr.Name, fr.Source.File, fr.Source.Line, fr.Source.Col)
			} else {
				w.emitf("frame(%s,-)", fr.Name)
			}
		}
		w.emit("}")
	} else {
		w.emit("stack(-)")
	}
	w.cells(v)
}

// env fingerprints an environment's own bindings and its parents', stopping
// at the global/root environment.  An environment reached twice renders as
// its ordinal alone, so two closures over one environment are visibly two
// closures over one environment.
func (w *fingerprinter) env(e *lisp.LEnv) {
	if e == nil {
		w.emit("env(nil)")
		return
	}
	n, first := w.id(e)
	if !first {
		w.emitf("env#%d", n)
		return
	}
	if e.Parent() == nil {
		// The global boundary.  Descending would fingerprint the whole
		// standard library on every probe, and the root's bindings are
		// already covered by the package channel.
		w.emitf("env#%d(global)", n)
		return
	}
	w.emitf("env#%d{", n)
	keys, vals := sortedBindings(e)
	for _, k := range keys {
		w.emitf("bind(%s)", k)
		w.push(k)
		w.value(vals[k])
		w.pop()
	}
	w.emit("}")
	w.push("parent")
	w.env(e.Parent())
	w.pop()
}

// packageMetadata fingerprints the three per-package tables that live beside
// the symbol table.  Fork COPIES all three rather than sharing them, because
// the template remains a live writer of each and a shared map is the issue
// #397 fatal-throw hazard rather than a mere stale read.  Nothing compared
// them before this channel: a regression that started sharing one would have
// passed every check the harness had, and would have shown up in production
// as one transaction's `defun` renaming a function in another's stack trace.
func (w *fingerprinter) packageMetadata(env *lisp.LEnv) {
	reg := env.Runtime.Registry
	names := reg.PackageNames()
	sort.Strings(names)
	w.push("<pkg-metadata>")
	for _, pn := range names {
		pkg := reg.Package(pn)
		if pkg == nil {
			continue
		}
		w.push(pn)
		w.emitf("pkg(%s,doc=%q)", pkg.Name, pkg.Doc)
		ext := append([]string(nil), pkg.Externals()...)
		sort.Strings(ext)
		w.emitf("externals%d[%s]", len(ext), strings.Join(ext, " "))
		syms := pkg.SymbolNames()
		sort.Strings(syms)
		for _, sn := range syms {
			if doc := pkg.SymbolDoc(sn); doc != "" {
				w.emitf("symdoc(%s,%q)", sn, doc)
			}
			v, ok := pkg.Symbol(sn)
			if !ok || v == nil || v.Type != lisp.LFun {
				continue
			}
			// funNames is only readable one FID at a time, so it is
			// probed through the functions actually bound here.  That is
			// the whole observable surface of the table.
			w.emitf("funname(%s,%s)", normalizeFunIDs(v.FID()), pkg.GetFunName(v.FID()))
		}
		w.pop()
	}
	w.pop()
}
