// Copyright © 2018 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
	"io"
)

type Loader func(*LEnv) *LVal

// Reader abstracts a parser implementation so that it may be implemented in a
// separate package as an optional/swappable component.
type Reader interface {
	// Read the contents of r and return the sequence of LVals that it
	// contains.  The returned LVals should be executed as if inside a progn.
	Read(name string, r io.Reader) ([]*LVal, error)
}

// LocationReader is like Reader but assigns physical locations to the tokens
// from r.
type LocationReader interface {
	// ReadLocation the contents of r, associated with physical location loc,
	// and return the sequence of LVals that it contains.  The returned LVals
	// should be executed as if inside a progn.
	ReadLocation(name string, loc string, r io.Reader) ([]*LVal, error)
}

// LoaderMust returns its first argument when err is nil.  If err is nil
// LoaderMust panics.
//
// NOT LISP-REACHABLE (#367): this is the Go `Must` idiom, for an embedder
// building a Loader from a source it controls (a //go:embed constant, a
// literal) at start-up.  Nothing in the interpreter calls it, so no evaluated
// program can reach it; an embedder wrapping a source a program supplies
// should handle the error instead.
func LoaderMust(fn Loader, err error) Loader {
	if err != nil {
		panic(err)
	}
	return fn
}

// TextLoader parses a text stream using r and returns a Loader which evaluates
// the stream's expressions when called.  The reader will be invoked only once.
//
// TextLoader returns an error if r produces any reference type (bytes, map,
// array, native, etc), a nil node, or output that is not finite: a cycle, or
// nesting past loaderWalkMaxDepth.  It does NOT reject node SHARING, imposes
// no node budget, and — unlike the Program constructors and the cache —
// tolerates a Native payload on a sealable type (see newTextLoaderWalk).  A
// Reader that interns symbols or subexpressions, one that returns a single
// very large expression, and one that annotates nodes through Native all
// load exactly as they always have.  Those rules exist for trees that are
// ALIASED between environments; every TextLoader load gets expr.Copy(), so
// none of them is a TextLoader concern.
func TextLoader(r Reader, name string, stream io.Reader) (Loader, error) {
	exprs, err := r.Read(name, stream)
	if err != nil {
		return nil, err
	}
	// One walk state for the whole stream, not one per expression: it costs a
	// single map for the file instead of len(exprs) of them, and it lets the
	// cycle guard see a cycle that closes across two top-level expressions.
	w := newTextLoaderWalk()
	for _, expr := range exprs {
		err := admitExpr(expr, w)
		if err != nil {
			lerr := Error(err)
			// Copied, not aliased: the error escapes to the embedder through
			// GoError while expr stays part of the loaded program, so the
			// two must not share a *token.Location (cold path; the copy is
			// free in practice).  expr itself may be the nil the walk just
			// refused, so the location is read only when there is one.
			if expr != nil {
				lerr.source = copyLocation(expr.source)
			}
			return nil, GoError(lerr)
		}
	}

	// THE PER-LOAD COPY STAYS, and the sealing work is why it is worth
	// saying so rather than leaving the Copy() unremarked.
	//
	// The seal makes sharing this parse SAFE -- sealed nodes are frozen
	// storage under the seal's write protection, and the ownership checker
	// exempts them for exactly that reason -- and elpstest.RunBenchmark
	// takes that share, because its consumer is in this repository and its
	// contract is about measurement rather than about what a caller may do
	// with the tree.
	//
	// TextLoader is different on both counts.  It is PUBLIC API whose
	// documented behaviour is that every load gets its own tree: issue #446
	// is specifically about that guarantee reaching positions as well as
	// cells, and TestTextLoaderEvaluationsGetPrivatePositions pins it.
	// Taking the share would move an embedder that mutates what a Loader
	// handed it -- legal, if unwise, under the current contract -- from
	// ownership to the seal's refusal.  And it would buy nothing today: TextLoader
	// has no callers in this repository, and the downstream sweep for issue
	// #379 found that embedders reach elps through the Reader path, which
	// never had this copy.  A public contract should not change for a
	// speculative consumer, so the optimization is left on the table with
	// its measurements recorded (#379 item 4: -72.3% sec/op, -61.9%
	// allocs/op on a 50 KB source) for whoever has a caller to justify it.
	fn := func(env *LEnv) *LVal {
		var lval *LVal
		for _, expr := range exprs {
			lval = env.Eval(expr.Copy())
			if lval.Type == LError {
				return lval
			}
		}
		if lval == nil {
			return Nil()
		}
		return lval
	}

	return fn, nil
}

// errReaderTreeUnbounded marks reader output that is not a finite tree: a
// cycle, nesting past the admission depth cap, or — on the cache path only —
// sharing whose UNFOLDED size is past loaderWalkUnfoldedCap.  A real parse is
// well within all three — rdparser caps parse depth at 10,000 — so output
// that trips them did not come from a parser and is refused.
//
// It is a distinct sentinel because, unlike an ordinary admission refusal (a
// reference type, or a node no seal can cover), such output is not merely
// un-cacheable: it is unsafe to hand to the evaluator too.  A cycle is
// stopped only by the eval nesting cap, after doing the work; and sharing
// that unfolds to 4.3e9 node EVALUATIONS does not terminate at all.
// (*LEnv).readCached fails the load on this sentinel instead of falling back
// to an uncached eval.
//
// "EVALUATIONS" is load-bearing and used to be wrong.  The count taken
// against this sentinel is loaderWalk.evalUnfolded, which charges one node
// for a subtree the evaluator never walks -- a quoted datum.  Counting the
// unfolded size of quoted DATA here hard-failed a load that terminates in
// O(1) with no cache installed (issue #536 round-four review, blocker); see
// loaderQuotePayload and loaderWalk.verdict.
//
// ORDINARY SHARING IS NOT IN THIS CLASS.  A composite node reached twice used
// to be, which broke a program a constant-interning Reader had loaded fine
// for as long as there was no cache; see loaderWalk.verdict for the rewrite
// and the red proof.
var errReaderTreeUnbounded = errors.New("reader output is not a finite tree (cycle, unbounded shared subtree, or too deep to admit)")

// errReaderTreeTooLarge marks reader output that is finite and legal but
// larger than the cache admission's node budget — in distinct nodes, in
// UNFOLDED size (loaderWalk.raw, which counts quoted data, because the walks
// that unfold — Copy, firstUnsealed, SealedASTFingerprint — do not care
// whether a subtree is quoted), or both.
//
// It is deliberately NOT errReaderTreeUnbounded, and the difference is the
// whole point: a cycle is unsafe to evaluate, but a node COUNT is not — a
// single enormous top-level expression, or a heavily interned large source,
// is an ordinary program.  Refusing to cache it must therefore behave exactly
// as it does with no cache installed, so (*LEnv).readCached falls back to an
// uncached load on this sentinel rather than failing the load (see
// readCached's fall-back list).  Nothing outside the cache path imposes the
// budget at all.
var errReaderTreeTooLarge = errors.New("reader output exceeds the cache admission node budget")

// errReaderNilNode marks reader output containing a nil *LVal — a root, or a
// cell inside an otherwise well-formed s-expression.
//
// Before the load-cache hook the admission walk dereferenced v.Type
// unconditionally, so a nil node panicked loudly at admission, immediately,
// on the goroutine that produced it.  A nil guard added at the head of the
// walk turned that into silence: the nil was admitted, firstUnsealed(nil)
// answered "sealed" (it returns nil, which its caller reads as "nothing
// unsealed here"), and the fast path put a tree containing a nil node into a
// PROCESS-WIDE cache, where every later load laundered it into a catchable
// internal-panic (issue #536 round-three review, suspicious 2).
//
// It is an ORDINARY refusal, not one of the two sentinels above: on the
// cache path it means "not cacheable", so the load falls back to an uncached
// parse and the nil reaches the evaluator exactly as it does with no cache
// installed.  That keeps the nil-cache path byte-identical while making sure
// nothing containing a nil is ever stored.
var errReaderNilNode = errors.New("reader output contains a nil expression")

const (
	// loaderWalkMaxNodes is the CACHE admission's node budget.  It bounds the
	// UNFOLDED size of an entry — the number of node visits a memo-less walk
	// makes, counting a shared subtree once per path — and, separately, the
	// number of DISTINCT nodes in it.  The unfolded number it is taken
	// against is loaderWalk.unfolded, the QUOTE-BLIND one, because what this
	// budget protects is the three walks downstream of admission that really
	// do unfold sharing: (*LVal).Copy on the private-copy path,
	// firstUnsealed, and SealedASTFingerprint.  None of them stops at a
	// quote.  It mirrors sealFPMaxNodes (lisp/sealfp.go):
	// orders of magnitude beyond any top-level expression a parser emits,
	// present only to bound hand-built or adversarial reader output.
	// Exceeding it is errReaderTreeTooLarge, which is not a failure — the
	// load proceeds uncached.  It applies to the cache path alone;
	// ReadProgram, ParseProgram and TextLoader have no node budget, so a
	// legal 1.2M-node expression still loads through them.
	loaderWalkMaxNodes = sealFPMaxNodes
	// loaderWalkHardMaxNodes stops the cache admission walk outright.  The
	// walk does NOT abandon at loaderWalkMaxNodes, because a budget overflow
	// in one top-level expression must not hide a cycle in the next one
	// (issue #536 round-three review, minor 1); it memoises, so continuing
	// costs O(distinct nodes) and nothing worse.  This is the point past
	// which even that is more than any reader output deserves.
	loaderWalkHardMaxNodes = 4 * loaderWalkMaxNodes
	// loaderWalkUnfoldedCap is where unfolded-size arithmetic saturates, and
	// also the line between "too big to cache" and "not a finite tree at
	// all".  4.3e9 node EVALUATIONS is not a program that finishes: no
	// unshared parse can reach it (the source would not fit in memory), and
	// a shared one that does is a sharing bomb whose evaluation is
	// exponential in the sharing depth.  4.3e9 nodes of shared DATA is a
	// different thing entirely and does not come near this line; see
	// loaderQuotePayload and loaderWalk.verdict.
	loaderWalkUnfoldedCap = int64(1) << 32
	// loaderWalkMaxDepth bounds admission recursion depth so a very deep tree
	// cannot exhaust the Go stack before anything else notices.  It mirrors
	// sealFPMaxDepth; parse depth caps at 10,000 (rdparser), leaving wide
	// headroom.  Unlike the node budget this DOES apply on every path,
	// because the alternative is not a refusal but an unrecoverable Go stack
	// overflow.
	loaderWalkMaxDepth = sealFPMaxDepth
)

// admitExpr reports whether v is admissible reader output, under the rules
// w carries.  It is the single admission walk: TextLoader, ReadProgram,
// ReadLocationProgram and ParseProgram pass a walk from newLoaderWalk(false),
// and Runtime.LoadCache passes newLoaderWalk(true), which adds a node budget.
//
// What EVERY path rejects, and why each rule is safe to apply to public API:
//
//   - A NIL NODE, root or cell.  It cannot be sealed, cannot be
//     fingerprinted, and firstUnsealed answers "sealed" for it, so admitting
//     one puts a tree that panics on evaluation into a process-wide cache.
//   - REFERENCE TYPES (bytes, map, array, native), whose mutable backing
//     every copy of the tree would share.  This is TextLoader's historical
//     rule, unchanged.
//   - A CYCLE: a node reached from itself.  Detected with an on-path set, so
//     ordinary node sharing is not mistaken for one.
//   - Nesting past loaderWalkMaxDepth.
//
// What NO path rejects, because a rule that is right for an aliased
// process-wide entry is not automatically right for public API (issue #368
// review, blockers 2 and 3):
//
//   - NODE SHARING.  A Reader that interns symbols — or constants, or whole
//     subexpressions — returns a DAG, which is an ordinary memory
//     optimization.  See verdict for what happens when the sharing is
//     pathological rather than ordinary.
//   - SIZE, off the cache path.  There is no node budget there, so one very
//     large legal expression is admitted.
//
// The cache path adds ONE rule, the node budget, because its admitted entry
// is aliased into unboundedly many environments instead of being handed to
// one load.  Exceeding it is errReaderTreeTooLarge, and readCached treats
// that as "do not cache this one" — the load proceeds uncached, exactly as
// it would with no cache installed.
//
// One further rule is the cache path's alone and is NOT about caching:
// a Native payload on a type SealAST marks is refused for the Program
// constructors and the cache, but tolerated by TextLoader.  See
// newLoaderWalk's allowNative.
//
// Because newProgram runs this pass FIRST, the walks after it (firstUnsealed
// and (*LVal).Copy) see output already known to be non-nil, acyclic and
// depth-bounded — and, on the cache path, of bounded unfolded size, which is
// what bounds those two walks there.
func admitExpr(v *LVal, w *loaderWalk) error {
	info, err := w.check(v, 0)
	if err != nil {
		return err
	}
	if w.strict {
		w.unfolded = saturatingAddNodes(w.unfolded, info.raw)
		w.evalUnfolded = saturatingAddNodes(w.evalUnfolded, info.eval)
	}
	return nil
}

// newLoaderWalk builds the admission walk state.  strict selects the cache
// path's extra rules (the node budget, and the Native-payload refusal); one
// walk is shared across all the top-level expressions of one stream so the
// cycle guard spans them, the budget is a property of the FILE rather than of
// each expression, and the state costs one map per stream rather than one per
// expression.
//
// THE NON-STRICT WALK ALLOCATES NOTHING for an ordinary parse, and that is a
// requirement rather than a nicety.  docs/embed.md promises that a nil
// LoadCache leaves the load path exactly what it was before the hook
// existed, and ReadProgram/ParseProgram/TextLoader are on that path: they
// are the Program half of the same admission and they run with no cache
// installed and no cache benefit.  A memo sized to the whole stream's node
// count made them 21% slower and 27% heavier per parse (issue #536
// round-three review, blocker 1), buying only the sealed-DAG case that no
// reader in this repository produces — and buying it one step short of
// (*LVal).Copy, which unfolds a DAG regardless.  So the O(nodes) state
// belongs to the cache path, which needs it to answer a different question
// (how big is this program really), and cycle detection off that path is
// carried by the on-path set alone.
func newLoaderWalk(strict bool) *loaderWalk {
	w := &loaderWalk{}
	if strict {
		w.strict = true
		w.sizes = make(map[*LVal]loaderNodeInfo)
	}
	return w
}

// newTextLoaderWalk is the non-strict walk with the Native-payload rule
// switched off — TextLoader's walk, and only TextLoader's.
//
// The rule (issue #368 review, finding 9) refuses a Native payload riding on
// a type SealAST marks, because nothing downstream covers it: the seal
// freezes LVal fields only, the fingerprint oracle skips Native by design,
// and the admission conjunction and ownership exemption both key off the
// TYPE, which is sealable.  That reasoning is about a tree ALIASED between
// environments, which is what a Program and a cache entry are — so it holds
// for ReadProgram, ReadLocationProgram, ParseProgram and Runtime.LoadCache.
//
// TextLoader is not that.  Every load it serves gets expr.Copy(), so no two
// loads share a node; Copy shallow-copying Native was already true before
// this hook, so nothing about the sharing is NEW.  Refusing there buys
// nothing a documented caveat would not, and it costs something real: in the
// LVal struct, source, meta and macroExpansion are all unexported, so Native
// is the ONLY exported per-node slot an embedder's Reader has for
// annotation.  elps's own parsers do not need it because they have the
// unexported meta; an embedder Reader annotating its nodes has exactly one
// place to go, and turning that into a hard error on a public constructor is
// a migration break for no safety (issue #536 round-three review,
// suspicious 3).
func newTextLoaderWalk() *loaderWalk {
	w := newLoaderWalk(false)
	w.allowNative = true
	return w
}

// loaderNodeInfo is the strict walk's memo entry for one node: everything a
// later visit to the same node needs in order to answer without walking it
// again.  Only the cache path builds these (loaderWalk.sizes).
type loaderNodeInfo struct {
	// raw is the node's UNFOLDED size, quote-blind: the number of node visits
	// a memo-less walk of it would make, counting a shared subtree once per
	// path and descending into quoted data like everything else.  It is what
	// the TooLarge budget is taken against, because it is what the three
	// downstream walks that really do unfold sharing cost — (*LVal).Copy on
	// the private-copy path, firstUnsealed, and SealedASTFingerprint.
	raw int64
	// eval is the same count with every subtree the EVALUATOR never walks
	// charged as a single node.  It is what the Unbounded hard-fail is taken
	// against, because that sentinel's claim is "this does not terminate",
	// and only work the evaluator actually does can support it.  See
	// loaderQuotePayload.
	eval int64
	// onPath marks the node as being on the current root->node path.  A
	// visit that reaches such an entry has found a cycle.
	onPath bool
}

// loaderQuotePayload returns the index of the cell of v that the EVALUATOR
// will never walk into, or -1 when there is no such cell.
//
// This is the round-four blocker (issue #536 round-four review).  The
// unfolded-size rule that hard-fails a load rests on "nothing that
// terminates looks like 4.3e9 node evaluations", and for CODE that is true.
// For DATA it is false, and not marginally: (*LEnv).eval returns a quoted
// node without touching its interior, and opQuote hands its argument back
// unwalked, so a quoted 41-node DAG whose unfolded size is 2^41 evaluates in
// O(1).  With no cache installed such a program loads and runs instantly;
// counting its unfolded size against the Unbounded cap hard-failed it the
// moment a cache was installed, which is the exact regression class round
// three's sharing rewrite existed to remove.
//
// Two shapes are recognised, and between them they cover both ways quoting
// reaches the walk:
//
//   - v.quoted, handled by check directly rather than here.  This is what
//     the PARSER produces: rdparser's ParseQuote calls lisp.Quote, which
//     sets the flag on a shallow copy (or builds an LQuote node, which is
//     always quoted).  (*LEnv).eval's first act on such a node is to return
//     it.
//   - An explicit (quote X) s-expression, which is what a hand-built Reader
//     and a macro body produce.  Cell 1 is the datum; opQuote never looks
//     inside it.
//
// QUASIQUOTE IS DELIBERATELY NOT IN THAT SET, and loaderQuasiquotePayload
// says what happens instead.
//
// Getting this wrong in the permissive direction is safe, and that is worth
// stating because the recognition is heuristic — `quote` is a symbol, and a
// program is free to shadow it.  Under-counting eval work cannot make elps
// do unbounded work anywhere: the walks that unfold are bounded by raw, not
// by eval (see loaderNodeInfo), so an under-counted entry is still refused
// as TooLarge and never stored, and TooLarge falls back to an uncached load.
// All an under-count can cost is the fail-fast — the load then behaves
// exactly as it does with no cache installed, which for a genuinely
// non-terminating program means the evaluator's own step, stack and
// deadline budgets stop it instead of admission.
func loaderQuotePayload(v *LVal) int {
	return loaderQuotingPayload(v, "quote")
}

// loaderQuasiquotePayload returns the index of the cell of a (quasiquote X)
// form, or -1.
//
// A quasiquoted subtree gets NO discount: opQuasiquote calls findAndUnquote,
// which descends through the whole structure — quote levels included —
// looking for unquote forms, so its cost is the quote-blind unfolded size
// and a quasiquoted sharing bomb genuinely does not terminate.  The walk
// therefore charges the payload its RAW size, which also means no discount
// survives anywhere beneath it: raw is quote-blind all the way down.
//
// That is conservative in the direction of the status quo — it is what every
// subtree was charged before the blocker fix — so it can refuse only what
// round three already refused.  It over-counts an (unquote e) interior,
// whose code really is evaluated and really may quote data of its own; that
// is a hard-fail for a program that could in principle terminate, and it is
// accepted rather than fixed because the alternative is tracking quoting
// depth through the memo for a shape no parser in this repository emits.
func loaderQuasiquotePayload(v *LVal) int {
	return loaderQuotingPayload(v, "quasiquote")
}

// loaderQuotingPayload matches (op X) where op is the unqualified or
// lisp-qualified symbol name, and returns 1.
func loaderQuotingPayload(v *LVal, name string) int {
	if v.Type != LSExpr || len(v.Cells) != 2 {
		return -1
	}
	head := v.Cells[0]
	// head may be nil here: the nil refusal happens when check descends into
	// the cell, which has not happened yet.
	if head == nil || head.Type != LSymbol || head.quoted {
		return -1
	}
	// The qualified spelling too, for a source that says lisp:quote.
	if head.Str == name || head.Str == "lisp:"+name {
		return 1
	}
	return -1
}

// loaderWalkPathRecordDepth is the depth past which the NON-STRICT walk
// starts recording its on-path set.
//
// Cycle detection needs only the nodes on the current root->node path, and a
// cycle is by construction unbounded in depth: whatever its circumference, a
// walk that follows it descends forever.  So recording can start late and
// still be exact — the cycle's own nodes are recorded on the lap that passes
// this depth and the repeat is caught on the next one.  Real parse trees are
// nowhere near this deep (rdparser caps parse nesting at 10,000, and phylum
// sources nest in the tens), so the map is never allocated on the path this
// walk is actually hot on, and the guarantee is unchanged for the input it
// exists to catch.
const loaderWalkPathRecordDepth = 64

// loaderWalk carries the admission walk's cycle guard and, on the cache path,
// its unfolded-size memo and node budget.  See admitExpr.
type loaderWalk struct {
	// sizes is the CACHE path's memo: one loaderNodeInfo per distinct node,
	// marked onPath while the node is on the current root->node path and
	// carrying its sizes and height once it is fully validated.  Memoising is
	// what lets the walk answer "how much work is this program" in O(distinct
	// nodes) instead of O(paths).  nil off the cache path (newLoaderWalk).
	sizes map[*LVal]loaderNodeInfo
	// onPath is the NON-STRICT path's cycle guard: only the nodes on the
	// current root->node path, and only those deeper than
	// loaderWalkPathRecordDepth, so an ordinary parse never allocates it.
	onPath map[*LVal]struct{}
	// unfolded is the stream's total QUOTE-BLIND unfolded size and
	// evalUnfolded its total EVALUATED size, both saturating at
	// loaderWalkUnfoldedCap; distinct counts the nodes actually visited.
	// See loaderNodeInfo for which budget each one answers.  Strict path
	// only: admitExpr does not accumulate them otherwise.
	unfolded     int64
	evalUnfolded int64
	distinct     int64
	strict       bool // cache path: node budget
	// allowNative tolerates a Native payload on a sealable type.  TextLoader
	// only; see newTextLoaderWalk.
	allowNative bool
}

// saturatingAddNodes adds two node counts, clamping at loaderWalkUnfoldedCap.
// Saturation is what makes the count safe to take on adversarial input: an
// unfolded size doubles per level of sharing, so an unclamped sum overflows
// int64 at sharing-depth 63 and would wrap to a small, admissible-looking
// number.
func saturatingAddNodes(a, b int64) int64 {
	n := a + b
	if n < 0 || n > loaderWalkUnfoldedCap {
		return loaderWalkUnfoldedCap
	}
	return n
}

// verdict reports the stream-level refusal the walk accumulated, or nil.
// Only the cache path has one.
//
// THE SHARING RULE LIVES HERE, and it is the round-three rewrite (issue #536
// round-three review, blocker 2).  The rule it replaces was "a composite node
// reached twice is refused, and the load FAILS", justified by "an interned
// subtree evaluates once per path, exponentially".  That justification is
// about NESTED sharing.  One small subexpression reached twice is linear, is
// exactly what a constant-interning Reader produces, and evaluated in
// microseconds with no cache installed — so the rule turned a working program
// into a broken one the moment a cache was installed:
//
//	(in-package 'user) (set 'a (+ 1 2)) (set 'b (+ 1 2))    ; interned
//	cache OFF: 3      cache ON: "reader output is not a finite tree"
//
// The quantity that actually separates the two is not "is anything shared"
// but HOW MUCH WORK THE SHARING IMPLIES, which the memo computes exactly and
// cheaply.  So:
//
//   - EVALUATED size at loaderWalkUnfoldedCap — 4.3e9 node evaluations —
//     is errReaderTreeUnbounded, a hard load failure.  Nothing that
//     terminates looks like this: an unshared parse that large would not fit
//     in memory, so reaching it means sharing that multiplies, and the
//     author's own 2^40-path example lands here in linear time.  Refusing it
//     cannot break a program that worked, because no such program works.
//
//     "EVALUATED" is the round-four correction (issue #536 round-four
//     review, blocker).  The count is evalUnfolded, which charges one node
//     for a quoted datum, because the evaluator does not walk one.  Taken
//     against the quote-blind count instead, this arm hard-failed a Reader
//     that shares quoted DATA — 41 distinct nodes, 2^41 unfolded, and a
//     load that finishes in O(1) with no cache installed.  Such a stream
//     still fails to be cacheable, but through the arm below, which falls
//     back instead of failing.
//
//   - Merely over loaderWalkMaxNodes — in QUOTE-BLIND unfolded size, in
//     distinct nodes, or both — is errReaderTreeTooLarge: a legal program
//     that is too big to be worth aliasing process-wide.  The load runs
//     UNCACHED, which is round two's fix 3 and is byte-identical to having
//     no cache installed.
//     This is where an ordinary interning Reader with a very large source
//     lands, and it is why the discriminator is not "distinct nodes are
//     under budget, so sharing is to blame": a lightly interned 1.1M-node
//     source has few distinct nodes to spare and is still an ordinary
//     program.  It is ALSO where the quoted-data case above lands, and that
//     is the load-bearing half of the blocker fix rather than a side effect:
//     the quote discount applies to this arm's cap NOWHERE, so a program
//     whose quoted sharing unfolds past the budget is refused for the cache
//     before newProgramAdmitted reaches (*LVal).Copy, firstUnsealed or the
//     admission fingerprint — the three walks that unfold sharing and none
//     of which stops at a quote.  Discounting quotes here as well would have
//     traded a spurious hard failure for a real hang.
//
//   - Anything under budget is admitted, sharing and all.  A repeated leaf
//     was always admitted; a repeated composite now is too.
func (w *loaderWalk) verdict() error {
	if !w.strict {
		return nil
	}
	if w.evalUnfolded >= loaderWalkUnfoldedCap {
		return errReaderTreeUnbounded
	}
	if w.unfolded > loaderWalkMaxNodes || w.distinct > loaderWalkMaxNodes {
		return errReaderTreeTooLarge
	}
	return nil
}

// check validates v and returns its loaderNodeInfo — unfolded sizes, height,
// and, through the memo, the cycle guard.  Off the strict path the numbers
// are not computed: nothing consults them there (verdict is strict-only), and
// this is the walk docs/embed.md promises is unchanged by the hook existing.
func (w *loaderWalk) check(v *LVal, depth int) (loaderNodeInfo, error) {
	if v == nil {
		return loaderNodeInfo{}, errReaderNilNode
	}
	// Singletons (Nil/true/false) are shared by design and immutable, so a
	// parse may legitimately reach one from many positions.  They are exempt
	// from every repeat rule and terminate the walk at once.
	if isSingleton(v) {
		return loaderNodeInfo{raw: 1, eval: 1}, nil
	}
	if depth > loaderWalkMaxDepth {
		return loaderNodeInfo{}, errReaderTreeUnbounded
	}
	recordPath := false
	if w.strict {
		if info, seen := w.sizes[v]; seen {
			if info.onPath {
				// Reached from itself: a cycle, on every path.
				return loaderNodeInfo{}, errReaderTreeUnbounded
			}
			// Shared, not cyclic.  Its sizes are already known, and counting
			// them again is the whole point: the unfolded total is what the
			// walks downstream will actually walk.
			return info, nil
		}
	} else if depth >= loaderWalkPathRecordDepth {
		if _, onPath := w.onPath[v]; onPath {
			return loaderNodeInfo{}, errReaderTreeUnbounded
		}
		recordPath = true
	}

	switch v.Type {
	case LBytes, LSortMap, LArray, LNative:
		// Reference types share mutable state with every copy of the cached
		// expression, so a cached loader would hand the same backing store to
		// each caller.
		return loaderNodeInfo{}, fmt.Errorf("cannot cache reference type expression: %v", v.Type)
	case LInvalid, LInt, LFloat, LError, LSymbol, LQSymbol, LSExpr, LFun,
		LQuote, LString, LTaggedVal,
		LMarkTerminal, LMarkTailRec, LMarkMacExpand, LTypeMax:
		// Value types are safe to cache; composite ones (LSExpr, LQuote,
		// LTaggedVal) are covered by the recursion over Cells below.  Listed
		// explicitly because this switch is a denylist: a new LType that
		// wraps shared state would otherwise be cached silently.
	}
	// A Native payload on a type the seal MARKS is not covered by anything
	// downstream: SealAST freezes LVal fields only, the fingerprint oracle
	// skips Native by design, and both the admission conjunction
	// (firstUnsealed) and the ownership exemption key off the TYPE, which is
	// sealable here.  See newLoaderWalk's allowNative for why TextLoader is
	// exempt.  Types the seal does not mark are unaffected: an LFun's
	// funData, an LError's CallStack and LNative's own payload reach their
	// own rejections.
	if v.Native != nil && sealableNodeType(v.Type) && !w.allowNative {
		return loaderNodeInfo{}, fmt.Errorf("cannot admit %v expression carrying a native payload", v.Type)
	}

	if w.strict {
		w.distinct++
		if w.distinct > loaderWalkHardMaxNodes {
			return loaderNodeInfo{}, errReaderTreeTooLarge
		}
		w.sizes[v] = loaderNodeInfo{onPath: true}
	} else if recordPath {
		if w.onPath == nil {
			w.onPath = make(map[*LVal]struct{})
		}
		w.onPath[v] = struct{}{}
	}
	info := loaderNodeInfo{raw: 1, eval: 1}
	quoted, quasi := -1, -1
	if w.strict {
		quoted, quasi = loaderQuotePayload(v), loaderQuasiquotePayload(v)
	}
	for i, cell := range v.Cells {
		ci, err := w.check(cell, depth+1)
		if err != nil {
			return loaderNodeInfo{}, err
		}
		if !w.strict {
			continue
		}
		info.raw = saturatingAddNodes(info.raw, ci.raw)
		switch i {
		case quoted:
			// The evaluator hands this cell back untouched, so it is one
			// node of work however big it is.  See loaderQuotePayload.
			info.eval = saturatingAddNodes(info.eval, 1)
		case quasi:
			// findAndUnquote descends the whole payload, and no quote
			// beneath it stops the descent, so it costs its quote-blind
			// size.  See loaderQuasiquotePayload.
			info.eval = saturatingAddNodes(info.eval, ci.raw)
		default:
			info.eval = saturatingAddNodes(info.eval, ci.eval)
		}
	}
	if w.strict {
		if v.quoted {
			// (*LEnv).eval returns a quoted node before it looks at anything
			// else, so nothing under it is ever evaluated — this is the
			// PARSER's quote form (rdparser.ParseQuote -> lisp.Quote), and
			// an LQuote node is always quoted too.
			info.eval = 1
		}
		w.sizes[v] = info
	} else if recordPath {
		delete(w.onPath, v)
	}
	return info, nil
}
