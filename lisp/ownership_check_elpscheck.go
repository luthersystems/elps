// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"fmt"
	"sync"
	"sync/atomic"
)

// Runtime ownership checking — the dynamic half of hermetic sealing.
//
// The Runtime doc comment promises "to evaluate ELPS code concurrently,
// create a separate Runtime (and LEnv tree) per goroutine".  That promise
// is only as good as the isolation between runtimes, and issues #362/#363
// showed the isolation is not airtight: package-level tables hand the same
// *LVal to every runtime in the process, and nothing watches for a value
// leaking from one runtime into another.  This file watches.
//
// # How it works
//
// A process-wide side table maps *LVal → the *Runtime that first used it.
// Constructors cannot populate it — Int(1) has no idea which runtime it is
// destined for — so the table is populated lazily: THE FIRST TOUCH ADOPTS.
// The first instrumented sighting of a value (LEnv.Put, LEnv.PutGlobal, or
// entry to the eval hot path) claims it for that sighting's Runtime.  Every
// later sighting asserts the same Runtime and panics on a mismatch, naming
// both runtimes and the value.  A found bug should be loud in checked
// builds: the panic deliberately punches through env.eval's recover() (see
// rethrowOwnershipViolation) so it cannot be laundered into an LError that
// a test — or ignore-errors — quietly absorbs.
//
// # What this does NOT catch
//
//   - It is SHALLOW.  Only the *LVal that passes through an instrumented
//     point is adopted — its Cells are not walked (walking would turn the
//     O(1) eval entry into O(tree) and the suite into a space heater).  A
//     shared value that only ever rides INSIDE another value is invisible.
//     That is not hypothetical: the shared builtin formals of issue #363
//     live in fun.Cells[0] and never surface at Put or eval — builtins have
//     no lexical env, so bind() routes their arguments through the
//     builtinArgs append path and never Puts a formal symbol.  The formals
//     sharing is documented, filed, and covered by the static rule
//     (cmd/elpsvet) and by sealing: definition-table formals are sealed at
//     construction and shared under the seal's write protection (see
//     registrationFormals in env.go); this checker covers the values that
//     actually flow.
//   - It only sees the instrumented points.  A value that crosses runtimes
//     purely through direct field reads (e.g. libschema's validator marker
//     cell, read straight out of a validator's cells) never hits Put or eval
//     and is not checked.  Again: cmd/elpsvet flags the producer pattern statically.
//   - Adoption order is first-touch, so a value CREATED for runtime B but
//     touched first by runtime A is blamed backwards.  The panic reports
//     both runtimes; which one is the rightful owner is for the reader to
//     decide.
//
// # Allowlist
//
// Two entries, and this file's own rule applies to both: an exemption is
// either a real bug or a deliberate design, and a deliberate design gets
// documented HERE with the reasoning.
//
//  1. The three singletons (Nil()/Bool(true)/Bool(false)).  Shared by
//     design, immutable by decree, guarded by checkSingleton.
//
//  2. SEALED nodes (LVal.sealed - lisp/seal.go), arrived at independently
//     three times - for issue #372, for #379 item 2 and for #380 - and a
//     deliberate design rather than a bug.  Worth spelling out, because
//     without it the checker forbade the exact topology the seal exists to
//     make safe.
//
//     The seal's contract, stated on the field itself in lisp/lisp.go, is
//     that a sealed node "may be shared by every environment that evaluates
//     the same parse - substrate's parse cache shares one tree
//     process-wide".  Sharing a parse ACROSS RUNTIMES is not an accident to
//     be caught; it is the point of sealing, and it is what
//     substrate#375/#378 does in production.  Before this exemption,
//     evaluating one lisp.Program in two Runtimes under `-tags elpscheck`
//     panicked on the first shared AST node — so an embedder running the
//     supported parse-cache topology could not use checked mode at all, and
//     the two halves of the #372 verification tooling contradicted each
//     other: the mode that hosts the sealed-AST inspector rejected the
//     topology the seal exists to sanction.  What the exemption permits is
//     therefore narrow and stated positively — a node that is sealed, and
//     only while it stays sealed, may be reached by more than one Runtime.
//
//     SANCTIONED SHARING TOPOLOGIES, which is what the exemption is sized
//     to.  Each was arrived at independently, which is itself the argument
//     that this is a design and not a bug:
//
//     - substrate's warm parse cache hands one sealed tree to many runtimes
//     (substrate#375/#378).
//     - elpstest.RunBenchmark shares the sealed program across its
//     per-iteration runtimes (#379 item 2, #387).
//     - LEnv.Fork shares every sealed node between a template environment
//     and its forks (#380, this branch).
//     - Runtime.LoadCache serves one sealed per-file parse to every
//     environment that load-files it (#368) — substrate's warm parse cache
//     again, moved inside the module that owns the AST type so the alias
//     can be established by construction instead of by convention.  It
//     needs no exemption of its own: newCachedSource admits through
//     newProgram, so a cache entry is sealed throughout and is already
//     inside this one (lisp/loadcache_elpscheck_test.go drives both
//     directions).
//
//     Found by lisp.FuzzSharedProgramMultiEnv (#389), whose whole subject is
//     the first of those.  The exemption itself lives on
//     claude/exp-seal-tooling, where sealing is introduced and where the
//     contradiction it resolves is created; the branches named above are
//     where it was discovered and where the topologies are added, not where
//     it belongs.
//
//     What licenses the exemption is that a sealed node's cross-runtime
//     safety does not rest on ownership at all: sealed bytes never change
//     after parse, and three independent mechanisms enforce that.  Every
//     kernel mutation site copies before writing and the evaluator's
//     metadata writes (stampMacroExpansion, SetSource) skip sealed nodes
//     (lisp/seal.go); the fingerprint oracle re-checks every sealed parse
//     in checked builds (lisp/sealfp.go, VerifySealedASTs), so a hole in
//     that protection is a test failure rather than a silently-trusted
//     exemption; and the -race seal watchdog covers the concurrent case.
//     The layers are laid out together in docs/sealed-ast.md section 3.
//     Immutability, not per-runtime confinement, is what protects a sealed
//     node — the same reasoning that exempts the singletons.
//
//     Ownership remains the right question for MUTABLE runtime storage,
//     and that stays fully checked: a value becomes unsealed the moment it
//     becomes runtime storage (Copy and detach clear the flag on fresh
//     storage), so crossing runtimes with one of those still trips the
//     gate.  The rule this checker enforces therefore keeps its spirit -
//     no MUTABLE value may be used by two Runtimes - and sealing is what
//     makes "not mutable" provable.  TestOwnershipCheck_SealedNodesExempt
//     (Put path, plus TestOwnershipCheck_CopyOfSealedIsChecked for the
//     Copy boundary) and TestOwnershipCheck_SealedASTExempt (eval path)
//     pin both directions.
//
//  3. CLOSURE-FREE BUILTIN FUNCTION VALUES (LFun, Builtin() != nil, no
//     captured environment) — added for issue #364, and again a deliberate
//     design rather than a bug.
//
//     libschema.NewValidator's doc comment states the topology outright:
//     "the returned value may be bound into ANY number of lisp.LEnv /
//     lisp.Runtime pairs, including concurrently.  That is the natural
//     reading of an extension point -- build the constraint set once at
//     process start, install it into every environment you create."  That is
//     shipped, guaranteed API, and libschema is not special: it is the shape
//     every Go embedder's extension point has.  Without this entry, checked
//     mode rejected the documented usage of an exported constructor, which is
//     the same failure mode entry 2 records — the verification tooling
//     forbidding a topology the library exists to sanction.
//
//     What licenses it is that such a value has no mutable state to race on,
//     and the argument is the file's own, applied positively rather than as a
//     limitation.  A closure-free builtin's funData is fixed at construction
//     (fid, package, the Go func, no *LEnv), and evaluation never writes its
//     Cells: bind() routes a builtin's arguments through the builtinArgs
//     append path and never Puts a formal symbol — which is exactly why the
//     "What this does NOT catch" section above says the shared formals of
//     issue #363 never surface at Put or eval.  The formals themselves are
//     sealed (sealDefaultFormals, the libutil constructors) or privately
//     copied (formalsCopier, issue #513), and NewValidator additionally
//     capacity-clamps the cell slice so no append through a view can
//     overwrite the credential.
//
//     The exemption is narrow in the one direction that matters: a function
//     that CAPTURED an environment is not exempt.  A closure's *LEnv is
//     mutable per-runtime state and is the deepest aliasing channel there is,
//     so an ELPS lambda crossing runtimes still trips the gate, and so does
//     a Go-built function value that closed over one.
//     TestOwnershipCheck_ClosureFreeBuiltinExempt pins both directions.
//
// Values whose Source is the shared native location need NO exemption —
// the Location is shared (#362) but the LVals carrying it are per-value.
// Nothing else is exempt.  If the suite finds a new cross-runtime flow,
// do not add an exemption reflexively: it is either a real bug (fix it,
// or file it like #363) or a deliberate design (document it HERE, with
// the reasoning, so the next reader knows which it was).
//
// # Declared affinity
//
// A second, narrower rule rides on this file's machinery: a native payload
// may DECLARE which Runtime it belongs to (RuntimeBound,
// lisp/runtime_bound.go), and checkNativeAffinity asserts the declaration.
// No adoption table is involved and no allowlist entry can apply, because
// there is nothing to infer and nothing to sanction — the payload carries
// its own truth, and a payload that answers nil is simply unchecked.  It is
// the type-level replacement for an embedder's load-time probe over the
// natives a loaded environment happens to hold (issue #546).
//
// The rule is enforced at the same instrumented points as ownership, and is
// therefore shallow in the same way: checkOwnership examines the payload of
// an LNative value crossing Put/PutGlobal/eval and never the payloads of
// natives riding inside its Cells.  The deep half is at FORK time, where
// the fork walker visits every reachable native payload whatever container
// it rides in, and checks whichever payload its replacer/NativeCloner/share
// policy resolved (lisp/fork.go, forker.native).  A violation raises the
// same ownershipViolation panic value as the ownership rule, for the same
// reason: an affinity bug found in a checked build must stop the test, not
// become a catchable LError.
//
// # Memory
//
// The table grows with every distinct LVal the process touches.  Go has no
// cheap weak references (runtime.AddCleanup exists but a cleanup per LVal
// costs more than the leak it prevents in a test process), so growth is
// accepted and bounded crudely instead: after ownershipTableMaxEntries
// adoptions the table is dropped and restarted empty.  A reset forgets
// every prior adoption, so a violation that straddles a reset is missed —
// the trade is boundedness (fuzzing would otherwise OOM) for a detection
// gap proportional to how rarely resets happen.  Violations overwhelmingly
// occur close in time to adoption, so the gap is small in practice, but it
// is a gap and this comment is where you learn about it.
//
// Cost: one sync.Map LoadOrStore per eval step and two per Put in checked
// builds — the tagged suite runs several times slower than untagged (see
// the ownership-tag experiment report for measured numbers).  Only enabled
// under the `elpscheck` build tag; release builds compile all of this out
// (no LVal field was added — the fieldalignment layout guard is untouched).
var ownershipTable ownershipState

// ownershipTableMaxEntries bounds table growth: after this many adoptions
// the table is dropped and restarted empty (see the Memory section above).
// 4M entries ≈ a few hundred MB of sync.Map overhead at worst — large
// enough that unit tests never reset, small enough that a fuzz worker
// stays bounded.
const ownershipTableMaxEntries = 4 << 20

type ownershipState struct {
	m     atomic.Pointer[sync.Map] // *LVal → *Runtime
	count atomic.Int64             // adoptions since the last reset
}

func init() {
	ownershipTable.m.Store(new(sync.Map))
}

// ownershipViolation is the panic value raised on a cross-runtime sighting.
// A distinct type — not a string, not an error — so rethrowOwnershipViolation
// can recognize it inside env.eval's recover() and re-panic instead of
// converting it into a catchable LError.
type ownershipViolation struct {
	msg string
}

func (v ownershipViolation) String() string { return v.msg }

// checkOwnership records rt as the owner of v on first sighting and panics
// if v was previously sighted under a different Runtime.  It is called from
// LEnv.Put, LEnv.PutGlobal, and env.eval — see the file comment for why
// those three points and what they miss.
func checkOwnership(rt *Runtime, v *LVal) {
	// v.sealed && sealableNodeType(v.Type): see allowlist entry 2 in the file
	// comment.  A sealed node is shared across runtimes BY DESIGN (that is what
	// a parse cache is), and its safety is carried by the seal's own three
	// checkers rather than by ownership.  The exemption is the CONJUNCTION, not
	// the flag alone: the sealed flag is one byte an untrusted Reader can set
	// on any node, and the seal's immutability guarantees only cover the types
	// SealAST actually marks.  A node whose flag is set but whose type is
	// mutable/reference (an LFun closure laundered into a cache entry, whose
	// captured *LEnv the seal never freezes) is NOT exempt, so it still trips
	// the gate when it crosses runtimes.  The admission side keys off the same
	// conjunction (firstUnsealed, lisp/program.go).
	if v == nil || rt == nil || isSingleton(v) || (v.sealed && sealableNodeType(v.Type)) {
		return
	}
	// Declared affinity (see the section of that name above).  Type-gated on
	// the LVal so the interface assertion is only ever paid by real native
	// handles — an LFun's *funData and the kernel's own internal payloads
	// (*MapData, *CallStack, *[]byte) never reach it.
	if v.Type == LNative {
		checkNativeAffinity(rt, v.Native)
	}
	// See allowlist entry 3.  A builtin that captured no environment holds no
	// mutable per-runtime state, and evaluation never writes it.
	if isClosureFreeBuiltin(v) {
		return
	}
	m := ownershipTable.m.Load()
	owner, loaded := m.LoadOrStore(v, rt)
	if !loaded {
		if ownershipTable.count.Add(1) >= ownershipTableMaxEntries {
			resetOwnershipTable()
		}
		return
	}
	if owner.(*Runtime) == rt {
		return
	}
	panic(ownershipViolation{msg: ownershipViolationMessage(owner.(*Runtime), rt, v)})
}

// checkNativeAffinity asserts a native payload's DECLARED runtime binding:
// a payload implementing RuntimeBound (lisp/runtime_bound.go) may only be
// used by the Runtime it names.  A payload that does not implement the
// interface, or that reports nil (unbound), is unchecked — declaring is
// opt-in and nil is the sanctioned "not bound to anything yet" answer.
//
// It is called from checkOwnership for LNative values (use time) and from
// forker.native for every payload a fork resolves (fork time).  The panic
// value is deliberately ownershipViolation, the same type the ownership
// rule raises, so rethrowOwnershipViolation keeps it a hard panic through
// env.eval's recover(): an affinity bug in a checked build must stop the
// test, not be laundered into an LError that ignore-errors absorbs.
func checkNativeAffinity(rt *Runtime, payload interface{}) {
	b, ok := payload.(RuntimeBound)
	if !ok {
		return
	}
	bound := b.BoundRuntime()
	if bound == nil || bound == rt {
		return
	}
	panic(ownershipViolation{msg: affinityViolationMessage(bound, rt, payload)})
}

// checkDetachedNativeUnbound asserts that a clone minted for a strict
// detach carries no runtime binding.  detach is the sanctioned way to move
// a value BETWEEN Runtimes, and CloneNative has no way to know the
// destination, so unbound is the only answer a detached clone can give:
// one still bound anywhere — most likely to the source, the exact tether
// the transfer exists to sever — is the same bug class the fork-time check
// catches, surfacing at the one other point where the kernel manufactures
// a cross-runtime clone.  Without this, a binding-retaining clone rides
// into the destination inside a container, below what the shallow use-time
// check can see.  The panic value is ownershipViolation for the usual
// reason: hard panic, never a catchable LError.
func checkDetachedNativeUnbound(payload interface{}) {
	b, ok := payload.(RuntimeBound)
	if !ok {
		return
	}
	bound := b.BoundRuntime()
	if bound == nil {
		return
	}
	panic(ownershipViolation{msg: fmt.Sprintf(
		"native affinity violation: detached clone retains a runtime binding\n"+
			"  payload type: %T\n"+
			"  bound runtime: %p (package %s)\n"+
			"a clone produced for detach must be unbound - CloneNative cannot know the"+
			" destination runtime; see lisp/runtime_bound.go",
		payload, bound, runtimePackageName(bound))})
}

// rethrowOwnershipViolation re-panics when r is an ownership violation.
// env.eval's recover() calls it first so the violation stays a hard panic
// instead of becoming a CondInternalPanic LError — an ownership bug found
// in a checked build must stop the test, not decorate its output.
func rethrowOwnershipViolation(r interface{}) {
	if v, ok := r.(ownershipViolation); ok {
		panic(v)
	}
}

// resetOwnershipTable drops every recorded adoption and starts the table
// empty.  Called automatically when the table exceeds its size bound; also
// available to long-running checked-build hosts as a periodic reset hook.
// Every reset opens a detection gap — see the Memory section above.
func resetOwnershipTable() {
	ownershipTable.m.Store(new(sync.Map))
	ownershipTable.count.Store(0)
}

// ownershipViolationMessage renders the panic message: both runtime
// identities and the value's type/str/source.  It deliberately avoids
// LVal.String() — rendering an arbitrary (possibly cyclic or corrupted)
// value inside a panic path is how one panic becomes two.
func ownershipViolationMessage(owner, second *Runtime, v *LVal) string {
	str := v.Str
	if len(str) > 64 {
		str = str[:64] + "..."
	}
	loc := "<nil>"
	if src, ok := v.Source(); ok {
		loc = src.String()
	}
	return fmt.Sprintf("ownership violation: LVal used by two Runtimes\n"+
		"  value: %p type=%s str=%q cells=%d source=%s\n"+
		"  owner runtime:  %p (package %s)\n"+
		"  second runtime: %p (package %s)\n"+
		"an LVal must be used by at most one Runtime; see lisp/ownership_check_elpscheck.go",
		v, v.Type, str, len(v.Cells), loc,
		owner, runtimePackageName(owner),
		second, runtimePackageName(second))
}

// affinityViolationMessage renders the panic message for a declared-affinity
// violation: the payload's Go type and both runtime identities.  It
// deliberately renders the payload with %T alone, for the same reason
// ownershipViolationMessage avoids LVal.String() — rendering an arbitrary
// embedder value inside a panic path is how one panic becomes two.  Even %p
// is out: on a value-type payload fmt falls back to %!p(type=value), which
// formats the value and so calls straight into an embedder String method.
func affinityViolationMessage(bound, using *Runtime, payload interface{}) string {
	return fmt.Sprintf("native affinity violation: payload used by a foreign Runtime\n"+
		"  payload type: %T\n"+
		"  bound runtime: %p (package %s)\n"+
		"  using runtime: %p (package %s)\n"+
		"a RuntimeBound native payload may only be used by the Runtime it declares;"+
		" see lisp/runtime_bound.go",
		payload,
		bound, runtimePackageName(bound),
		using, runtimePackageName(using))
}

func runtimePackageName(rt *Runtime) string {
	if rt == nil || rt.Package == nil {
		return "<nil>"
	}
	return rt.Package.Name
}

// isClosureFreeBuiltin reports whether v is a function value backed by a Go
// builtin that captured no lexical environment — see allowlist entry 3 in the
// file comment for why such a value is exempt from ownership checking, and for
// why the captured-environment half of the test is the load-bearing one.
func isClosureFreeBuiltin(v *LVal) bool {
	if v.Type != LFun {
		return false
	}
	fd, ok := v.Native.(*funData)
	// A malformed or payload-less LFun is NOT exempted: the question is
	// whether this value provably has no mutable state, and an LFun whose
	// Native is not a *funData has not answered it.
	return ok && fd != nil && fd.builtin != nil && fd.env == nil
}
