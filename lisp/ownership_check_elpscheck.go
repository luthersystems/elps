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
//     construction and shared under copy-on-write protection (see
//     registrationFormals in env.go); this checker covers the values that
//     actually flow.
//   - It only sees the instrumented points.  A value that crosses runtimes
//     purely through direct field reads (e.g. libschema's validatorMarker,
//     compared by pointer identity only) never hits Put or eval and is not
//     checked.  Again: cmd/elpsvet flags the producer pattern statically.
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
//  2. SEALED nodes (LVal.sealed - lisp/seal.go), added independently for
//     issue #372 and for #379 item 2, and a deliberate design rather than
//     a bug.  Worth spelling out, because without it the checker forbade
//     the exact topology the seal exists to make safe.
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
//     - substrate's warm parse cache hands one sealed tree to many
//     runtimes (substrate#375/#378).
//     - elpstest.RunBenchmark shares the sealed program across its
//     per-iteration runtimes (#379 item 2, this branch).
//
//     Found by lisp.FuzzSharedProgramMultiEnv (#389), whose whole subject is
//     the first of those.  The exemption itself lives on
//     claude/exp-seal-tooling, where sealing is introduced and where the
//     contradiction it resolves is created; the branches below are where it
//     was discovered and where the topologies are added, not where it
//     belongs.
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
// Values whose Source is the shared native location need NO exemption —
// the Location is shared (#362) but the LVals carrying it are per-value.
// Nothing else is exempt.  If the suite finds a new cross-runtime flow,
// do not add an exemption reflexively: it is either a real bug (fix it,
// or file it like #363) or a deliberate design (document it HERE, with
// the reasoning, so the next reader knows which it was).
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
	// v.sealed: see allowlist entry 2 in the file comment.  A sealed node is
	// shared across runtimes BY DESIGN (that is what a parse cache is), and
	// its safety is carried by the seal's own three checkers rather than by
	// ownership.
	if v == nil || rt == nil || isSingleton(v) || v.sealed {
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

func runtimePackageName(rt *Runtime) string {
	if rt == nil || rt.Package == nil {
		return "<nil>"
	}
	return rt.Package.Name
}
