// Copyright © 2026 The ELPS authors

package lisp

import "sort"

// The value-rebuilding walkers and the payload kinds each memoises.
//
// # Why a registry exists at all
//
// Three primitives in this package rebuild a value graph — the fork walker
// (lisp/fork.go), the detach/copy walker (lisp/detach.go, lisp/copy.go) and
// the macro-expansion stamper (lisp/macro.go) — and the first two have to
// memoise the same set of PAYLOAD kinds for the same reason.  An *LVal
// header and the storage behind it are not one-to-one: Quote, Splice,
// shallowUnquote and FunRef copy an LVal's struct and keep its Native, so
// `(quasiquote (unquote a))` is a second header on a's sorted map, bytes or
// native handle.  A walker that memoises per header alone rebuilds such a
// payload once per header and the two names come apart in the copy.
//
// That bug was found and fixed in the fork walker (issue #576) while the
// IDENTICAL defect sat live in the detach walker for the whole time the
// fork guard was green; it surfaced later as issue #585.  The guard was
// bound to one walker, so it could only ever see one walker's version of
// the bug.
//
// This registry is the machine-readable half of the fix (issue #598).  Each
// walker declares the payload kinds it memoises, right here in production
// code beside the walkers themselves; the drift guard in
// lisp/walkers_drift_test.go then asserts two things that prose cannot:
//
//   - REGISTRY: every walker that rebuilds payloads memoises the same set,
//     so a kind added to one and not the other fails CI rather than
//     shipping.
//   - SOURCE SCAN: every memo-shaped struct field in this package — a map
//     keyed by a pointer type — belongs to a registered walker or to the
//     shrink-only exemption list.  The registry alone cannot catch a BRAND
//     NEW walker nobody told it about; the scan can.

// PayloadKind names a class of storage a value-rebuilding walker must
// memoise per payload rather than per *LVal header.
type PayloadKind string

const (
	// PayloadSortedMap is the *MapData behind an LSortMap.
	PayloadSortedMap PayloadKind = "*MapData"
	// PayloadBytes is the *[]byte behind an LBytes.
	PayloadBytes PayloadKind = "*[]byte"
	// PayloadNative is a native payload held by pointer — the NativeCloner
	// protocol's subject.  A non-pointer payload has no identity to
	// preserve and is deliberately not memoised.
	PayloadNative PayloadKind = "native pointer payload"
	// PayloadValue is the *LVal header itself.  Every walker memoises it;
	// it is what bounds the walk and reproduces header-level aliasing.
	PayloadValue PayloadKind = "*LVal"
	// PayloadEnv is an *LEnv.  Only the fork walker copies environments.
	PayloadEnv PayloadKind = "*LEnv"
)

// WalkerMemo records one walker's memo tables.
type WalkerMemo struct {
	// Fields maps each memoised kind to the struct field holding its memo,
	// so the source scan can tie a field to a kind and a deleted field
	// fails the guard even when no test generates the shape it protects.
	Fields map[PayloadKind]string
	// Walker is the Go type that performs the walk.
	Walker string
	// Doc points at the prose governing the walker.
	Doc string
	// Payloads are the payload kinds the walker memoises per payload.
	// Identical across every walker whose Rebuilds is true.
	Payloads []PayloadKind
	// Graph are the memos that bound the walk itself rather than
	// reproducing payload sharing.
	Graph []PayloadKind
	// Rebuilds reports whether the walker rebuilds payload storage.  A
	// walker that shares payloads by design — the macro stamper, which
	// replaces headers and never their contents — is exempt from the
	// "every rebuilding walker memoises the same payload kinds" rule.
	Rebuilds bool
}

// Kinds returns every kind the walker memoises, payloads first, in a
// canonical order.
func (m WalkerMemo) Kinds() []PayloadKind {
	out := append(append([]PayloadKind(nil), m.Payloads...), m.Graph...)
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

// walkerMemos is the registry.  Adding a walker is one row; a walker with a
// memo-shaped field and no row fails the source scan.
var walkerMemos = []WalkerMemo{
	{
		Walker:   "forker",
		Rebuilds: true,
		Payloads: []PayloadKind{PayloadSortedMap, PayloadBytes, PayloadNative},
		Graph:    []PayloadKind{PayloadValue, PayloadEnv},
		Fields: map[PayloadKind]string{
			PayloadSortedMap: "maps",
			PayloadBytes:     "bytes",
			PayloadNative:    "natives",
			PayloadValue:     "vals",
			PayloadEnv:       "envs",
		},
		Doc: "lisp/fork.go (issue #576)",
	},
	{
		Walker:   "detacher",
		Rebuilds: true,
		Payloads: []PayloadKind{PayloadSortedMap, PayloadBytes, PayloadNative},
		Graph:    []PayloadKind{PayloadValue},
		Fields: map[PayloadKind]string{
			PayloadSortedMap: "maps",
			PayloadBytes:     "bytes",
			PayloadNative:    "natives",
			PayloadValue:     "seen",
		},
		Doc: "lisp/detach.go, lisp/copy.go (issue #585)",
	},
	{
		Walker:   "macroStamper",
		Rebuilds: false,
		Graph:    []PayloadKind{PayloadValue},
		Fields: map[PayloadKind]string{
			PayloadValue: "copies",
		},
		Doc: "lisp/macro.go (issues #582, #583, #586)",
	},
	{
		// (*LVal).Copy REBUILDS payload storage -- it allocates a fresh
		// *MapData per header (copyMapData) -- and memoises NOTHING.  That
		// is the #576/#585 defect shape, live today, and registering it
		// with Rebuilds: true is what makes the registry check say so.
		//
		// Declaring Rebuilds: false here would be the comfortable lie: true
		// of the bytes payload, which Copy shares, and beside the point.  A
		// registration describes the walker, not the subset of its
		// behaviour that keeps the guard green.  Its two known defects are
		// carried by knownDefectiveWalkers below, each with the pin that
		// measures it, and the row disappears when the defect does.
		Walker:   "(*LVal).Copy",
		Rebuilds: true,
		Doc:      "lisp/lisp.go ((*LVal).Copy)",
	},
}

// WalkerMemos returns the registry, DEEP-copied so a caller cannot edit it.
// A shallow copy would share Fields, Payloads and Graph with the registry
// itself, and a caller that edited one — a test building a weakened
// variant, say — would silently rewrite what every later caller reads.
func WalkerMemos() []WalkerMemo {
	out := make([]WalkerMemo, len(walkerMemos))
	for i, m := range walkerMemos {
		m.Payloads = append([]PayloadKind(nil), m.Payloads...)
		m.Graph = append([]PayloadKind(nil), m.Graph...)
		if m.Fields != nil {
			f := make(map[PayloadKind]string, len(m.Fields))
			for k, v := range m.Fields {
				f[k] = v
			}
			m.Fields = f
		}
		out[i] = m
	}
	return out
}

// WalkerMemoKinds returns the payload kinds the named walker memoises, or
// nil when the name is not registered.  It is what the alias guard mirrors
// into its own walker table, so a walker registered in the guard with no
// production memo declaration is visibly empty.
func WalkerMemoKinds(walker string) []PayloadKind {
	for _, m := range walkerMemos {
		if m.Walker == walker {
			return m.Kinds()
		}
	}
	return nil
}

// MemoExemption is one row of the shrink-only exemption list: a memo-shaped
// field, or a payload type a rebuilding walker copies without memoising,
// that is deliberately outside the registry.
type MemoExemption struct {
	// Subject is the struct field ("lisp.cycleState.path") or payload type
	// ("*CallStack") the row exempts.
	Subject string
	// Reason states why it is safe, in a sentence an operator can check.
	Reason string
}

// memoExemptions is SHRINK-ONLY: a row may be deleted when the exemption
// stops being needed, and a new row is a design decision that belongs in a
// review, not a way to make a red guard green.  Every row states why the
// subject cannot carry the aliasing bug the registry exists to prevent.
var memoExemptions = []MemoExemption{
	{
		Subject: "*CallStack",
		Reason: "an LError's recorded stack, deep-copied per header by detachCallStack rather than memoised per payload. " +
			"Its IDENTITY carries no observable state: CallStack.Copy allocates an exact-length Frames slice " +
			"(lisp/stack.go), every capture site calls it (op.go, env.go, builtins.go), and the only writers of a " +
			"CallStack -- PushFID and Pop -- are called on env.Runtime.Stack, the live evaluator stack, never on a " +
			"captured one. So two headers over one *CallStack cannot observe each other. " +
			"CORRECTED in PR #599: this row used to claim \"no constructor can alias one across two headers: " +
			"SetCallStack is called once\", which is FALSE -- (*LVal).Copy does `*cp = *v`, a shallow copy that " +
			"carries Native, so it aliases one across two headers. Measured by " +
			"TestCopyAliasesCallStackAcrossHeaders. The conclusion survived; the stated reason did not.",
	},
	{
		Subject: "lisp.cycleState.path",
		Reason:  "the cycle guard's descent path (lisp/cycle.go). It bounds a walk; it holds no copy and reproduces no sharing.",
	},
	{
		Subject: "lisp.loaderWalk.sizes",
		Reason:  "the loader's per-node size accounting (lisp/loader.go). It maps a node to a measurement, not to a copy.",
	},
	{
		Subject: "lisp.loaderWalk.onPath",
		Reason:  "the loader's cycle guard (lisp/loader.go). Same as cycleState.path: it bounds a walk.",
	},
	{
		Subject: "lisp.sealCheckState.roots",
		Reason: "the checked-build seal watchdog's fingerprint table (lisp/seal_check_elpscheck.go, -tags elpscheck). " +
			"It maps a sealed root to the digest it carried at seal time; it produces no copy.",
	},
}

// WalkerDefect is one row of the shrink-only KNOWN-DEFECT allowlist: a
// registered walker that fails a registry rule today, recorded so the guard
// reports it as known rather than either failing the build or passing
// silently.
//
// This is NOT memoExemptions.  An exemption says a payload kind cannot
// carry the bug.  A defect row says the walker HAS the bug, names it, and
// names the test that measures it -- so the row is falsifiable from both
// ends: the pin goes red the day the defect is fixed, and the allowlist is
// shrink-only, so the row cannot outlive it.
type WalkerDefect struct {
	// Walker is the registry row this defect belongs to.
	Walker string
	// Payload is the payload type the defect is about ("*MapData").
	Payload string
	// Defect states what the walker does wrong, in one line.
	Defect string
	// Pin names the test that asserts the defect STILL EXISTS.  When the
	// defect is fixed that test goes red, which is the signal to delete
	// this row.
	Pin string
}

// knownDefectiveWalkers is SHRINK-ONLY.  A row is deleted when its defect
// is fixed; a NEW row is an admission that belongs in a review, not a way
// to make a red guard green.
//
// (*LVal).Copy is here rather than fixed because fixing it is a behaviour
// change to the most-called copy path in the interpreter, which is its own
// stacked PR.  Until that lands, this is what keeps the guard honest: the
// registry check REPORTS Copy as known-defective, and a reader of
// WalkerMemos cannot mistake it for a clean walker.
var knownDefectiveWalkers = []WalkerDefect{
	{
		Walker:  "(*LVal).Copy",
		Payload: "*MapData",
		Defect: "rebuilt per HEADER and not memoised: copyMapData allocates a fresh *MapData for every " +
			"header it visits, so two headers over one sorted map come apart in the copy. This is the " +
			"issue #576 / #585 defect shape exactly, in a walker neither fix reached.",
		Pin: "TestCopyDeAliasesMapPayloadAcrossHeaders",
	},
	{
		Walker:  "(*LVal).Copy",
		Payload: "*[]byte",
		Defect: "SHARED across headers: `*cp = *v` carries Native and no arm rebuilds it for LBytes, so a " +
			"copy of a bytes value and its source append into one backing array (issue #551). The opposite " +
			"failure to the map one, in the same function.",
		Pin: "TestCopySharesBytesPayloadAcrossHeaders",
	},
}

// WalkerDefects returns the known-defect allowlist, copied.
func WalkerDefects() []WalkerDefect {
	out := make([]WalkerDefect, len(knownDefectiveWalkers))
	copy(out, knownDefectiveWalkers)
	return out
}

// IsKnownDefective reports whether the named walker has an open defect row.
// The registry checks consult it so an allowlisted walker is reported as
// KNOWN-DEFECTIVE rather than passing, and callers reading the registry can
// tell a clean walker from a tolerated one.
func IsKnownDefective(walker string) bool {
	for _, d := range knownDefectiveWalkers {
		if d.Walker == walker {
			return true
		}
	}
	return false
}

// MemoExemptions returns the exemption list, copied.
func MemoExemptions() []MemoExemption {
	out := make([]MemoExemption, len(memoExemptions))
	copy(out, memoExemptions)
	return out
}
