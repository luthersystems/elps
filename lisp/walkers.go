// Copyright © 2026 The ELPS authors

package lisp

import "sort"

// The value-rebuilding walkers and the payload kinds each memoises.
//
// # Why a registry exists at all
//
// Several primitives in this package walk a value graph and rebuild it —
// the detach/copy walker (lisp/detach.go, lisp/copy.go), the template
// compiler (lisp/template_plan.go) with the admission scan that feeds it
// (lisp/template.go), the macro-expansion stamper (lisp/macro.go) and
// (*LVal).Copy (lisp/lisp.go) — and the ones that rebuild PAYLOAD storage
// have to memoise the same payload kinds for the same reason.  An *LVal
// header and the storage behind it are not one-to-one: Quote, Splice,
// shallowUnquote and FunRef copy an LVal's struct and keep its Native, so
// `(quasiquote (unquote a))` is a second header on a's sorted map, bytes or
// native handle.  A walker that memoises per header alone rebuilds such a
// payload once per header and the two names come apart in the copy.
//
// That bug was found and fixed in one walker (issue #576) while the
// IDENTICAL defect sat live in the detach walker for the whole time the
// first guard was green; it surfaced later as issue #585.  The guard was
// bound to one walker, so it could only ever see one walker's version of
// the bug.
//
// This registry is the machine-readable half of the fix (issue #598).  Each
// walker declares the payload kinds it memoises, right here in production
// code beside the walkers themselves; the drift guard in
// lisp/walkers_drift_test.go then asserts three things that prose cannot:
//
//   - REGISTRY: every walker that rebuilds payload storage memoises the same
//     set of CROSS-WALKER payload kinds, so a kind added to one and not the
//     other fails CI rather than shipping.
//   - SOURCE SCAN: every memo-shaped struct field in this package — a map
//     keyed by a pointer type — belongs to a registered walker or to the
//     shrink-only exemption list, and every field the registry names still
//     exists.  The registry alone cannot catch a BRAND NEW walker nobody
//     told it about, nor a memo DELETED from a registered one; the scan
//     catches both.
//   - PAYLOAD SCAN: every case arm of the walkers' `v.Native` type switches
//     is a registered memo kind or an exempted one.  That is issue #585
//     stated exactly — a payload the walker rebuilds without memoising is a
//     payload two headers come apart over.

// payloadKind names a class of storage a value-rebuilding walker must
// memoise per payload rather than per *LVal header.
type payloadKind string

const (
	// payloadSortedMap is the *MapData behind an LSortMap.
	payloadSortedMap payloadKind = "*MapData"
	// payloadBytes is the *[]byte behind an LBytes.
	payloadBytes payloadKind = "*[]byte"
	// payloadNative is a native payload held by pointer — the NativeCloner
	// protocol's subject.  A non-pointer payload has no identity to
	// preserve and is deliberately not memoised.
	payloadNative payloadKind = "native pointer payload"
	// payloadFunction is the *funData behind an LFun.
	payloadFunction payloadKind = "*funData"
	// payloadValue is the *LVal header itself.  Every walker memoises it;
	// it is what bounds the walk and reproduces header-level aliasing.
	payloadValue payloadKind = "*LVal"
	// payloadSealed is a set of sealed *LVal roots: the template path's
	// record of which admitted values are published by reference rather
	// than rebuilt.
	payloadSealed payloadKind = "sealed *LVal set"
	// payloadEnv is an *LEnv.  Only the template path copies environments.
	payloadEnv payloadKind = "*LEnv"
)

// walkerMemo records one walker's memo tables.
type walkerMemo struct {
	// Fields maps each memoised kind to the struct field holding its memo,
	// so the source scan can tie a field to a kind and a deleted field
	// fails the guard even when no test generates the shape it protects.
	Fields map[payloadKind]string
	// Walker is the Go type that performs the walk.
	Walker string
	// Doc points at the prose governing the walker.
	Doc string
	// Payloads are the CROSS-WALKER payload kinds the walker memoises per
	// payload.  Identical across every walker whose Rebuilds is true: this
	// is the set the registry check compares, and issue #585 is a kind
	// present in one walker's set and absent from another's.
	Payloads []payloadKind
	// Local are payload memos this walker keeps that no other rebuilding
	// walker has an equivalent for, because no other walker rebuilds that
	// storage at all.  They are NOT compared across walkers — extra
	// memoisation is never the #585 bug — but they are still named here so
	// the source scan can tie the field to a kind.
	Local []payloadKind
	// Graph are the memos that bound the walk itself, or record admitted
	// identity, rather than reproducing payload sharing.
	Graph []payloadKind
	// Rebuilds reports whether the walker rebuilds payload storage.  A
	// walker that shares payloads by design — the macro stamper, which
	// replaces headers and never their contents, and the template admission
	// scan, which indexes a graph it does not copy — is exempt from the
	// "every rebuilding walker memoises the same payload kinds" rule.
	Rebuilds bool
}

// Kinds returns every kind the walker memoises, payloads first, in a
// canonical order.
func (m walkerMemo) Kinds() []payloadKind {
	out := append(append([]payloadKind(nil), m.Payloads...), m.Local...)
	out = append(out, m.Graph...)
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

// walkerMemos is the registry.  Adding a walker is one row; a walker with a
// memo-shaped field and no row fails the source scan.
var walkerMemos = []walkerMemo{
	{
		Walker:   "detacher",
		Rebuilds: true,
		Payloads: []payloadKind{payloadSortedMap, payloadBytes, payloadNative},
		Graph:    []payloadKind{payloadValue},
		Fields: map[payloadKind]string{
			payloadSortedMap: "maps",
			payloadBytes:     "bytes",
			payloadNative:    "natives",
			payloadValue:     "seen",
		},
		Doc: "lisp/detach.go, lisp/copy.go (issue #585)",
	},
	{
		// The template COMPILER is the second rebuilding walker: it turns
		// the admitted graph into a plan whose instantiation allocates a
		// fresh payload per plan entry, so a payload reachable from two
		// headers must land in ONE entry or the two names come apart in
		// every VM the template mints.
		Walker:   "templateCompiler",
		Rebuilds: true,
		Payloads: []payloadKind{payloadSortedMap, payloadBytes, payloadNative},
		Local:    []payloadKind{payloadFunction},
		Graph:    []payloadKind{payloadValue, payloadEnv, payloadSealed},
		Fields: map[payloadKind]string{
			payloadSortedMap: "maps",
			payloadBytes:     "bytes",
			payloadNative:    "natives",
			payloadFunction:  "functions",
			payloadValue:     "values",
			payloadEnv:       "envs",
			payloadSealed:    "sealed",
		},
		Doc: "lisp/template_plan.go (Template.NewVM)",
	},
	{
		// Admission, not a copy: it assigns every private value an index,
		// validates the graph and records which storage it saw.  Its tables
		// are identity and seen-sets, so they are all Graph; the copying
		// happens in the compiler above, which INHERITS values/envs/sealed
		// from here (compileTemplate reuses the same maps), which is why
		// those two rows name the same kinds.
		Walker:   "templateInventory",
		Rebuilds: false,
		Graph:    []payloadKind{payloadValue, payloadEnv, payloadSealed, payloadSortedMap, payloadBytes},
		Fields: map[payloadKind]string{
			payloadValue:     "values",
			payloadEnv:       "envs",
			payloadSealed:    "sealed",
			payloadSortedMap: "maps",
			payloadBytes:     "byteSeen",
		},
		Doc: "lisp/template.go (lisp.NewTemplate admission)",
	},
	{
		Walker:   "macroStamper",
		Rebuilds: false,
		Graph:    []payloadKind{payloadValue},
		Fields: map[payloadKind]string{
			payloadValue: "copies",
		},
		Doc: "lisp/macro.go (issues #582, #583, #586)",
	},
	{
		// (*LVal).Copy is the copier (lisp/copier.go, #604): the fifth
		// rebuilding walker.  It carries the detacher's three payload memos
		// verbatim -- one copied payload per *MapData, *[]byte and
		// pointer-held NativeCloner -- plus the header memo every walker has,
		// which is an inline array that spills to the `seen` map on a large
		// walk (the array is not memo-shaped, so only the map is named here).
		Walker:   "copier",
		Rebuilds: true,
		Payloads: []payloadKind{payloadSortedMap, payloadBytes, payloadNative},
		Graph:    []payloadKind{payloadValue},
		Fields: map[payloadKind]string{
			payloadSortedMap: "maps",
			payloadBytes:     "bytes",
			payloadNative:    "natives",
			payloadValue:     "seen",
		},
		Doc: "lisp/copier.go ((*LVal).Copy, issue #604)",
	},
}

// registeredWalkerMemos returns the registry, DEEP-copied so a caller
// cannot edit it.  A shallow copy would share Fields, Payloads, Local and
// Graph with the registry itself, and a caller that edited one — a test
// building a weakened variant, say — would silently rewrite what every later
// caller reads.
func registeredWalkerMemos() []walkerMemo {
	out := make([]walkerMemo, len(walkerMemos))
	for i, m := range walkerMemos {
		m.Payloads = append([]payloadKind(nil), m.Payloads...)
		m.Local = append([]payloadKind(nil), m.Local...)
		m.Graph = append([]payloadKind(nil), m.Graph...)
		if m.Fields != nil {
			f := make(map[payloadKind]string, len(m.Fields))
			for k, v := range m.Fields {
				f[k] = v
			}
			m.Fields = f
		}
		out[i] = m
	}
	return out
}

// memoExemption is one row of the shrink-only exemption list: a memo-shaped
// field, or a payload type a rebuilding walker copies without memoising,
// that is deliberately outside the registry.
type memoExemption struct {
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
var memoExemptions = []memoExemption{
	{
		Subject: "*CallStack",
		Reason: "an LError's recorded stack, deep-copied per header by detachCallStack rather than memoised per payload. " +
			"Its IDENTITY carries no observable state: CallStack.Copy allocates an exact-length Frames slice " +
			"(lisp/stack.go), every capture site calls it (op.go, env.go, builtins.go), and the only writers of a " +
			"CallStack -- PushFID and Pop -- are called on env.Runtime.Stack, the live evaluator stack, never on a " +
			"captured one. So two headers over one *CallStack cannot observe each other. The row does NOT claim the " +
			"aliasing is unreachable -- every shallow header copy in the package (Quote, Splice, shallowUnquote) " +
			"carries Native across -- only that it is unobservable, which is what the paragraph above establishes.",
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
	{
		Subject: "lisp.templateStorage.cellViews",
		Reason: "a construction-time index from an admitted storage owner to the SPAN it occupies in a shared backing " +
			"array (lisp/template_storage.go). It maps a value to a descriptor -- storage group, offset, length, " +
			"capacity -- not to a copy, and overlap has already been resolved before it is built, so it cannot hand " +
			"two headers one rebuild.",
	},
	{
		Subject: "lisp.templateStorage.byteViews",
		Reason:  "the byte-span half of cellViews above, and exempt for the same reason: a descriptor per admitted owner, not a copy.",
	},
}

// walkerNote is one row of the shrink-only list of registered walkers that
// keep NO payload memo tables at all.
//
// It is not an exemption and not a verdict.  An exemption says a payload
// kind cannot carry the aliasing bug; this row says the walker declares no
// memos, so the registry check has nothing to compare it against, and
// records what the walker does today so a reader of registeredWalkerMemos cannot
// mistake it for a walker that was compared and passed.
type walkerNote struct {
	// Walker is the registry row this note belongs to.
	Walker string
	// Note describes the walker's memo behaviour as it is TODAY, in terms
	// a reader can check against the source.
	Note string
}

// unmemoisedWalkers is SHRINK-ONLY.  A row is deleted when its walker
// declares memo Fields; a NEW row is a design decision that belongs in a
// review, not a way to make a red guard green.
var unmemoisedWalkers = []walkerNote{}

// registeredWalkerNotes returns the unmemoised-walker list, copied.
func registeredWalkerNotes() []walkerNote {
	out := make([]walkerNote, len(unmemoisedWalkers))
	copy(out, unmemoisedWalkers)
	return out
}

// isUnmemoisedWalker reports whether the named walker has an open row in
// unmemoisedWalkers.  The registry checks consult it so such a walker is
// REPORTED rather than silently compared against walkers that do memoise.
func isUnmemoisedWalker(walker string) bool {
	for _, n := range unmemoisedWalkers {
		if n.Walker == walker {
			return true
		}
	}
	return false
}

// lvalCopyExemption is one row of the shrink-only allowlist for STRUCT
// COPIES of an LVal that happen outside a registered walker.
//
// `*cp = *v` on an LVal is how implicit payload sharing is born: every
// pointer field -- Native, and the Cells slice header -- rides across, and
// nothing rebuilds it. Inside a registered walker that is the walker's job
// and the registry above governs it. Outside one it is unreviewed, so each
// site is either routed through a walker or recorded here with the reason
// it is safe.
//
// Keyed by ENCLOSING FUNCTION plus a site count rather than by file:line:
// a line number drifts on any edit above it, but a second copy appearing in
// an allowlisted function is exactly the change that should be re-reviewed,
// and the count catches it.
type lvalCopyExemption struct {
	// Func is the enclosing function, as the scan renders it:
	// "Quote", "(*templateCompiler).value".
	Func string
	// Reason names the class the copy belongs to, not merely that it is
	// safe.
	Reason string
	// Sites is how many struct copies that function is allowed to contain.
	// A function that grows one more fails until this is updated, which is
	// the review the row exists to force.
	Sites int
}

// lvalCopyExemptions is SHRINK-ONLY: a row whose function no longer
// contains a struct copy is dead and must be deleted.
//
// Four of these are not hazards that happen to be safe. Quote, Splice,
// shallowUnquote and FunRef are where the INTENTIONAL aliasing lives -- the
// aliasing every walker in the registry above exists to PRESERVE. A walker
// that de-aliased them would be the defect.
//
// A row also wins over walker membership: the template walker's own copies
// are audited here individually rather than left to the blanket exemption
// its registry row would otherwise give them.
var lvalCopyExemptions = []lvalCopyExemption{
	{
		Func:  "Quote",
		Sites: 1,
		Reason: "the second header over one payload, at its source. `(quasiquote (unquote v))` is this " +
			"function, and the resulting two-names-one-payload graph is what the walkers must reproduce " +
			"rather than flatten -- measured by TestCopySelfReferenceThroughAliasedHeaderStaysAliased " +
			"(detach_alias_test.go) and TestForkSelfReferenceThroughAliasedHeaderStaysAliased " +
			"(fork_mapalias_test.go), whose fixtures build this shape by hand.",
	},
	{
		Func:   "Splice",
		Sites:  1,
		Reason: "the same intentional second header as Quote, flagged spliced rather than quoted.",
	},
	{
		Func:   "shallowUnquote",
		Sites:  1,
		Reason: "the same intentional second header as Quote, clearing the flag rather than setting it.",
	},
	{
		Func:  "FunRef",
		Sites: 1,
		Reason: "a second name for ONE function: the copy shares *funData deliberately, which is what " +
			"makes the reference a reference. Rebuilding it would give the alias its own function.",
	},
	{
		Func:  "builtinSortStable",
		Sites: 1,
		Reason: "the empty carve-out. The source is the shared sealed EMPTY list, which carries no payload, " +
			"and Cells is set to nil on the next line -- so nothing is shared onward. The copy exists only " +
			"to hand back a fresh unsealed header rather than the sealed singleton.",
	},
	{
		Func:  "(*formalsCopier).copy",
		Sites: 2,
		Reason: "block storage the call owns. Both copies land in c.vals, allocated by newFormalsCopier for " +
			"this registration and unreachable by anything else; the source is read-only, the subject is a " +
			"symbol list with no payload, and both sites already carry //elps:mutates annotations saying so.",
	},
	{
		Func:  "(*templateCompiler).value",
		Sites: 2,
		Reason: "the plan's header, STRIPPED before publication: the copy exists to take v's scalar fields, " +
			"and the three lines after it drop everything that could carry storage across -- Native and " +
			"macroExpansion are nilled, and Cells is replaced by the admitted view, an owned span or nil " +
			"before `out.header = *header` publishes it into the plan (the second site).",
	},
	{
		Func:  "(*templatePlan).instantiate",
		Sites: 1,
		Reason: "a write into a PRIVATE per-VM allocation: instance.values comes from templateObjects, which " +
			"news one LVal per plan entry for this instantiation alone, and every pointer field written into " +
			"it names another object of the same private instance. The site already carries the matching " +
			"//elps:mutates annotation.",
	},
	{
		Func:  "TestTemplateCompilerRejectsMissingAdmittedIdentity",
		Sites: 1,
		Reason: "a read-only field SNAPSHOT, not a header: `original := *leaf` is compared field-by-field " +
			"against leaf after admission to show the rejected scan left it untouched, and the copy is " +
			"never published, mutated or given a payload of its own.",
	},
	{
		Func:  "TestCopySelfReferenceThroughAliasedHeaderStaysAliased",
		Sites: 1,
		Reason: "a FIXTURE that builds the two-headers-over-one-*MapData shape on purpose, so the test can " +
			"assert the walker preserves it. The copy is the thing under test.",
	},
	{
		Func:   "TestCopyClonesANativePayloadOncePerPayload",
		Sites:  1,
		Reason: "the same fixture for a pointer native payload rather than a map.",
	},
	{
		Func:   "TestForkSelfReferenceThroughAliasedHeaderStaysAliased",
		Sites:  1,
		Reason: "the template-snapshot half of the *MapData fixture above.",
	},
}

// registeredLValCopyExemptions returns the struct-copy allowlist, copied.
func registeredLValCopyExemptions() []lvalCopyExemption {
	out := make([]lvalCopyExemption, len(lvalCopyExemptions))
	copy(out, lvalCopyExemptions)
	return out
}
