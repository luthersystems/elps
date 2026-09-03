// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"errors"
	"fmt"
	"io"
)

// Template-environment forking (issue #380).
//
// A fully loaded environment — stdlib loaded, program loaded, ready to
// evaluate — is expensive to rebuild from source but cheap to clone, because
// most of what it holds is sealed program structure that can never change
// again (see docs/sealed-ast.md and lisp/seal.go).  Fork exploits that: it
// produces an independent environment tree on a fresh Runtime, sharing every
// sealed node with the template and hermetically copying only the mutable
// fraction.  At production scale the mutable fraction is small (~17% of
// reachable values in a measured production phylum, and shrinking as
// programs grow, since program AST is sealed parser output), which is why a
// fork costs a fraction of a full load.
//
// Sharing policy, per value class:
//
//   - sealed values: SHARED.  Safety rests on the existing seal invariant —
//     a sealed node's bytes never change after parsing completes — which is
//     machine-verified elsewhere (fingerprint oracle, checked-mode
//     inspector, -race watchdog; see docs/sealed-ast.md §3).
//   - the three singletons (nil, true, false): shared by decree.
//   - LFun: header copy; the funData is rebuilt with the captured
//     environment remapped onto the fork's copies so closures close over
//     fork state, never template state.  Builtin Go funcs travel by
//     reference (Go code is immutable).  Formals and lambda bodies are
//     sealed parser output and therefore shared.
//   - LNative payloads: SHARED by default.  A native handle cannot be
//     hermetically copied by the kernel (the same reason detach rejects
//     it), and most native payloads held by loaded environments are
//     genuinely immutable (compiled regexps, timestamps).  Stateful handles
//     opt into fork-time duplication by implementing NativeCloner, or the
//     embedder substitutes them per fork with ForkWithNativeReplacer.
//   - all other mutable data: hermetically copied (bytes backings, sorted
//     map storage, error call stacks), memoized so aliasing and cycles
//     inside the template are reproduced exactly inside the fork.
//   - source locations and format metadata (Meta): shared — read-only after
//     parse.
//   - macro-expansion debug metadata: dropped.  It is only populated while
//     a debugger is attached and its expansion contexts alias values in the
//     template's runtime.
//
// The forked tree lives on a DISTINCT Runtime.  That is not an
// implementation detail, it is the concurrency contract: the Runtime doc
// comment promises one Runtime (and LEnv tree) per goroutine, and a fork is
// exactly a new such tree.  Under `-tags elpscheck` the ownership checker
// enforces the model, with sealed values exempted as the sanctioned
// cross-runtime class (see lisp/ownership_check_elpscheck.go).
//
// Fork never mutates the template: the walk only reads.  Concurrent Fork
// calls against the same quiescent template are therefore safe.
//
// See docs/fork.md for the embedder contract (quiescence, stateful natives,
// pre-hook ordering, pool-refill and test-runner patterns) and measured
// numbers.

// NativeCloner is the kernel's opt-in duplication protocol for native
// payloads, shared with the broader native-contract design of issue #383.
// The kernel cannot copy an LVal's Native payload — it is an opaque
// interface{} — so every primitive that duplicates a value shares payloads
// by reference by default.  A payload type whose identity or state must NOT
// be shared with the duplicate (an accumulator, a stateful handle)
// implements NativeCloner, and one implementation settles the question
// everywhere the kernel copies a value: Fork, the lisp `copy` builtin
// (deepCopy, lisp/copy.go) and detach (lisp/detach.go), which clones such a
// payload rather than refusing it (issue #546).
//
// CloneNative must return a payload that is independent of the receiver:
// mutations on either side must be invisible to the other.  It must also
// retain no reference into the Runtime or LEnv tree the receiver lives in:
// Fork and detach both land the clone on a DIFFERENT runtime, where such a
// reference reconnects the two trees the primitive exists to separate.
// Under `copy` the duplicate stays inside one environment, so a payload
// written for that path alone still has to meet the stricter bar to be
// fork-safe.
type NativeCloner interface {
	CloneNative() interface{}
}

// ForkOption configures a single Fork call.
type ForkOption func(*forkConfig)

type forkConfig struct {
	ctx            context.Context
	stderr         io.Writer
	nativeReplacer func(payload interface{}) (interface{}, bool)
}

// ForkWithContext rebinds the fork's evaluation context: the returned
// environment starts with ctx as its context.Context, in place of the
// template's (which does not travel — a fork never inherits the template's
// context).  Use this at pool-checkout time to bind a request-scoped
// context to the fork serving that request.  For per-call control the
// *Context methods on LEnv work on a fork exactly as on any environment.
func ForkWithContext(ctx context.Context) ForkOption {
	return func(c *forkConfig) { c.ctx = ctx }
}

// ForkWithStderr sets the forked Runtime's Stderr.  Without this option the
// fork shares the template's Stderr writer.  A fork-served test runner
// wants this: each test's environment must write diagnostics to that test's
// logger, not to the template's.
func ForkWithStderr(w io.Writer) ForkOption {
	return func(c *forkConfig) { c.stderr = w }
}

// ForkWithNativeReplacer installs a per-fork substitution hook for native
// payloads.  For every native payload the fork walker encounters, fn is
// consulted first: returning (replacement, true) stores replacement in the
// forked value; returning (_, false) falls through to the NativeCloner
// protocol and then to the default share-by-reference policy.
//
// This is the escape hatch for payload types the embedder cannot modify to
// implement NativeCloner (third-party handles), and for rebinding
// fork-specific instances (a per-fork storage handle, a per-test
// accumulator).  fn may be called more than once for payloads reachable
// through multiple values; it must be pure with respect to the template
// (never mutate the payload it is given).
func ForkWithNativeReplacer(fn func(payload interface{}) (interface{}, bool)) ForkOption {
	return func(c *forkConfig) { c.nativeReplacer = fn }
}

// Fork clones a quiescent, fully loaded template environment, returning an
// independent environment on a fresh Runtime.  Sealed structure (program
// AST, formals, quoted literals — the large majority of a loaded
// environment) is shared with the template; mutable data is hermetically
// copied; closures are remapped so they capture fork state.  The entire
// package registry reachable from env's Runtime is forked along with the
// environment tree, and the returned environment is the fork's counterpart
// of env itself (embedders fork the root environment they were handed at
// load time).
//
// The template must be QUIESCENT: fully loaded, with no evaluation in
// flight (empty call stack, no active eval entry, no pending condition
// handlers).  Fork returns an error otherwise.  There is deliberately no
// way to bypass the check — forking a mid-evaluation environment would
// capture torn state.
//
// Fork never mutates the template.  The template remains fully usable, and
// concurrent Fork calls against the same quiescent template are safe; but
// Fork must not race with evaluation on the template (quiescence is
// asserted, not synchronized).
//
// The forked Runtime starts with a fresh call stack and empty condition
// stack, inherits the template's limit configuration (MaxAlloc, stack
// bounds, step budget configuration, ...), and continues the template's
// environment-ID and gensym counters so identifiers minted after the fork
// (lambda FIDs, gensyms) can never collide with identifiers the fork
// inherited.  Profiler and Debugger do not travel: a fork starts with
// neither, and the embedder attaches its own if wanted.  The Reader,
// SourceLibrary and LoadCache are shared (a reader cache is deliberately
// process-wide; the source library is read-only at runtime; and a load
// cache's entries are immutable, sealed, and explicitly safe to serve to
// any number of Runtimes -- see lisp/loadcache.go), as is Stderr unless
// ForkWithStderr overrides it.  Step accounting (Runtime.TotalSteps) starts
// at zero.
//
// LoadCache travels for the same reason Reader does, and the reason is the
// topology: "preheat a template, fork per environment" is elps's own
// recommended shape, and it is the exact shape the load cache exists to
// serve.  A fork that dropped the cache would reparse every file the
// template had already parsed -- silently, since nothing fails -- which is
// the cost this hook was added to remove.  The per-Runtime re-entrancy
// guard (loadCacheActive) is deliberately NOT copied: it is state about a
// load in progress, and the template is quiescent.
func (env *LEnv) Fork(opts ...ForkOption) (*LEnv, error) {
	if env == nil || env.Runtime == nil {
		return nil, errors.New("fork: nil environment or runtime")
	}
	var config forkConfig
	for _, opt := range opts {
		opt(&config)
	}
	old := env.Runtime
	if err := checkQuiescent(old); err != nil {
		return nil, err
	}
	if old.Registry == nil {
		return nil, errors.New("fork: template runtime has no package registry")
	}
	newRT := &Runtime{
		Registry: NewRegistry(),
		Stderr:   old.Stderr,
		Stack: &CallStack{
			MaxHeightLogical:  old.Stack.MaxHeightLogical,
			MaxHeightPhysical: old.Stack.MaxHeightPhysical,
			MaxTailIterations: old.Stack.MaxTailIterations,
		},
		Reader:                 old.Reader,
		Library:                old.Library,
		LoadCache:              old.LoadCache,
		MaxAlloc:               old.MaxAlloc,
		MaxMacroExpansionDepth: old.MaxMacroExpansionDepth,
		MaxEvalNesting:         old.MaxEvalNesting,
		MaxSleep:               old.MaxSleep,
		maxSteps:               old.maxSteps,
	}
	if config.stderr != nil {
		newRT.Stderr = config.stderr
	}
	newRT.Registry.Lang = old.Registry.Lang
	// Counter continuity: the fork's env-ID counter continues past the
	// template's so post-fork lambdas mint FIDs ("_fun<envID>") that cannot
	// collide with FIDs inherited from the template (FIDs key funNames
	// tables and participate in tail-call TerminalFID matching), and the
	// gensym counter continues so runtime gensyms cannot re-mint load-time
	// gensym names.  The template is quiescent, so a plain copy of the
	// atomic counters is stable.
	//
	// The Runtime's third monotonic counter, macroExpSeq, deliberately does
	// NOT continue: it numbers macro-expansion debug records, which are only
	// minted under an attached debugger and which a fork drops wholesale
	// (see val).  A fork starts with an empty macro-expansion ID space, so
	// unlike env IDs and gensyms it has nothing inherited to collide with.
	newRT.numenv = old.numenv
	newRT.numsym = old.numsym

	f := &forker{
		rt:             newRT,
		envs:           make(map[*LEnv]*LEnv, 256),
		vals:           make(map[*LVal]*LVal, 4096),
		maps:           make(map[*MapData]*MapData, 64),
		nativeReplacer: config.nativeReplacer,
	}
	newRoot := f.env(env)
	for name, opkg := range old.Registry.packages {
		newRT.Registry.packages[name] = f.pkg(opkg)
	}
	if old.Package != nil {
		newRT.Package = newRT.Registry.packages[old.Package.Name]
	}
	if config.ctx != nil {
		newRoot.evalCtx = config.ctx
	}
	return newRoot, nil
}

// checkQuiescent asserts that rt has no evaluation in flight.  A template
// is forkable only between evaluations: a non-empty call stack, an active
// eval entry, or a pending condition handler all mean values reachable from
// the environment are mid-mutation and a fork would capture torn state.
func checkQuiescent(rt *Runtime) error {
	if rt.Stack == nil {
		return errors.New("fork: template runtime has no call stack")
	}
	if n := len(rt.Stack.Frames); n != 0 {
		return fmt.Errorf("fork: template not quiescent: call stack height %d", n)
	}
	if rt.evalDepth != 0 {
		return fmt.Errorf("fork: template not quiescent: eval depth %d", rt.evalDepth)
	}
	if n := len(rt.conditionStack); n != 0 {
		return fmt.Errorf("fork: template not quiescent: %d pending condition handlers", n)
	}
	return nil
}

// forker performs one Fork call's graph walk.  The three memo tables are the
// heart of the algorithm: each maps a template object to its fork-side copy
// and is seeded BEFORE descending into the object's children, so values
// reachable along multiple paths map to one copy (aliasing is reproduced,
// not multiplied) and reference cycles — labels mutual recursion,
// closure↔environment cycles — terminate instead of recursing forever.
//
// maps is keyed on the *MapData, not the *LVal header over it, because the
// two are not one-to-one: Quote, Splice, shallowUnquote and opQuasiquote copy
// an LVal's struct and keep its Native, so `(quasiquote (unquote a))` is a
// second header on a's map.  Memoising per header alone rebuilt such a map
// once per header, and a write through the fork's `a` was invisible through
// the fork's `b` (issue #576).  A map reaching itself through a second
// header is the same bug at its sharpest: the *LVal memo never sees the
// cycle, because every visit arrives through a header it has not met.
type forker struct {
	rt             *Runtime
	envs           map[*LEnv]*LEnv
	vals           map[*LVal]*LVal
	maps           map[*MapData]*MapData
	nativeReplacer func(payload interface{}) (interface{}, bool)
}

// pkg clones one package, remapping symbol values through the walker.
//
// The three metadata tables beside symbols — symbolDocs, funNames,
// externals — are REBUILT, not shared, and the reason is the same for all
// three: the template is a live writer of each of them, and Fork's contract
// says the template "remains fully usable" after a fork.  Sharing a map
// with a live writer is not a stale-read hazard, it is the issue #397
// hazard: a fork reading pkg.funNames while the template's putName writes
// it is a concurrent map read and map write, which the Go runtime turns
// into a fatal throw that neither recover() nor handler-bind can intercept.
// Making a share safe would take a copy-on-write flag that Fork sets on the
// TEMPLATE's package — a template mutation, and a racing one under the
// concurrent Fork calls this function is documented to support.  So the
// per-package tables stay copies.  Measured on
// BenchmarkEnvConstruction/mode=fork over a fully loaded environment (13
// packages, 389 funNames entries): sharing funNames instead of copying it
// is worth -45 allocs/op of 1273, and sharing externals another -14, which
// is the price of keeping the template writable.  See the same
// optimization-left-on-the-table note in lisp/loader.go.
//
// symbolDocs is the exception, and not by sharing: it is allocated lazily
// (lisp/package.go), so an undocumented package now forks with a nil table
// instead of an empty map.
func (f *forker) pkg(p *Package) *Package {
	np := &Package{
		Name:     p.Name,
		Doc:      p.Doc,
		symbols:  make(map[string]*LVal, len(p.symbols)),
		funNames: make(map[string]string, len(p.funNames)),
	}
	for k, v := range p.symbols {
		np.symbols[k] = f.val(v)
	}
	if len(p.symbolDocs) > 0 {
		np.symbolDocs = make(map[string]string, len(p.symbolDocs))
		for k, v := range p.symbolDocs {
			np.symbolDocs[k] = v
		}
	}
	for k, v := range p.funNames {
		np.funNames[k] = v
	}
	if len(p.externals) > 0 {
		np.externals = append([]string(nil), p.externals...)
	}
	return np
}

// env clones one environment node, remapping its scope values and parent
// chain.  The template's env IDs are preserved: they were unique within the
// template's runtime, so they are unique within the fork's, and inherited
// FIDs ("_fun<envID>") keep referring to the right environments.
//
// The scope map is COPIED and always will be: it is the binding table, the
// most mutable thing an environment owns, and both sides write it.  There
// is no immutable subset to carve off cheaply — a binding's key is live
// program state, not sealed structure — so the per-env scope rebuild stands.
func (f *forker) env(e *LEnv) *LEnv {
	if e == nil {
		return nil
	}
	if ne, ok := f.envs[e]; ok {
		return ne
	}
	ne := &LEnv{
		// loc DOES NOT TRAVEL, and it is not shared either: a fork starts
		// with no current evaluator location at all.
		//
		// e.loc is not state the environment owns, it is the evaluator's
		// location register — eval rebinds it to v.source on every step
		// (see the //elps:aliases note on newEnvN, which aliases the
		// parent's register for the same reason: "both registers are
		// rebound on every eval step").  What a quiescent template holds
		// there is the leftover location of the last node it evaluated
		// before the fork, and a fork is not evaluating that node.  It is
		// transient per-evaluation state of exactly the kind Fork already
		// drops: the call stack starts empty, the condition stack starts
		// empty, TotalSteps starts at zero, and evalCtx — the OTHER
		// transient register on this struct — is not carried across
		// either.  Dropping it is strictly more hermetic than the copy it
		// replaces (nothing of the template's reaches the fork, not even a
		// duplicated Location's contents), and it costs the walk one
		// allocation per environment rather than one per distinct
		// location: 557 of them per fork in the downstream template that
		// motivated this (issue #440).
		//
		// The first eval in the fork sets it, so the only observable
		// difference is an error raised in a forked environment BEFORE it
		// evaluates anything: that error now reports "<native code>"
		// (the nil-location convention) instead of a source position
		// inherited from the template's last evaluation, which was never
		// this environment's position to report.
		scope:   make(map[string]*LVal, len(e.scope)),
		Runtime: f.rt,
		ID:      e.ID,
	}
	// Memo before descending: closures in scope may capture e.
	f.envs[e] = ne
	for k, v := range e.scope {
		ne.scope[k] = f.val(v)
	}
	ne.parent = f.env(e.parent)
	return ne
}

// val maps one template value to its fork-side counterpart: sealed values
// and singletons are shared, funs are remapped, native payloads follow the
// replacer/NativeCloner/share policy, and everything else is hermetically
// copied.
func (f *forker) val(v *LVal) *LVal {
	if v == nil {
		return nil
	}
	if isSingleton(v) {
		return v
	}
	if v.sealed && sealableNodeType(v.Type) {
		// Immutable by the seal invariant: shared for free.  This branch is
		// the reason forking beats reloading — at production scale it takes
		// the overwhelming majority of reachable values (see docs/fork.md).
		//
		// The test is the CONJUNCTION, not the flag alone, and it is the same
		// conjunction the admission gate (firstUnsealed, lisp/program.go) and
		// the checked-mode ownership gate (lisp/ownership_check_elpscheck.go)
		// use.  The seal's guarantees only cover the types SealAST actually
		// marks; a node whose flag is set but whose type is mutable or
		// reference (an LFun closure laundered in through a Reader, whose
		// captured *LEnv the seal never freezes) would be SHARED between the
		// template and every fork instead of remapped, silently reconnecting
		// the two environments this function exists to separate.  This is the
		// permissive direction, which is why it matters: the seal's other
		// consumers (the guarded mutation sites, stampGuarded, SetSource) read
		// the flag PROTECTIVELY, where a laundered flag only ever buys an
		// extra refusal.
		return v
	}
	if cp, ok := f.vals[v]; ok {
		return cp
	}
	cp := &LVal{}
	// Struct copy carries the scalar payloads and unexported flags (quoted,
	// spliced) and shares the read-only metadata (source location, Meta).
	*cp = *v
	// Memo before descending: v's children may reach back to v.
	f.vals[v] = cp
	// Macro-expansion debug metadata does not travel; it is only populated
	// under an attached debugger and its contexts alias template values.
	cp.macroExpansion = nil
	switch v.Type {
	case LFun:
		if fd, ok := v.Native.(*funData); ok && fd != nil {
			cp.Native = &funData{
				builtin: fd.builtin, // Go code: travels by reference
				fid:     fd.fid,
				pkg:     fd.pkg,
				env:     f.env(fd.env),
			}
		}
	case LNative:
		cp.Native = f.native(v.Native)
	default:
		switch native := v.Native.(type) {
		case nil:
		case *[]byte:
			if native != nil {
				b := append([]byte(nil), *native...)
				cp.Native = &b
			}
		case *MapData:
			cp.Native = f.mapData(native)
		case *CallStack:
			// An LError's recorded stack: deep-copied so the fork's error
			// values do not alias template frame storage.
			cp.Native = detachCallStack(native)
		default:
			// A payload the kernel has no copy strategy for, riding on a
			// non-LNative type.  Apply the native policy: the embedder put
			// it there, the embedder's protocol decides.
			cp.Native = f.native(v.Native)
		}
	}
	if len(v.Cells) > 0 {
		cells := make([]*LVal, len(v.Cells))
		for i, c := range v.Cells {
			cells[i] = f.val(c)
		}
		cp.Cells = cells
	} else {
		// The struct copy above aliased v.Cells' slice HEADER, and a
		// zero-length slice can still carry spare capacity pointing into the
		// template's backing array ((list) allocates len 0 / cap 2).  elps
		// grows cell slices in place -- append! extends a vector's storage,
		// and (append 'vector seq x) deliberately appends into a sequence's
		// spare capacity -- so keeping the header would let a fork-side
		// append write template memory and vice versa.  Drop it, exactly as
		// detach does (detachCells returns nil for an empty slice).
		cp.Cells = nil
	}
	return cp
}

// native resolves the fork policy for one native payload: the per-fork
// replacer hook first, the NativeCloner protocol second, share-by-reference
// last.
//
// Whichever of the three produced it, the RESOLVED payload is then checked
// against the fork's own runtime (checkNativeAffinity — a no-op in
// production builds, the established pattern for checkOwnership's
// unconditional calls from Put and eval).  A payload that declares a
// binding to another Runtime must not travel into the fork, and this is the
// deep half of that rule: the walk reaches natives riding inside containers,
// which the use-time checks are too shallow to see (issue #546,
// lisp/runtime_bound.go).  A replacer's return value is checked like any
// other — an embedder's hook handing back a template-bound instance is
// precisely the bug class, not an exception to it.
func (f *forker) native(payload interface{}) interface{} {
	if payload == nil {
		return nil
	}
	resolved, replaced := payload, false
	if f.nativeReplacer != nil {
		if replacement, ok := f.nativeReplacer(payload); ok {
			resolved, replaced = replacement, true
		}
	}
	if !replaced {
		if cloner, ok := payload.(NativeCloner); ok {
			resolved = cloner.CloneNative()
		}
	}
	checkNativeAffinity(f.rt, resolved)
	return resolved
}

// mapData rebuilds md as a fresh sorted map whose keys and values are
// remapped through the walker (unlike detachMapData, entries may legally
// contain funs and natives — the walker applies fork policy to them).
//
// Memoised per *MapData, and seeded BEFORE the entries are walked, for the
// same two reasons f.vals is: a map reachable through several headers maps
// to one clone, and a map that reaches itself terminates (issue #576; see
// the forker doc).
func (f *forker) mapData(md *MapData) *MapData {
	if md == nil {
		return nil
	}
	if cp, ok := f.maps[md]; ok {
		return cp
	}
	if md.mapBacking == nil {
		// Degenerate MapData with no implementation (possible via
		// SortedMapFromData(NewMapData(nil))): fresh struct, nil backing preserved.
		cp := &MapData{}
		f.maps[md] = cp
		return cp
	}
	entries := sortedMapEntries(md)
	if entries.Type == LError {
		// Unreachable for the stock sorted map (its entry enumeration
		// cannot fail); loud beats silent corruption if an embedder map
		// implementation ever gets here.
		panic(fmt.Sprintf("fork: sorted-map entries cannot be enumerated: %v", entries))
	}
	m := NewMapData(newmap())
	f.maps[md] = m // memo before descending, as above
	for _, pair := range entries.Cells {
		if lerr := m.Set(f.val(pair.Cells[0]), f.val(pair.Cells[1])); lerr.Type == LError {
			panic(fmt.Sprintf("fork: sorted-map key %s cannot be stored: %v", pair.Cells[0], lerr))
		}
	}
	return m
}
