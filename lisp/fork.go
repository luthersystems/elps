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

// NativeCloner is the opt-in fork-time duplication protocol for native
// payloads, shared with the broader native-contract design of issue #383.
// The kernel cannot copy an LVal's Native payload — it is an opaque
// interface{} — so Fork shares payloads by reference by default.  A payload
// type whose identity or state must NOT be shared between a template and
// its forks (an accumulator, a stateful handle) implements NativeCloner;
// Fork then stores the value returned by CloneNative in the forked LVal
// instead of the shared reference.
//
// CloneNative must return a payload that is independent of the receiver:
// mutations on either side must be invisible to the other.  It must not
// retain references into the template's Runtime or LEnv tree — the clone
// crosses a runtime boundary.
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
// neither, and the embedder attaches its own if wanted.  The Reader and
// SourceLibrary are shared (a reader cache is deliberately process-wide;
// the source library is read-only at runtime), as is Stderr unless
// ForkWithStderr overrides it.  Step accounting (Runtime.TotalSteps) starts
// at zero.
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

// forker performs one Fork call's graph walk.  The two memo tables are the
// heart of the algorithm: each maps a template object to its fork-side copy
// and is seeded BEFORE descending into the object's children, so values
// reachable along multiple paths map to one copy (aliasing is reproduced,
// not multiplied) and reference cycles — labels mutual recursion,
// closure↔environment cycles — terminate instead of recursing forever.
type forker struct {
	rt             *Runtime
	envs           map[*LEnv]*LEnv
	vals           map[*LVal]*LVal
	nativeReplacer func(payload interface{}) (interface{}, bool)
}

// pkg clones one package, remapping symbol values through the walker.
func (f *forker) pkg(p *Package) *Package {
	np := &Package{
		Name:       p.Name,
		Doc:        p.Doc,
		symbols:    make(map[string]*LVal, len(p.symbols)),
		symbolDocs: make(map[string]string, len(p.symbolDocs)),
		funNames:   make(map[string]string, len(p.funNames)),
	}
	for k, v := range p.symbols {
		np.symbols[k] = f.val(v)
	}
	for k, v := range p.symbolDocs {
		np.symbolDocs[k] = v
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
func (f *forker) env(e *LEnv) *LEnv {
	if e == nil {
		return nil
	}
	if ne, ok := f.envs[e]; ok {
		return ne
	}
	ne := &LEnv{
		// The env location is mutable state: the evaluator rebinds and
		// mutates its own in place (#382), so a fork that aliased the
		// template's pointer would let each runtime's evaluation write
		// through into the other's.  Copy it, exactly as the kernel's
		// other env-location consumers now do (ErrorCondition,
		// ErrorConditionf, ErrorAssociate).
		loc:     copyLocation(e.loc),
		scope:   make(map[string]*LVal, len(e.scope)),
		funName: make(map[string]string, len(e.funName)),
		Runtime: f.rt,
		ID:      e.ID,
	}
	// Memo before descending: closures in scope may capture e.
	f.envs[e] = ne
	for k, v := range e.scope {
		ne.scope[k] = f.val(v)
	}
	for k, v := range e.funName {
		ne.funName[k] = v
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
	if v.sealed {
		// Immutable by the seal invariant: shared for free.  This branch is
		// the reason forking beats reloading — at production scale it takes
		// the overwhelming majority of reachable values (see docs/fork.md).
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
func (f *forker) native(payload interface{}) interface{} {
	if payload == nil {
		return nil
	}
	if f.nativeReplacer != nil {
		if replacement, ok := f.nativeReplacer(payload); ok {
			return replacement
		}
	}
	if cloner, ok := payload.(NativeCloner); ok {
		return cloner.CloneNative()
	}
	return payload
}

// mapData rebuilds md as a fresh sorted map whose keys and values are
// remapped through the walker (unlike detachMapData, entries may legally
// contain funs and natives — the walker applies fork policy to them).
func (f *forker) mapData(md *MapData) *MapData {
	if md == nil {
		return nil
	}
	if md.mapBacking == nil {
		// Degenerate MapData with no implementation (possible via
		// SortedMapFromData(NewMapData(nil))): fresh struct, nil backing preserved.
		return &MapData{}
	}
	entries := sortedMapEntries(md)
	if entries.Type == LError {
		// Unreachable for the stock sorted map (its entry enumeration
		// cannot fail); loud beats silent corruption if an embedder map
		// implementation ever gets here.
		panic(fmt.Sprintf("fork: sorted-map entries cannot be enumerated: %v", entries))
	}
	m := NewMapData(newmap())
	for _, pair := range entries.Cells {
		if lerr := m.Set(f.val(pair.Cells[0]), f.val(pair.Cells[1])); lerr.Type == LError {
			panic(fmt.Sprintf("fork: sorted-map key %s cannot be stored: %v", pair.Cells[0], lerr))
		}
	}
	return m
}
