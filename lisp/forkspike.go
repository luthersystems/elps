// Copyright © 2026 The ELPS authors

// SPIKE (issue #380): in-package template-environment fork prototype.
//
// NOT production code.  This file extends the exported-API fork spike
// (lisp/lisplib/forkspike_test.go) to the shape a real lisp/fork.go would
// take: direct map clones instead of Package.Put, counter continuity by
// direct copy, and a quiescence assertion.  It exists to produce the
// phylum-scale fork-vs-load measurement the #380 decision needs; every
// policy shortcut is marked SPIKE below.
//
// Sharing policy (matches the #380 design comment):
//   - sealed values: shared (the point of the design; safety rests on the
//     seal invariant, machine-verified elsewhere).
//   - singletons: shared by decree.
//   - LFun: header copy, cells remapped, fresh LFunData with the captured
//     env remapped onto the fork's copies.  Builtin Go funcs travel.
//   - LNative payloads: SHARED and counted (Detach's reject case; fork
//     policy is share+document, matching how embedder natives are already
//     process-shared today).
//   - other mutable data: hermetic copy (bytes backing, sortmap data,
//     error call stacks), memoized so internal aliasing and cycles are
//     preserved inside the fork.
//   - source locations and Meta: SHARED (read-only after parse; Detach
//     copies them for hermetic transfer, a fork does not need that).
package lisp

import (
	"fmt"
	"hash"
	"hash/fnv"
	"sort"
)

// SpikeForkStats reports what one SpikeFork call touched.
type SpikeForkStats struct {
	EnvsRemapped      int // fresh LEnv copies created
	ValsRemapped      int // fresh LVal copies created (shared nodes excluded)
	SharedNatives     int // LNative payloads shared into the fork by policy
	SharedNativeTypes map[string]int
}

// SpikeFork clones a quiescent, fully loaded template environment into a
// fresh Runtime, sharing all sealed structure and copying only the mutable
// fraction.  root must be the env handed to the embedder at load time; the
// entire package registry reachable from root.Runtime is forked with it.
//
// SPIKE: error handling, option plumbing (Reader/Stderr/Profiler policy),
// and API shape are all placeholders for a real lisp/fork.go.
func SpikeFork(root *LEnv) (*LEnv, *SpikeForkStats, error) {
	if root == nil || root.Runtime == nil {
		return nil, nil, fmt.Errorf("forkspike: nil env or runtime")
	}
	old := root.Runtime
	// Template quiescence (invariant #2): no in-flight evaluation.
	if n := len(old.Stack.Frames); n != 0 {
		return nil, nil, fmt.Errorf("forkspike: template not quiescent: stack height %d", n)
	}
	if old.evalDepth != 0 {
		return nil, nil, fmt.Errorf("forkspike: template not quiescent: eval depth %d", old.evalDepth)
	}
	newRT := &Runtime{
		Registry: NewRegistry(),
		Stderr:   old.Stderr, // SPIKE: shared; production wants an option
		Stack: &CallStack{
			MaxHeightLogical:  old.Stack.MaxHeightLogical,
			MaxHeightPhysical: old.Stack.MaxHeightPhysical,
			MaxTailIterations: old.Stack.MaxTailIterations,
		},
		// Reader and Library are shared: the reader is stateless or an
		// embedder-managed cache (substrate's parse cache is deliberately
		// process-shared), and the source library is read-only at runtime.
		Reader:                 old.Reader,
		Library:                old.Library,
		Profiler:               nil, // profilers/debuggers do not travel
		Debugger:               nil,
		MaxAlloc:               old.MaxAlloc,
		MaxMacroExpansionDepth: old.MaxMacroExpansionDepth,
		MaxEvalNesting:         old.MaxEvalNesting,
		MaxSleep:               old.MaxSleep,
		maxSteps:               old.maxSteps,
	}
	newRT.Registry.Lang = old.Registry.Lang
	// Counter continuity (invariant #1): the fork's env-ID counter must
	// continue past the template's so post-fork lambdas mint fresh FIDs
	// ("_fun<envID>"), and the gensym counter must continue so runtime
	// gensyms cannot re-mint load-time names.  Quiescent template, so a
	// plain struct copy of the atomic counters is safe.
	newRT.numenv = old.numenv
	newRT.numsym = old.numsym

	f := &spikeForker{
		rt:    newRT,
		envs:  make(map[*LEnv]*LEnv, 256),
		vals:  make(map[*LVal]*LVal, 4096),
		stats: &SpikeForkStats{SharedNativeTypes: make(map[string]int)},
	}
	newRoot := f.env(root)
	for name, opkg := range old.Registry.packages {
		newRT.Registry.packages[name] = f.pkg(opkg)
	}
	if old.Package != nil {
		newRT.Package = newRT.Registry.packages[old.Package.Name]
	}
	f.stats.EnvsRemapped = len(f.envs)
	f.stats.ValsRemapped = len(f.vals)
	return newRoot, f.stats, nil
}

type spikeForker struct {
	rt    *Runtime
	envs  map[*LEnv]*LEnv
	vals  map[*LVal]*LVal
	stats *SpikeForkStats
}

func (f *spikeForker) pkg(p *Package) *Package {
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

func (f *spikeForker) env(e *LEnv) *LEnv {
	if e == nil {
		return nil
	}
	if ne, ok := f.envs[e]; ok {
		return ne
	}
	ne := &LEnv{
		Loc:     e.Loc,
		Scope:   make(map[string]*LVal, len(e.Scope)),
		FunName: make(map[string]string, len(e.FunName)),
		Runtime: f.rt,
		ID:      e.ID, // unique within the fork: IDs were unique in the template
	}
	f.envs[e] = ne // memo before descending: closures may capture e
	for k, v := range e.Scope {
		ne.Scope[k] = f.val(v)
	}
	for k, v := range e.FunName {
		ne.FunName[k] = v
	}
	ne.Parent = f.env(e.Parent)
	return ne
}

func (f *spikeForker) val(v *LVal) *LVal {
	if v == nil {
		return nil
	}
	if isSingleton(v) {
		return v
	}
	if v.sealed {
		return v // immutable by the seal invariant: shared for free
	}
	if cp, ok := f.vals[v]; ok {
		return cp
	}
	cp := &LVal{}
	*cp = *v // struct copy keeps unexported flags (Quoted, source, ...)
	f.vals[v] = cp
	// Debugger-only metadata does not travel (its context aliases values in
	// the template runtime).
	cp.MacroExpansion = nil
	switch v.Type {
	case LFun:
		if fd, ok := v.Native.(*LFunData); ok && fd != nil {
			cp.Native = &LFunData{
				Builtin: fd.Builtin, // Go code: travels
				FID:     fd.FID,
				Package: fd.Package,
				Env:     f.env(fd.Env),
			}
		}
	case LNative:
		// SPIKE policy: share the payload, count it.  The header copy above
		// already aliases v.Native.
		f.stats.SharedNatives++
		f.stats.SharedNativeTypes[fmt.Sprintf("%T", v.Native)]++
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
			cp.Native = detachCallStack(native)
		default:
			// Unknown payload on a non-LNative type: share it and count it
			// with the natives so the measurement stays honest.
			f.stats.SharedNatives++
			f.stats.SharedNativeTypes[fmt.Sprintf("%T", v.Native)]++
		}
	}
	if len(v.Cells) > 0 {
		cells := make([]*LVal, len(v.Cells))
		for i, c := range v.Cells {
			cells[i] = f.val(c)
		}
		cp.Cells = cells
	}
	return cp
}

func (f *spikeForker) mapData(md *MapData) *MapData {
	if md == nil {
		return nil
	}
	if md.Map == nil {
		return &MapData{}
	}
	entries := sortedMapEntries(md)
	if entries.Type == LError {
		// Cannot happen for the stock map; keep the spike loud if it does.
		panic(fmt.Sprintf("forkspike: sorted-map entries: %v", entries))
	}
	m := &MapData{newmap()}
	for _, pair := range entries.Cells {
		if lerr := m.Set(f.val(pair.Cells[0]), f.val(pair.Cells[1])); lerr.Type == LError {
			panic(fmt.Sprintf("forkspike: sorted-map set: %v", lerr))
		}
	}
	return m
}

// ---------------------------------------------------------------------------
// Measurement helpers (walker + fingerprint), exported so an embedder-side
// harness can run them against a production-scale template.
// ---------------------------------------------------------------------------

// SpikeWalkStats classifies every LVal reachable from env (its env tree plus
// the entire package registry).
type SpikeWalkStats struct {
	Total   int // distinct LVal nodes reached
	Sealed  int // sealed: shared by a fork for free
	Funs    int // unsealed LFun: header copy + env remap
	Natives int // unsealed LNative: shared by policy
	Mutable int // everything else: hermetically copied
	Envs    int // distinct LEnvs reached (scope chains + closure captures)
}

// SpikeWalk classifies the reachable LVal graph of a loaded environment.
func SpikeWalk(env *LEnv) *SpikeWalkStats {
	w := &spikeWalker{
		seenVal: make(map[*LVal]bool, 1<<16),
		seenEnv: make(map[*LEnv]bool, 256),
		stats:   &SpikeWalkStats{},
	}
	w.env(env)
	if env.Runtime != nil {
		for _, p := range env.Runtime.Registry.packages {
			for _, v := range p.symbols {
				w.val(v)
			}
		}
	}
	w.stats.Envs = len(w.seenEnv)
	return w.stats
}

type spikeWalker struct {
	seenVal map[*LVal]bool
	seenEnv map[*LEnv]bool
	stats   *SpikeWalkStats
}

func (w *spikeWalker) val(v *LVal) {
	if v == nil || w.seenVal[v] {
		return
	}
	w.seenVal[v] = true
	w.stats.Total++
	switch {
	case v.sealed:
		w.stats.Sealed++
	case v.Type == LFun:
		w.stats.Funs++
	case v.Type == LNative:
		w.stats.Natives++
	default:
		w.stats.Mutable++
	}
	for _, c := range v.Cells {
		w.val(c)
	}
	if v.Type == LFun {
		if fd, ok := v.Native.(*LFunData); ok && fd != nil {
			w.env(fd.Env)
		}
	}
	if md, ok := v.Native.(*MapData); ok && md != nil && md.Map != nil {
		entries := sortedMapEntries(md)
		if entries.Type != LError {
			for _, pair := range entries.Cells {
				w.val(pair.Cells[0])
				w.val(pair.Cells[1])
			}
		}
	}
}

func (w *spikeWalker) env(e *LEnv) {
	if e == nil || w.seenEnv[e] {
		return
	}
	w.seenEnv[e] = true
	for _, v := range e.Scope {
		w.val(v)
	}
	w.env(e.Parent)
}

// SpikeFingerprint returns a deterministic hash of all state reachable from
// env — env-tree scopes and every package symbol — suitable for detecting
// whether *any* reachable value changed.  Two envs with identical logical
// state (e.g. a template and a fresh fork) hash identically; a single
// mutation anywhere reachable changes the hash.  Pointer identity does not
// enter the hash, only structure/content, so it is comparable across
// runtimes.  LNative payloads hash as an opaque marker (their content is
// outside the LVal universe).
func SpikeFingerprint(env *LEnv) uint64 {
	h := fnv.New64a()
	fp := &spikeFingerprinter{h: h, idx: make(map[*LVal]int, 1<<16), envIdx: make(map[*LEnv]int, 256)}
	fp.env(env)
	if env.Runtime != nil {
		reg := env.Runtime.Registry
		names := make([]string, 0, len(reg.packages))
		for name := range reg.packages {
			names = append(names, name)
		}
		sort.Strings(names)
		for _, name := range names {
			p := reg.packages[name]
			fp.str("pkg:" + name)
			syms := make([]string, 0, len(p.symbols))
			for s := range p.symbols {
				syms = append(syms, s)
			}
			sort.Strings(syms)
			for _, s := range syms {
				fp.str("sym:" + s)
				fp.val(p.symbols[s])
			}
		}
	}
	return h.Sum64()
}

type spikeFingerprinter struct {
	h      hash.Hash64
	idx    map[*LVal]int
	envIdx map[*LEnv]int
}

func (fp *spikeFingerprinter) str(s string) { _, _ = fp.h.Write([]byte(s)); _, _ = fp.h.Write([]byte{0}) }

func (fp *spikeFingerprinter) val(v *LVal) {
	if v == nil {
		fp.str("<nil>")
		return
	}
	if i, ok := fp.idx[v]; ok {
		// Back-reference: preserves cycle/aliasing structure without
		// diverging and without hashing pointer identity.
		fp.str(fmt.Sprintf("ref:%d", i))
		return
	}
	fp.idx[v] = len(fp.idx)
	fp.str(fmt.Sprintf("t:%d q:%d s:%s i:%d f:%g", v.Type, boolInt(v.Quoted), v.Str, v.Int, v.Float))
	switch native := v.Native.(type) {
	case nil:
	case *[]byte:
		if native != nil {
			fp.str("bytes")
			_, _ = fp.h.Write(*native)
		}
	case *MapData:
		if native.Map != nil {
			entries := sortedMapEntries(native)
			if entries.Type != LError {
				fp.str(fmt.Sprintf("map:%d", len(entries.Cells)))
				for _, pair := range entries.Cells {
					fp.val(pair.Cells[0])
					fp.val(pair.Cells[1])
				}
			}
		}
	case *LFunData:
		fp.str("fun:" + native.FID + ":" + native.Package)
		fp.env(native.Env)
	case *CallStack:
		fp.str(fmt.Sprintf("stack:%d", len(native.Frames)))
	default:
		fp.str(fmt.Sprintf("native:%T", v.Native))
	}
	fp.str(fmt.Sprintf("cells:%d", len(v.Cells)))
	for _, c := range v.Cells {
		fp.val(c)
	}
}

func (fp *spikeFingerprinter) env(e *LEnv) {
	if e == nil {
		fp.str("env:<nil>")
		return
	}
	if i, ok := fp.envIdx[e]; ok {
		fp.str(fmt.Sprintf("envref:%d", i))
		return
	}
	fp.envIdx[e] = len(fp.envIdx)
	keys := make([]string, 0, len(e.Scope))
	for k := range e.Scope {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	fp.str(fmt.Sprintf("env:%d", len(keys)))
	for _, k := range keys {
		fp.str("k:" + k)
		fp.val(e.Scope[k])
	}
	fp.env(e.Parent)
}

func boolInt(b bool) int {
	if b {
		return 1
	}
	return 0
}
