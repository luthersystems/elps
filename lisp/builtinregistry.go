// Copyright © 2026 The ELPS authors

package lisp

import (
	"sync"
	"sync/atomic"
)

// The builtin registry (luthersystems/elps#800) records every function,
// macro and special operator as LEnv.AddBuiltins, LEnv.BindBuiltins,
// LEnv.AddMacros and LEnv.AddSpecialOps registered it, keyed by package and
// name.  A package binding can change later (set, set!, defun, a shadowing
// use-package or Put); the registry entry cannot.  Only a later
// registration of the same package and name replaces it.
//
// Identity is a registration record that forks share.  The record is the
// function value registration built (its *LVal header), stored in the
// function's own funData as reg.  The registered name is the record's Str,
// which registration sets and nothing writes after, and the package is the
// function's package, which registration sets and nothing changes.  Every
// header of the function (FunRef, Copy) shares that funData, and a template
// copies funData headers by value, so reg is the same pointer in the source
// runtime and in every VM a template mints: cold, eager, lazy or
// prewarmed.  A VM never evaluates the record or reads anything of it but
// Str and its package; it uses its own copy of the function.  So a template
// and its VMs keep the source runtime's registered headers alive, one small
// header per registration, and nothing else of the source.
//
// Registration stores the value in a chunk of builtinChunkSize: 8 bytes
// per definition and one allocation per chunk.  It builds no lookup
// structure: lookups are rare, so the index is built on the first one.

// builtinKey is a registry key.
type builtinKey struct {
	pkg, name string
}

// builtinChunkSize is the number of registrations in one chunk.
const builtinChunkSize = 64

// builtinChunk holds registrations in order.  Chunks are linked newest
// first.
type builtinChunk struct {
	prev *builtinChunk
	vals [builtinChunkSize]*LVal
	n    int
}

// builtinSlotIndex tracks current slots only after the first shadowing.
// last and n mark the indexed prefix; ordinary registration only appends.
type builtinSlotIndex struct {
	byKey   map[builtinKey]**LVal
	last    *builtinChunk
	n       int
	cleared int
}

// registrationKey returns the key of a function registration built: its
// package and its registered name.
func registrationKey(fn *LVal) builtinKey {
	return builtinKey{fn.funData().pkg, fn.Str}
}

// templateBuiltin is one registry entry in a template plan, in
// registration order.  Only the current registration of each key is
// published.
type templateBuiltin struct {
	rec   *LVal
	value templateRef
}

// templateBuiltins is a plan's registry.  It is immutable after
// publication and shared by every VM of the template.  index is built on
// the first lookup in any VM, once, and only read after.
type templateBuiltins struct {
	index   map[builtinKey]int
	entries []templateBuiltin
	once    sync.Once
}

// lookupIndex returns the shared index of the plan's entries.
func (p *templateBuiltins) lookupIndex() map[builtinKey]int {
	p.once.Do(func() {
		p.index = make(map[builtinKey]int, len(p.entries))
		for i, e := range p.entries {
			p.index[registrationKey(e.rec)] = i
		}
	})
	return p.index
}

// builtinRegistry is a runtime's registry of builtins as registered.
//
//   - last holds the registrations made in this runtime, in chunks: every
//     one in a cold environment, and those made after NewVM in a template
//     VM.  ownIndex maps each key to its current registration; it is built
//     on the first lookup and kept up to date after.  It is published
//     atomically once complete, because an eager VM and a cold runtime
//     allow concurrent reads, and a first lookup is a read.  Two
//     concurrent first lookups may both build it; each builds the same
//     map, and one wins.
//   - plan holds the entries a template VM inherited.  It belongs to the
//     template and is shared by every VM.  An own registration of the same
//     key replaces the plan entry.
//   - values holds this VM's value for each plan entry: all of them in an
//     eager VM, and in a lazy VM each one built on its first lookup through
//     lazy.
//   - slots is built only on shadowing.  It indexes newly appended slots
//     and clears displaced ones directly.  Compaction bounds cleared slots
//     by live entries while preserving registration order.  A non-nil slots
//     also reports that an inherited plan entry may be displaced.
type builtinRegistry struct {
	ownIndex atomic.Pointer[map[builtinKey]*LVal]
	lazy     *lazyInstance
	plan     *templateBuiltins
	last     *builtinChunk
	slots    *builtinSlotIndex
	values   []*LVal
}

// register records fn, which registrationFunValue built with fn as its own
// record, as the registered builtin of its package and name.  shadows
// reports that the name was already bound in the package.
func (r *builtinRegistry) register(fn *LVal, shadows bool) {
	if r.last == nil || r.last.n == builtinChunkSize {
		r.last = &builtinChunk{prev: r.last}
	}
	r.last.vals[r.last.n] = fn
	r.last.n++
	if shadows {
		r.dropDisplaced()
	}
	if idx := r.ownIndex.Load(); idx != nil {
		(*idx)[registrationKey(fn)] = fn
	}
}

// dropDisplaced indexes new slots in registration order and clears each
// displaced slot directly.  Each slot is indexed once between compactions.
// Compaction runs only after cleared slots outnumber live entries, so the
// work is amortized constant per registration, independent of history.
func (r *builtinRegistry) dropDisplaced() {
	if r.slots == nil {
		r.slots = &builtinSlotIndex{byKey: make(map[builtinKey]**LVal)}
	}
	s := r.slots
	var stack [8]*builtinChunk
	chunks := stack[:0]
	for c := r.last; c != nil; c = c.prev {
		chunks = append(chunks, c)
		if c == s.last {
			break
		}
	}
	for i := len(chunks) - 1; i >= 0; i-- {
		c := chunks[i]
		start := 0
		if c == s.last {
			start = s.n
		}
		for j := start; j < c.n; j++ {
			slot := &c.vals[j]
			key := registrationKey(*slot)
			if old := s.byKey[key]; old != nil {
				*old = nil
				s.cleared++
			}
			s.byKey[key] = slot
		}
	}
	s.last, s.n = r.last, r.last.n
	if s.cleared > len(s.byKey) {
		r.compactSlots()
	}
}

// compactSlots copies live registrations into fresh chunks, in order, and
// replaces every slot pointer so obsolete chunks can be released.
func (r *builtinRegistry) compactSlots() {
	var last *builtinChunk
	r.eachOwn(func(fn *LVal) {
		if last == nil || last.n == builtinChunkSize {
			last = &builtinChunk{prev: last}
		}
		last.vals[last.n] = fn
		r.slots.byKey[registrationKey(fn)] = &last.vals[last.n]
		last.n++
	})
	r.last = last
	r.slots.last, r.slots.n = last, last.n
	r.slots.cleared = 0
}

// eachOwn calls f with each current registration made in this runtime, in
// registration order.
func (r *builtinRegistry) eachOwn(f func(fn *LVal)) {
	var stack [8]*builtinChunk
	chunks := stack[:0]
	for c := r.last; c != nil; c = c.prev {
		chunks = append(chunks, c)
	}
	for i := len(chunks) - 1; i >= 0; i-- {
		for _, fn := range chunks[i].vals[:chunks[i].n] {
			if fn != nil {
				f(fn)
			}
		}
	}
}

// numOwn returns an upper bound on the number of registrations made in this
// runtime: dropped ones are counted.
func (r *builtinRegistry) numOwn() int {
	n := 0
	for c := r.last; c != nil; c = c.prev {
		n += c.n
	}
	return n
}

// ownLookup returns the current own registration of key.
func (r *builtinRegistry) ownLookup(key builtinKey) *LVal {
	if r.last == nil {
		return nil
	}
	idx := r.ownIndex.Load()
	if idx == nil {
		m := make(map[builtinKey]*LVal, r.numOwn())
		r.eachOwn(func(fn *LVal) { m[registrationKey(fn)] = fn })
		if !r.ownIndex.CompareAndSwap(nil, &m) {
			idx = r.ownIndex.Load()
		} else {
			idx = &m
		}
	}
	return (*idx)[key]
}

// planLookup returns the index of the plan entry of key.
func (r *builtinRegistry) planLookup(key builtinKey) (int, bool) {
	if r.plan == nil {
		return 0, false
	}
	i, ok := r.plan.lookupIndex()[key]
	return i, ok
}

// currentRecord returns the record of the current registration of pkg and
// name, or nil.  It builds no value.
func (r *builtinRegistry) currentRecord(pkg, name string) *LVal {
	key := builtinKey{pkg, name}
	if fn := r.ownLookup(key); fn != nil {
		return fn.funData().reg
	}
	if i, ok := r.planLookup(key); ok {
		return r.plan.entries[i].rec
	}
	return nil
}

// lookup returns this runtime's value of the registered builtin pkg:name,
// or nil.  In a lazy VM the first lookup of a plan entry builds it.
func (r *builtinRegistry) lookup(pkg, name string) *LVal {
	key := builtinKey{pkg, name}
	if fn := r.ownLookup(key); fn != nil {
		return fn
	}
	if i, ok := r.planLookup(key); ok {
		return r.planValue(i)
	}
	return nil
}

// planValue returns this VM's value of plan entry i, building it in a lazy
// VM.
func (r *builtinRegistry) planValue(i int) *LVal {
	if r.values == nil {
		r.values = make([]*LVal, len(r.plan.entries))
	}
	if v := r.values[i]; v != nil {
		return v
	}
	v := r.lazy.ref(r.plan.entries[i].value)
	r.values[i] = v
	return v
}

// current returns this runtime's value of the current registration of
// every key, building each value, in registration order: inherited plan
// entries first, then own registrations.  Each value's funData.reg is its
// record.  Template publication uses it.  A displaced registration is left
// out, so a builtin policy is never asked about one; finding them needs the
// index, which is built only when some registration replaced a binding.
func (r *builtinRegistry) current() []*LVal {
	n := r.numOwn()
	if r.plan != nil {
		n += len(r.plan.entries)
	}
	out := make([]*LVal, 0, n)
	if r.plan != nil {
		for i := range r.plan.entries {
			out = append(out, r.planValue(i))
		}
	}
	r.eachOwn(func(fn *LVal) { out = append(out, fn) })
	if r.slots == nil {
		return out
	}
	kept := out[:0]
	for _, fn := range out {
		rec := fn.funData().reg
		k := registrationKey(rec)
		if r.currentRecord(k.pkg, k.name) == rec {
			kept = append(kept, fn)
		}
	}
	return kept
}

// RegisteredBuiltin returns the function, macro or special operator
// registered as pkg:name, or nil when nothing is registered under that
// name.  Registration is LEnv.AddBuiltins, LEnv.BindBuiltins,
// LEnv.AddMacros or LEnv.AddSpecialOps.  The answer does not depend on what
// pkg binds name to now: set, set!, defun, Put and shadowing never change
// it.  A later registration of the same package and name replaces it.
//
// In a template VM the answer is the VM's own value of the builtin, the
// same value the package binds while the name is unchanged.  In a lazy VM
// the first call for a name builds the value, so the single-goroutine rule
// of Template.NewVM applies.
func (r *PackageRegistry) RegisteredBuiltin(pkg, name string) *LVal {
	if r == nil {
		return nil
	}
	return r.builtins.lookup(pkg, name)
}

// RegisteredBuiltinName returns the package and name under which fn was
// registered, and true, when fn is the function, macro or special
// operator that r's registration of that name created.  A header that
// shares fn's function data (a FunRef or Copy of it) is the same function
// and answers the same.
//
// The answer comes from the registration record elps stores with the
// function, never from its FID text.  It is false for every other value: a
// lambda, a builtin made by FunInPackage, a captured builtin (a libschema
// validator, for one), a native, a non-function, and a builtin that a later
// registration of the same name replaced.  Rebinding or shadowing the name
// does not change the answer.  The record is shared by every VM a template
// mints, so the answer is the same in the source and in each VM.
//
//nolint:revive // exported API; changing it breaks embedders
func (r *PackageRegistry) RegisteredBuiltinName(fn *LVal) (string, string, bool) {
	if r == nil || fn == nil || fn.Type != LFun {
		return "", "", false
	}
	fd, _ := fn.Native.(*funData)
	if fd == nil || fd.reg == nil {
		return "", "", false
	}
	name := fd.reg.Str
	if r.builtins.currentRecord(fd.pkg, name) != fd.reg {
		return "", "", false
	}
	return fd.pkg, name, true
}

// builtinRegisteredBuiltin is lisp:builtin, the Lisp face of
// PackageRegistry.RegisteredBuiltin for exported names.
func builtinRegisteredBuiltin(env *LEnv, args *LVal) *LVal {
	sym := args.Cells[0]
	if sym.Type != LSymbol {
		return env.Errorf("name is not a symbol: %v", sym.Type)
	}
	parts2 := splitSymbolParts(sym.Str)
	pkg, name, n := parts2.namespace, parts2.name, parts2.parts
	if n != 2 || pkg == "" || name == "" {
		return env.Errorf("name must be qualified (PKG:NAME): %v", sym.Str)
	}
	// A name its package does not export gets the error of an unregistered
	// name, so the answer does not reveal which internals exist.  The export
	// check scans the export list, so it charges one step per started 64
	// exports before it starts.
	reg := env.Runtime.Registry
	p := reg.Package(pkg)
	if p != nil {
		if lerr := env.ChargeSteps(int64((p.NumExternals() + 63) / 64)); lerr.Type == LError {
			return lerr
		}
	}
	fn := reg.RegisteredBuiltin(pkg, name)
	if fn == nil || !p.exports(name) {
		return env.Errorf("no builtin is registered as %v", sym.Str)
	}
	return fn
}

// builtinRegisteredBuiltinName is lisp:builtin-name, the Lisp face of
// PackageRegistry.RegisteredBuiltinName.
func builtinRegisteredBuiltinName(env *LEnv, args *LVal) *LVal {
	pkg, name, ok := env.Runtime.Registry.RegisteredBuiltinName(args.Cells[0])
	if !ok {
		return Nil()
	}
	return String(pkg + ":" + name)
}
