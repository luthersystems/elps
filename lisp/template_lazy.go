// Copyright © 2026 The ELPS authors

package lisp

import (
	"cmp"
	"errors"
	"slices"

	"github.com/luthersystems/elps/internal/packagetable"
)

// Lazy template instantiation.
//
// Template.NewVM does not rebuild the plan's value graph. It creates the
// runtime, the package shells and the root environment; every other object is
// created the first time something reaches it:
//
//   - a package slot, through Package.baseValue (unfrozen packages as well as
//     frozen ones get a plan base when the plan is lazy, so every package
//     binding starts unmaterialized);
//   - a sorted-map entry, through sortedmap.lookup/forceAll; and
//   - everything a materialized object points at (its cells, function data,
//     closure environment chain, map backing), because LVal, LEnv and funData
//     hold direct pointers.
//
// Each plan object is materialized at most once per VM: lazyInstance memoizes
// by plan index, so pointer identity is stable and objects shared between
// slots are shared in the VM exactly as in the source. Objects are allocated
// and memoized before any is filled, and fills run from an explicit work
// queue, so cycles close and arbitrarily deep values need no Go recursion.
//
// The plan is immutable after publication, so materializing late observes the
// same state as materializing at NewVM.
//
// Concurrency: materialization writes VM state from what look like reads
// (a symbol lookup, a sorted-map get). A VM is single-goroutine: the
// Template.NewVM contract. Checked builds (-tags elpscheck) detect an
// overlapping fill and panic; see lazyGuard.
//
// Retention: a package with unmaterialized slots, or a sorted map with
// pending entries, keeps its lazyInstance and therefore the plan and every
// object this VM has materialized alive. Both drop the reference once their
// last pending entry is filled.

// lazyPending marks a sorted-map entry or a thawed package binding that has
// not been materialized. It is never returned by an accessor: every direct
// read of those tables is confined, by the elpslazyread analyzer, to the
// functions that replace it.
var lazyPending = &LVal{Type: LInvalid, Str: "lazy-pending"} //elpsvet:allow identity-only marker never returned to a program; the elpslazyread rule confines every table read that could see it

type lazyTask struct {
	kind uint8
	idx  int
}

const (
	lazyFillValue uint8 = iota
	lazyFillCells
	lazyFillEnv
	lazyFillFunction
	lazyFillBacking
)

type lazyInstance struct {
	p            *templatePlan
	rt           *Runtime
	values       []*LVal
	envs         []*LEnv
	functions    []*funData
	maps         []*MapData
	mapBackings  []Map
	bytes        []*[]byte
	cells        [][]*LVal
	byteBackings [][]byte
	queue        []lazyTask
	mapsDone     []bool
	guard        lazyGuard
	count        int
}

// lazyPackage is a package's link to the lazy instance. refs[i] describes
// slot i of index; after a thaw it resolves pending bindings by name.
type lazyPackage struct {
	inst    *lazyInstance
	index   packagetable.Map[int]
	refs    []templateRef
	pending int
}

// lazySorted is a sorted map's link to the lazy instance.
type lazySorted struct {
	inst    *lazyInstance
	entries []templateBinding // sorted by name
	pending int
}

// ref returns the VM object for r, materializing it and everything it reaches.
func (l *lazyInstance) ref(r templateRef) *LVal {
	if r.index == 0 {
		return r.shared
	}
	l.guard.enter()
	v := l.value(r.index - 1)
	l.drain()
	l.guard.leave()
	return v
}

func (l *lazyInstance) rootEnv(i int) *LEnv {
	l.guard.enter()
	e := l.env(i)
	l.drain()
	l.guard.leave()
	return e
}

func (l *lazyInstance) allocRef(r templateRef) *LVal {
	if r.index == 0 {
		return r.shared
	}
	return l.value(r.index - 1)
}

func (l *lazyInstance) drain() {
	for len(l.queue) > 0 {
		task := l.queue[len(l.queue)-1]
		l.queue = l.queue[:len(l.queue)-1]
		switch task.kind {
		case lazyFillValue:
			l.fillValue(task.idx)
		case lazyFillCells:
			c := l.cells[task.idx]
			for k, r := range l.p.cells[task.idx] {
				c[k] = l.allocRef(r)
			}
		case lazyFillEnv:
			l.fillEnv(task.idx)
		case lazyFillFunction:
			tf := &l.p.functions[task.idx]
			data := l.functions[task.idx]
			data.env = l.env(tf.env)
			if tf.captures != 0 {
				data.captures.values = l.allocRef(tf.values)
			}
		case lazyFillBacking:
			l.fillBacking(task.idx)
		}
	}
}

func (l *lazyInstance) value(i int) *LVal {
	if v := l.values[i]; v != nil {
		return v
	}
	out := new(LVal)
	l.values[i] = out
	l.count++
	l.queue = append(l.queue, lazyTask{lazyFillValue, i})
	return out
}

// fillValue writes the header of a destination value allocates for this VM.
func (l *lazyInstance) fillValue(i int) {
	tv := &l.p.values[i]
	header := tv.header
	if tv.hasView {
		v := tv.view
		header.Cells = l.cellGroup(v.storage)[v.offset : v.offset+v.length : v.offset+v.capacity]
	}
	switch tv.kind {
	case templateFunction:
		header.Native = l.function(tv.payload)
	case templateMapPayload:
		header.Native = l.mapData(tv.payload)
	case templateBytePayload:
		header.Native = l.byteData(tv.payload)
	case templateNativePayload:
		//elpsvet:allow-native lazy instantiation replays a payload the PLAN already holds: templateInventory.native admitted it at publication and checkNativeAffinity re-checked it at NewVM, so this store opens no channel the audit did not take
		header.Native = l.p.natives[tv.payload]
	}
	*l.values[i] = header //elps:mutates value allocated this private destination for this VM; only the fill queue has seen it before this store
}

func (l *lazyInstance) cellGroup(g int) []*LVal {
	if c := l.cells[g]; c != nil {
		return c
	}
	c := make([]*LVal, len(l.p.cells[g]))
	l.cells[g] = c
	l.queue = append(l.queue, lazyTask{lazyFillCells, g})
	return c
}

func (l *lazyInstance) env(i int) *LEnv {
	if i == 0 {
		return nil
	}
	if e := l.envs[i-1]; e != nil {
		return e
	}
	te := &l.p.envs[i-1]
	e := &LEnv{ID: te.id, Runtime: l.rt, scopeHint: te.scopeHint}
	l.envs[i-1] = e
	l.queue = append(l.queue, lazyTask{lazyFillEnv, i - 1})
	return e
}

func (l *lazyInstance) fillEnv(i int) {
	te := &l.p.envs[i]
	e := l.envs[i]
	e.parent = l.env(te.parent)
	if len(te.bindings) > 0 {
		e.scope = make(map[string]*LVal, len(te.bindings))
		for _, b := range te.bindings {
			e.scope[b.name] = l.allocRef(b.value)
		}
	}
}

func (l *lazyInstance) function(i int) *funData {
	if f := l.functions[i]; f != nil {
		return f
	}
	tf := &l.p.functions[i]
	data := new(funData)
	*data = tf.header
	l.functions[i] = data
	if tf.captures != 0 {
		captures := &builtinCaptures{code: tf.code}
		data.captures = captures
		data.builtin = captures.call
	}
	l.queue = append(l.queue, lazyTask{lazyFillFunction, i})
	return data
}

func (l *lazyInstance) mapData(i int) *MapData {
	if l.mapsDone[i] {
		return l.maps[i]
	}
	l.mapsDone[i] = true
	m := l.p.maps[i]
	if m.nilData {
		return nil
	}
	out := new(MapData)
	l.maps[i] = out
	if m.backing != 0 {
		out.mapBacking = l.mapBacking(m.backing - 1)
	}
	return out
}

func (l *lazyInstance) mapBacking(i int) Map {
	if b := l.mapBackings[i]; b != nil {
		return b
	}
	backing := &l.p.mapBackings[i]
	var m Map
	if backing.json {
		m = make(jsonMap, len(backing.entries))
	} else {
		sm := sortedmap{m: make(map[string]*LVal, len(backing.entries)), tm: make(typemap, len(backing.types))}
		if backing.indexed {
			sm.lz = &lazySorted{}
		}
		m = sm
	}
	l.mapBackings[i] = m
	l.queue = append(l.queue, lazyTask{lazyFillBacking, i})
	return m
}

// fillBacking fills a map backing. JSON maps are filled eagerly (their entry
// storage is an interface map the decoder owns); sorted-map entries whose
// value is not yet materialized are left pending.
func (l *lazyInstance) fillBacking(i int) {
	backing := &l.p.mapBackings[i]
	if backing.json {
		m := l.mapBackings[i].(jsonMap)
		for _, entry := range backing.entries {
			m[entry.name] = l.allocRef(entry.value)
		}
		return
	}
	sm := l.mapBackings[i].(sortedmap)
	for _, kind := range backing.types {
		sm.tm[kind.key] = kind.kind
	}
	pending := 0
	for _, entry := range backing.entries {
		switch {
		case entry.value.index == 0:
			sm.m[entry.name] = entry.value.shared
		case l.values[entry.value.index-1] != nil:
			sm.m[entry.name] = l.values[entry.value.index-1]
		default:
			sm.m[entry.name] = lazyPending
			pending++
		}
	}
	if pending > 0 && sm.lz != nil {
		// sortedmap is a value type; the lz pointer is shared by every copy.
		*sm.lz = lazySorted{inst: l, entries: backing.entries, pending: pending}
	}
}

func (l *lazyInstance) byteData(i int) *[]byte {
	if b := l.bytes[i]; b != nil {
		return b
	}
	b := new([]byte)
	l.bytes[i] = b
	tb := l.p.bytes[i]
	if tb.backed {
		v := tb.view
		if l.byteBackings[v.storage] == nil {
			l.byteBackings[v.storage] = append([]byte(nil), l.p.byteBackings[v.storage]...)
		}
		*b = l.byteBackings[v.storage][v.offset : v.offset+v.length : v.offset+v.capacity]
	} else if tb.nonnil {
		*b = []byte{}
	}
	return b
}

// resolve returns the materialized value for a pending sorted-map key.
func (z *lazySorted) resolve(key string) *LVal {
	i, ok := slices.BinarySearchFunc(z.entries, key, func(b templateBinding, k string) int { return cmp.Compare(b.name, k) })
	if !ok {
		return nil
	}
	inst, ref := z.inst, z.entries[i].value
	z.pending--
	if z.pending == 0 {
		z.inst, z.entries = nil, nil
	}
	return inst.ref(ref)
}

func (p *templatePlan) instantiateLazy(config vmConfig) *LEnv {
	rt := p.runtime.newRuntime(config)
	for _, payload := range p.natives {
		checkNativeAffinity(rt, payload)
	}
	l := &lazyInstance{p: p, rt: rt,
		values: make([]*LVal, len(p.values)), envs: make([]*LEnv, len(p.envs)),
		functions: make([]*funData, len(p.functions)), maps: make([]*MapData, len(p.maps)),
		mapsDone: make([]bool, len(p.maps)), mapBackings: make([]Map, len(p.mapBackings)),
		bytes: make([]*[]byte, len(p.bytes)), cells: make([][]*LVal, len(p.cells)),
		byteBackings: make([][]byte, len(p.byteBackings)),
	}
	var slots []*LVal
	if p.numPackageSlots > 0 {
		slots = make([]*LVal, p.numPackageSlots)
	}
	for i := range p.packages {
		pkg := &p.packages[i]
		values := slots[:len(pkg.refs):len(pkg.refs)]
		slots = slots[len(pkg.refs):]
		var lazy *lazyPackage
		if pkg.pending > 0 {
			lazy = &lazyPackage{inst: l, index: pkg.base.index, refs: pkg.refs, pending: pkg.pending}
		}
		for slot, ref := range pkg.refs {
			if ref.index == 0 {
				values[slot] = ref.shared
			}
		}
		rt.Registry.packages[pkg.name] = &Package{Name: pkg.name, Doc: pkg.doc, bindingsSealed: pkg.bindingsSealed,
			base: pkg.base, baseValues: values, lazy: lazy, unfrozenBase: pkg.unfrozen}
	}
	if p.runtime.hasCurrentPackage {
		rt.Package = rt.Registry.packages[p.runtime.currentPackage]
	}
	root := l.rootEnv(p.root)
	if config.ctx != nil {
		root.evalCtx = config.ctx
	}
	return root
}

func (p *templatePlan) instantiate(opts []VMOption) (*LEnv, error) {
	var config vmConfig
	for _, opt := range opts {
		if opt == nil {
			return nil, errors.New("template: nil VM option")
		}
		opt(&config)
	}
	if p.eager {
		return p.instantiateEager(config), nil
	}
	return p.instantiateLazy(config), nil
}
