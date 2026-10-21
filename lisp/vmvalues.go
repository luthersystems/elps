// Copyright © 2026 The ELPS authors

package lisp

// VM values are named Lisp values that an embedder keeps on a Runtime
// instead of in a package.  Lisp code has no symbol for them.  A runtime
// keeps them in one sorted map that no package binds, so a template
// publishes them exactly as it publishes a sorted map bound to a package
// global: admission checks every value at NewTemplate, and every VM that
// Template.NewVM mints gets its own copy of the map and of what it reaches.
// A lazy VM builds the map on the first VM value call and each entry on its
// first read, through the sorted-map lazy path.  A value that a VM value
// shares with a package global, or with another VM value, stays shared in
// each VM, as in the source.

// vmValueTable is a runtime's VM values.
//
//   - m is the sorted map of names to values, or nil before it is built.
//   - lazy and ref build m on first use in a lazy template VM.  Both are
//     dropped when m is built.
type vmValueTable struct {
	m    *LVal
	lazy *lazyInstance
	ref  templateRef
}

// VMValue returns the VM value called name and whether it is set.  See
// SetVMValue.  In a lazy template VM the first read of a published value
// builds this VM's copy, so the single-goroutine rule of Template.NewVM
// applies.
func (rt *Runtime) VMValue(name string) (*LVal, bool) {
	m := rt.vmValueMap(false)
	if m == nil {
		return nil, false
	}
	return m.Map().getString(name)
}

// SetVMValue sets the VM value called name to v.  A nil v stores nil, the
// empty list.  VM values are per-VM interpreter state that holds Lisp
// values of any type: functions, closures and sorted maps included.  Lisp
// code cannot reach them by a symbol.  Only Go code that the embedder
// provides can read or write them.
//
// NewTemplate publishes the source's VM values with the same admission and
// copy rules as package global values: a value that a template cannot
// publish as a global makes NewTemplate fail.  Every VM that Template.NewVM
// mints starts with its own copy of each published value.  A write, a
// delete or an in-place change (assoc! on a sorted map) in one VM is seen
// by no other VM, by the template, or by the source.  A cold runtime has no
// VM values.
//
// SetVMValue stores v itself, not a copy, as Package.Put does.
func (rt *Runtime) SetVMValue(name string, v *LVal) {
	if v == nil {
		v = Nil()
	}
	rt.vmValueMap(true).Map().Set(String(name), v)
}

// DeleteVMValue removes the VM value called name.  Removing an unset name
// does nothing.  A delete in one VM is seen by no other VM, by the template,
// or by the source.
func (rt *Runtime) DeleteVMValue(name string) {
	if m := rt.vmValueMap(false); m != nil {
		m.Map().Del(String(name))
	}
}

// vmValueMap returns the runtime's map of VM values, building this VM's
// copy in a lazy template VM.  It returns nil when the runtime has none and
// create is false.
func (rt *Runtime) vmValueMap(create bool) *LVal {
	t := rt.vmValues
	if t == nil {
		if !create {
			return nil
		}
		t = &vmValueTable{}
		rt.vmValues = t
	}
	if t.m == nil {
		if t.lazy != nil {
			t.m = t.lazy.ref(t.ref)
			t.lazy, t.ref = nil, templateRef{}
		} else if create {
			t.m = SortedMap()
		}
	}
	return t.m
}

// vmValueEntries returns the runtime's map of VM values and its entries,
// sorted by name, building each published value.  Template publication
// admits each entry under its name, then the map.
func (rt *Runtime) vmValueEntries() (*LVal, []templateMapEntry) {
	m := rt.vmValueMap(false)
	if m == nil {
		return nil, nil
	}
	pairs := m.MapEntries().Cells // sorted by key
	out := make([]templateMapEntry, len(pairs))
	for i, pair := range pairs {
		out[i] = templateMapEntry{key: pair.Cells[0].Str, v: pair.Cells[1]}
	}
	return m, out
}
