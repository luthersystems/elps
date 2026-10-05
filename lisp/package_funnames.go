// Copyright © 2026 The ELPS authors

package lisp

// NumBindings returns the number of names bound in pkg, without reading or
// materializing any binding.
func (pkg *Package) NumBindings() int {
	if pkg.base != nil {
		return pkg.base.index.Len()
	}
	return len(pkg.symbols)
}

// FunNamesByFID returns the canonical global name of every regular function
// pkg defines and binds: for each FID, the first name in sorted order under
// which pkg binds a function whose Package is pkg.Name and whose FunType is
// LFunNone.  It also returns the number of bindings it read, so a caller can
// charge for the work.
//
// It reads a binding a lazy template has not materialized from the
// template's plan, so it builds no value.  A rebinding (set, putSlot, a
// thawed table) is read as the binding it now holds.  An FID is unique
// within its package, so the FID names one function of pkg.
func (pkg *Package) FunNamesByFID() (map[string]string, int) {
	out := make(map[string]string)
	read := 0
	// The first name in sorted order is the minimum, which needs no sort:
	// the bindings are read in any order.
	visit := func(name string, identity functionIdentity) {
		read++
		if !identity.ok || identity.funType != LFunNone {
			return
		}
		fid, fpkg := "", ""
		if identity.data != nil {
			fid, fpkg = identity.data.fid, identity.data.pkg
		}
		if fpkg != pkg.Name {
			return
		}
		if prev, seen := out[fid]; !seen || name < prev {
			out[fid] = name
		}
	}
	if pkg.base != nil {
		for name, i := range pkg.base.index.Unordered() {
			v := pkg.slotValue(i)
			if v == nil && pkg.lazy.inst != nil {
				visit(name, pkg.lazy.peekFun(pkg.lazy.base.bindings[i].value))
				continue
			}
			visit(name, funIdentity(v))
		}
		return out, read
	}
	for name, v := range pkg.symbols {
		if v == lazyPending {
			i, _ := pkg.lazy.base.index.Lookup(name)
			visit(name, pkg.lazy.peekFun(pkg.lazy.base.bindings[i].value))
			continue
		}
		visit(name, funIdentity(v))
	}
	return out, read
}

// functionIdentity holds a function identity and its lookup status.
type functionIdentity struct {
	// data contains the function identifier and package name.
	data *funData
	// funType is the function kind.
	funType LFunType
	// ok reports a valid function identity.
	ok bool
}

// funIdentity reports the FID, package and function type of a function
// value; ok is false for anything else.
func funIdentity(v *LVal) functionIdentity {
	if v == nil || v.Type != LFun {
		return functionIdentity{}
	}
	return functionIdentity{data: v.funData(), funType: v.FunType, ok: true}
}

// peekFun reports the function identity of a binding the lazy plan has not
// materialized, reading the plan instead of building the value.
func (lazy *lazyPackage) peekFun(ref templateRef) functionIdentity {
	if ref.index == 0 {
		return funIdentity(ref.shared)
	}
	if mv := lazy.inst.values[ref.index-1]; mv != nil {
		// Materialized through another path; outside ref the fill queue
		// is drained, so the header is complete.
		return funIdentity(mv)
	}
	tv := &lazy.inst.p.values[ref.index-1]
	if tv.header.Type != LFun || tv.kind != templateFunction {
		return functionIdentity{}
	}
	fd := &lazy.inst.p.functions[tv.payload].header
	return functionIdentity{data: fd, funType: tv.header.FunType, ok: true}
}

// FirstNameOf returns the first name, in sorted order, under which pkg
// binds fn itself: a binding whose function data is fn's, so any header
// copy of fn (a FunRef or Copy) matches, and another function with the same
// FID does not.  It returns false when no binding holds fn, or when fn is
// not a function.  It also returns the number of bindings it read, so a
// caller can charge for the work.
//
// Like FunNamesByFID it reads a binding a lazy template has not
// materialized from the template's plan, so it builds no value.
//
//nolint:revive // exported API; changing it breaks embedders
func (pkg *Package) FirstNameOf(fn *LVal) (string, bool, int) {
	if fn == nil || fn.Type != LFun {
		return "", false, 0
	}
	fd, _ := fn.Native.(*funData)
	if fd == nil {
		return "", false, 0
	}
	best, found, read := "", false, 0
	visit := func(name string, same bool) {
		read++
		if same && (!found || name < best) {
			best, found = name, true
		}
	}
	if pkg.base != nil {
		for name, i := range pkg.base.index.Unordered() {
			v := pkg.slotValue(i)
			if v == nil && pkg.lazy.inst != nil {
				visit(name, pkg.lazy.peekSameFun(pkg.lazy.base.bindings[i].value, fd))
				continue
			}
			visit(name, sameFun(v, fd))
		}
		return best, found, read
	}
	for name, v := range pkg.symbols {
		if v == lazyPending {
			i, _ := pkg.lazy.base.index.Lookup(name)
			visit(name, pkg.lazy.peekSameFun(pkg.lazy.base.bindings[i].value, fd))
			continue
		}
		visit(name, sameFun(v, fd))
	}
	return best, found, read
}

// sameFun reports whether v is a function whose data is fd.
func sameFun(v *LVal, fd *funData) bool {
	return v != nil && v.Type == LFun && v.Native == fd
}

// peekSameFun reports whether a binding the lazy plan has not materialized
// is the function whose data is fd, reading the plan instead of building
// the value.  A plan function this VM has not materialized cannot be fd,
// which is materialized.
func (lazy *lazyPackage) peekSameFun(ref templateRef, fd *funData) bool {
	if ref.index == 0 {
		return sameFun(ref.shared, fd)
	}
	if mv := lazy.inst.values[ref.index-1]; mv != nil {
		return sameFun(mv, fd)
	}
	tv := &lazy.inst.p.values[ref.index-1]
	if tv.header.Type != LFun || tv.kind != templateFunction {
		return false
	}
	return lazy.inst.functions[tv.payload] == fd
}
