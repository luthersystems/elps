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
	visit := func(name, fid, fpkg string, ft LFunType, ok bool) {
		read++
		if !ok || fpkg != pkg.Name || ft != LFunNone {
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
				fid, fpkg, ft, ok := pkg.lazy.peekFun(pkg.lazy.base.bindings[i].value)
				visit(name, fid, fpkg, ft, ok)
				continue
			}
			fid, fpkg, ft, ok := funIdentity(v)
			visit(name, fid, fpkg, ft, ok)
		}
		return out, read
	}
	for name, v := range pkg.symbols {
		if v == lazyPending {
			i, _ := pkg.lazy.base.index.Lookup(name)
			fid, fpkg, ft, ok := pkg.lazy.peekFun(pkg.lazy.base.bindings[i].value)
			visit(name, fid, fpkg, ft, ok)
			continue
		}
		fid, fpkg, ft, ok := funIdentity(v)
		visit(name, fid, fpkg, ft, ok)
	}
	return out, read
}

// funIdentity reports the FID, package and function type of a function
// value; ok is false for anything else.
func funIdentity(v *LVal) (string, string, LFunType, bool) {
	if v == nil || v.Type != LFun {
		return "", "", LFunNone, false
	}
	return v.FID(), v.Package(), v.FunType, true
}

// peekFun reports the function identity of a binding the lazy plan has not
// materialized, reading the plan instead of building the value.
func (lazy *lazyPackage) peekFun(ref templateRef) (string, string, LFunType, bool) {
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
		return "", "", LFunNone, false
	}
	fd := &lazy.inst.p.functions[tv.payload].header
	return fd.fid, fd.pkg, tv.header.FunType, true
}
