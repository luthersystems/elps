// Copyright © 2026 The ELPS authors

package lisp

// VMCache returns the value that Go code cached on this VM under key, or nil
// when there is none.  See SetVMCache.
func (rt *Runtime) VMCache(key any) any {
	return rt.vmCache[key]
}

// SetVMCache caches v on this VM under key.  A nil v removes the entry.
//
// The VM cache is for a library that keeps work it can do again, such as a
// value derived from a pattern.  A builtin may not keep that work in a Go
// variable or in its receiver: a template shares one builtin with every VM
// it mints, and those VMs can run on different goroutines
// (luthersystems/elps#680).  The cache belongs to one Runtime instead, which
// one goroutine runs at a time, so the cache needs no lock.
//
// The cache holds Go values only.  Lisp code cannot reach it, a template
// does not publish it, and Template.NewVM and a fork start with an empty
// cache.  So the result, the step count and the error of each call must not
// depend on whether an entry is there.  Bound what you keep: the cache lives
// as long as the VM.  Use a key of an unexported type of your own package,
// as for a context key, so two packages cannot use one key.
func (rt *Runtime) SetVMCache(key, v any) {
	if v == nil {
		delete(rt.vmCache, key)
		return
	}
	if rt.vmCache == nil {
		rt.vmCache = make(map[any]any, 1)
	}
	rt.vmCache[key] = v
}
