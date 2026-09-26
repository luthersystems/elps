// Copyright © 2026 The ELPS authors

package lisp

// scopeBinding is one lexical binding in a scopeTable.
type scopeBinding struct {
	val  *LVal
	name string
}

// scopeTable is an LEnv's lexical scope.  Almost every scope is small -- a
// function call's formals, a let's bindings -- so a small scope's bindings
// live in a slice searched linearly.  That costs one allocation where a map
// costs two (its header and its first group), beats hashing for a handful of
// short names, and lets allocEnvScope co-allocate the storage with the LEnv.
//
// A large scope is a map[string]*LVal, exactly the representation every scope
// had before: one sized for more than scopeMapThreshold bindings from the
// start (a wide let, a template's instantiated root) is a map from its first
// binding, and a slice-backed scope that grows past scopeMapThreshold moves
// its bindings into one.  Large scopes therefore cost what they always did,
// and lookups in them stay O(1).  At most one of m and bindings is in use.
//
// The zero scopeTable is an empty scope that has allocated nothing.
type scopeTable struct {
	m        map[string]*LVal // large scopes; nil while the scope is slice-backed
	bindings []scopeBinding   // small scopes, in insertion order
}

// scopeMapThreshold is the binding count past which a scope is a map.  Up to
// it the slice saves the map's allocations; lookups are a linear scan, which
// beats hashing for a few short names, though a put-heavy fill of 8-16 names
// runs somewhat slower than a map's (put checks for a duplicate).  The
// evaluator workloads are allocation-bound, so the trade favors the slice.
const scopeMapThreshold = 16

// unsizedScopeCap is the room an unsized scope gets on its first put.
const unsizedScopeCap = 8

// newScopeTable returns an empty scope with room for n bindings.
func newScopeTable(n int) scopeTable {
	if n > scopeMapThreshold {
		return scopeTable{m: make(map[string]*LVal, n)}
	}
	return scopeTable{bindings: make([]scopeBinding, 0, n)}
}

// allocated reports whether the scope has storage.  A lazy scope has none
// until its first successful put; one co-allocated by allocEnvScope has it
// from the start.
func (t *scopeTable) allocated() bool { return t.m != nil || t.bindings != nil }

func (t *scopeTable) len() int {
	if t.m != nil {
		return len(t.m)
	}
	return len(t.bindings)
}

// each calls yield for every binding until it returns false: in insertion
// order for a slice-backed scope, in map order for a large one.
func (t *scopeTable) each(yield func(name string, v *LVal) bool) {
	if t.m != nil {
		for k, v := range t.m {
			if !yield(k, v) {
				return
			}
		}
		return
	}
	for _, b := range t.bindings {
		if !yield(b.name, b.val) {
			return
		}
	}
}

// get returns the value bound to name in this scope alone.
func (t *scopeTable) get(name string) (*LVal, bool) {
	if t.m != nil {
		v, ok := t.m[name]
		return v, ok
	}
	for i := range t.bindings {
		if t.bindings[i].name == name {
			return t.bindings[i].val, true
		}
	}
	return nil, false
}

// update overwrites an existing binding, reporting whether name was bound.
func (t *scopeTable) update(name string, v *LVal) bool {
	if t.m != nil {
		if _, ok := t.m[name]; ok {
			t.m[name] = v
			return true
		}
		return false
	}
	for i := range t.bindings {
		if t.bindings[i].name == name {
			t.bindings[i].val = v
			return true
		}
	}
	return false
}

// put binds name to v, overwriting an existing binding in place (as a map
// assignment would), and allocating the scope on first use with room for
// hint bindings.  An unsized scope (hint 0: NewEnv, an embedder's root) gets
// room for unsizedScopeCap, so a Go caller filling one with Puts does not
// grow the slice 1, 2, 4, 8.
func (t *scopeTable) put(name string, v *LVal, hint int) {
	switch {
	case t.m != nil:
		t.m[name] = v
		return
	case t.bindings == nil:
		if hint <= 0 {
			hint = unsizedScopeCap
		}
		*t = newScopeTable(hint)
	case t.update(name, v):
		return
	}
	t.appendNew(name, v)
}

// appendNew adds a binding for a name the caller knows is not bound in t,
// skipping put's duplicate check -- template instantiation, whose plan
// descriptors come from one scope and so are already unique.
//
// A slice-backed scope growing past scopeMapThreshold moves into a map.  The
// array it leaves (the co-allocated one included) is not cleared, so it may
// keep up to its capacity of superseded values reachable for the
// environment's lifetime.  That is bounded, reachable only through the Go API
// (a Lisp scope never outgrows the capacity its binding form sized it with),
// and clearing it would hand zeroed bindings to a Bindings iteration that is
// still ranging over the old array.
func (t *scopeTable) appendNew(name string, v *LVal) {
	switch {
	case t.m != nil:
		t.m[name] = v
	case len(t.bindings) == scopeMapThreshold:
		m := make(map[string]*LVal, 2*scopeMapThreshold)
		for _, b := range t.bindings {
			m[b.name] = b.val
		}
		m[name] = v
		*t = scopeTable{m: m}
	default:
		t.bindings = append(t.bindings, scopeBinding{name: name, val: v})
	}
}

// allocEnvScope returns a zero LEnv whose scope has room for n bindings
// allocated together with the LEnv itself, so a function call or let that
// binds a handful of names costs one allocation for its whole scope instead
// of three (the LEnv, a map header and its first group).  The caller fills
// every other register.  For n == 0 or n > 4 the scope is left for put to
// allocate on demand with the scopeHint the caller sets.
//
// The bindings slice has capacity exactly n, so a scope that outgrows it
// (only possible through the Go API: every Lisp binding form sizes its scope
// exactly) appends into a fresh array rather than past the co-allocated one.  The array shares the LEnv's lifetime,
// which it did in effect before: the scope map lived exactly as long as its
// environment.
func allocEnvScope(n int) *LEnv {
	switch {
	case n <= 0:
		return &LEnv{}
	case n <= 2:
		x := &struct {
			b [2]scopeBinding
			e LEnv
		}{}
		x.e.scope.bindings = x.b[:0:n]
		return &x.e
	case n <= 4:
		x := &struct {
			b [4]scopeBinding
			e LEnv
		}{}
		x.e.scope.bindings = x.b[:0:n]
		return &x.e
	default:
		return &LEnv{}
	}
}
