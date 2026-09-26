// Copyright © 2026 The ELPS authors

package lisp

// scopeBinding is one lexical binding in a scopeTable.
type scopeBinding struct {
	val  *LVal
	name string
}

// scopeTable is an LEnv's lexical scope.  Almost every scope is small -- a
// function call's formals, a let's bindings -- so bindings live in a slice
// searched linearly, which costs one allocation where a map costs two (its
// header and its first group) and beats hashing for a handful of short names.
// A scope that grows past scopeIndexThreshold also keeps a name index so a
// large scope (an embedder's root env, a wide let) stays O(1).
//
// The zero scopeTable is an empty scope that has allocated nothing.
type scopeTable struct {
	index    map[string]int // nil until len(bindings) > scopeIndexThreshold
	bindings []scopeBinding
}

// scopeIndexThreshold is the binding count past which a scopeTable keeps a
// name index.  Below it a linear scan over short names is cheaper than a hash.
const scopeIndexThreshold = 16

// newScopeTable returns an empty scope with room for n bindings.
func newScopeTable(n int) scopeTable {
	return scopeTable{bindings: make([]scopeBinding, 0, n)}
}

func (t *scopeTable) len() int { return len(t.bindings) }

// all returns the bindings in insertion order.  The slice aliases the table:
// callers must not retain it across a put.
func (t *scopeTable) all() []scopeBinding { return t.bindings }

func (t *scopeTable) find(name string) int {
	if t.index != nil {
		if i, ok := t.index[name]; ok {
			return i
		}
		return -1
	}
	for i := range t.bindings {
		if t.bindings[i].name == name {
			return i
		}
	}
	return -1
}

// get returns the value bound to name in this scope alone.
func (t *scopeTable) get(name string) (*LVal, bool) {
	if i := t.find(name); i >= 0 {
		return t.bindings[i].val, true
	}
	return nil, false
}

// update overwrites an existing binding, reporting whether name was bound.
func (t *scopeTable) update(name string, v *LVal) bool {
	if i := t.find(name); i >= 0 {
		t.bindings[i].val = v
		return true
	}
	return false
}

// put binds name to v, overwriting an existing binding in place (as a map
// assignment would), and allocating the table with capacity hint on first
// use.
func (t *scopeTable) put(name string, v *LVal, hint int) {
	if t.bindings == nil {
		t.bindings = make([]scopeBinding, 0, max(hint, 1))
	} else if t.update(name, v) {
		return
	}
	t.bindings = append(t.bindings, scopeBinding{name: name, val: v})
	if t.index != nil {
		t.index[name] = len(t.bindings) - 1
	} else if len(t.bindings) > scopeIndexThreshold {
		t.index = make(map[string]int, len(t.bindings)*2)
		for i, b := range t.bindings {
			t.index[b.name] = i
		}
	}
}

// allocEnvScope returns a zero LEnv whose scope has room for n bindings
// allocated together with the LEnv itself, so a function call or let that
// binds a handful of names costs one allocation for its whole scope instead
// of three (the LEnv, a map header and its first group).  The caller fills
// every other register.  For n == 0 or n > 4 the scope is left for put to
// allocate on demand with the scopeHint the caller sets.
//
// The bindings slice has capacity exactly n, so a scope that outgrows its
// formals (a define inside a let, say) appends into a fresh array rather
// than past the co-allocated one.  The array shares the LEnv's lifetime,
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
