// Copyright © 2026 The ELPS authors

package lisp

import "sort"

// scopeOf builds a scope holding m's bindings in name order.
func scopeOf(m map[string]*LVal) scopeTable {
	names := make([]string, 0, len(m))
	for k := range m {
		names = append(names, k)
	}
	sort.Strings(names)
	t := newScopeTable(len(m))
	for _, k := range names {
		t.put(k, m[k], 0)
	}
	return t
}

// val returns the binding for name, or nil.
func (t *scopeTable) val(name string) *LVal {
	v, _ := t.get(name)
	return v
}
