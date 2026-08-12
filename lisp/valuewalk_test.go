// Copyright © 2026 The ELPS authors

package lisp_test

import "github.com/luthersystems/elps/lisp"

// The shared reachability walk for package lisp_test.
//
// Several unrelated tests need the same primitive — "visit every LVal
// reachable from this root, once" — and each of them used to carry its own
// copy: cow_seal_test.go walked Cells, detach_test.go walked Cells plus a
// sorted-map's entry values, and containergen_test.go walked through the Map
// interface behind a node budget.  Three walks answering one question is
// three places for a container type to be forgotten, which is precisely how
// a sharing relationship goes unnoticed by the test that exists to find it.
//
// So there is one walk here, and the tests differ only in the visit function
// they pass:
//
//   - cow_seal_test.go / seal_watchdog_evalpath_test.go / copy_test.go count
//     sealed nodes over a parsed AST.
//   - detach_test.go asserts pointer-disjointness of a detached graph.
//   - containergen_test.go collects a node set and probes another value for
//     an intersection with it (sharesStorage).
//
// # Termination
//
// The seen set is the bound.  Every node is entered at most once, so an
// aliased or cyclic runtime graph — a vector pushed into itself is ordinary,
// legal lisp — terminates without a depth cap.  The callers that previously
// carried a depth cap alongside their seen set were doubly protected; the
// cap is dropped rather than kept at an arbitrary value, because a cap that
// fires would silently SHRINK the node set a disjointness or sealing
// assertion is computed over, turning a real finding into a pass.
func walkValueGraph(v *lisp.LVal, seen map[*lisp.LVal]bool, visit func(*lisp.LVal)) {
	if v == nil || seen[v] {
		return
	}
	seen[v] = true
	visit(v)
	forEachChild(v, func(c *lisp.LVal) { walkValueGraph(c, seen, visit) })
}

// forEachChild visits the LVals a value directly holds.
//
// A sorted-map's entries live in Native, not Cells, so a plain Cells walk
// would treat every map as a leaf — which is exactly how a sharing
// relationship through a map would go unnoticed.  Entries are reached
// through the Map interface's SORTED Keys() rather than the backing Go map,
// so the traversal order is stable across runs and a failure message names
// the same cell every time.
//
// Only entry VALUES are visited, not keys: a sorted-map key is a string held
// by the map itself rather than a node the caller can alias, so visiting
// keys would add nodes to a disjointness set that no caller can share.
func forEachChild(v *lisp.LVal, fn func(*lisp.LVal)) {
	if v.Type == lisp.LSortMap {
		m := v.Map()
		if m == nil {
			return
		}
		ks := m.Keys()
		if ks == nil || ks.Type == lisp.LError {
			return
		}
		for _, k := range ks.Cells {
			if val, ok := m.Get(k); ok {
				fn(val)
			}
		}
		return
	}
	for _, c := range v.Cells {
		fn(c)
	}
}
