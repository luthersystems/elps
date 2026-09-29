// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// reserve only ever presizes an empty, unfrozen, unplanned package: a package
// that already holds a binding keeps it.
func TestPackageReserveKeepsBindings(t *testing.T) {
	pkg := NewPackage("p")
	pkg.reserve(64)
	pkg.putName("a", Int(1))
	pkg.reserve(64) // not empty any more: a no-op
	v, ok := pkg.Symbol("a")
	if !ok || v.Int != 1 {
		t.Fatalf("binding lost after reserve: %v %v", v, ok)
	}
	if n := len(pkg.SymbolNames()); n != 1 {
		t.Fatalf("reserve changed the package: %d names", n)
	}
}
