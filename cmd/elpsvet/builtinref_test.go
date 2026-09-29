// Copyright © 2026 The ELPS authors

package main

import (
	"go/types"
	"testing"

	"golang.org/x/tools/go/packages"
)

// docs/embed.md recommends resolving a builtin handle once, in a package
// var (var builtinGet = lisp.BuiltinFunc("get")).  That is only sound if
// lisp.BuiltinRef keeps no *lisp.LVal reachable, and elpsownership checks
// exactly that, so the pattern must pass it (luthersystems/elps#745).
func TestBuiltinRefPassesOwnership(t *testing.T) {
	pkgs, err := packages.Load(&packages.Config{Mode: packages.NeedTypes | packages.NeedName}, lispPkgPath)
	if err != nil {
		t.Fatal(err)
	}
	if len(pkgs) != 1 || pkgs[0].Types == nil {
		t.Fatalf("could not load %s: %v", lispPkgPath, pkgs)
	}
	obj := pkgs[0].Types.Scope().Lookup("BuiltinRef")
	if obj == nil {
		t.Fatal("lisp.BuiltinRef not found")
	}
	if containsLVal(obj.Type(), make(map[types.Type]bool)) {
		t.Errorf("lisp.BuiltinRef keeps *lisp.LVal reachable; a package-level handle trips elpsownership")
	}
}
