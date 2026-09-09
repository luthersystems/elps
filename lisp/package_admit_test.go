// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// This file covers issue #524: PackageRegistry.AddPackage admitting an
// externally built package with no admission check, so a caller's mutable
// symbol-table values became the interpreter state of one Runtime (or of
// several) while the caller still held pointers to them.
//
// It is the same root shape #523 fixed for Programs, on a surface with a
// different contract: a package legitimately holds builtins, natives,
// sorted-maps and other runtime data, so the admission cannot be "seal it or
// refuse it".  The rule it implements instead is stated per value class in
// lisp/package_admit.go, and this file is that rule's test:
//
//	(1) TestAddPackageSealsAliasedSymbolValues — the reproduction: one
//	    package, two registries, an unsealed list the caller keeps.  Pre-fix
//	    the first environment's stable-sort rewrote the literal for the
//	    second environment AND for the caller.
//	(2) TestAddPackageDoesNotHoldCallersPackage — the container half: what
//	    is registered is a snapshot, so a later write through the caller's
//	    *Package does not reach the Runtime.
//	(3) TestAddPackageSharesAdmissibleValues — the fast-path pin: sealed
//	    values, builtins, natives and maps are admitted BY REFERENCE.  If
//	    someone later swaps the admission for an unconditional deep copy,
//	    this fails and they have to say so — the doc/LSP/MCP registry merges
//	    (and substrate's booted shiro registry behind them) are made almost
//	    entirely of these classes.
//	(4) TestAddPackageSnapshotPreservesPackageMetadata — the snapshot is a
//	    snapshot: docs, exports and the FID→name table survive it.
//	(5) TestAddPackageAdmitsCyclicValueByReference — a value that contains
//	    itself is admitted by reference rather than copied, because Copy()
//	    on it would not terminate (issue #390).
//
// The checked-mode half of the red proof lives in
// package_admit_elpscheck_test.go.

// addPkgRedName is the package the red-proof fixtures build by hand, the way
// an embedder assembling a package in Go does.
const addPkgRedName = "addpkg-red"

// addPkgProbe is the probe program, in the shape lisp/program_seal_gap_test.go
// uses: the let materializes the list's head BEFORE the sort touches
// anything, so the value returned is an immutable snapshot of what THIS
// environment saw — 10 from a pristine binding, 30 from the wreckage another
// environment left behind.
//
// The sort sits under ignore-errors because on a properly admitted (sealed)
// binding it is refused with modify-literal-error.  That refusal is not what
// this file tests; swallowing it keeps the probe alive for what it does test.
const addPkgProbe = `(use-package 'addpkg-red)
(let ([pre (car limits)]) (ignore-errors (stable-sort > limits)) pre)
`

// addPkgWant is what every environment must read: the pristine head.
const addPkgWant = "10"

// hostilePackage builds the fixture: a package bound to a runtime-built list
// — fresh mutable storage, sealed by nothing — which the caller retains and
// hands to as many registries as it likes.  Returns the package and the list
// the caller keeps aliasing.
func hostilePackage(t *testing.T) (*lisp.Package, *lisp.LVal) {
	t.Helper()
	limits := lisp.QExpr([]*lisp.LVal{lisp.Int(10), lisp.Int(20), lisp.Int(30)})
	// Anti-vacuity: a fixture that arrived sealed would exercise the fast
	// path and prove nothing about the hazard class.
	if limits.IsSealed() {
		t.Fatal("anti-vacuity: the hand-built list is sealed; the fixture no longer models unsealed runtime storage")
	}
	pkg := lisp.NewPackage(addPkgRedName)
	if lerr := pkg.Put(lisp.Symbol("limits"), limits); lerr.Type == lisp.LError {
		t.Fatalf("put limits: %v", lerr)
	}
	pkg.Export("limits")
	return pkg, limits
}

// TestAddPackageSealsAliasedSymbolValues is issue #524's reproduction.
//
// One hand-built package is added to two independent environments — the
// topology of every registry merge, and of an embedder that installs its
// package into every environment it creates.  Each environment snapshots the
// binding's head before sorting it in place; every snapshot must be the
// pristine one, and the caller's own list must be untouched afterwards.
//
// On the unfixed tree environment 1's stable-sort rewrote the one shared
// list, so environment 2 read 30 before evaluating anything of its own, and
// the caller's `limits` came back reordered from a call it never made.
func TestAddPackageSealsAliasedSymbolValues(t *testing.T) {
	pkg, limits := hostilePackage(t)

	envs := []*lisp.LEnv{programTestEnv(t), programTestEnv(t)}
	for i, env := range envs {
		if !env.Runtime.Registry.AddPackage(pkg) {
			t.Fatalf("environment %d: AddPackage refused the package", i+1)
		}
	}

	// The admitted binding must be a private, sealed copy in each registry:
	// not the caller's node, and not the other registry's node either.
	admitted := make([]*lisp.LVal, len(envs))
	for i, env := range envs {
		v, ok := env.Runtime.Registry.Package(addPkgRedName).Symbol("limits")
		if !ok {
			t.Fatalf("environment %d: admitted package has no `limits` binding", i+1)
		}
		if v == limits {
			t.Errorf("environment %d: the registry admitted the caller's retained node by reference", i+1)
		}
		if !v.IsSealed() {
			t.Errorf("environment %d: the admitted binding is unsealed; the admission did not seal the copy", i+1)
		}
		admitted[i] = v
	}
	if admitted[0] == admitted[1] {
		t.Error("two registries share one admitted node; the admission copied per registry, or should have")
	}

	for i, env := range envs {
		got := env.LoadString("addpkg.lisp", addPkgProbe)
		if got.Type == lisp.LError {
			t.Fatalf("environment %d: %v", i+1, got)
		}
		if got.String() != addPkgWant {
			t.Errorf("environment %d read a write through the shared package: got %v, want %s (the pristine binding)",
				i+1, got, addPkgWant)
		}
	}

	// The caller's list is the other direction of the same guarantee: a
	// Runtime evaluating an admitted package must not rewrite what the
	// caller still holds.
	if got := limits.Cells[0].Int; got != 10 {
		t.Errorf("the caller's retained list was reordered by an environment: head = %d, want 10", got)
	}
}

// TestAddPackageDoesNotHoldCallersPackage covers the container half of the
// contract.  The registry stores a snapshot, so a caller that keeps writing
// its own *Package after registering it — the widest write channel into
// another Runtime's interpreter state — no longer reaches the registry.
func TestAddPackageDoesNotHoldCallersPackage(t *testing.T) {
	pkg, _ := hostilePackage(t)
	env := programTestEnv(t)
	if !env.Runtime.Registry.AddPackage(pkg) {
		t.Fatal("AddPackage refused the package")
	}
	registered := env.Runtime.Registry.Package(addPkgRedName)
	if registered == pkg {
		t.Fatal("the registry stored the caller's *Package; a later write through it lands in the Runtime")
	}

	// Every write an embedder has: a new binding, a rebound one, a new
	// export.
	if lerr := pkg.Put(lisp.Symbol("injected"), lisp.String("surprise")); lerr.Type == lisp.LError {
		t.Fatalf("put injected: %v", lerr)
	}
	if lerr := pkg.Put(lisp.Symbol("limits"), lisp.String("replaced")); lerr.Type == lisp.LError {
		t.Fatalf("rebind limits: %v", lerr)
	}
	pkg.Export("injected")

	if _, ok := registered.Symbol("injected"); ok {
		t.Error("a binding created after AddPackage reached the registry")
	}
	v, ok := registered.Symbol("limits")
	if !ok {
		t.Fatal("the registered package lost its `limits` binding")
	}
	if v.Type != lisp.LSExpr {
		t.Errorf("a rebind after AddPackage reached the registry: limits is now %v", v.Type)
	}
	for _, name := range registered.Externals() {
		if name == "injected" {
			t.Error("an export declared after AddPackage reached the registry")
		}
	}
}

// TestAddPackageSharesAdmissibleValues pins the by-reference rows of the
// admission table.  Sealed values are the sanctioned cross-environment share
// and must not be forked; functions, natives, sorted-maps and mixed trees
// are classes no seal covers, and copying them would be either impossible or
// a semantic change.  The registry merges in cmd/doc.go and mcpserver (which
// is how a booted embedder registry reaches the doc/LSP/MCP tools) are made
// almost entirely of these values.
func TestAddPackageSharesAdmissibleValues(t *testing.T) {
	// A sealed value, obtained the way real bindings get one: by evaluating
	// a literal.
	src := programTestEnv(t)
	sealedVal := src.LoadString("sealed.lisp", `'(1 2 3)`)
	if sealedVal.Type == lisp.LError {
		t.Fatalf("evaluate literal: %v", sealedVal)
	}
	if !sealedVal.IsSealed() {
		t.Fatal("anti-vacuity: a quoted literal evaluated to an unsealed value; the fast-path fixture is broken")
	}

	fn := lisp.Fun("addpkg-share-fun", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Nil() })
	native := lisp.Native(t)
	vector := lisp.Vector([]*lisp.LVal{lisp.Int(1)})
	sortmap := lisp.SortedMap()
	// A mixed tree: a list holding a value the seal cannot mark.  Copying it
	// would deep-copy around a node that must keep reference semantics, so
	// it is admitted whole, by reference.
	mixed := lisp.QExpr([]*lisp.LVal{lisp.Int(1), native})

	pkg := lisp.NewPackage("addpkg-share")
	shared := map[string]*lisp.LVal{
		"sealed": sealedVal,
		"fn":     fn,
		"native": native,
		"vector": vector,
		"map":    sortmap,
		"mixed":  mixed,
	}
	for name, v := range shared {
		if lerr := pkg.Put(lisp.Symbol(name), v); lerr.Type == lisp.LError {
			t.Fatalf("put %s: %v", name, lerr)
		}
	}

	env := programTestEnv(t)
	if !env.Runtime.Registry.AddPackage(pkg) {
		t.Fatal("AddPackage refused the package")
	}
	registered := env.Runtime.Registry.Package("addpkg-share")
	for name, want := range shared {
		got, ok := registered.Symbol(name)
		if !ok {
			t.Errorf("%s: binding lost by admission", name)
			continue
		}
		if got != want {
			t.Errorf("%s: admission copied a value it must share by reference (%v)", name, want.Type)
		}
	}
}

// TestAddPackageSnapshotPreservesPackageMetadata proves the snapshot carries
// everything the registry serves besides the values: the package doc, the
// export list (so use-package still imports), per-symbol docs, and the
// FID→name table that names functions in stack traces and error messages.
func TestAddPackageSnapshotPreservesPackageMetadata(t *testing.T) {
	pkg, _ := hostilePackage(t)
	pkg.Doc = "red-proof package"
	fn := lisp.FunInPackage(addPkgRedName, "addpkg-red-fid", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Nil() })
	if lerr := pkg.Put(lisp.Symbol("noop"), fn); lerr.Type == lisp.LError {
		t.Fatalf("put noop: %v", lerr)
	}
	pkg.Export("noop")

	env := programTestEnv(t)
	if !env.Runtime.Registry.AddPackage(pkg) {
		t.Fatal("AddPackage refused the package")
	}
	registered := env.Runtime.Registry.Package(addPkgRedName)

	if registered.Doc != pkg.Doc {
		t.Errorf("package doc lost by admission: %q", registered.Doc)
	}
	if got, want := strings.Join(registered.Externals(), ","), strings.Join(pkg.Externals(), ","); got != want {
		t.Errorf("export list = %q, want %q", got, want)
	}
	if got := registered.GetFunName(fn.FID()); got != "noop" {
		t.Errorf("GetFunName(%s) = %q, want \"noop\"; the FID→name table did not survive admission", fn.FID(), got)
	}

	// use-package must still import the exports out of the snapshot.
	res := env.LoadString("meta.lisp", `(use-package 'addpkg-red) (car limits)`)
	if res.Type == lisp.LError {
		t.Fatalf("use-package the admitted package: %v", res)
	}
	if res.String() != addPkgWant {
		t.Errorf("(car limits) = %v, want %s", res, addPkgWant)
	}
}

// TestAddPackageAdmitsCyclicValueByReference covers the walk's termination
// guarantee.  A value can contain itself (issue #390); the classification
// reports "neither sealed nor sealable" for a cycle -- it cannot be sealed
// throughout -- and the value takes the by-reference row.  (Copy() used not
// to terminate on a cycle either; since lisp/copier.go it does, and
// TestCopyTerminatesOnACycle pins that, but the by-reference row does not
// depend on it.)  The
// assertion that matters is that AddPackage returns at all.
func TestAddPackageAdmitsCyclicValueByReference(t *testing.T) {
	cyclic := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	cyclic.Cells = append(cyclic.Cells, cyclic)

	pkg := lisp.NewPackage("addpkg-cyclic")
	if lerr := pkg.Put(lisp.Symbol("loop"), cyclic); lerr.Type == lisp.LError {
		t.Fatalf("put loop: %v", lerr)
	}

	env := programTestEnv(t)
	if !env.Runtime.Registry.AddPackage(pkg) {
		t.Fatal("AddPackage refused the package")
	}
	got, ok := env.Runtime.Registry.Package("addpkg-cyclic").Symbol("loop")
	if !ok {
		t.Fatal("cyclic binding lost by admission")
	}
	if got != cyclic {
		t.Error("admission copied a cyclic value; the copy cannot terminate and the walk must have refused it")
	}
}
