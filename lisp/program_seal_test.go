// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestProgramSeal is the compile-time seal's regression guard.  Program's
// entire value is that no exported surface hands out the sealed AST: the
// moment someone adds `func (p Program) Exprs() []*LVal` the boundary
// guarantee is gone, silently, for every embedder.  A negative-compilation
// testdata harness would prove the same thing with far more machinery; this
// test walks the reflected method set instead and fails if any exported
// method can reach a *lisp.LVal through its results.
//
// There is no exemption: the deep-copy escape hatch (Program.detach,
// lisp/program.go) is unexported until a real embedder consumer
// materializes, so today NO exported method may expose *LVal.  The bridge
// in export_test.go is a package-level function, not a Program method, so
// it never appears in this method set.
//
// Anti-vacuity: the test also asserts the surface it expects to exist
// (Len, String, and the LEnv producers/consumers), so an accidental
// mass-deletion cannot pass as "no leaks found".
func TestProgramSeal(t *testing.T) {
	lvalPtr := reflect.TypeOf((*lisp.LVal)(nil))

	// exposes reports whether typ can reach a *lisp.LVal through values of
	// itself: directly, or via pointer/slice/array/map/chan elements, struct
	// fields (exported or not — an exported field of struct type could nest
	// one), or the results of a func value.
	seen := map[reflect.Type]bool{}
	var exposes func(typ reflect.Type) bool
	exposes = func(typ reflect.Type) bool {
		if typ == lvalPtr {
			return true
		}
		if seen[typ] {
			return false // already under inspection or known clean
		}
		seen[typ] = true
		switch typ.Kind() {
		case reflect.Ptr, reflect.Slice, reflect.Array, reflect.Chan:
			return exposes(typ.Elem())
		case reflect.Map:
			return exposes(typ.Key()) || exposes(typ.Elem())
		case reflect.Struct:
			for i := range typ.NumField() {
				if exposes(typ.Field(i).Type) {
					return true
				}
			}
		case reflect.Func:
			for i := range typ.NumOut() {
				if exposes(typ.Out(i)) {
					return true
				}
			}
		case reflect.Interface:
			// An interface result could dynamically hold anything, including
			// a *LVal.  Program has no interface-returning methods today
			// (String returns string); adding one would need a conscious
			// exemption here.
			return typ.NumMethod() > 0 || typ == reflect.TypeOf((*any)(nil)).Elem()
		default:
			// Scalar kinds cannot reference an LVal.
		}
		return false
	}

	// Walk *Program's method set: it is a superset of Program's, so value
	// and pointer receivers are both covered.
	typ := reflect.TypeOf((*lisp.Program)(nil))
	found := map[string]bool{}
	for i := range typ.NumMethod() {
		m := typ.Method(i)
		found[m.Name] = true
		for j := range m.Type.NumOut() {
			if exposes(m.Type.Out(j)) {
				t.Errorf("Program.%s result %d (%s) can expose *lisp.LVal — the seal is broken",
					m.Name, j, m.Type.Out(j))
			}
		}
	}

	// Anti-vacuity: the expected surface exists with the expected shapes.
	if !found["Len"] || !found["String"] {
		t.Fatalf("Program method set %v is missing expected methods Len/String", found)
	}
	lenM, _ := typ.MethodByName("Len")
	if lenM.Type.NumOut() != 1 || lenM.Type.Out(0).Kind() != reflect.Int {
		t.Errorf("Len has unexpected signature %v", lenM.Type)
	}
	// The escape hatch stays unexported: re-exporting Detach is a conscious
	// API decision (it needs a consumer), not something that should slip in.
	if _, ok := typ.MethodByName("Detach"); ok {
		t.Error("Program.Detach is exported; the escape hatch is meant to stay unexported until a real consumer appears")
	}

	// No exported fields: an exported exprs slice would be a leak without
	// any method at all.
	pt := reflect.TypeOf(lisp.Program{})
	if pt.NumField() == 0 {
		t.Error("Program has no fields; expected the unexported exprs slice")
	}
	for i := range pt.NumField() {
		if f := pt.Field(i); f.IsExported() {
			t.Errorf("Program has exported field %s %s — the seal is broken", f.Name, f.Type)
		}
	}

	// The producer/consumer surface exists on LEnv (anti-vacuity for the
	// entrypoint half of the design).
	envT := reflect.TypeOf((*lisp.LEnv)(nil))
	for _, name := range []string{"ParseProgram", "LoadProgram", "LoadProgramContext"} {
		if _, ok := envT.MethodByName(name); !ok {
			t.Errorf("(*LEnv).%s is missing", name)
		}
	}
}
