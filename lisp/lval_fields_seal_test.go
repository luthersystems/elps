// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// newSealTestEnv builds a stock environment, the way an embedder gets one.
func newSealTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if err := lisp.GoError(lisp.InitializeUserEnv(env)); err != nil {
		t.Fatalf("InitializeUserEnv: %v", err)
	}
	if err := lisp.GoError(lisplib.LoadLibrary(env)); err != nil {
		t.Fatalf("LoadLibrary: %v", err)
	}
	if err := lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))); err != nil {
		t.Fatalf("InPackage: %v", err)
	}
	return env
}

// TestLValFieldSeal is the regression guard for the issue #382 field
// privatization.  Every historical metadata corruption travelled through an
// exported LVal field: the #333/#334 singleton race wrote Quoted, #370's
// stamp wrote MacroExpansion and source metadata onto shared parser nodes,
// and the post-seal leak fixes were Meta-adjacent writes.  Those fields are
// unexported now — the compile-time close for the whole class — and this
// test fails the moment any of them (or a new metadata field) is exported
// again, silently reopening the channel for every embedder.
//
// The allowlist is the deliberate data-read surface that stayed exported:
// Native plus Type/Str/Bytes-via-Native/Int/Float/FunType/Cells (the
// read-accessor migration for those was priced at ~3,000 downstream sites
// and rejected; their writes are covered by the runtime seal, elpsvet, and
// checked-mode verification).  Adding a NEW exported field requires
// updating this list — which is the point: it should take a review
// conversation, not a keystroke.
//
// Anti-vacuity: the test also asserts the exported surface it expects to
// exist — the allowlisted fields themselves and the read accessors that
// replaced the unexported fields (IsQuoted, MacroExpansion, Source) — so an
// accidental mass-unexport cannot pass as "no fields found".
func TestLValFieldSeal(t *testing.T) {
	allowed := map[string]bool{
		"Native":  true,
		"Str":     true,
		"Cells":   true,
		"Type":    true,
		"Int":     true,
		"Float":   true,
		"FunType": true,
	}

	typ := reflect.TypeOf(lisp.LVal{})
	exported := map[string]bool{}
	for i := range typ.NumField() {
		f := typ.Field(i)
		if !f.IsExported() {
			continue
		}
		exported[f.Name] = true
		if !allowed[f.Name] {
			t.Errorf("LVal exports field %q outside the sanctioned data-read surface; "+
				"issue #382 unexported the metadata/flag fields (Quoted, Spliced, Meta, "+
				"MacroExpansion) because every historical shared-AST corruption went "+
				"through one — new exported fields need a review conversation, an "+
				"accessor design, and an entry in this allowlist", f.Name)
		}
	}
	for name := range allowed {
		if !exported[name] {
			t.Errorf("LVal no longer exports %q — the allowlisted data-read surface "+
				"is public API; removing it is a downstream break this test must not "+
				"mask", name)
		}
	}

	// The unexported fields are readable through these accessors; their
	// presence proves the channel was mediated, not deleted.
	for _, method := range []string{"IsQuoted", "MacroExpansion", "Source", "FID", "Package", "Builtin"} {
		if _, ok := reflect.PtrTo(typ).MethodByName(method); !ok {
			t.Errorf("(*LVal).%s missing — the unexported-field read surface changed; "+
				"update the #382 accessor set deliberately, not by accident", method)
		}
	}

	// MapData joined the seal in the same release: its backing Map is fixed
	// at construction (NewMapData) and the embedded field is unexported, so
	// v.Map().Map = other no longer compiles anywhere outside this module.
	mdType := reflect.TypeOf(lisp.MapData{})
	for i := range mdType.NumField() {
		if f := mdType.Field(i); f.IsExported() {
			t.Errorf("MapData exports field %q; the backing was fixed at construction "+
				"in issue #382 — swapping a shared sorted-map's backing in place was an "+
				"open mutation channel", f.Name)
		}
	}
	// Anti-vacuity: the promoted read/write surface embedders rely on.
	for _, method := range []string{"Get", "Set", "Del", "Keys", "Entries", "Len"} {
		if _, ok := reflect.PtrTo(mdType).MethodByName(method); !ok {
			t.Errorf("(*MapData).%s missing — the promoted Map method set is public "+
				"API; the #382 seal must not remove it", method)
		}
	}
}

// TestFunDataPayloadFieldSeal guards the one privatized field the #382
// unexporting did NOT actually seal.
//
// Unexporting a TYPE is not the same as sealing it.  An LFun carries its
// funData in LVal.Native, which is exported and stays exported (it is the
// generic Go-value channel, ~3,000 downstream sites).  reflect can read an
// EXPORTED field of an unexported struct reached that way and hand back a
// usable value — reflect.Value.Interface only refuses values obtained from
// UNEXPORTED fields.  So while funData.Env was exported, this compiled in
// any embedder, with no unsafe:
//
//	fd := reflect.ValueOf(fn.Native).Elem()
//	captured := fd.FieldByName("Env").Interface().(*lisp.LEnv)
//	captured.Put(lisp.Symbol("n"), lisp.Int(9999)) // rebinds inside the closure
//
// and the closure's result changed underneath its owner — the captured-
// environment aliasing channel internal/funraw's doc comment says an
// embedder "cannot reach at all".  Every other privatized field was
// already immune because it is an unexported FIELD (LVal.source, meta,
// macroExpansion, quoted, spliced, sealed; LEnv.scope, funName, parent,
// loc; MapData's backing) — reflect panics on those.  funData's fields
// were the asymmetry; they are unexported now.
//
// This guard fails if any of them is exported again, which would silently
// reopen the reflective route.
func TestFunDataPayloadFieldSeal(t *testing.T) {
	env := newSealTestEnv(t)
	fn := env.LoadString("seal-probe", `(lambda (n) n)`)
	if fn.Type != lisp.LFun {
		t.Fatalf("expected a function value, got %v: %v", fn.Type, fn)
	}
	if fn.Native == nil {
		t.Fatal("function value carries no Native payload; this guard needs one to inspect")
	}

	typ := reflect.TypeOf(fn.Native)
	if typ.Kind() == reflect.Ptr {
		typ = typ.Elem()
	}
	if typ.Kind() != reflect.Struct {
		t.Fatalf("function payload is a %v, not a struct — reword this guard", typ.Kind())
	}
	// Anti-vacuity: a payload with no fields at all would pass trivially.
	if typ.NumField() == 0 {
		t.Fatal("function payload has no fields; the guard would pass vacuously")
	}
	for i := range typ.NumField() {
		if f := typ.Field(i); f.IsExported() {
			t.Errorf("the LFun payload exports field %q. LVal.Native is exported, so an "+
				"exported field on the payload is readable — and usable — from any embedder "+
				"through plain reflection, which defeats the issue #382 seal for that field. "+
				"The captured environment reached this way is a live rebinding channel into "+
				"every closure sharing it. Keep the payload's fields unexported; in-repo "+
				"tooling uses internal/funraw", f.Name)
		}
	}

	// The identity surface the unexporting must keep serving.
	if fn.FID() == "" {
		t.Error("(*LVal).FID returned empty for a lambda; unexporting the payload fields " +
			"must not break the mediated identity reads")
	}
	if fn.Package() == "" {
		t.Error("(*LVal).Package returned empty for a lambda")
	}
	if fn.Builtin() != nil {
		t.Error("(*LVal).Builtin returned non-nil for a user-defined lambda")
	}
}

// TestLEnvFieldSeal guards the environment half of the issue #382 close.
// LVal was never the only mutable channel an embedder held: every builtin is
// handed an *LEnv, and while its scope map was exported, `env.Scope[sym] = v`
// rebound a symbol in a live environment — or, through a closure's captured
// environment, in every function value sharing it — without passing Put, the
// runtime seal, or elpsvet.  `env.Loc = loc` aliased a caller's mutable
// location into every error and frame the evaluator stamped next, the #362
// class one layer up.  Those fields are unexported; Runtime and ID stay
// public (16 and 3 downstream production reads respectively, and neither is
// a container an embedder can corrupt in place).
func TestLEnvFieldSeal(t *testing.T) {
	allowed := map[string]bool{
		"Runtime": true,
		"ID":      true,
	}
	typ := reflect.TypeOf(lisp.LEnv{})
	exported := map[string]bool{}
	for i := range typ.NumField() {
		f := typ.Field(i)
		if !f.IsExported() {
			continue
		}
		exported[f.Name] = true
		if !allowed[f.Name] {
			t.Errorf("LEnv exports field %q outside the sanctioned surface; the "+
				"binding state (scope, funName), the lexical chain (parent) and the "+
				"evaluator location (loc) were unexported in issue #382 — a new "+
				"exported field on the type every builtin receives needs a review "+
				"conversation and an entry in this allowlist", f.Name)
		}
	}
	for name := range allowed {
		if !exported[name] {
			t.Errorf("LEnv no longer exports %q — it is public API with downstream "+
				"production readers; removing it is a break this test must not mask", name)
		}
	}

	// Anti-vacuity: the mediated read surface that replaced the fields.
	for _, method := range []string{"Bindings", "NumBindings", "Parent", "Source", "Get", "Put", "GetGlobal", "PutGlobal"} {
		if _, ok := reflect.PtrTo(typ).MethodByName(method); !ok {
			t.Errorf("(*LEnv).%s missing — the environment read/bind surface changed; "+
				"update the #382 accessor set deliberately, not by accident", method)
		}
	}
}
