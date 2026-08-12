// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

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
