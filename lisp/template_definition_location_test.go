// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// Issue #624: function definition locations are retained program metadata,
// unlike the environment's transient current-evaluation location register.
func TestTemplatePlanOwnsFunctionDefinitionLocation(t *testing.T) {
	source := templateOwnershipEnv()
	source.loc = &token.Location{File: "definitions.lisp", Line: 7, Col: 3}
	fn := source.Lambda(Formals(), []*LVal{Int(1)})
	source.Runtime.Package.symbols["fn"] = fn
	fd := fn.Native.(*funData)
	plan, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	// The compiler must own the definition snapshot, not a live source pointer.
	fd.loc.File = "source-mutated.lisp"
	fd.loc.Line = 99
	for range 2 {
		vm, err := plan.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		got := vm.Runtime.Package.symbols["fn"].Native.(*funData)
		if got.loc == nil || got.loc == fd.loc || got.loc.File != "definitions.lisp" || got.loc.Line != 7 || got.loc.Col != 3 {
			t.Fatalf("definition location was dropped or borrowed: %+v", got.loc)
		}
		if got.env != vm || vm.loc != nil {
			t.Fatal("definition environment not remapped or transient register retained")
		}
	}
}
