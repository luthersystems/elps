// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// TestMacroExpansionDoesNotStampFunctionValues pins that a VALUE yielded
// by a macro expansion keeps its own location.  stampGuarded walks the
// expansion and writes the macro call site onto every unsealed node that
// has no location; a builtin handed out by Get has no location by design
// (it renders as "<native code>"), and it is a live binding, not syntax --
// Package.Get returns the stored value, and LEnv.Get does too for a name
// that matches.  Before the guard, (defmacro m () lisp:car) followed by
// (m) moved lisp:car's definition site onto the call site for the rest of
// the process, which the profiler then reported as the builtin's file.
// The same holds for any non-syntax value a macro yields: the sorted map
// bound to a global below must not gain a location either.
func TestMacroExpansionDoesNotStampFunctionValues(t *testing.T) {
	env := newForkTestEnv(t)
	for i, form := range []*LVal{
		SExpr([]*LVal{Symbol("defmacro"), Symbol("m"), SExpr(nil), Symbol("lisp:car")}),
		SExpr([]*LVal{Symbol("m")}),
		SExpr([]*LVal{Symbol("set"), SExpr([]*LVal{Symbol("quote"), Symbol("table")}), SExpr([]*LVal{Symbol("sorted-map")})}),
		SExpr([]*LVal{Symbol("defmacro"), Symbol("mt"), SExpr(nil), Symbol("table")}),
		SExpr([]*LVal{Symbol("mt")}),
	} {
		// The stamp is the macro CALL SITE, so the call form needs a real
		// location, as every form read from a file has.
		form.SetSource(&token.Location{File: "stamp.lisp", Pos: 20 * i, Line: i + 1, Col: 1})
		form.SealAST()
		if res := env.Eval(form); res.Type == LError {
			t.Fatalf("eval %v: %v", form, res)
		}
	}
	pkg := env.Runtime.Registry.packages["lisp"]
	if pkg == nil {
		t.Fatal("lisp package not in registry")
	}
	car, ok := pkg.Symbol("car")
	if !ok || car == nil || car.Type != LFun {
		t.Fatalf("lisp:car is not a bound function: %v", car)
	}
	if loc, ok := car.Source(); ok {
		t.Errorf("lisp:car gained a source location from a macro expansion: %v", loc)
	}
	if car.macroExpansion != nil {
		t.Errorf("lisp:car gained macro-expansion metadata")
	}
	table := env.Runtime.Package.Get(Symbol("table"))
	if table.Type != LSortMap {
		t.Fatalf("table is not a sorted map: %v", table)
	}
	if loc, ok := table.Source(); ok {
		t.Errorf("a sorted-map value gained a source location from a macro expansion: %v", loc)
	}
}

// TestMacroExpansionStampsValuesOnPrivateHeaders is the class-level guard
// behind the warning above stampMacroExpansion: the stamp must never write to
// a VALUE.  Placed at the root of an expansion the value is returned as a
// stamped header copy; placed inside an expansion-owned list it is replaced
// in that list by one.  In both cases the original keeps a nil source and no
// macro-expansion metadata, and the copy shares the value's storage.  SYNTAX
// nodes with no location (the reader's types) are likewise replaced by
// stamped copies in the returned tree (issue #582); the stamp writes to
// nothing it was handed.
//
// COVERAGE, stated exactly rather than as "every value type": the fixtures
// below are the value types a macro body can actually hand back -- LFun,
// LNative, LSortMap, LArray, LBytes and LTaggedVal (a deftype/new value).
// The remaining non-syntax types are unreachable here and are deliberately
// not fixtures: LError is returned before the stamp (macroCall bails on
// r.Type == LError), and LInvalid, LMarkTerminal, LMarkTailRec,
// LMarkMacExpand and LTypeMax are evaluator-internal marks that no macro body
// can produce -- macroCall wraps its own result in the last of them AFTER
// stamping.  If a new LType is added, add it here or record why it cannot
// reach this walk.
func TestMacroExpansionStampsValuesOnPrivateHeaders(t *testing.T) {
	env := newForkTestEnv(t)
	car, ok := env.Runtime.Registry.packages["lisp"].Symbol("car")
	if !ok || car.Type != LFun {
		t.Fatalf("lisp:car is not a bound function: %v", car)
	}
	callSite := &token.Location{File: "stamp.lisp", Pos: 40, Line: 3, Col: 1}
	// A deftype/new value.  Its location is cleared because LEnv.TaggedValue
	// constructs one at env.loc, and a value that already carries a location
	// is returned unchanged -- the fixture has to be unlocated to exercise
	// the copy, like every other value here.
	tagged := env.TaggedValue(Symbol("user:point"), Int(1))
	if tagged.Type != LTaggedVal {
		t.Fatalf("TaggedValue did not produce a tagged value: %v", tagged)
	}
	tagged.SetSource(nil)
	values := map[string]*LVal{
		"builtin":      car,
		"native":       Native(&struct{ n int }{n: 1}),
		"sorted-map":   SortedMap(),
		"vector":       Vector([]*LVal{Int(1), Int(2)}),
		"bytes":        Bytes([]byte("b")),
		"tagged-value": tagged,
	}
	for name, v := range values {
		t.Run(name+"/root", func(t *testing.T) {
			if sealableNodeType(v.Type) {
				t.Fatalf("%v is a syntax type; the fixture is wrong", v.Type)
			}
			got := stampMacroExpansion(v, callSite, nil, env.Runtime)
			if got == v {
				t.Fatalf("a value root was returned as itself, not a private copy")
			}
			if got.source != callSite {
				t.Errorf("the copy is not stamped: %v", got.source)
			}
			if v.source != nil || v.macroExpansion != nil {
				t.Errorf("the value itself was written to: source %v, macroExpansion %v", v.source, v.macroExpansion)
			}
			if got.Type != v.Type || got.Native != v.Native || (len(v.Cells) > 0 && &got.Cells[0] != &v.Cells[0]) {
				t.Errorf("the copy does not share the value's storage")
			}
		})
		t.Run(name+"/child", func(t *testing.T) {
			expansion := SExpr([]*LVal{Symbol("identity"), v})
			got := stampMacroExpansion(expansion, callSite, nil, env.Runtime)
			if got == expansion {
				t.Fatalf("a syntax root that needs a stamp must be replaced by a stamped copy, not written (issue #582)")
			}
			if got.source != callSite || got.Cells[0].source != callSite {
				t.Errorf("syntax nodes of the returned tree were not stamped")
			}
			if expansion.source != nil || expansion.Cells[0].source != nil || expansion.Cells[1] != v {
				t.Errorf("the input expansion was written to")
			}
			if got.Cells[1] == v {
				t.Fatalf("the value was left in the expansion instead of a private copy")
			}
			if got.Cells[1].source != callSite {
				t.Errorf("the copy in the expansion is not stamped: %v", got.Cells[1].source)
			}
			if v.source != nil || v.macroExpansion != nil {
				t.Errorf("the value itself was written to: source %v, macroExpansion %v", v.source, v.macroExpansion)
			}
		})
	}
	// With a debugger context every stamped node also gets expansion
	// metadata; the value must not.
	ctx := &macroExpansionContext{CallSite: callSite, Name: "m"}
	v := SortedMap()
	got := stampMacroExpansion(v, callSite, ctx, env.Runtime)
	if got.macroExpansion == nil {
		t.Errorf("the copy did not get macro-expansion metadata under a debugger context")
	}
	if v.macroExpansion != nil {
		t.Errorf("the value got macro-expansion metadata")
	}
}
