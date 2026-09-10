// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
)

func TestWalkerBehaviorOracleSelectionSurvivesReordering(t *testing.T) {
	walkers := oracleWalkers()
	// Every adapter occupies every index in these rotations. Check actual
	// operation behavior too, not just a name copied onto the wrong callback.
	for shift := range walkers {
		reordered := append(slices.Clone(walkers[shift:]), walkers[:shift]...)
		for _, tc := range []struct {
			name, owners                      string
			shares, refuses, stamps, template bool
		}{
			{name: "copy", owners: "detacher", shares: true},
			{name: "detach", owners: "detacher", refuses: true},
			{name: "LVal.Copy", owners: "copier"},
			{name: "macro stamp", owners: "macroStamper", stamps: true},
			{name: "Template.NewVM", owners: "templateInventory templateCompiler", template: true},
		} {
			t.Run(fmt.Sprintf("rotation%d/%s", shift, tc.name), func(t *testing.T) {
				w, err := oracleWalkerByName(reordered, tc.name)
				if err != nil {
					t.Fatal(err)
				}
				if w.name != tc.name || w.owners != tc.owners || w.stamps != tc.stamps || w.template != tc.template {
					t.Fatalf("wrong adapter for %q: name=%q owners=%q stamps=%t template=%t", tc.name, w.name, w.owners, w.stamps, w.template)
				}
				v := Native(int64(7))
				a, b, err := w.makeCopies(v)
				if tc.refuses {
					if err == nil || a != nil || b != nil || !strings.Contains(err.Error(), "cannot be detached") {
						t.Fatalf("strict detach selected the wrong operation: %v", err)
					}
					return
				}
				if err != nil {
					t.Fatal(err)
				}
				for _, result := range []*LVal{a, b} {
					if result == nil || result.Type != LNative || result.Native != int64(7) || (result == v) != tc.shares || (result.macroExpansion != nil) != tc.stamps {
						t.Fatalf("%s selected the wrong behavior: %v", tc.name, result)
					}
				}
			})
		}
	}
	if _, err := oracleWalkerByName(walkers, "removed operation"); err == nil || !strings.Contains(err.Error(), "missing oracle walker") {
		t.Fatalf("a missing adapter must not fall back to another operation: %v", err)
	}
	duplicate := append(slices.Clone(walkers), mustOracleWalker(t, walkers, "copy"))
	if _, err := oracleWalkerByName(duplicate, "copy"); err == nil || !strings.Contains(err.Error(), "duplicate oracle walker") {
		t.Fatalf("ambiguous names must not pick whichever adapter came first: %v", err)
	}
}

func TestWalkerBehaviorOracleReportsPostWriteTraversalFailure(t *testing.T) {
	for _, w := range oracleWalkers() {
		if w.stamps {
			continue // Stamping intentionally performs no payload mutation probes.
		}
		t.Run(w.name, func(t *testing.T) {
			cells := make([]*LVal, 200)
			for i := range cells {
				cells[i] = SortedMap()
				cells[i].Map().Set(String("n"), Int(i))
			}
			root := SExpr(cells)
			nodes, err := oracleNodes(root)
			if err != nil || len(nodes) != 401 {
				t.Fatalf("fixture must fit before probing: nodes=%d error=%v", len(nodes), err)
			}
			// Adding a probe entry to each map exceeds the census bound on
			// the mutated arm itself. That is not evidence of a sibling leak.
			err = oracleCheck(w, root)
			if err == nil || !strings.Contains(err.Error(), "snapshot arm 0 after writes to arm 0") || strings.Contains(err.Error(), "isolation:") {
				t.Fatalf("want a contextual traversal error, not an isolation claim: %v", err)
			}
			cause := errors.Unwrap(err)
			if cause == nil || !strings.Contains(cause.Error(), "graph traversal exceeded bound") {
				t.Fatalf("underlying traversal failure was lost: %v", err)
			}
		})
	}
}

type oracleImmutable struct{ n int }

type oracleFailingMap struct{ Map }

func (*oracleFailingMap) Entries([]*LVal) *LVal { return Errorf("oracle map enumeration failure") }

func TestWalkerBehaviorOracleFailedCopyIsNotAContainer(t *testing.T) {
	for _, w := range oracleWalkers() {
		t.Run(w.name, func(t *testing.T) {
			m := SortedMapFromData(NewMapData(&oracleFailingMap{Map: SortedMap().Map()}))
			root := SExpr([]*LVal{Bytes([]byte{1}), m, Quote(m), m})
			a, b, err := w.makeCopies(root)
			if w.stamps {
				// Stamping does not enumerate value payloads: a map callback
				// failure is not an error for an operation that never calls it.
				if err != nil || a.Cells[1].Native != m.Native || b.Cells[1].Native != m.Native {
					t.Fatal("stamper walked or rebuilt an opaque value payload")
				}
				return
			}
			// Templates reject external maps at admission. The copy walkers
			// try Entries and must discard the entire partially built graph.
			if err == nil || a != nil || b != nil {
				t.Fatal("failed graph construction returned a usable container")
			}
			want := "oracle map enumeration failure"
			if w.template {
				want = "not interpreter-owned"
			}
			if !strings.Contains(err.Error(), want) {
				t.Fatalf("wrong refusal: %v", err)
			}
		})
	}
}

func TestWalkerBehaviorOracleImmutableNative(t *testing.T) {
	// No mutation hooks and no writes after this declaration: approval means
	// transitive immutability, not permission to skip a required clone.
	p := &oracleImmutable{n: 7}
	v := Native(p)
	root := SExpr([]*LVal{v, Quote(v)})
	a, b, err := oracleTemplateCopiesWithOptions(root, TemplateWithNativePolicy(func(value any) bool { return value == p }))
	if err != nil {
		t.Fatal(err)
	}
	for _, result := range []*LVal{a, b} {
		if result == root || result.Cells[0] == v || result.Cells[0] == result.Cells[1] || result.Cells[0].Native != p || result.Cells[1].Native != p {
			t.Fatal("approved immutable native must be shared behind private, distinct headers")
		}
	}
}

// These mutations wrap REAL successful walks. Checking the clean arm first
// prevents an unrelated fixture failure from masquerading as a killed mutant.
func TestWalkerBehaviorOracleNegativeControls(t *testing.T) {
	runOracleControlOrders(t, checkOracleNegativeControls)
}

func runOracleControlOrders(t *testing.T, check func(*testing.T, []oracleWalker)) {
	t.Helper()
	walkers := oracleWalkers()
	for shift := range walkers {
		reordered := append(slices.Clone(walkers[shift:]), walkers[:shift]...)
		t.Run(fmt.Sprintf("rotation%d", shift), func(t *testing.T) { check(t, reordered) })
	}
}

func checkOracleNegativeControls(t *testing.T, walkers []oracleWalker) {
	t.Helper()
	for _, tc := range []struct {
		name, channel string
		mutate        func(source, a, b *LVal)
	}{
		{"source byte retained", "ownership", func(source, a, _ *LVal) { a.Cells[0].Cells[3].Native = source.Cells[0].Cells[3].Native }},
		{"payload memo lost", "alias", func(_, a, _ *LVal) { a.Cells[0].Cells[1].Native = a.Cells[0].Cells[1].Copy().Native }},
		{"equal payloads coalesced", "alias", func(_, a, _ *LVal) { a.Cells[0].Cells[2].Native = a.Cells[0].Cells[0].Native }},
		{"sibling sharing", "isolation", func(_, a, b *LVal) {
			b.Cells[0].Cells[3].Native = a.Cells[0].Cells[3].Native
			b.Cells[0].Cells[4].Native = a.Cells[0].Cells[4].Native
		}},
		{"sibling map sharing", "isolation", func(_, a, b *LVal) {
			b.Cells[4].Native = a.Cells[4].Native
		}},
		{"sibling native sharing", "isolation", func(_, a, b *LVal) {
			b.Cells[0].Cells[6].Native = a.Cells[0].Cells[6].Native
			b.Cells[0].Cells[7].Native = a.Cells[0].Cells[7].Native
		}},
		{"sibling cell slot sharing", "isolation", func(_, a, b *LVal) { b.Cells[3].Cells = a.Cells[3].Cells }},
		{"sibling array slot sharing", "isolation", func(_, a, b *LVal) { b.Cells[5].Cells[1].Cells = a.Cells[5].Cells[1].Cells }},
		{"sibling source location sharing", "isolation", func(_, a, b *LVal) { b.source = a.source }},
		{"sibling format metadata sharing", "isolation", func(_, a, b *LVal) { b.meta = a.meta }},
		{"zeroed bytes", "contents", func(_, a, _ *LVal) { a.Cells[0].Cells[3].Bytes()[0] = 0 }},
		{"lost quote", "flags", func(_, a, _ *LVal) { a.quoted = false }},
		{"retained debugger metadata", "retained macro", func(source, a, _ *LVal) { a.macroExpansion = source.macroExpansion }},
		{"metadata only input write", "source changed", func(source, _, _ *LVal) { source.macroExpansion.ID++ }},
		{"macro argument slot input write", "source changed", func(source, _, _ *LVal) { source.macroExpansion.Args[0] = Int(99) }},
		{"macro location hidden field input write", "source changed", func(source, _, _ *LVal) { source.macroExpansion.CallSite.EndCol++ }},
		{"comment location input write", "source changed", func(source, _, _ *LVal) { source.meta.TrailingComment.Source.Line++ }},
		{"lost source location", "source/format", func(_, a, _ *LVal) { a.source = nil }},
		{"lost format metadata", "source/format", func(_, a, _ *LVal) { a.meta = nil }},
	} {
		t.Run(tc.name, func(t *testing.T) {
			w := mustOracleWalker(t, walkers, "LVal.Copy")
			if err := oracleCheck(w, oracleGraph(8, true)); err != nil {
				t.Fatalf("clean control: %v", err)
			}
			clean := w.makeCopies
			w.makeCopies = func(v *LVal) (*LVal, *LVal, error) {
				a, b, err := clean(v)
				if err == nil {
					tc.mutate(v, a, b)
				}
				return a, b, err
			}
			err := oracleCheck(w, oracleGraph(8, true))
			if err == nil || !strings.Contains(err.Error(), tc.channel) {
				t.Fatalf("mutant must hit %q channel, got %v", tc.channel, err)
			}
		})
	}
}

func TestWalkerBehaviorOracleStampLiveControl(t *testing.T) {
	runOracleControlOrders(t, checkOracleStampLiveControl)
}

func checkOracleStampLiveControl(t *testing.T, walkers []oracleWalker) {
	t.Helper()
	w := mustOracleWalker(t, walkers, "macro stamp")
	if err := oracleCheck(w, oracleGraph(4, true)); err != nil {
		t.Fatal(err)
	}
	w.makeCopies = func(v *LVal) (*LVal, *LVal, error) { return v, v, nil }
	if err := oracleCheck(w, oracleGraph(4, true)); err == nil || !strings.Contains(err.Error(), "debug stamp path did not execute") {
		t.Fatalf("a no-op stamp must fail its live-path assertion, got %v", err)
	}
	w = mustOracleWalker(t, walkers, "macro stamp")
	clean := w.makeCopies
	w.makeCopies = func(v *LVal) (*LVal, *LVal, error) {
		a, b, err := clean(v)
		if err == nil {
			a.Cells[0].macroExpansion = nil
		}
		return a, b, err
	}
	if err := oracleCheck(w, oracleGraph(4, true)); err == nil || !strings.Contains(err.Error(), "missing descendant debug stamp") {
		t.Fatalf("child-only missing metadata must fail: %v", err)
	}
	w = mustOracleWalker(t, walkers, "macro stamp")
	clean = w.makeCopies
	w.makeCopies = func(v *LVal) (*LVal, *LVal, error) {
		a, b, err := clean(v)
		if err == nil {
			// Use the audited header-view constructor, retaining every flag
			// and metadata field so only the cycle edge's arity is corrupted.
			truncated := shallowUnquote(a)
			truncated.quoted = a.quoted
			truncated.Cells = nil
			a.Cells[len(a.Cells)-1] = truncated
		}
		return a, b, err
	}
	if err := oracleCheck(w, oracleGraph(4, true)); err == nil || !strings.Contains(err.Error(), "cell arity") {
		t.Fatalf("a truncated cycle edge must fail even with the same traversal paths: %v", err)
	}
}

func TestWalkerBehaviorOracleSealedCode(t *testing.T) {
	for _, w := range oracleWalkers() {
		t.Run(w.name, func(t *testing.T) {
			root := SExpr([]*LVal{Symbol("+"), Int(1), Int(2)})
			root.SealAST()
			a, b, err := w.makeCopies(root)
			if err != nil {
				t.Fatal(err)
			}
			shared := w.stamps || w.template
			if (a == root) != shared || a.IsSealed() != shared || (a == b) != shared {
				t.Fatal("immutable program sharing policy changed")
			}
			if !shared {
				a.Cells[1] = Int(97)
				if root.Cells[1].Int != 1 || b.Cells[1].Int != 1 || a.Cells[0].IsSealed() {
					t.Fatal("mutable copy of sealed code retained shared storage or a seal")
				}
			}
		})
	}
}

// Native annotations outside LNative are legal at Copy's within-runtime
// boundary (shared, NOT cloned); strict detach and publication refuse them.
// The census must still see them, without skipping container descendants.
func TestWalkerBehaviorOracleNativeAnnotations(t *testing.T) {
	for _, typ := range []LType{LNative, LString, LSExpr} {
		t.Run(typ.String(), func(t *testing.T) {
			p := &oracleNative{n: 7}
			v := &LVal{Type: typ, Native: p}
			if typ == LSExpr {
				v.Cells = []*LVal{Bytes([]byte{1}), SortedMap()}
			}
			nodes, err := oracleNodes(v)
			if err != nil {
				t.Fatal(err)
			}
			want := 1
			if typ == LSExpr {
				want = 3
			}
			count := 0
			for _, n := range nodes {
				if n.payload != nil {
					count++
				}
			}
			if count != want || nodes[0].payload != p {
				t.Fatalf("census: %d payloads, want %d including the root annotation", count, want)
			}
			before, err := oracleSnapshot(v)
			if err != nil {
				t.Fatal(err)
			}
			p.n++
			after, err := oracleSnapshot(v)
			if err != nil || before == after {
				t.Fatal("annotation mutation was invisible")
			}
			cp := v.Copy()
			if (cp.Native == p) != (typ != LNative) {
				t.Fatal("Copy's annotation/NativeCloner contract changed")
			}
			_, err = v.detach()
			if (err == nil) != (typ == LNative) {
				t.Fatalf("strict detach annotation contract: %v", err)
			}
			if a, _, err := oracleTemplateCopies(v); err == nil || a != nil {
				t.Fatal("template admitted a mutable native annotation")
			}
		})
	}
}

// Cell-slot preservation is a VM invariant, not a deep-copy invariant. Test
// the policy explicitly so extending the shared oracle cannot weaken one to
// accommodate the other (the historical #600 cdr/sort blind spot).
func TestWalkerBehaviorOracleCellSlots(t *testing.T) {
	for _, w := range oracleWalkers() {
		if w.stamps {
			continue // Syntax stamping is not a container ownership operation.
		}
		t.Run(w.name, func(t *testing.T) {
			cells := []*LVal{Int(1), Int(2), Int(3)}
			root := SExpr([]*LVal{SExpr(cells), SExpr(cells[1:])})
			a, b, err := w.makeCopies(root)
			if err != nil {
				t.Fatal(err)
			}
			a.Cells[0].Cells[1] = Int(97)
			want := 2
			if w.template {
				want = 97
			}
			if a.Cells[1].Cells[0].Int != want || root.Cells[1].Cells[0].Int != 2 || b.Cells[1].Cells[0].Int != 2 {
				t.Fatal("cell-slot fidelity or isolation contract changed")
			}
		})
	}
}
