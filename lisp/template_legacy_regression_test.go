// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Issue #625 retains PR #602's empty-vector seal regression, now also through
// one published Template. These are surviving Lisp semantics, unlike the old
// implementation's internal cellsView links.
func TestTemplateLegacyEmptyVectorCarveouts(t *testing.T) {
	for _, build := range []string{
		`(append 'vector lit)`,
		`(append 'vector (rest '(1)))`,
		`(append 'vector (cdr '(1)))`,
		`(slice 'vector lit 0 0)`,
		`(slice 'vector '(1 2 3) 1 1)`,
		`(slice 'vector (rest '(1)) 0 0)`,
		`(append 'vector (append 'vector lit))`,
	} {
		t.Run(build, func(t *testing.T) {
			program := `(set 'lit '()) (set 'v ` + build + `)`
			source, cold := copyTestEnv(t), copyTestEnv(t)
			for _, env := range []*lisp.LEnv{source, cold} {
				if got := mustEval(t, env, program); got.String() != "(vector)" {
					t.Fatalf("constructor: %s", got)
				}
			}
			plan, err := lisp.NewTemplate(source, templateCorePolicy())
			if err != nil {
				t.Fatal(err)
			}
			arms := []*lisp.LEnv{cold}
			for range 2 {
				vm, err := plan.NewVM()
				if err != nil {
					t.Fatal(err)
				}
				arms = append(arms, vm)
			}
			for i, env := range arms {
				for _, step := range []struct{ src, want string }{
					{`(append! v 3 1 2)`, `(vector 3 1 2)`},
					{`(slice 'vector v 0 2)`, `(vector 3 1)`},
					{`(stable-sort < (slice 'list v 0 3))`, `'(1 2 3)`},
					{`v`, `(vector 1 2 3)`},
					{`(append 'vector v 5)`, `(vector 1 2 3 5)`},
					{`(append 'vector (append 'vector v) 6)`, `(vector 1 2 3 6)`},
					{`(stable-sort > (cdr (append 'list v)))`, `'(3 2)`},
					{`lit`, `'()`},
				} {
					if got := mustEval(t, env, step.src); got.String() != step.want {
						t.Fatalf("arm %d %s: got %s, want %s", i, step.src, got, step.want)
					}
				}
				v := env.GetGlobal(lisp.Symbol("v"))
				if v.Type != lisp.LArray || v.Cells[1].IsSealed() {
					t.Fatalf("arm %d: mutable vector holder was sealed: %s", i, v)
				}
			}
			if got := mustEval(t, source, `(list lit v)`); got.String() != "'('() (vector))" {
				t.Fatalf("mutating forks changed the source: %s", got)
			}
		})
	}
}

func TestTemplateLegacyEmptyVectorAppendSeed(t *testing.T) {
	// The human-readable form of FuzzSequenceOps/7192dc3ce712e22c.
	if got := mustEval(t, copyTestEnv(t), `(append! (append 'vector '()) ())`); got.String() != "(vector ())" {
		t.Fatalf("empty-vector append: %s", got)
	}
}

// PR #616 confused a native *LVal payload with a kernel cell-view link. Keep
// the typed-accessor and header-field assertions without restoring that link.
// This is the Go Copy contract; it does not assert PR #604's different proposed
// container-copy semantics.
func TestTemplateLegacyCopyKeepsNativeLValPayload(t *testing.T) {
	payload := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	value := lisp.NativeOf[*lisp.LVal](payload)
	value.Int = 73 // A nonzero sentinel detects an accidental view-offset reset.
	cp := value.Copy()
	got, ok := lisp.NativeValue[*lisp.LVal](cp)
	if !ok || cp.Type != lisp.LNative || cp == value || got != payload || cp.Int != 73 {
		t.Fatalf("Copy changed the native/header contract: ok=%t type=%v payload=%p want=%p int=%d", ok, cp.Type, got, payload, cp.Int)
	}
}

// Sorting copies comparator arguments internally. The regression must retain
// native payloads on that actual Lisp path, not only a direct Go Copy call.
// Mutable native boxes are installed after NewVM, as required by Template.
func TestTemplateLegacySortKeepsNativeLValPayload(t *testing.T) {
	source := copyTestEnv(t)
	plan, err := lisp.NewTemplate(source, templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	arms := []*lisp.LEnv{copyTestEnv(t)}
	for range 2 {
		vm, err := plan.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		arms = append(arms, vm)
	}
	for i, env := range arms {
		seen := 0
		rank := lisp.FunInPackage(lisp.DefaultUserPackage, "boxed-rank", lisp.Formals("x"),
			func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				seen++
				inner, lerr := lisp.RequireNative[*lisp.LVal](args.Cells[0])
				if lerr != nil {
					return lerr
				}
				return inner.Cells[0]
			})
		put := func(name string, value *lisp.LVal) {
			if rc := env.PutGlobal(lisp.Symbol(name), value); rc.Type == lisp.LError {
				t.Fatal(rc)
			}
		}
		put("boxed-rank", rank)
		for name, n := range map[string]int{"hi": 2, "lo": 1} {
			put(name, lisp.NativeOf[*lisp.LVal](lisp.QExpr([]*lisp.LVal{lisp.Int(n)})))
		}
		for _, expr := range []string{
			`(stable-sort < (list hi lo) boxed-rank)`,
			`(insert-sorted 'list (list lo) < hi boxed-rank)`,
		} {
			before := seen
			result := mustEval(t, env, expr)
			if seen == before {
				t.Fatalf("arm %d: %s did not invoke the native key function", i, expr)
			}
			if len(result.Cells) != 2 {
				t.Fatalf("arm %d: %s returned %d elements", i, expr, len(result.Cells))
			}
			for j, value := range result.Cells {
				inner, err := lisp.RequireNative[*lisp.LVal](value)
				if err != nil || len(inner.Cells) != 1 || inner.Cells[0].Type != lisp.LInt || inner.Cells[0].Int != j+1 {
					t.Fatalf("arm %d: sorted native rank %d: %v %s", i, j, err, inner)
				}
			}
		}
		if got := mustEval(t, env, `(list (boxed-rank hi) (boxed-rank lo))`); got.String() != "'(2 1)" {
			t.Fatalf("arm %d source native boxes changed: %s", i, got)
		}
	}
}
