// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// An LNative whose payload happens to be a *LVal is a payload like any
// other -- lisp.NativeOf[*lisp.LVal] is a spelling the kernel blesses at
// native.go's NativeOf (issue #546), and NativeValue[*lisp.LVal] is the
// documented way to read it back.  It is NOT a cell view: a cell view is an
// LSExpr whose Native names the header its Cells window (the convention on
// cellsView), and cellsView itself gates on that header type before it
// looks at the payload.
//
// Copy briefly keyed "this link is a cell view" on the payload type alone,
// so every one of these natives came back from Copy with a nil payload.
// These tests pin the header-type gate that detach.go's twin arm always
// had.

// TestCopyKeepsNativeLValPayload is the unit form: the payload survives a
// Copy, and the copy still reads back through the typed accessor.
func TestCopyKeepsNativeLValPayload(t *testing.T) {
	payload := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	v := lisp.NativeOf[*lisp.LVal](payload)
	if v.Type != lisp.LNative {
		t.Fatalf("NativeOf built a %v, want %v", v.Type, lisp.LNative)
	}

	cp := v.Copy()
	if cp.Type != lisp.LNative {
		t.Fatalf("Copy produced a %v, want %v", cp.Type, lisp.LNative)
	}
	if cp.Native == nil {
		t.Fatalf("Copy dropped the native payload: %#v", cp.Native)
	}
	got, ok := lisp.NativeValue[*lisp.LVal](cp)
	if !ok {
		t.Fatalf("NativeValue[*lisp.LVal] on the copy: not ok, payload %T", cp.Native)
	}
	if got != payload {
		t.Errorf("Copy replaced the payload: got %p, want %p", got, payload)
	}
	// Copy shares native leaves by identity (TestCopySharesFunctionAndNativeLeaves),
	// so Int -- which the cell-view convention uses as the view's offset --
	// must not be zeroed either.
	if cp.Int != v.Int {
		t.Errorf("Copy changed Int: got %d, want %d", cp.Int, v.Int)
	}
}

// TestCopyKeepsNativeLValPayloadUnderStableSort is the reachable form, and
// the reason this matters outside a unit test: no Go code has to call Copy
// for the payload to vanish.  stable-sort's comparator copies both
// elements it compares (lvalByFun.Less), so ordinary lisp sorting a list
// of these natives hands the key function arguments whose payload is gone
// -- surfacing to an embedder as "expected native *lisp.LVal value, got
// native <nil>".  builtinInsertSorted copies the same way, so insert-sorted
// is exercised alongside it.
func TestCopyKeepsNativeLValPayloadUnderStableSort(t *testing.T) {
	env := copyTestEnv(t)

	// `boxed-rank` is the embedder-side reader: it asks for the payload
	// through the typed accessor exactly as RequireNative documents, and
	// reports the failure the way an embedder would see it.
	var seen, nilPayload int
	rank := lisp.FunInPackage(lisp.DefaultUserPackage, lisp.DefaultUserPackage+":boxed-rank", lisp.Formals("x"),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			seen++
			box := args.Cells[0]
			inner, lerr := lisp.RequireNative[*lisp.LVal](box)
			if lerr != nil {
				nilPayload++
				return lerr
			}
			return inner.Cells[0]
		})
	if lerr := env.PutGlobal(lisp.Symbol("boxed-rank"), rank); lerr.Type == lisp.LError {
		t.Fatalf("PutGlobal boxed-rank: %v", lerr)
	}

	box := func(n int) *lisp.LVal {
		return lisp.NativeOf[*lisp.LVal](lisp.QExpr([]*lisp.LVal{lisp.Int(n)}))
	}
	env.PutGlobal(lisp.Symbol("hi"), box(2))
	env.PutGlobal(lisp.Symbol("lo"), box(1))

	sorted := env.LoadString("copy_native_lval_test.lisp",
		`(stable-sort < (list hi lo) boxed-rank)`)
	if sorted.Type == lisp.LError {
		t.Fatalf("stable-sort over *LVal-payload natives failed (%d/%d arguments arrived with a nil payload): %v",
			nilPayload, seen, sorted)
	}
	if nilPayload != 0 {
		t.Errorf("stable-sort delivered %d/%d arguments with a nil payload", nilPayload, seen)
	}
	if got := ranks(t, sorted); got != "[1 2]" {
		t.Errorf("stable-sort ordered the boxes %s, want [1 2]", got)
	}

	inserted := env.LoadString("copy_native_lval_test.lisp",
		`(insert-sorted 'list (list lo) < hi boxed-rank)`)
	if inserted.Type == lisp.LError {
		t.Fatalf("insert-sorted over *LVal-payload natives failed (%d/%d arguments arrived with a nil payload): %v",
			nilPayload, seen, inserted)
	}
	if nilPayload != 0 {
		t.Errorf("insert-sorted delivered %d/%d arguments with a nil payload", nilPayload, seen)
	}
	if got := ranks(t, inserted); got != "[1 2]" {
		t.Errorf("insert-sorted ordered the boxes %s, want [1 2]", got)
	}
}

// ranks renders the payload rank of each box in a sequence of natives, so
// the order assertions above read the payloads rather than the natives'
// opaque printed form.
func ranks(t *testing.T, seq *lisp.LVal) string {
	t.Helper()
	out := make([]int, len(seq.Cells))
	for i, c := range seq.Cells {
		inner, lerr := lisp.RequireNative[*lisp.LVal](c)
		if lerr != nil {
			t.Fatalf("element %d: %v", i, lerr)
		}
		out[i] = inner.Cells[0].Int
	}
	return fmt.Sprint(out)
}
