// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"context"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
)

// This file covers issue #364: libschema.NewValidator is the package's
// exported extension point, and an embedder's natural reading of an extension
// point is "build the constraint set once at process start, install it into
// every environment I create". Nothing in the signature said otherwise, and
// nothing verified that doing so was safe.
//
// Three separate properties have to hold for that reading to be correct, and
// they are covered separately below because they failed differently:
//
//  1. Constructing validators must be safe from more than one runtime at once.
//     This was BROKEN -- see TestValidatorConstructionIsRaceFree.
//  2. The returned value must own its state, so the caller cannot reach back
//     into it after the fact. This was BROKEN for the formals -- see
//     TestNewValidatorOwnsItsFormals -- and for the cell slice's spare
//     capacity, see TestValidatorCellsCarryNoSpareCapacity.
//  3. The interpreter must not write into a validator while running it. This
//     already held; TestSharedValidatorIsNotMutatedByEvaluation is a GUARD
//     that keeps it holding, not a catch of a live defect.

// evenValidator builds the constraint used throughout this file: a Go-side
// schema constraint of exactly the shape the doc comment on NewValidator
// describes.
func evenValidator() *lisp.LVal {
	return libschema.NewValidator(lisp.Formals("input"),
		func(_ *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
			if input.Type == lisp.LInt && input.Int%2 == 0 {
				return lisp.Nil()
			}
			return lisp.ErrorConditionf(libschema.FailedConstraint, "not even: %v", input)
		})
}

// TestValidatorConstructionIsRaceFree is the catch.
//
// Every validator this package mints -- through NewValidator, and through
// every s: constructor reachable from ordinary ELPS source -- took its
// function ID from GenSymbol, which incremented an unsynchronised
// package-level int. Two runtimes evaluating schema code on two goroutines
// therefore raced on that counter, with no Go embedder involvement required:
// (s:deftype ...) alone is enough.
//
// On main this failed under -race with, per goroutine pair:
//
//	WARNING: DATA RACE
//	Read at 0x000000c85f20 by goroutine 10:
//	  libschema.GenSymbol()  libschema.go:357
//	  libschema.newValidator()  libschema.go:438
//	  libschema.builtinGreaterThan()  libschema.go:684
//	Previous write at 0x000000c85f20 by goroutine 18:
//	  libschema.GenSymbol()  libschema.go:357
//	  libschema.newValidator()  libschema.go:438
//	  libschema.builtinCheckAny()  libschema.go:514
//
// The visible damage from losing the race is a duplicate FID, which is only a
// debugging-output defect. The race itself is not: it is undefined behaviour
// in an interpreter that substrate runs with dozens of live environments
// evaluating concurrently.
//
// This test is meaningful only under -race. Without it a lost increment
// produces a duplicate name and nothing observable, which is exactly why the
// bug survived.
func TestValidatorConstructionIsRaceFree(t *testing.T) {
	const nenv = 8
	const niter = 20

	envs := make([]*lisp.LEnv, nenv)
	for i := range envs {
		envs[i] = newSchemaEnv(t)
	}

	var wg sync.WaitGroup
	wg.Add(nenv)
	for i := range nenv {
		go func() {
			defer wg.Done()
			for range niter {
				// Every constructor on this line mints a validator:
				// s:int and s:gt each call newValidator, and deftype
				// wraps them in another.
				res := envs[i].LoadStringContext(context.Background(), "multiruntime",
					`(s:deftype "T" s:int (s:gt 0)) (s:validate T 4)`)
				if res.Type == lisp.LError {
					t.Errorf("runtime %d: %v", i, res)
					return
				}
			}
		}()
	}
	wg.Wait()
}

// TestNewValidatorConcurrentFromGo is the same race reached through the
// exported extension point rather than through ELPS source, which is the
// literal shape issue #364 reports: an embedder minting its constraint set
// concurrently, or minting one per runtime as its runtime pool fills.
func TestNewValidatorConcurrentFromGo(t *testing.T) {
	const n = 16
	var wg sync.WaitGroup
	wg.Add(n)
	ids := make([]string, n)
	for i := range n {
		go func() {
			defer wg.Done()
			ids[i] = evenValidator().FID()
		}()
	}
	wg.Wait()

	// A lost increment shows up as a duplicate FID. This assertion is a
	// bonus: the race detector is the real signal, since the counter can
	// race without losing an increment on any given run.
	seen := make(map[string]int, n)
	for i, id := range ids {
		if prev, dup := seen[id]; dup {
			t.Errorf("validators %d and %d share FID %q: GenSymbol lost an increment", prev, i, id)
		}
		seen[id] = i
	}
}

// TestNewValidatorOwnsItsFormals is a catch.
//
// NewValidator stored the caller's formals LVal by reference in Cells[0]. A
// caller that kept the formals around -- which is the natural thing to do
// when minting a family of constraints that all take one argument, i.e.
// exactly what every constructor in this package does internally -- could
// mutate it afterwards and have the mutation land in every validator built
// from it, in every runtime any of them were bound into.
//
// On main this failed with:
//
//	formals leaked: validator's formal argument is now "hijacked"
//
// The fix is a defensive copy at the exported boundary. It is NOT applied to
// the package-internal constructor, whose callers all build a fresh formals
// list inline on the call, so the hot path pays nothing.
func TestNewValidatorOwnsItsFormals(t *testing.T) {
	formals := lisp.Formals("input")
	v := libschema.NewValidator(formals,
		func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return lisp.Nil() })

	// The caller still holds the formals it passed in and writes through it.
	formals.Cells[0] = lisp.Symbol("hijacked")

	got := v.Cells[0]
	if got == formals {
		t.Fatalf("formals leaked: NewValidator stored the caller's LVal by reference")
	}
	if len(got.Cells) != 1 || got.Cells[0].Str != "input" {
		t.Errorf("formals leaked: validator's formal argument is now %q", got.Cells[0].Str)
	}
}

// TestNewValidatorFormalsCopyIsFaithful checks that the defensive copy above
// reproduces the formals list rather than flattening it. A copy that dropped
// the control symbols would turn a variadic constraint into a fixed-arity one,
// which is a worse bug than the leak it was added to close.
func TestNewValidatorFormalsCopyIsFaithful(t *testing.T) {
	for _, formals := range []*lisp.LVal{
		lisp.Formals(),
		lisp.Formals("input"),
		lisp.Formals("input", lisp.OptArgSymbol, "extra"),
		lisp.Formals("input", lisp.VarArgSymbol, "rest"),
	} {
		want := formals.String()
		v := libschema.NewValidator(formals,
			func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return lisp.Nil() })
		if got := v.Cells[0].String(); got != want {
			t.Errorf("formals copy is not faithful: got %s, want %s", got, want)
		}
	}
}

// TestValidatorCellsCarryNoSpareCapacity is a catch, of the same class as
// issue #373: a slice handed out with spare capacity is a slice something else
// can append into, and the append writes through into cells the owner still
// considers its own.
//
// A validator's cells are [formals, docstring, marker], built by appending the
// marker onto the two-cell slice lisp.FunInPackage returns. That append grew
// the backing array to capacity 4, so the value carried one spare slot for
// life. Appending onto any view of those cells -- append(v.Cells[:2], x) --
// would have overwritten the marker in place, silently revoking the validator
// credential of a value shared by every runtime it was bound into.
//
// Nothing does that today. The clamp is what keeps "nothing does that" from
// being load-bearing, and it costs nothing: allocating the slice at its exact
// length is one allocation, the same as the append it replaces.
//
// On main this failed with:
//
//	validator cells carry spare capacity: len=3 cap=4
func TestValidatorCellsCarryNoSpareCapacity(t *testing.T) {
	for _, tc := range []struct {
		name string
		make func(t *testing.T) *lisp.LVal
	}{
		{"NewValidator", func(t *testing.T) *lisp.LVal { return evenValidator() }},
		{"s:make-validator", func(t *testing.T) *lisp.LVal {
			env := newSchemaEnv(t)
			v := env.LoadStringContext(context.Background(), "cap", `(s:make-validator "T" s:int)`)
			if v.Type == lisp.LError {
				t.Fatalf("s:make-validator: %v", v)
			}
			return v
		}},
		{"s:gt", func(t *testing.T) *lisp.LVal {
			env := newSchemaEnv(t)
			v := env.LoadStringContext(context.Background(), "cap", `(s:gt 0)`)
			if v.Type == lisp.LError {
				t.Fatalf("s:gt: %v", v)
			}
			return v
		}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			v := tc.make(t)
			if cap(v.Cells) != len(v.Cells) {
				t.Errorf("validator cells carry spare capacity: len=%d cap=%d",
					len(v.Cells), cap(v.Cells))
			}
		})
	}
}

// TestSharedValidatorIsNotMutatedByEvaluation is a GUARD, not a catch: it
// passes on main.
//
// It pins the property issue #364 asks about most directly -- that the
// interpreter never writes into a validator LFun while running it -- so that
// the documented "safe to bind into any number of runtimes" guarantee on
// NewValidator is verified rather than asserted. Without it, the guarantee
// would be a claim about code in lisp/env.go that nothing in this package
// notices changing.
func TestSharedValidatorIsNotMutatedByEvaluation(t *testing.T) {
	even := evenValidator()

	type snapshot struct {
		cells   []*lisp.LVal
		cap     int
		fid     string
		pkg     string
		builtin bool
	}
	take := func() snapshot {
		return snapshot{
			cells:   append([]*lisp.LVal(nil), even.Cells...),
			cap:     cap(even.Cells),
			fid:     even.FID(),
			pkg:     even.Package(),
			builtin: even.Builtin() != nil,
		}
	}
	before := take()

	for _, tc := range []struct {
		src   string
		valid bool
	}{
		{`(s:deftype "T" s:int even?) (s:validate T 4)`, true},
		{`(s:deftype "T" s:int even?) (s:validate T 5)`, false},
		{`(s:deftype "T" s:sorted-map (s:has-key "a" even?)) (s:validate T (sorted-map "a" 2))`, true},
	} {
		// A FRESH runtime per case: this is the usage the issue reports,
		// and the point is that the validator survives it unchanged.
		env := newSchemaEnv(t)
		if rc := env.PutGlobal(lisp.Symbol("even?"), even); rc.Type == lisp.LError {
			t.Fatalf("bind: %v", rc)
		}
		res := env.LoadStringContext(context.Background(), "shared", tc.src)
		if got := res.Type != lisp.LError; got != tc.valid {
			t.Errorf("%s: expected valid=%v, got %v (%v)", tc.src, tc.valid, got, res)
		}
	}

	after := take()
	if len(before.cells) != len(after.cells) {
		t.Fatalf("cell count changed under evaluation: %d -> %d",
			len(before.cells), len(after.cells))
	}
	for i := range before.cells {
		if before.cells[i] != after.cells[i] {
			t.Errorf("cell %d was replaced under evaluation", i)
		}
	}
	if before.cap != after.cap {
		t.Errorf("cell capacity changed under evaluation: %d -> %d", before.cap, after.cap)
	}
	if before.fid != after.fid || before.pkg != after.pkg || !after.builtin {
		t.Errorf("function data changed under evaluation: %+v -> %+v", before, after)
	}
}

// TestSharedValidatorConcurrentAcrossRuntimes drives the reported usage
// pattern in full: ONE validator built once, bound into many independent
// runtimes, exercised on many goroutines at once. It is the end-to-end
// statement of what NewValidator now promises.
func TestSharedValidatorConcurrentAcrossRuntimes(t *testing.T) {
	even := evenValidator()

	const nenv = 8
	const niter = 20
	envs := make([]*lisp.LEnv, nenv)
	for i := range envs {
		envs[i] = newSchemaEnv(t)
		if rc := envs[i].PutGlobal(lisp.Symbol("even?"), even); rc.Type == lisp.LError {
			t.Fatalf("bind: %v", rc)
		}
	}

	var wg sync.WaitGroup
	wg.Add(nenv)
	for i := range nenv {
		go func() {
			defer wg.Done()
			for range niter {
				res := envs[i].LoadStringContext(context.Background(), "shared",
					`(s:deftype "T" s:int even?) (s:validate T 4)`)
				if res.Type == lisp.LError {
					t.Errorf("runtime %d: %v", i, res)
					return
				}
			}
		}()
	}
	wg.Wait()
}
