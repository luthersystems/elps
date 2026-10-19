// Copyright © 2026 The ELPS authors

// Package oracle holds the inputs and the comparisons for differential tests
// of Go code that does what an elps builtin does: an LEnv helper such as
// LEnv.MapLookup, or an embedder's Go port of a Lisp function that called
// builtins.  The builtin is the oracle.  A test runs the Go code and the
// builtin on the same input (Values) and requires the same result (Same), and
// requires both to check the evaluation's context at the same points (Sweep).
//
// elpstest.ParityCheck compares a Go native with the Lisp source it replaces.
// This package compares Go code with a builtin called from Go, so it needs no
// Lisp source.  Only tests import it.
package oracle

import (
	"context"
	"fmt"
	"math"
	"sync/atomic"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Value is one generated input and a label for test names.
type Value struct {
	V     *lisp.LVal
	Label string
}

// Values returns fresh inputs of every LType a phylum can hand a builtin:
// ints, floats (-0.0, NaN, both infinities, values past the int range),
// strings, bytes, symbols, lists (an error value as first element
// included), vectors, a two-dimensional array, sorted-maps (empty, string,
// symbol and int keys), nil, an error value, a quoted value, a native value
// and a function.  Each call builds new values, so a test can mutate them.
func Values() []Value {
	vec := func(xs ...*lisp.LVal) *lisp.LVal { return lisp.Vector(xs) }
	list := func(xs ...*lisp.LVal) *lisp.LVal { return lisp.QExpr(xs) }
	strMap := lisp.MapOf("a", lisp.Int(1), "b", lisp.String("x"))
	mixedMap := lisp.SortedMap()
	mixedMap.MapSetLVal(lisp.Symbol("s"), lisp.Int(1))
	mixedMap.MapSetLVal(lisp.Int(7), lisp.Int(2))
	mixedMap.MapSetLVal(lisp.String("t"), lisp.Nil())
	return []Value{
		{lisp.Int(0), "int-0"},
		{lisp.Int(1), "int-1"},
		{lisp.Int(-1), "int-neg"},
		{lisp.Int(math.MaxInt), "int-max"},
		{lisp.Int(math.MinInt), "int-min"},
		{lisp.Float(0), "float-0"},
		{lisp.Float(math.Copysign(0, -1)), "float-neg0"},
		{lisp.Float(1), "float-1"},
		{lisp.Float(1.5), "float-1.5"},
		{lisp.Float(-2.5), "float-neg2.5"},
		{lisp.Float(math.NaN()), "float-nan"},
		{lisp.Float(math.Inf(1)), "float-inf"},
		{lisp.Float(math.Inf(-1)), "float-neginf"},
		{lisp.Float(1e300), "float-huge"},
		{lisp.Float(math.Ldexp(1, 63)), "float-2^63"},
		{lisp.Float(-math.Ldexp(1, 63)), "float-neg2^63"},
		{lisp.String(""), "string-empty"},
		{lisp.String("abc"), "string-abc"},
		{lisp.String("12"), "string-12"},
		{lisp.Bytes(nil), "bytes-empty"},
		{lisp.Bytes([]byte("ab")), "bytes-ab"},
		{lisp.Symbol("a"), "symbol"},
		{lisp.Symbol(":k"), "keyword"},
		{lisp.Nil(), "nil"},
		{list(lisp.Int(1)), "list-1"},
		{list(lisp.Int(1), lisp.String("a"), lisp.Int(3)), "list-3"},
		{list(lisp.ErrorConditionf("stored", "stored error"), lisp.Int(2)), "list-error-first"},
		{list(lisp.Int(1), lisp.ErrorConditionf("stored", "stored error")), "list-error-second"},
		{vec(), "vector-empty"},
		{vec(lisp.Int(1), lisp.Int(2)), "vector-2"},
		{vec(lisp.ErrorConditionf("stored", "stored error")), "vector-error"},
		{lisp.Array(list(lisp.Int(2), lisp.Int(2)), []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4)}), "array-2x2"},
		{lisp.SortedMap(), "map-empty"},
		{strMap, "map-strings"},
		{mixedMap, "map-mixed"},
		{lisp.ErrorConditionf("custom", "an error value"), "error"},
		{lisp.Quote(lisp.Symbol("q")), "quote"},
		//elpsvet:transient a test input; no template or flow holds it
		{lisp.Native(int64(5)), "native"},
		{lisp.FunInPackage(lisp.DefaultUserPackage, "f", lisp.Formals(), identity), "function"},
	}
}

func identity(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return args }

// NewEnv returns an environment that can call the language builtins and
// load source, with ctx as its evaluation context.
func NewEnv(tb testing.TB, ctx context.Context) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if lerr := lisp.InitializeUserEnv(env); lerr.IsError() {
		tb.Fatalf("initialize env: %v", lerr)
	}
	// The context is set after the setup, so its checks are the caller's.
	_ = lisp.WithContext(ctx)(env)
	return env
}

// Sweep holds run to oracle at every context check: it counts the checks
// oracle makes, then, for each n from 1 to one past that count, runs both
// with the context cancelled at the n-th check and compares the results
// (Same) and the number of checks each made.  So run checks the context at
// the same points as oracle, with nothing observable between them that
// differs.  Each call of run or oracle gets a new environment, and a value
// belongs to one runtime, so run and oracle build their own inputs (call
// Values again) rather than share one across calls.
func Sweep(tb testing.TB, label string, run, oracle func(env *lisp.LEnv) *lisp.LVal) {
	tb.Helper()
	count := NewCancelAt(0)
	oracle(NewEnv(tb, count))
	for n := 1; n <= count.Checks()+1; n++ {
		gctx, wctx := NewCancelAt(n), NewCancelAt(n)
		got := run(NewEnv(tb, gctx))
		want := oracle(NewEnv(tb, wctx))
		Same(tb, fmt.Sprintf("%s/cancel-at-%d", label, n), got, want)
		if gctx.Checks() != wctx.Checks() {
			tb.Errorf("%s/cancel-at-%d: Go made %d context checks, the builtins %d", label, n, gctx.Checks(), wctx.Checks())
		}
	}
}

// SameObject is Same, and a non-error got must also be the very value
// want is.
func SameObject(tb testing.TB, label string, got, want *lisp.LVal) {
	tb.Helper()
	if !want.IsError() && !got.IsError() && got.Type == want.Type && got != want {
		tb.Errorf("%s: Go returned a different value than the builtin", label)
		return
	}
	Same(tb, label, got, want)
}

// Same fails tb unless got is what the builtin returned (want): both
// errors with the same condition, or two values of the same type that are
// equal (a float compared bit for bit, so -0.0 is not 0.0 and NaN is NaN).
// Messages are not compared.  SameObject also requires the very value.
func Same(tb testing.TB, label string, got, want *lisp.LVal) {
	tb.Helper()
	if want.IsError() {
		if !got.IsError() {
			tb.Errorf("%s: builtin raised %s, Go returned %s %v", label, want.Str, got.Type, got)
			return
		}
		if got.Str != want.Str {
			tb.Errorf("%s: builtin raised %s, Go raised %s (%v)", label, want.Str, got.Str, got)
		}
		return
	}
	if got.IsError() {
		tb.Errorf("%s: builtin returned %s %v, Go raised %v", label, want.Type, want, got)
		return
	}
	if got.Type != want.Type {
		tb.Errorf("%s: builtin returned %s, Go returned %s", label, want.Type, got.Type)
		return
	}
	if got.Type == lisp.LFloat {
		if math.Float64bits(got.Float) != math.Float64bits(want.Float) && !(math.IsNaN(got.Float) && math.IsNaN(want.Float)) {
			tb.Errorf("%s: builtin returned %v, Go returned %v", label, want.Float, got.Float)
		}
		return
	}
	if eq := got.Equal(want); eq.IsError() || !lisp.True(eq) {
		tb.Errorf("%s: builtin returned %v, Go returned %v", label, want, got)
	}
}

// SameErrorData fails tb unless got and want are errors with the same
// condition and the same data cells (the same values, in order).
func SameErrorData(tb testing.TB, label string, got, want *lisp.LVal) {
	tb.Helper()
	Same(tb, label, got, want)
	if !got.IsError() || !want.IsError() {
		return
	}
	if len(got.Cells) != len(want.Cells) {
		tb.Errorf("%s: builtin's error has %d data cells, Go's has %d", label, len(want.Cells), len(got.Cells))
		return
	}
	for i := range want.Cells {
		if got.Cells[i] != want.Cells[i] {
			if eq := got.Cells[i].Equal(want.Cells[i]); eq.IsError() || !lisp.True(eq) {
				tb.Errorf("%s: data cell %d: builtin %v, Go %v", label, i, want.Cells[i], got.Cells[i])
			}
		}
	}
}

// CancelAt is a context that reports itself cancelled from its n-th Err
// call on (n > 0), and never when n is 0.  CheckContext calls Err once per
// check, so a run under CancelAt(n) is cancelled at its n-th context check.
// Checks reports how many Err calls were made.
type CancelAt struct {
	context.Context
	n     int64
	calls atomic.Int64
}

// NewCancelAt returns a CancelAt cancelled at the n-th check.
func NewCancelAt(n int) *CancelAt {
	return &CancelAt{Context: context.Background(), n: int64(n)}
}

// Err returns context.Canceled from the n-th call on.
func (c *CancelAt) Err() error {
	if k := c.calls.Add(1); c.n > 0 && k >= c.n {
		return context.Canceled
	}
	return nil
}

// Checks returns the number of Err calls so far.
func (c *CancelAt) Checks() int { return int(c.calls.Load()) }
