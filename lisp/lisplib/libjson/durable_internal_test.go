// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"reflect"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// TestDurableFunctionReserveWritesNothing pins the ~#fn reserve: one byte
// short of ["~#fn","NAME"], the encoder refuses before it writes a byte.
func TestDurableFunctionReserveWritesNothing(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if err := lisp.GoError(lisp.InitializeUserEnv(env)); err != nil {
		t.Fatal(err)
	}
	f := env.LoadString("t", `(defun my-fn () 1) my-fn`)
	want := len(`["~#fn","user:my-fn"]`)
	e := newDurableEncoder(env, nil, newTypedConfig([]TypedOption{WithTypedMaxBytes(want - 1)}))
	if err := e.scan(f, 0); err != nil {
		t.Fatal(err)
	}
	if err := e.value(f, 0); err == nil {
		t.Fatal("no error one byte short")
	}
	if len(e.buf) != 0 {
		t.Fatalf("wrote %q before refusing", e.buf)
	}
	e = newDurableEncoder(env, nil, newTypedConfig([]TypedOption{WithTypedMaxBytes(want)}))
	if err := e.scan(f, 0); err != nil {
		t.Fatal(err)
	}
	if err := e.value(f, 0); err != nil || len(e.buf) != want {
		t.Fatalf("exact size: %q, %v", e.buf, err)
	}
}

// TestDurableIntegerKeyText pins the first pass's key accounting: integer
// keys count their "~i" text, so the key scratch stays under the byte limit.
func TestDurableIntegerKeyText(t *testing.T) {
	if strconv.IntSize < 64 {
		t.Skip("int is 32 bits")
	}
	env := lisp.NewEnv(nil)
	m := lisp.SortedMap()
	base := int64(1_000_000_000_000_000_000)
	for i := range 10000 {
		m.MapSetLVal(lisp.Int(int(base)+i), lisp.Int(0))
	}
	const limit = 65536
	e := newDurableEncoder(env, nil, newTypedConfig([]TypedOption{WithTypedMaxBytes(limit)}))
	e.scanning = true
	if err := e.scan(m, 0); !errors.Is(err, ErrTypedLimit) {
		t.Fatalf("scan: %v", err)
	}
	if len(e.keys) > limit {
		t.Fatalf("key scratch holds %d bytes, past the %d-byte limit", len(e.keys), limit)
	}
}

// nestedPairs returns struct{ L, R *A } nested n times over struct{ X int }:
// a type with 2^n paths to its innermost struct but only n+1 unnamed types.
func nestedPairs(n int) reflect.Type {
	a := reflect.TypeFor[struct{ X int }]()
	for range n {
		p := reflect.PointerTo(a)
		a = reflect.StructOf([]reflect.StructField{{Name: "L", Type: p}, {Name: "R", Type: p}})
	}
	return a
}

// TestTypeShapeLinear pins that a repeated unnamed subtype is written once
// and referenced after, so the shape is linear in the distinct types.
func TestTypeShapeLinear(t *testing.T) {
	shape := typeShape(nestedPairs(20), false)
	if len(shape) > 4096 {
		t.Fatalf("shape of a 20-level repeated subtype is %d bytes", len(shape))
	}
	if strings.Contains(shape, "...") {
		t.Fatalf("shape was cut short: %s", shape)
	}
	// Back-references keep different structures apart.
	same := reflect.TypeFor[struct{ A, B *struct{ X int } }]()
	diff := reflect.TypeFor[struct {
		A *struct{ X int }
		B *struct{ Y int }
	}]()
	if a, b := typeShape(same, false), typeShape(diff, false); a == b {
		t.Fatalf("one shape for two structures: %s", a)
	}
	if typeShape(nestedPairs(3), false) == typeShape(nestedPairs(4), false) {
		t.Fatal("one shape for two nesting depths")
	}
}
