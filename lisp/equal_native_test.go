// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
)

// equalByID is a native payload that defines equal? with NativeEqualer.
type equalByID struct{ id int }

func (e equalByID) EqualNative(other any) bool {
	o, ok := other.(equalByID)
	return ok && o.id == e.id
}

type scalarNative int64

// acceptsAll claims equality with any equalByID, which equalByID does not
// return. equal? needs both to agree.
type acceptsAll struct{}

func (acceptsAll) EqualNative(other any) bool {
	_, ok := other.(equalByID)
	return ok
}

type empty struct{}

type structNative struct{ n int }

// equal? on native values follows lisp.NativeEqualer, then scalar ==, then
// pointer identity (luthersystems/elps#997).
func TestEqualNativeValues(t *testing.T) {
	t.Parallel()
	p := &structNative{n: 1}
	same := lisp.Native(structNative{n: 1})
	err := lisp.Errorf("boom")
	ep := &equalByID{1}
	t0 := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	tests := []struct {
		name string
		a, b *lisp.LVal
		want bool
	}{
		{"same value", same, same, true},
		{"equaler equal", lisp.Native(equalByID{1}), lisp.Native(equalByID{1}), true},
		{"equaler differ", lisp.Native(equalByID{1}), lisp.Native(equalByID{2}), false},
		{"equaler on the right", lisp.Native(scalarNative(1)), lisp.Native(equalByID{1}), false},
		{"scalar equal", lisp.Native(scalarNative(5)), lisp.Native(scalarNative(5)), true},
		{"scalar differ", lisp.Native(scalarNative(5)), lisp.Native(scalarNative(6)), false},
		{"scalar types differ", lisp.Native(scalarNative(5)), lisp.Native(int64(5)), false},
		{"same pointer", lisp.Native(p), lisp.Native(p), true},
		{"other pointer", lisp.Native(p), lisp.Native(&structNative{n: 1}), false},
		{"struct by value", lisp.Native(structNative{n: 1}), lisp.Native(structNative{n: 1}), false},
		{"native and int", lisp.Native(scalarNative(5)), lisp.Int(5), false},
		{"same error", err, err, true},
		{"other error", lisp.Errorf("boom"), lisp.Errorf("boom"), false},
		{"same pointer, value-receiver equaler", lisp.Native(ep), lisp.Native(ep), true},
		{"nil pointer, value-receiver equaler", lisp.Native((*equalByID)(nil)), lisp.Native(ep), false},
		{"both nil pointers", lisp.Native((*equalByID)(nil)), lisp.Native((*equalByID)(nil)), true},
		{"equalers disagree", lisp.Native(acceptsAll{}), lisp.Native(equalByID{1}), false},
		{"zero-size pointers", lisp.Native(&empty{}), lisp.Native(&empty{}), false},
		{"host times, same instant", lisp.Native(t0), lisp.Native(t0.In(time.FixedZone("x", 3600))), true},
		{"host times differ", lisp.Native(t0), lisp.Native(t0.Add(time.Second)), false},
		{"in a list", lisp.QExpr([]*lisp.LVal{lisp.Native(p)}), lisp.QExpr([]*lisp.LVal{lisp.Native(p)}), true},
	}
	for _, tt := range tests {
		got := tt.a.Equal(tt.b)
		if got.IsError() {
			t.Fatalf("%s: %v", tt.name, got)
		}
		if lisp.True(got) != tt.want {
			t.Errorf("%s: equal? = %v, want %v", tt.name, got, tt.want)
		}
		if back := tt.b.Equal(tt.a); lisp.True(back) != tt.want {
			t.Errorf("%s: reversed equal? = %v, want %v", tt.name, back, tt.want)
		}
	}
}
