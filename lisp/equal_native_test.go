// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// equalByID is a native payload that defines equal? with NativeEqualer.
type equalByID struct{ id int }

func (e equalByID) EqualNative(other any) bool {
	o, ok := other.(equalByID)
	return ok && o.id == e.id
}

type scalarNative int64

type structNative struct{ n int }

// equal? on native values follows lisp.NativeEqualer, then scalar ==, then
// pointer identity (luthersystems/elps#997).
func TestEqualNativeValues(t *testing.T) {
	t.Parallel()
	p := &structNative{n: 1}
	same := lisp.Native(structNative{n: 1})
	err := lisp.Errorf("boom")
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
