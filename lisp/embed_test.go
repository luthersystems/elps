package lisp

import (
	"reflect"
	"testing"
)

func TestVectorGoValue(t *testing.T) {
	tests := []struct {
		lval *LVal
		gval interface{}
	}{
		{
			Array(QExpr([]*LVal{Int(0)}), []*LVal{}),
			[]interface{}{},
		},
		{
			Array(nil, []*LVal{}),
			[]interface{}{},
		},
		{
			Array(QExpr([]*LVal{Int(1)}), []*LVal{Int(1)}),
			[]interface{}{int(1)},
		},
		{
			Array(nil, []*LVal{Int(1)}),
			[]interface{}{int(1)},
		},
	}
	for i, test := range tests {
		gval := GoValue(test.lval)
		if !reflect.DeepEqual(gval, test.gval) {
			t.Errorf("test %d:  lisp value %v with unexpected go value %#v (expected %#v)", i, test.lval, gval, test.gval)
		}
	}
}

// TestBytesGoValue pins what GoValue hands an embedder for an LBytes value.
//
// The bug this covers (#548) was `return v.Bytes` in goValueNode's LBytes
// arm: Bytes is a METHOD, not a field, so the arm returned a bound method
// value -- a func() []byte -- rather than the bytes.  The arm's result type
// is interface{}, so it compiled, and every caller that only passed the
// result along kept working; it failed at use, far from the mistake.
//
// Hence the assertion on the CONCRETE DYNAMIC TYPE.  A test that only did
// reflect.DeepEqual against []byte would have caught this one, but a test
// asserting the type says what the contract is: this arm returns data, like
// every other arm.
func TestBytesGoValue(t *testing.T) {
	src := []byte("here I stand")
	// Captured as a string BEFORE anything runs, and compared against
	// throughout.  Bytes(src) stores a slice header over src's OWN backing
	// array, so an assertion phrased against src after a mutation compares
	// two values that both changed and passes whatever the code does --
	// which is how the first draft of this test let a no-copy
	// implementation through its own red-proof.  A string conversion copies.
	want := string(src)
	v := Bytes(src)

	got := GoValue(v)
	b, ok := got.([]byte)
	if !ok {
		t.Fatalf("GoValue of an LBytes returned %T, want []byte", got)
	}
	if string(b) != want {
		t.Errorf("GoValue returned %q, want %q", b, want)
	}

	// The copy is the other half of the contract, and it is not cosmetic:
	// an LBytes stores its bytes in a *[]byte under Native so append! can
	// grow them in place, so handing back that backing would let an
	// embedder mutate a live lisp value the kernel still owns.  goSlice and
	// goMap build fresh containers for the same reason.
	//
	// Written as a mutation rather than a pointer comparison because that
	// is the property that matters: whatever the aliasing, a write through
	// the result must not be observable in the lisp value.
	b[0] = 'H'
	if after := string(v.Bytes()); after != want {
		t.Errorf("writing through GoValue's result changed the lisp value to %q, want %q",
			after, want)
	}

	// Empty and nil inputs go through the same path; neither should return
	// a func, and neither should panic.
	for _, empty := range [][]byte{{}, nil} {
		got := GoValue(Bytes(empty))
		if _, ok := got.([]byte); !ok {
			t.Errorf("GoValue of empty LBytes returned %T, want []byte", got)
		}
	}
}
