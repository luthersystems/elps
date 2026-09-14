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

// Native payloads are opaque even when their reflected value is invalid or
// contains nil pointers. Conversion must preserve them inside containers too.
func TestNativeGoValueShapes(t *testing.T) {
	type fields struct{ X int }
	type embedded struct{ *fields }
	for _, payload := range []any{
		nil, (*fields)(nil), embedded{}, &embedded{}, reflect.Value{},
		[]int(nil), map[string]int(nil), (chan int)(nil), (func())(nil),
	} {
		v := Native(payload)
		m := SortedMap()
		m.MapSet("native", v)
		for _, tc := range []struct {
			value *LVal
			want  any
		}{
			{v, payload},
			{QExpr([]*LVal{v}), []any{payload}},
			{m, map[any]any{"native": payload}},
		} {
			if got := GoValue(tc.value); !reflect.DeepEqual(got, tc.want) {
				t.Errorf("GoValue with payload %T = %#v, want %#v", payload, got, tc.want)
			}
		}
	}
}

func TestGoMapKeyReflectionGuards(t *testing.T) {
	for _, tc := range []struct {
		key  *LVal
		name string
		ok   bool
	}{
		{Nil(), "nil-list", false},
		{Native(nil), "nil-native", false},
		{Native([]int{1}), "slice", false},
		{Native(struct{ X any }{X: []int{1}}), "struct-interface-slice", false},
		{Native([1]any{[]int{1}}), "array-interface-slice", false},
		{Native(map[string]int(nil)), "nil-map", false},
		{Native((*int)(nil)), "nil-pointer", true},
		{String("key"), "string", true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var st cycleState
			m := make(gomap)
			if ok := checkGoMapInsert(m, tc.key, Int(42), cycleGuard{state: &st}); ok != tc.ok {
				t.Fatalf("checkGoMapInsert = %v, want %v", ok, tc.ok)
			}
			if tc.ok {
				if got := m[GoValue(tc.key)]; got != 42 {
					t.Errorf("map value = %v, want 42", got)
				}
			} else if len(m) != 0 {
				t.Errorf("rejected key inserted into map: %v", m)
			}
		})
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

	// Empty and nil inputs go through the same path.  Bytes(nil) does NOT
	// short-circuit in goValue -- IsNil() is (LSExpr && no Cells), and this
	// value is LBytes -- so the arm really is reached.
	//
	// Asserting the type alone was too weak: an implementation returning
	// some other zero-length or arbitrary slice for len(b)==0 passed it.
	// Assert the length and emptiness too, and name which input failed, so
	// a failure distinguishes {} from nil.
	for _, empty := range []struct {
		name string
		in   []byte
	}{{"empty", []byte{}}, {"nil", nil}} {
		got := GoValue(Bytes(empty.in))
		b, ok := got.([]byte)
		if !ok {
			t.Errorf("GoValue of %s LBytes returned %T, want []byte", empty.name, got)
			continue
		}
		if len(b) != 0 {
			t.Errorf("GoValue of %s LBytes returned %q (len %d), want a zero-length slice",
				empty.name, b, len(b))
		}
	}
}
