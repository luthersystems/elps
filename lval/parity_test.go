// Copyright © 2026 The ELPS authors

package lval_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lval"
)

// The tests in this file hold lval to libjson's typed JSON, which is the
// format's one definition: what one side writes, the other reads back to
// the same bytes.

// TestMarshalMatchesLoadTyped: libjson.LoadTyped reads what Marshal writes,
// and libjson.DumpTyped writes the same bytes back, so Marshal's output is
// the canonical typed JSON of each value.
func TestMarshalMatchesLoadTyped(t *testing.T) {
	for _, v := range samples() {
		b, err := lval.Marshal(v)
		if err != nil {
			t.Fatalf("%s: %v", v, err)
		}
		lv, err := libjson.LoadTyped(b)
		if err != nil {
			t.Fatalf("LoadTyped(%s): %v", b, err)
		}
		back, err := libjson.DumpTyped(lv)
		if err != nil {
			t.Fatal(err)
		}
		if string(back) != string(b) {
			t.Errorf("Marshal wrote %s, DumpTyped writes %s", b, back)
		}
	}
}

// TestUnmarshalMatchesDumpTyped: Unmarshal reads what libjson.DumpTyped
// writes for every value oracle generates that typed JSON can hold, and
// Marshal writes the same bytes back.
func TestUnmarshalMatchesDumpTyped(t *testing.T) {
	n := 0
	for _, in := range oracle.Values() {
		b, err := libjson.DumpTyped(in.V)
		if err != nil {
			continue // an error, a native or a function: typed JSON cannot hold it
		}
		n++
		v, err := lval.Unmarshal(b)
		if err != nil {
			t.Errorf("%s: Unmarshal(%s): %v", in.Label, b, err)
			continue
		}
		back, err := lval.Marshal(v)
		if err != nil {
			t.Errorf("%s: Marshal: %v", in.Label, err)
			continue
		}
		if string(back) != string(b) {
			t.Errorf("%s: DumpTyped wrote %s, Marshal writes %s", in.Label, b, back)
		}
	}
	if n < 20 {
		t.Fatalf("only %d of oracle's values cross as typed JSON", n)
	}
}

// TestEncodeMatchesLisp: Encode and lisp.Encode turn the same Go value into
// the same ELPS value, and Decode and lisp.DecodeInto read it back alike.
func TestEncodeMatchesLisp(t *testing.T) {
	discount := 0.25
	o := order{
		Item:     "widget",
		Lines:    []line{{SKU: "a", Price: 2}, {SKU: "b", Price: 0.5}},
		Meta:     map[string]string{"k": "v"},
		Quantity: 3,
		Discount: &discount,
	}
	v, err := lval.Encode(o)
	if err != nil {
		t.Fatal(err)
	}
	got, err := lval.Marshal(v)
	if err != nil {
		t.Fatal(err)
	}
	lv, err := lisp.Encode(o)
	if err != nil {
		t.Fatal(err)
	}
	want, err := libjson.DumpTyped(lv)
	if err != nil {
		t.Fatal(err)
	}
	if string(got) != string(want) {
		t.Fatalf("lval.Encode: %s\nlisp.Encode: %s", got, want)
	}
	var fromLval, fromLisp order
	if err := lval.Decode(v, &fromLval); err != nil {
		t.Fatal(err)
	}
	if err := lisp.DecodeInto(lv, &fromLisp); err != nil {
		t.Fatal(err)
	}
	a, _ := lval.Encode(fromLval)
	b, _ := lval.Encode(fromLisp)
	ab, _ := lval.Marshal(a)
	bb, _ := lval.Marshal(b)
	if string(ab) != string(bb) {
		t.Fatalf("lval.Decode: %s\nlisp.DecodeInto: %s", ab, bb)
	}
}

// FuzzUnmarshal: Unmarshal accepts every input libjson.LoadTyped accepts,
// and both sides write it back as the same bytes. Unmarshal does not check
// that its input is canonical, so it may accept an input LoadTyped refuses.
func FuzzUnmarshal(f *testing.F) {
	for _, v := range samples() {
		b, err := lval.Marshal(v)
		if err != nil {
			f.Fatal(err)
		}
		f.Add(b)
	}
	for _, in := range oracle.Values() {
		if b, err := libjson.DumpTyped(in.V); err == nil {
			f.Add(b)
		}
	}
	f.Fuzz(func(t *testing.T, b []byte) {
		lv, err := libjson.LoadTyped(b)
		if err != nil {
			return
		}
		want, err := libjson.DumpTyped(lv)
		if err != nil {
			t.Fatal(err)
		}
		v, err := lval.Unmarshal(b)
		if err != nil {
			t.Fatalf("LoadTyped accepts %q, Unmarshal refuses it: %v", b, err)
		}
		got, err := lval.Marshal(v)
		if err != nil {
			t.Fatalf("Unmarshal(%q) gave a value Marshal refuses: %v", b, err)
		}
		if string(got) != string(want) {
			t.Fatalf("input %q\nlval:    %s\nlibjson: %s", b, got, want)
		}
	})
}
