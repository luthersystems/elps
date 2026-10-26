// Copyright © 2026 The ELPS authors

package lval_test

import (
	"math"
	"testing"

	"github.com/luthersystems/elps/lval"
)

func samples() []lval.Value {
	return []lval.Value{
		{},
		lval.MakeInt(0), lval.MakeInt(-7), lval.MakeInt(1 << 53), lval.MakeInt(1<<53 + 1), lval.MakeInt(math.MinInt64),
		lval.MakeFloat(1), lval.MakeFloat(math.Copysign(0, -1)), lval.MakeFloat(0.5), lval.MakeFloat(1e21), lval.MakeFloat(1e-7),
		lval.MakeFloat(123456789.25), lval.MakeFloat(math.NaN()), lval.MakeFloat(math.Inf(-1)), lval.MakeFloat(5e-324),
		lval.MakeString(""), lval.MakeString("~tilde"), lval.MakeString("a<b>&\"\\\n\t\x01 é 😀  "),
		lval.MakeSymbol("pkg:sym"), lval.MakeKeyword(":kw"), lval.MakeBool(true), lval.MakeBool(false),
		lval.MakeBytes([]byte{0, 1, 2, 255}), lval.MakeBytes(nil),
		lval.MakeList(lval.MakeInt(1), lval.MakeString("x")), lval.MakeVector(), lval.MakeVector(lval.MakeList()),
		lval.MakeMap(
			lval.MakeString("b"), lval.MakeInt(1),
			lval.MakeString("a"), lval.MakeVector(lval.MakeFloat(2)),
			lval.MakeSymbol("c"), lval.MakeKeyword("k"),
			lval.MakeKeyword("a"), lval.MakeString("~x"),
			lval.MakeInt(-3), lval.MakeBool(true),
			lval.MakeString("é"), lval.MakeString("😀"),
			lval.MakeString("￿"), lval.MakeString("~z"),
		),
		lval.MakeTagged("pkg:point", lval.MakeMap(lval.MakeString("x"), lval.MakeInt(1))),
		lval.MakeArray([]int{2, 2}, lval.MakeInt(1), lval.MakeInt(2), lval.MakeInt(3), lval.MakeInt(4)),
		lval.MakeArray([]int{}, lval.MakeString("scalar")),
	}
}

func TestMapKeysAsELPS(t *testing.T) {
	m := lval.MakeMap(lval.MakeString("a"), lval.MakeInt(1), lval.MakeSymbol("a"), lval.MakeInt(2))
	if m.Len() != 1 {
		t.Fatalf("len %d, want 1", m.Len())
	}
	v, ok := m.Get(lval.MakeSymbol("a"))
	if n, _ := v.Int(); !ok || n != 2 {
		t.Fatalf("got %v, %v", v, ok)
	}
}

type order struct {
	Item     string            `elps:"item"`
	Lines    []line            `elps:"lines"`
	Meta     map[string]string `elps:"meta,omitempty"`
	Quantity int
	Discount *float64 `elps:"discount"`
}

type line struct {
	SKU   string  `elps:"sku"`
	Price float64 `elps:"price"`
}

func TestDecodeEncode(t *testing.T) {
	v := lval.MakeMap(
		lval.MakeString("item"), lval.MakeString("widget"),
		lval.MakeKeyword("quantity"), lval.MakeInt(3),
		lval.MakeString("lines"), lval.MakeList(lval.MakeMap(lval.MakeString("sku"), lval.MakeString("a"), lval.MakeString("price"), lval.MakeInt(2))),
		lval.MakeString("discount"), lval.MakeFloat(0.5),
	)
	var o order
	if err := lval.Decode(v, &o); err != nil {
		t.Fatal(err)
	}
	if o.Item != "widget" || o.Quantity != 3 || len(o.Lines) != 1 || o.Lines[0] != (line{SKU: "a", Price: 2}) || o.Discount == nil || *o.Discount != 0.5 {
		t.Fatalf("decoded %+v", o)
	}
	back, err := lval.Encode(o)
	if err != nil {
		t.Fatal(err)
	}
	if got, ok := back.Get("Quantity"); !ok {
		t.Fatal("no Quantity")
	} else if n, _ := got.Int(); n != 3 {
		t.Fatalf("Quantity %d", n)
	}
	if _, ok := back.Get("meta"); ok {
		t.Fatal("omitempty kept meta")
	}
	var bad struct{ N int }
	err = lval.Decode(lval.MakeMap(lval.MakeString("n"), lval.MakeString("x")), &bad)
	if err == nil || err.Error() != "lval: N: cannot store string in int" {
		t.Fatalf("got %v", err)
	}
	var anyv any
	if err := lval.Decode(v, &anyv); err != nil {
		t.Fatal(err)
	}
	if m, ok := anyv.(map[string]any); !ok || m["item"] != "widget" {
		t.Fatalf("got %v", anyv)
	}
}

// TestFieldRules pins the elpstag rules lisp.Decode and lisp.Encode follow
// too: an exact key wins over a key that differs in case, and a
// field whose name repeats an earlier one's, ignoring case, maps to no key.
func TestFieldRules(t *testing.T) {
	var p struct {
		Name string `elps:"name"`
	}
	v := lval.MakeMap(
		lval.MakeString("NAME"), lval.MakeString("upper"),
		lval.MakeString("name"), lval.MakeString("exact"),
	)
	if err := lval.Decode(v, &p); err != nil {
		t.Fatal(err)
	}
	if p.Name != "exact" {
		t.Fatalf("Name = %q, want the exact key's value", p.Name)
	}
	var q struct {
		Name string `elps:"name"`
	}
	v = lval.MakeMap(
		lval.MakeString("NAME"), lval.MakeString("upper"),
		lval.MakeKeyword("Name"), lval.MakeString("mixed"),
	)
	if err := lval.Decode(v, &q); err != nil {
		t.Fatal(err)
	}
	if want, _ := v.Keys()[0].Str(); q.Name != map[string]string{"NAME": "upper", "Name": "mixed"}[want] {
		t.Fatalf("Name = %q, want the first key's value in sorted order (%q)", q.Name, want)
	}
	type dup struct {
		First  string `elps:"id"`
		Second string `elps:"ID"`
	}
	var d dup
	if err := lval.Decode(lval.MakeMap(lval.MakeString("ID"), lval.MakeString("x")), &d); err != nil {
		t.Fatal(err)
	}
	if d.First != "x" || d.Second != "" {
		t.Fatalf("decoded %+v, want only the first field set", d)
	}
	enc, err := lval.Encode(dup{First: "a", Second: "b"})
	if err != nil {
		t.Fatal(err)
	}
	if keys := enc.Keys(); len(keys) != 1 {
		t.Fatalf("encoded %d keys, want 1", len(keys))
	}
}

func TestUnmarshalRefusesArrayOfWrongSize(t *testing.T) {
	for _, in := range []string{`["~#array",[[2,2],[1]]]`, `["~#array",[[3],[1,2]]]`, `["~#array",[[3,9223372036854775807],[1,2,3]]]`} {
		if _, err := lval.Unmarshal([]byte(in)); err == nil {
			t.Errorf("Unmarshal(%s) did not fail", in)
		}
	}
	if _, err := lval.Unmarshal([]byte(`["~#array",[[2,2],[1,2,3,4]]]`)); err != nil {
		t.Fatal(err)
	}
}
