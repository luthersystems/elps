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

// TestDepthMatchesLibjson: Unmarshal and Marshal draw the nesting limit
// where libjson does, for every kind of container.
func TestDepthMatchesLibjson(t *testing.T) {
	wrappers := []struct {
		name string
		json func(string) string
		val  func(lval.Value) lval.Value
	}{
		{"list", func(x string) string { return `["~#list",[` + x + `]]` }, func(x lval.Value) lval.Value { return lval.MakeList(x) }},
		{"vector", func(x string) string { return `[` + x + `]` }, func(x lval.Value) lval.Value { return lval.MakeVector(x) }},
		{"map", func(x string) string { return `{"a":` + x + `}` }, func(x lval.Value) lval.Value { return lval.MakeMap(lval.MakeString("a"), x) }},
		{"tagged", func(x string) string { return `["~#tagged",["t",` + x + `]]` }, func(x lval.Value) lval.Value { return lval.MakeTagged("t", x) }},
		{"array", func(x string) string { return `["~#array",[[1,1],[` + x + `]]]` }, func(x lval.Value) lval.Value { return lval.MakeArray([]int{1, 1}, x) }},
	}
	for _, w := range wrappers {
		for _, n := range []int{1, 1022, 1023, 1024, 1025} {
			in, v := "1", lval.MakeInt(1)
			for range n {
				in, v = w.json(in), w.val(v)
			}
			_, lerr := libjson.LoadTyped([]byte(in))
			if _, err := lval.Unmarshal([]byte(in)); (err == nil) != (lerr == nil) {
				t.Errorf("%s %d: Unmarshal: %v, LoadTyped: %v", w.name, n, err, lerr)
			}
			b, err := lval.Marshal(v)
			if (err == nil) != (lerr == nil) {
				t.Errorf("%s %d: Marshal: %v, LoadTyped: %v", w.name, n, err, lerr)
			}
			if err == nil && string(b) != in {
				t.Errorf("%s %d: Marshal wrote other bytes than %.60s...", w.name, n, in)
			}
		}
	}
}

// TestMapKeyKindsMatchLibjson: a map whose keys are one ELPS key of two
// kinds keeps the later key, and Marshal writes it in libjson's order.
func TestMapKeyKindsMatchLibjson(t *testing.T) {
	built := lval.MakeMap(lval.MakeString("a"), lval.MakeInt(1), lval.MakeString("b"), lval.MakeInt(2), lval.MakeSymbol("a"), lval.MakeInt(3))
	var set lval.Value
	set.Set(lval.MakeString("a"), lval.MakeInt(1))
	set.Set(lval.MakeString("b"), lval.MakeInt(2))
	set.Set(lval.MakeSymbol("a"), lval.MakeInt(3))
	for _, v := range []lval.Value{built, set} {
		b, err := lval.Marshal(v)
		if err != nil {
			t.Fatal(err)
		}
		lv, err := libjson.LoadTyped(b)
		if err != nil {
			t.Fatalf("LoadTyped(%s): %v", b, err)
		}
		back, _ := libjson.DumpTyped(lv)
		if string(back) != string(b) || string(b) != `{"b":2,"~$a":3}` {
			t.Errorf("Marshal wrote %s, DumpTyped writes %s", b, back)
		}
	}
	u, err := lval.Unmarshal([]byte(`{"a":1,"b":2,"~$a":3}`))
	if err != nil {
		t.Fatal(err)
	}
	if b, _ := lval.Marshal(u); string(b) != `{"b":2,"~$a":3}` {
		t.Errorf("Unmarshal then Marshal wrote %s", b)
	}
}

// TestDecodeMatchesLisp: Decode and lisp.DecodeInto store the same typed
// JSON in the same Go type alike, or both fail.
func TestDecodeMatchesLisp(t *testing.T) {
	type item struct{ Item int }
	type named struct {
		Name string `elps:"name"`
	}
	minInt, err := libjson.DumpTyped(lisp.Float(-9223372036854775808.0))
	if err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		in  string
		new func() any
	}{
		{`{"item":2,"~$ITEM":1}`, func() any { return new(item) }},
		{`{"NAME":"upper","~:Name":"mixed"}`, func() any { return new(named) }},
		{`{"name":"exact","~:name":"kw"}`, func() any { return new(named) }},
		{`null`, func() any { return new(bool) }},
		{`{"x":null}`, func() any { return new(struct{ X bool }) }},
		{`true`, func() any { return new(bool) }},
		{`{"a":1,"~:a":2}`, func() any { return new(map[string]int) }},
		{`{"a":1,"~:a":2}`, func() any { return new(any) }},
		{`{"~?t":1}`, func() any { return new(any) }},
		{`{"~i3":1}`, func() any { return new(map[int]int) }},
		{string(minInt), func() any { return new(int64) }},
		{`5`, func() any { return new(uintptr) }},
		{`null`, func() any { return new([]int) }},
		{`["~#list",[1,2]]`, func() any { return new([]int) }},
		{`[1,2]`, func() any { return new([2]int) }},
		{`"~:kw"`, func() any { return new(string) }},
		{`1.5`, func() any { return new(int) }},
	}
	for _, c := range cases {
		lv, err := libjson.LoadTyped([]byte(c.in))
		if err != nil {
			t.Fatalf("LoadTyped(%s): %v", c.in, err)
		}
		v, err := lval.Unmarshal([]byte(c.in))
		if err != nil {
			t.Fatal(err)
		}
		fromLisp, fromLval := c.new(), c.new()
		lerr := lisp.DecodeInto(lv, fromLisp)
		verr := lval.Decode(v, fromLval)
		if (lerr == nil) != (verr == nil) {
			t.Errorf("%s into %T: lisp: %v, lval: %v", c.in, fromLisp, lerr, verr)
			continue
		}
		if lerr != nil {
			continue
		}
		a, err := lval.Encode(fromLisp)
		if err != nil {
			t.Fatal(err)
		}
		b, err := lval.Encode(fromLval)
		if err != nil {
			t.Fatal(err)
		}
		ab, _ := lval.Marshal(a)
		bb, _ := lval.Marshal(b)
		if string(ab) != string(bb) {
			t.Errorf("%s into %T: lisp stored %s, lval %s", c.in, fromLisp, ab, bb)
		}
	}
}

// TestEncodeErrorsMatchLisp: Encode refuses what lisp.Encode refuses,
// a Go value that holds itself included, and accepts what it accepts.
func TestEncodeErrorsMatchLisp(t *testing.T) {
	type node struct{ Next *node }
	cyclic := &node{}
	cyclic.Next = cyclic
	type hidden struct{ n int }
	for _, x := range []any{
		cyclic,
		hidden{n: 1},
		struct{}{},
		map[bool]int{true: 1},
		map[float64]int{1: 1},
		uintptr(7),
		[]byte("b"),
		[2]string{"a", "b"},
	} {
		lv, lerr := lisp.Encode(x)
		v, verr := lval.Encode(x)
		if (lerr == nil) != (verr == nil) {
			t.Errorf("%T: lisp: %v, lval: %v", x, lerr, verr)
			continue
		}
		if lerr != nil {
			continue
		}
		want, err := libjson.DumpTyped(lv)
		if err != nil {
			t.Fatal(err)
		}
		got, err := lval.Marshal(v)
		if err != nil {
			t.Fatal(err)
		}
		if string(got) != string(want) {
			t.Errorf("%T: lisp %s, lval %s", x, want, got)
		}
	}
}

// FuzzMarshal: what Marshal writes for a value built from the fuzzer's
// bytes, libjson.LoadTyped reads and DumpTyped writes back unchanged.
func FuzzMarshal(f *testing.F) {
	f.Add([]byte{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})
	f.Add([]byte("\x09\x03\x05a\x02\x06a\x01\x04~b"))
	f.Fuzz(func(t *testing.T, data []byte) {
		g := gen{b: data}
		v := g.value(0)
		b, err := lval.Marshal(v)
		if err != nil {
			return
		}
		lv, err := libjson.LoadTyped(b)
		if err != nil {
			t.Fatalf("Marshal wrote %s, LoadTyped refuses it: %v", b, err)
		}
		back, err := libjson.DumpTyped(lv)
		if err != nil {
			t.Fatal(err)
		}
		if string(back) != string(b) {
			t.Fatalf("Marshal wrote %s\nDumpTyped writes %s", b, back)
		}
	})
}

// gen builds a Value from a fuzzer's bytes.
type gen struct {
	b []byte
}

func (g *gen) byte() byte {
	if len(g.b) == 0 {
		return 0
	}
	c := g.b[0]
	g.b = g.b[1:]
	return c
}

func (g *gen) str() string {
	n := int(g.byte() % 8)
	n = min(n, len(g.b))
	s := string(g.b[:n])
	g.b = g.b[n:]
	return s
}

func (g *gen) value(depth int) lval.Value {
	if depth > 6 {
		return lval.MakeInt(int64(g.byte()))
	}
	switch g.byte() % 14 {
	case 0:
		return lval.Value{}
	case 1:
		return lval.MakeInt((int64(g.byte()) - 128) << (g.byte() % 64))
	case 2:
		return lval.MakeFloat(float64(int(g.byte())-128) / float64(g.byte()|1))
	case 3:
		return lval.MakeString(g.str())
	case 4:
		return lval.MakeSymbol(g.str())
	case 5:
		return lval.MakeKeyword(g.str())
	case 6:
		return lval.MakeBool(g.byte()%2 == 0)
	case 7:
		return lval.MakeBytes([]byte(g.str()))
	case 8:
		return lval.MakeList(g.items(depth)...)
	case 9:
		return lval.MakeVector(g.items(depth)...)
	case 10:
		items := g.items(depth)
		kv := make([]lval.Value, 0, 2*len(items))
		for _, it := range items {
			var k lval.Value
			switch g.byte() % 5 {
			case 0:
				k = lval.MakeString(g.str())
			case 1:
				k = lval.MakeSymbol(g.str())
			case 2:
				k = lval.MakeKeyword(g.str())
			case 3:
				k = lval.MakeInt(int64(g.byte()) - 128)
			default:
				k = lval.MakeBool(g.byte()%2 == 0)
			}
			kv = append(kv, k, it)
		}
		return lval.MakeMap(kv...)
	case 11:
		return lval.MakeTagged(g.str(), g.value(depth+1))
	case 12:
		dims := []int{int(g.byte() % 3), int(g.byte() % 3)}
		cells := make([]lval.Value, dims[0]*dims[1])
		for i := range cells {
			cells[i] = g.value(depth + 1)
		}
		return lval.MakeArray(dims, cells...)
	default:
		return lval.MakeInt(int64(g.byte()))
	}
}

func (g *gen) items(depth int) []lval.Value {
	items := make([]lval.Value, g.byte()%4)
	for i := range items {
		items[i] = g.value(depth + 1)
	}
	return items
}
