// Copyright © 2026 The ELPS authors

package lval_test

import (
	"reflect"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lval"
)

// TestDecodeStructFieldNames checks how Decode matches map keys to struct
// fields, for ASCII and non-ASCII names, against lisp.DecodeInto. A key
// matches a field by strings.EqualFold, so "ſtatus" (with U+017F) matches
// "status" (luthersystems/elps#993).
func TestDecodeStructFieldNames(t *testing.T) {
	type ascii struct {
		Status string `elps:"status"`
		Other  string
	}
	type longS struct {
		Status string `elps:"ſtatus"`
	}
	type kelvin struct {
		Temp string `elps:"kelvin"`
	}
	cases := []struct {
		in   string
		new  func() any
		want any
	}{
		{`{"status":"a"}`, func() any { return new(ascii) }, &ascii{Status: "a"}},
		{`{"STATUS":"a","oTHER":"b"}`, func() any { return new(ascii) }, &ascii{Status: "a", Other: "b"}},
		{`{"STATUS":"a","status":"b"}`, func() any { return new(ascii) }, &ascii{Status: "b"}},
		{`{"~$Status":"b","~:STATUS":"a"}`, func() any { return new(ascii) }, &ascii{Status: "a"}},
		{`{"ſtatus":"a"}`, func() any { return new(ascii) }, &ascii{Status: "a"}},
		{`{"status":"a"}`, func() any { return new(longS) }, &longS{Status: "a"}},
		{`{"status":"b","ſtatus":"a"}`, func() any { return new(longS) }, &longS{Status: "a"}},
		{`{"STATUS":"a"}`, func() any { return new(longS) }, &longS{Status: "a"}},
		{"{\"Kelvin\":\"a\"}", func() any { return new(kelvin) }, &kelvin{Temp: "a"}},
		{`{"kelvinx":"a"}`, func() any { return new(kelvin) }, &kelvin{}},
	}
	for _, c := range cases {
		v, err := lval.Unmarshal([]byte(c.in))
		if err != nil {
			t.Fatal(err)
		}
		got := c.new()
		if derr := lval.Decode(v, got); derr != nil {
			t.Errorf("%s: %v", c.in, derr)
			continue
		}
		if !reflect.DeepEqual(got, c.want) {
			t.Errorf("%s: got %+v, want %+v", c.in, got, c.want)
		}
		lv, err := libjson.LoadTyped([]byte(c.in))
		if err != nil {
			t.Fatal(err)
		}
		fromLisp := c.new()
		if err := lisp.DecodeInto(lv, fromLisp); err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(got, fromLisp) {
			t.Errorf("%s: lval stored %+v, lisp %+v", c.in, got, fromLisp)
		}
	}
}

// TestEncodeStructFirstErrorInFieldOrder checks that Encode reports the
// first bad field in struct order, not in key order.
func TestEncodeStructFirstErrorInFieldOrder(t *testing.T) {
	type bad struct {
		Zed chan int
		Abc func()
	}
	_, err := lval.Encode(bad{Zed: make(chan int), Abc: func() {}})
	if err == nil || !strings.Contains(err.Error(), "Zed") {
		t.Fatalf("got %v, want an error about Zed", err)
	}
}

// TestEncodeStructOmitEmpty checks the keys Encode writes when omitempty
// leaves fields out.
func TestEncodeStructOmitEmpty(t *testing.T) {
	type s struct {
		B int    `elps:"b,omitempty"`
		A int    `elps:"a"`
		C string `elps:"~c,omitempty"`
		D int    `elps:"D,omitempty"`
	}
	for _, tc := range []struct {
		in   s
		want string
	}{
		{s{}, `{"a":0}`},
		{s{B: 1, C: "x", D: 2}, `{"D":2,"a":0,"b":1,"~~c":"x"}`},
		{s{C: "x"}, `{"a":0,"~~c":"x"}`},
	} {
		v, err := lval.Encode(tc.in)
		if err != nil {
			t.Fatal(err)
		}
		b, err := lval.Marshal(v)
		if err != nil {
			t.Fatal(err)
		}
		if string(b) != tc.want {
			t.Errorf("%+v: got %s, want %s", tc.in, b, tc.want)
		}
	}
}

type benchLineItem struct {
	SKU      string  `elps:"sku"`
	Name     string  `elps:"name"`
	Quantity int     `elps:"quantity"`
	Price    float64 `elps:"price"`
	Taxable  bool    `elps:"taxable"`
}

type benchOrder struct {
	OrderID    string          `elps:"order_id"`
	CustomerID string          `elps:"customer_id"`
	Status     string          `elps:"status"`
	Currency   string          `elps:"currency"`
	TotalCents int64           `elps:"total_cents"`
	TaxCents   int64           `elps:"tax_cents"`
	Discount   float64         `elps:"discount"`
	Expedited  bool            `elps:"expedited"`
	Notes      string          `elps:"notes,omitempty"`
	Items      []benchLineItem `elps:"items"`
}

func benchOrderValue() benchOrder {
	return benchOrder{
		OrderID: "ord-1", CustomerID: "cust-42", Status: "confirmed", Currency: "USD",
		TotalCents: 15990, TaxCents: 1290, Discount: 0.1, Expedited: true,
		Items: []benchLineItem{
			{SKU: "SKU-001", Name: "Widget A", Quantity: 2, Price: 49.95, Taxable: true},
			{SKU: "SKU-002", Name: "Widget B", Quantity: 1, Price: 60, Taxable: false},
		},
	}
}

func BenchmarkDecodeStruct(b *testing.B) {
	v, err := lval.Encode(benchOrderValue())
	if err != nil {
		b.Fatal(err)
	}
	b.ReportAllocs()
	for b.Loop() {
		var out benchOrder
		if err := lval.Decode(v, &out); err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkEncodeStruct(b *testing.B) {
	o := benchOrderValue()
	b.ReportAllocs()
	for b.Loop() {
		if _, err := lval.Encode(o); err != nil {
			b.Fatal(err)
		}
	}
}
