// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// BenchmarkChain benchmarks chain operation.
func BenchmarkChain(t *testing.B) {
	path := Chain(Dot("a"), Dot("b"))
	data := []byte(`{"a":{"b":"value"}}`)
	lval := libjson.Load(data, false)

	for range t.N {
		_, err := path.Get(lval)
		if err != nil {
			t.FailNow()
			return
		}
	}
}

// TestChain tests the chain operation.
func TestChain(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name     string
		In       string
		Path     Path
		Expected string
		HasError bool
	}{
		{
			Name:     "simple",
			In:       `{"hello":"world"}`,
			Path:     Chain(Dot("hello")),
			Expected: `"world"`,
		},
		{
			Name:     "nested",
			In:       `{"a":{"b":"world"}}`,
			Path:     Chain(Dot("a"), Dot("b")),
			Expected: `"world"`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			lval := libjson.Load([]byte(tc.In), false)
			data, err := tc.Path.Get(lval)
			if tc.HasError {
				if err == nil {
					t.FailNow()
				}
			} else {
				if err != nil {
					t.FailNow()
				}
				ejson, err := libjson.Dump(data, false)
				if err != nil {
					t.FailNow()
				}
				if string(ejson) != tc.Expected {
					t.FailNow()
				}
			}
		})
	}
}

func TestChainString(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name     string
		Path     Path
		Expected string
	}{
		{
			Name:     "simple",
			Path:     Root(Chain(Dot("hello"))),
			Expected: `.["hello"]`,
		},
		{
			Name:     "nested",
			Path:     Root(Chain(Dot("a"), Dot("b"), Index(0))),
			Expected: `.["a"]["b"][0]`,
		},
		{
			Name:     "nested chains",
			Path:     Root(Chain(Dot("a"), Chain(Dot("b"), Index(0)))),
			Expected: `.["a"]["b"][0]`,
		},
		{
			Name:     "escape",
			Path:     Root(Chain(Dot("\"\n "))),
			Expected: `.["\"\n "]`,
		},
		{
			Name:     "iter chains",
			Path:     Root(Iter(Chain(Dot("a"), Chain(Dot("b"), Index(0))))),
			Expected: `.[]["a"]["b"][0]`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			gotStr := tc.Path.String()
			if gotStr != tc.Expected {
				t.Fatalf("got: %s != expected; %s", gotStr, tc.Expected)
			}
		})
	}
}
