// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"io"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Benchmarks for builtins that grow a result of known final length one
// element at a time.  Each one evaluates a single pre-parsed call against a
// prepared environment, so the timed region is the builtin and the call
// that reaches it; the input is built once, outside the loop.
//
//	go test -run '^$' -bench 'SliceTo|MakeSequenceInt' -benchmem -count=10 -cpu 1 ./lisp/

func benchCall(b *testing.B, setup, call string) {
	b.Helper()
	p := parser.NewReader()
	env := lisp.NewEnv(nil)
	if err := lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(p), lisp.WithStderr(io.Discard))); err != nil {
		b.Fatal(err)
	}
	exprs, err := p.Read("setup", strings.NewReader(setup))
	if err != nil {
		b.Fatal(err)
	}
	for _, expr := range exprs {
		if lerr := env.Eval(expr); lerr.Type == lisp.LError {
			b.Fatal(lerr)
		}
	}
	exprs, err = p.Read("call", strings.NewReader(call))
	if err != nil {
		b.Fatal(err)
	}
	if len(exprs) != 1 {
		b.Fatalf("call must be one expression, got %d", len(exprs))
	}
	expr := exprs[0]
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if lerr := env.Eval(expr); lerr.Type == lisp.LError {
			b.Fatal(lerr)
		}
	}
}

var benchSizes = []int{10, 100, 1000}

func BenchmarkSliceToString(b *testing.B) {
	for _, n := range benchSizes {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			benchCall(b, fmt.Sprintf("(set 'l (map 'list (lambda (x) (mod x 256)) (make-sequence 0 %d)))", n),
				fmt.Sprintf("(slice 'string l 0 %d)", n))
		})
	}
}

func BenchmarkSliceToBytes(b *testing.B) {
	for _, n := range benchSizes {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			benchCall(b, fmt.Sprintf("(set 'l (map 'list (lambda (x) (mod x 256)) (make-sequence 0 %d)))", n),
				fmt.Sprintf("(slice 'bytes l 0 %d)", n))
		})
	}
}

func BenchmarkMakeSequenceInt(b *testing.B) {
	for _, n := range benchSizes {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			benchCall(b, "()", fmt.Sprintf("(make-sequence 0 %d)", n))
		})
	}
}
