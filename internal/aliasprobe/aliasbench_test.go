// Copyright © 2026 The ELPS authors
//
// Microbenchmarks for the borrowed-backing alias class.  Identical in
// every design arm; the arms differ only in lisp/ kernel code.
//
// Each benchmark pre-parses its expression once and times only env.Eval,
// so the number reported is the builtin's cost, not the parser's or the
// environment's.

package aliasprobe_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath"
	"github.com/luthersystems/elps/parser"
)

func benchEnv(b *testing.B) *lisp.LEnv {
	b.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
		b.Fatalf("InitializeUserEnv: %v", lerr)
	}
	if lerr := lisplib.LoadLibrary(env); lerr.Type == lisp.LError {
		b.Fatalf("LoadLibrary: %v", lerr)
	}
	if lerr := env.InPackage(lisp.String(lisp.DefaultUserPackage)); lerr.Type == lisp.LError {
		b.Fatalf("InPackage: %v", lerr)
	}
	return env
}

// benchEval evaluates setup once, then times body b.N times in one env.
func benchEval(b *testing.B, setup, body string) {
	env := benchEnv(b)
	p := parser.NewReader()
	if setup != "" {
		exprs, err := p.Read("setup.lisp", strings.NewReader(setup))
		if err != nil {
			b.Fatalf("parse setup: %v", err)
		}
		for _, e := range exprs {
			if v := env.Eval(e); v.Type == lisp.LError {
				b.Fatalf("setup: %v", v)
			}
		}
	}
	exprs, err := p.Read("body.lisp", strings.NewReader(body))
	if err != nil {
		b.Fatalf("parse body: %v", err)
	}
	// One warm pass, untimed.
	for _, e := range exprs {
		if v := env.Eval(e); v.Type == lisp.LError {
			b.Fatalf("warmup: %v", v)
		}
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		for _, e := range exprs {
			if v := env.Eval(e); v.Type == lisp.LError {
				b.Fatalf("iter: %v", v)
			}
		}
	}
}

// ---- the query path (substrate's per-transaction cost) -----------------

func BenchmarkPathRangeGetList(b *testing.B) {
	benchEval(b, `(defun data () '(1 2 3 4 5 6 7 8 9 10))`,
		`(elpspath:? (data) '(range 2 8))`)
}

func BenchmarkPathRangeGetVector(b *testing.B) {
	benchEval(b, `(set 'v (vector 1 2 3 4 5 6 7 8 9 10))`,
		`(elpspath:? v '(range 2 8))`)
}

func BenchmarkPathIndexGet(b *testing.B) {
	benchEval(b, `(set 'v (vector 1 2 3 4 5 6 7 8 9 10))`,
		`(elpspath:? v 4)`)
}

func BenchmarkPathChainGet(b *testing.B) {
	benchEval(b, `(set 'm (sorted-map "a" (sorted-map "b" (vector 1 2 3 4 5 6))))`,
		`(elpspath:? m "a" "b" '(range 1 4))`)
}

// ---- list/vector kernel sites -----------------------------------------

func BenchmarkSliceListSealed(b *testing.B) {
	benchEval(b, `(defun data () '(1 2 3 4 5 6 7 8 9 10))`, `(slice 'list (data) 2 8)`)
}

func BenchmarkSliceListUnsealed(b *testing.B) {
	benchEval(b, `(set 'v (list 1 2 3 4 5 6 7 8 9 10))`, `(slice 'list v 2 8)`)
}

func BenchmarkSliceVectorUnsealed(b *testing.B) {
	benchEval(b, `(set 'v (vector 1 2 3 4 5 6 7 8 9 10))`, `(slice 'vector v 2 8)`)
}

func BenchmarkCdrSealed(b *testing.B) {
	benchEval(b, `(defun data () '(1 2 3 4 5 6 7 8 9 10))`, `(cdr (data))`)
}

func BenchmarkAppendVectorUnsealed(b *testing.B) {
	benchEval(b, `(set 'v (vector 1 2 3 4 5 6 7 8 9 10))`, `(append 'vector v 11)`)
}

func BenchmarkStableSortUnsealed(b *testing.B) {
	benchEval(b, `(set 'v (vector 5 3 9 1 7 2 8 4 6 0))`, `(stable-sort < v)`)
}

// ---- bytes ------------------------------------------------------------

func BenchmarkToBytes(b *testing.B) {
	benchEval(b, ``, `(to-bytes "ABCDEFGHIJKLMNOPQRSTUVWXYZ")`)
}

func BenchmarkSliceBytes(b *testing.B) {
	benchEval(b, `(set 'src (to-bytes "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))`, `(slice 'bytes src 4 20)`)
}

func BenchmarkAppendBytes(b *testing.B) {
	benchEval(b, `(set 'src (to-bytes "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))`, `(append-bytes src (to-bytes "xy"))`)
}

func BenchmarkAppendBytesMutate(b *testing.B) {
	// append-bytes! grows one value forever; reset it each iteration so the
	// benchmark measures the op, not an ever-growing buffer.
	benchEval(b, `(set 'src (to-bytes "ABCDEFGHIJ"))`,
		`(append-bytes! (set 'src (to-bytes "ABCDEFGHIJ")) (to-bytes "xy"))`)
}

func BenchmarkBytesRoundTrip(b *testing.B) {
	benchEval(b, `(set 'src (to-bytes "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))`,
		`(to-string (slice 'bytes src 0 13))`)
}

func BenchmarkBytesEqual(b *testing.B) {
	benchEval(b, `(set 'a (to-bytes "ABCDEFGHIJ")) (set 'b (to-bytes "ABCDEFGHIJ"))`,
		`(equal? a b)`)
}

// ---- direct-call benchmarks --------------------------------------------
//
// The lisp-level benchmarks above are what a phylum actually pays, but they
// are dominated by the evaluator (symbol lookup, argument evaluation, call
// frame), which buries a mechanism whose whole cost is a few pointer
// copies.  These call the same code through its Go API so the mechanism's
// cost is visible on its own; the two together say both "what it costs"
// and "where the cost is expensed".

func benchPathGet(b *testing.B, sealed bool, arr bool, n int, from, to int) {
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	var src *lisp.LVal
	if arr {
		src = lisp.Vector(cells)
	} else {
		src = lisp.QExpr(cells)
	}
	if sealed {
		src.SealAST()
	}
	p := libelpspath.Range(from, to, false)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		v, err := p.Get(src)
		if err != nil || v == nil {
			b.Fatalf("Get: %v", err)
		}
	}
}

func BenchmarkDirectRangeGetSealedList10(b *testing.B)   { benchPathGet(b, true, false, 10, 2, 8) }
func BenchmarkDirectRangeGetUnsealedList10(b *testing.B) { benchPathGet(b, false, false, 10, 2, 8) }
func BenchmarkDirectRangeGetSealedList256(b *testing.B)  { benchPathGet(b, true, false, 256, 8, 248) }
func BenchmarkDirectRangeGetVector256(b *testing.B)      { benchPathGet(b, false, true, 256, 8, 248) }

// benchFunCall drives one builtin through env.FunCall with pre-built
// arguments: no parse, no symbol resolution, no argument evaluation.
func benchFunCall(b *testing.B, name string, mkArgs func() []*lisp.LVal) {
	env := benchEnv(b)
	fun := env.GetGlobal(lisp.Symbol(name))
	if fun.Type != lisp.LFun {
		b.Fatalf("%s is not a function: %v", name, fun)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if rc := env.FunCall(fun, lisp.SExpr(mkArgs())); rc.Type == lisp.LError {
			b.Fatalf("%s: %v", name, rc)
		}
	}
}

func BenchmarkDirectSliceListSealed(b *testing.B) {
	cells := make([]*lisp.LVal, 64)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	src := lisp.QExpr(cells)
	src.SealAST()
	benchFunCall(b, "slice", func() []*lisp.LVal {
		return []*lisp.LVal{lisp.Symbol("list"), src, lisp.Int(4), lisp.Int(60)}
	})
}

func BenchmarkDirectSliceBytes(b *testing.B) {
	src := lisp.Bytes([]byte("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ12"))
	benchFunCall(b, "slice", func() []*lisp.LVal {
		return []*lisp.LVal{lisp.Symbol("bytes"), src, lisp.Int(4), lisp.Int(60)}
	})
}

func BenchmarkDirectCdrSealed(b *testing.B) {
	cells := make([]*lisp.LVal, 64)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	src := lisp.QExpr(cells)
	src.SealAST()
	benchFunCall(b, "cdr", func() []*lisp.LVal { return []*lisp.LVal{src} })
}

func BenchmarkDirectAppendBytes(b *testing.B) {
	suffix := lisp.Bytes([]byte("xy"))
	benchFunCall(b, "append-bytes", func() []*lisp.LVal {
		return []*lisp.LVal{lisp.Bytes([]byte("ABCDEFGHIJKLMNOPQRSTUVWXYZ012345")), suffix}
	})
}

// BenchmarkCdrWalk is the asymptotic canary.  cdr sharing its argument's
// backing array is what makes a recursive list walk O(n); an arm that
// copies on cdr makes the same walk O(n^2).  A 128-element list is small
// enough that the constant factors still show and large enough that the
// asymptotic difference is unmistakable.
func BenchmarkCdrWalk(b *testing.B) {
	benchEval(b, `
(defun walk (xs acc)
  (if (nil? xs) acc (walk (cdr xs) (+ acc (car xs)))))
(set 'big (make-sequence 0 128))`,
		`(walk big 0)`)
}
