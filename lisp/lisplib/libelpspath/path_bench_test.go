// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// benchDoc is a document shaped like the records this package serves on the
// transaction path: a map of scalars with a nested map and a small array of
// records. Its nesting is far below the cycle guard's escalation depth, which
// is the case the guard has to be free in.
func benchDoc() *lisp.LVal {
	rec := func(id int) *lisp.LVal {
		m := lisp.SortedMap()
		m.MapSetString("id", lisp.Int(id))
		m.MapSetString("name", lisp.String("name"))
		m.MapSetString("active", lisp.Bool(true))
		return m
	}
	addr := lisp.SortedMap()
	addr.MapSetString("city", lisp.String("London"))
	addr.MapSetString("postcode", lisp.String("EC1"))

	doc := lisp.SortedMap()
	doc.MapSetString("id", lisp.String("abc123"))
	doc.MapSetString("count", lisp.Int(7))
	doc.MapSetString("address", addr)
	doc.MapSetString("items", lisp.Vector([]*lisp.LVal{rec(1), rec(2), rec(3), rec(4)}))
	return doc
}

// BenchmarkTypeCheck measures the gate every builtin runs before touching a
// value, which is where the cycle guard's per-frame cost lands.
func BenchmarkTypeCheck(b *testing.B) {
	doc := benchDoc()
	b.ReportAllocs()
	for b.Loop() {
		if err := okSimpleType(doc); err != nil {
			b.Fatal(err)
		}
	}
}

// BenchmarkCopy measures the copy walk the non-mutating operations run.
func BenchmarkCopy(b *testing.B) {
	doc := benchDoc()
	b.ReportAllocs()
	for b.Loop() {
		if _, err := copyLVal(doc, 0); err != nil {
			b.Fatal(err)
		}
	}
}

// The three builtin shapes: a read, a copying write and a copying delete.
// These are the whole per-call cost — gate plus path plus, for the last two,
// a copy — which is what a caller on the transaction path actually pays.

func BenchmarkBuiltinGet(b *testing.B) {
	benchBuiltin(b, BuiltinQueryGet, lisp.String("address"), lisp.String("city"))
}

func BenchmarkBuiltinSetCopy(b *testing.B) {
	benchBuiltin(b, BuiltinQuerySet, lisp.String("address"), lisp.String("city"), lisp.String("Paris"))
}

func BenchmarkBuiltinNilCopy(b *testing.B) {
	benchBuiltin(b, BuiltinQueryNil, lisp.String("id"))
}

func benchBuiltin(b *testing.B, fn func(*lisp.LEnv, *lisp.LVal) *lisp.LVal, steps ...*lisp.LVal) {
	b.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil
	args := make([]*lisp.LVal, 0, len(steps)+1)
	args = append(args, benchDoc())
	args = append(args, steps...)
	call := lisp.QExpr(args)
	b.ReportAllocs()
	for b.Loop() {
		if v := fn(env, call); v.Type == lisp.LError {
			b.Fatal(v)
		}
	}
}
