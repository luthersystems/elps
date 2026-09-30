// Copyright © 2026 The ELPS authors

package libjson

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// typedBenchFrame is a frame of ten symbol-keyed variables of
// mixed types.
func typedBenchFrame() *lisp.LVal {
	sig := make([]byte, 32)
	for i := range sig {
		sig[i] = byte(i * 7)
	}
	m := lisp.SortedMap()
	for _, kv := range [][2]*lisp.LVal{
		{lisp.Symbol("order-id"), lisp.String("ord-2026-000123")},
		{lisp.Symbol("amount"), lisp.Int(125000)},
		{lisp.Symbol("rate"), lisp.Float(0.0375)},
		{lisp.Symbol("status"), lisp.Symbol(":pending")},
		{lisp.Symbol("approved"), lisp.Symbol("false")},
		{lisp.Symbol("retries"), lisp.Int(2)},
		{lisp.Symbol("owner"), lisp.String("alice@example.com")},
		{lisp.Symbol("sig"), lisp.Bytes(sig)},
		{lisp.Symbol("steps"), lisp.QExpr([]*lisp.LVal{lisp.Symbol(":kyc"), lisp.Symbol(":credit"), lisp.Symbol(":fund")})},
		{lisp.Symbol("next"), lisp.Symbol("await-approval")},
	} {
		m.MapSetLVal(kv[0], kv[1])
	}
	return m
}

// typedBenchRecords is n string-keyed records in a vector.
func typedBenchRecords(n int) *lisp.LVal {
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		m := lisp.SortedMap()
		m.MapSetLVal(lisp.String("id"), lisp.String(fmt.Sprintf("acct-%06d", i)))
		m.MapSetLVal(lisp.String("balance"), lisp.Int(i*7919%10_000_000))
		m.MapSetLVal(lisp.String("rate"), lisp.Float(float64(i)/997))
		m.MapSetLVal(lisp.String("name"), lisp.String(fmt.Sprintf("Customer %d Ltd", i)))
		m.MapSetLVal(lisp.String("active"), lisp.Symbol("true"))
		m.MapSetLVal(lisp.String("tags"), lisp.QExpr([]*lisp.LVal{lisp.String("retail"), lisp.String("eu")}))
		cells[i] = m
	}
	return lisp.Vector(cells)
}

func BenchmarkTypedJSON(b *testing.B) {
	for _, p := range []struct {
		name string
		v    *lisp.LVal
	}{{"frame", typedBenchFrame()}, {"records400", typedBenchRecords(400)}} {
		enc, err := DumpTyped(p.v)
		if err != nil {
			b.Fatal(err)
		}
		b.Run("dump/"+p.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if _, err := DumpTyped(p.v); err != nil {
					b.Fatal(err)
				}
			}
		})
		b.Run("load/"+p.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if _, err := LoadTyped(enc); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}
