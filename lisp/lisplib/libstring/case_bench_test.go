// Copyright © 2026 The ELPS authors

package libstring

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// BenchmarkCase is string:lowercase and string:uppercase over ASCII text
// that changes, ASCII text that does not, and text with non-ASCII runes.
//
//	go test -run '^$' -bench Case -benchmem -count=10 -cpu 1 ./lisp/lisplib/libstring/
func BenchmarkCase(b *testing.B) {
	for _, in := range []struct{ name, s string }{
		{"ascii-mixed-16", "Order-ID 0042 xY"},
		{"ascii-mixed-1k", strings.Repeat("Order-ID 0042 xY", 64)},
		{"ascii-lower-1k", strings.Repeat("order-id 0042 xy", 64)},
		{"unicode-1k", strings.Repeat("Ordré-ID 0042 xY", 60)},
	} {
		for _, op := range []struct {
			name string
			fn   lisp.LBuiltin
		}{{"lowercase", builtinLower}, {"uppercase", builtinUpper}} {
			b.Run(op.name+"/"+in.name, func(b *testing.B) {
				env := lisp.NewEnv(nil)
				args := lisp.QExpr([]*lisp.LVal{lisp.String(in.s)})
				b.ReportAllocs()
				for b.Loop() {
					if v := op.fn(env, args); v.Type == lisp.LError {
						b.Fatal(v)
					}
				}
			})
		}
	}
}
