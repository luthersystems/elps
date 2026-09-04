// Copyright © 2026 The ELPS authors

package libstring

import (
	"fmt"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// BenchmarkJoin is string:join over n short strings.
//
//	go test -run '^$' -bench Join -benchmem -count=10 -cpu 1 ./lisp/lisplib/libstring/
func BenchmarkJoin(b *testing.B) {
	for _, n := range []int{10, 100, 1000} {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			env := lisp.NewEnv(nil)
			cells := make([]*lisp.LVal, n)
			for i := range cells {
				cells[i] = lisp.String(fmt.Sprintf("item%d", i))
			}
			args := lisp.QExpr([]*lisp.LVal{lisp.QExpr(cells), lisp.String(", ")})
			b.ReportAllocs()
			for b.Loop() {
				if v := builtinJoin(env, args); v.Type == lisp.LError {
					b.Fatal(v)
				}
			}
		})
	}
}
