// Copyright © 2026 The ELPS authors

package libregexp_test

import (
	"regexp"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libregexp"
)

// BenchmarkMatch measures one regexp-match? of a UUID pattern, which has
// counted repetition, so the charge needs its program size.  The three
// forms of the regexp argument are a compiled value, a pattern string and a
// host *regexp.Regexp.
func BenchmarkMatch(b *testing.B) {
	const patt = `^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$`
	text := lisp.String("01234567-89ab-cdef-0123-456789abcdef")
	for _, tc := range []struct {
		name string
		re   func(env *lisp.LEnv) *lisp.LVal
	}{
		{"compiled", func(env *lisp.LEnv) *lisp.LVal {
			return libregexp.BuiltinCompile(env, lisp.SExpr([]*lisp.LVal{lisp.String(patt)}))
		}},
		{"string", func(*lisp.LEnv) *lisp.LVal { return lisp.String(patt) }},
		{"host", func(*lisp.LEnv) *lisp.LVal { return lisp.Native(regexp.MustCompile(patt)) }},
	} {
		b.Run(tc.name, func(b *testing.B) {
			env := chargeEnv(b)
			call := lisp.SExpr([]*lisp.LVal{tc.re(env), text})
			b.ReportAllocs()
			for b.Loop() {
				if v := libregexp.BuiltinIsMatch(env, call); v.Type != lisp.LSymbol {
					b.Fatal(v)
				}
			}
		})
	}
}
