// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// BenchmarkInterfaceCheck measures s:validate of an interface with three
// methods against a Lisp object (#905).
func BenchmarkInterfaceCheck(b *testing.B) {
	env := newSchemaForkEnvB(b)
	v := env.LoadString("b.lisp", `
(set 'p (s:make-interface "p" (pay (amount)) (total ()) (note (&optional x))))
(set 'inv (s:make-record "inv" (s:has-key "t" s:int)
  (s:method pay (o a) a) (s:method total (o) 1) (s:method note (o &optional x) x)))
(set 'o (inv :t 1))`)
	if v.IsError() {
		b.Fatal(v)
	}
	p, o := env.GetGlobal(lisp.Symbol("p")), env.GetGlobal(lisp.Symbol("o"))
	call := lisp.QExpr([]*lisp.LVal{p, o})
	validate := env.GetGlobal(lisp.Symbol("s:validate"))
	b.ReportAllocs()
	for b.Loop() {
		if r := env.FunCall(validate, call); r.IsError() {
			b.Fatal(r)
		}
	}
}
