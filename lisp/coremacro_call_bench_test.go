// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// BenchmarkCoreMacroCall calls a function whose body uses a core Go macro.
// elps expands a macro in a function body on every call, so this measures
// what a program pays for the macro's expansion along with its evaluation.
func BenchmarkCoreMacroCall(b *testing.B) {
	for _, bm := range []struct{ name, def, call string }{
		{"get-default", `(defun f (m) (get-default m "k" 0))`, `(f (sorted-map "k" 1))`},
		{"curry-function", `(defun f (x) ((curry-function + 1) x))`, `(f 2)`},
	} {
		b.Run(bm.name, func(b *testing.B) {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			if lerr := lisp.InitializeUserEnv(env); lerr.IsError() {
				b.Fatal(lerr)
			}
			if lerr := env.LoadString("def.lisp", bm.def); lerr.IsError() {
				b.Fatal(lerr)
			}
			forms, err := parser.NewReader().Read("call.lisp", strings.NewReader(bm.call))
			if err != nil {
				b.Fatal(err)
			}
			b.ReportAllocs()
			b.ResetTimer()
			for range b.N {
				if v := env.Eval(forms[0]); v.IsError() {
					b.Fatal(v)
				}
			}
		})
	}
}
