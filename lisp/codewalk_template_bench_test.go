// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// BenchmarkMacroExpandAllTemplate walks quasiquote templates, the path
// through CodeWalker.templateList: one whose lists are unchanged and one
// whose unquoted forms expand a macro.
func BenchmarkMacroExpandAllTemplate(b *testing.B) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.IsError() {
		b.Fatal(rc)
	}
	if v := env.LoadString("bench", `(defmacro m (x) (quasiquote (list (unquote x))))`); v.IsError() {
		b.Fatal(v)
	}
	for _, tc := range []struct{ name, src string }{
		{"unchanged", `(quasiquote (a (b c d) (e (f g) h) (i j k l) (unquote x)))`},
		{"changed", `(quasiquote (a (b c d) (e (f g) h) (i j k l) (unquote (m x))))`},
	} {
		forms, err := env.Runtime.Reader.Read("bench", strings.NewReader(tc.src))
		if err != nil {
			b.Fatal(err)
		}
		form := forms[0]
		b.Run(tc.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if v := env.MacroExpandAll(form); v.IsError() {
					b.Fatal(v)
				}
			}
		})
	}
}
