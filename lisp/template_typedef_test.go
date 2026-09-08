// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// The core metatype constructor is nested data, not a registered callback
// root. Its empty capture declaration must let hosts approve only their
// actual registrations, without granting trust to every reachable function.
func TestTemplateTypedefRequiresOnlyRegisteredBuiltinApproval(t *testing.T) {
	cases := []struct{ code, want string }{
		{`(set 'box (new lisp:typedef 'user:box identity)) (new box 17)`, `#{user:box 17}`},
		{`(deftype twice (x) (* x 2)) (new twice 21)`, `#{user:twice 42}`},
		{`(new lisp:typedef 1 identity)`, `typedef.lisp:1:1: lisp:_fun2: first argument is not a symbol: 'int`},
		{`(new lisp:typedef 'user:bad 1)`, `typedef.lisp:1:1: lisp:_fun2: second argument is not a function: 'int`},
		{`(new lisp:typedef 'user:bad if)`, `typedef.lisp:1:1: lisp:_fun2: second argument is not a regular function`},
	}
	check := func(label string, env *lisp.LEnv) {
		t.Helper()
		for _, tc := range cases {
			if got := env.LoadString("typedef.lisp", tc.code).String(); got != tc.want {
				t.Fatalf("%s %s: got %s, want %s", label, tc.code, got, tc.want)
			}
		}
	}
	// Pin the existing cold behavior before trying publication.
	check("cold", templateTestEnv(t))
	source := templateTestEnv(t)
	registered := make(map[any]bool)
	for _, pkgName := range source.Runtime.Registry.PackageNames() {
		pkg := source.Runtime.Registry.Package(pkgName)
		for _, name := range pkg.SymbolNames() {
			value, _ := pkg.Symbol(name)
			if value.Type == lisp.LFun && value.Builtin() != nil {
				registered[value.Native] = true
			}
		}
	}
	tmpl, err := lisp.NewTemplate(source, lisp.TemplateWithBuiltinPolicy(func(value *lisp.LVal) bool {
		return registered[value.Native]
	}))
	if err != nil {
		t.Fatal(err)
	}
	for range 2 {
		vm, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		check("template", vm)
	}
}
