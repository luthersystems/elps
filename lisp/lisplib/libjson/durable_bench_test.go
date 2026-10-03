// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
)

// benchFunctionEnv is a user package with 6000 list bindings and a function
// that sorts after them, as a large phylum package might hold.
func benchFunctionEnv(b testing.TB) *lisp.LEnv {
	b.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	for _, r := range []*lisp.LVal{lisp.InitializeUserEnv(env), lisplib.LoadRuntimeLibrary(env), env.InPackage(lisp.String(lisp.DefaultUserPackage))} {
		if r.Type == lisp.LError {
			b.Fatal(r)
		}
	}
	user := env.Runtime.Registry.Package(lisp.DefaultUserPackage)
	for i := range 6000 {
		user.Put(lisp.Symbol("v"+strconv.Itoa(i)), lisp.QExpr([]*lisp.LVal{lisp.Int(i), lisp.String("s"), lisp.Vector([]*lisp.LVal{lisp.Int(i)})}))
	}
	if r := env.LoadString("bench.lisp", `(defun zz-fn () 1)`); r.Type == lisp.LError {
		b.Fatal(r)
	}
	return env
}

// BenchmarkDurableFunctionName dumps one function on a cold environment, an
// eager template VM and a lazy template VM.  Each template iteration mints
// a fresh VM, outside the timer, so the lazy VM starts unmaterialized.
func BenchmarkDurableFunctionName(b *testing.B) {
	env := benchFunctionEnv(b)
	dump := func(b *testing.B, vm *lisp.LEnv) {
		f := vm.LoadString("bench.lisp", `zz-fn`)
		if _, err := libjson.DumpDurable(vm, f, nil); err != nil {
			b.Fatal(err)
		}
	}
	b.Run("cold", func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			dump(b, env)
		}
	})
	for _, c := range []struct {
		name string
		opts []lisp.TemplateOption
	}{
		{"eager-template", []lisp.TemplateOption{lisp.TemplateWithEagerInstantiation()}},
		{"lazy-template", nil},
	} {
		b.Run(c.name, func(b *testing.B) {
			tmpl, err := lisp.NewTemplate(env, append([]lisp.TemplateOption{lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })}, c.opts...)...)
			if err != nil {
				b.Fatal(err)
			}
			b.ReportAllocs()
			for b.Loop() {
				b.StopTimer()
				vm, err := tmpl.NewVM()
				if err != nil {
					b.Fatal(err)
				}
				f := vm.LoadString("bench.lisp", `zz-fn`)
				b.StartTimer()
				if _, err := libjson.DumpDurable(vm, f, nil); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}
