// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

const frozenProgram = `
(in-package 'frozen-lib)
(export 'counter 'bump 'state)
(set 'counter 1 "a documented counter")
(set 'state (sorted-map "n" 0))
(defun bump () (assoc! state "n" (+ (get state "n") 1)) (get state "n"))
(in-package 'user)
(use-package 'frozen-lib)
(set 'greeting "hello")
(defun greet () greeting)
`

func frozenTemplate(t *testing.T) (*lisp.LEnv, *lisp.Template) {
	t.Helper()
	env := templateTestEnv(t)
	if rc := env.LoadString("frozen.lisp", frozenProgram); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy(), lisp.TemplateWithFrozenPackages("lisp", "frozen-lib"))
	if err != nil {
		t.Fatal(err)
	}
	return env, tmpl
}

func frozenVM(t *testing.T, tmpl *lisp.Template) *lisp.LEnv {
	t.Helper()
	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	return vm
}

func frozenEval(t *testing.T, env *lisp.LEnv, src string) string {
	t.Helper()
	v := env.LoadString("probe.lisp", src)
	if v.Type == lisp.LError {
		t.Fatalf("%s: %v", src, v)
	}
	return v.String()
}

// Every Lisp-level write to a frozen package fails with the frozen-package
// error; siblings, later VMs and the source are unaffected; writes to the
// unfrozen user package keep working per VM.
func TestTemplateFrozenPackageWrites(t *testing.T) {
	source, tmpl := frozenTemplate(t)
	a, b := frozenVM(t, tmpl), frozenVM(t, tmpl)
	for _, tc := range []struct{ src, want string }{
		{`(set 'frozen-lib:counter 2)`, "frozen-lib: symbol counter"},
		{`(set! frozen-lib:counter 2)`, "frozen-lib: symbol counter"},
		{`(in-package 'frozen-lib) (set! counter 2)`, "frozen-lib: symbol counter"},
		{`(in-package 'frozen-lib) (set 'fresh 2)`, "frozen-lib: symbol fresh"},
		{`(in-package 'frozen-lib) (defun bump () 0)`, "frozen-lib: symbol bump"},
		{`(in-package 'frozen-lib) (defmacro mac () 0)`, "frozen-lib: symbol mac"},
		{`(in-package 'frozen-lib) (export 'counter)`, "frozen-lib: symbol counter"},
		{`(in-package 'frozen-lib) (use-package 'user)`, ""},
		{`(in-package 'frozen-lib "new doc")`, "frozen-lib: package documentation"},
		{`(set 'lisp:car 1)`, "lisp: symbol car"},
	} {
		t.Run(tc.src, func(t *testing.T) {
			got := a.LoadString("write.lisp", tc.src)
			if tc.want == "" {
				// user exports nothing: use-package into a frozen package is a no-op.
				if got.Type == lisp.LError {
					t.Fatalf("got %v", got)
				}
			} else if got.Type != lisp.LError || !strings.Contains(got.String(), "cannot modify frozen package "+tc.want) {
				t.Fatalf("want frozen error %q, got %v", tc.want, got)
			}
			if rc := a.InPackage(lisp.String("user")); rc.Type == lisp.LError {
				t.Fatal(rc)
			}
		})
	}
	frozenEval(t, a, `(bump) (set! greeting "changed") (set 'mine 42) (defun greet () "shadowed")`)
	if got := frozenEval(t, a, `(list frozen-lib:counter (get frozen-lib:state "n") greeting mine (greet))`); got != `'(1 1 "changed" 42 "shadowed")` {
		t.Fatalf("writing VM: %s", got)
	}
	for name, env := range map[string]*lisp.LEnv{"sibling": b, "later": frozenVM(t, tmpl), "source": source} {
		if got := frozenEval(t, env, `(list frozen-lib:counter (get frozen-lib:state "n") greeting (greet))`); got != `'(1 0 "hello" "hello")` {
			t.Fatalf("%s observed another VM's writes: %s", name, got)
		}
		if v := env.LoadString("probe.lisp", `mine`); v.Type != lisp.LError {
			t.Fatalf("%s sees another VM's binding: %v", name, v)
		}
		if doc := env.Runtime.Registry.Package("frozen-lib").SymbolDoc("counter"); doc != "a documented counter" {
			t.Fatalf("%s doc = %q", name, doc)
		}
	}
}

// packageView renders everything iteration exposes about a registry.
func packageView(env *lisp.LEnv) string {
	var sb strings.Builder
	reg := env.Runtime.Registry
	for _, name := range reg.PackageNames() {
		pkg := reg.Package(name)
		fmt.Fprintf(&sb, "%s %q %v\n", name, pkg.Doc, pkg.Externals())
		for _, sym := range pkg.SymbolNames() {
			v, _ := pkg.Symbol(sym)
			fn := ""
			if v.Type == lisp.LFun {
				fn = pkg.GetFunName(v.FID())
			}
			fmt.Fprintf(&sb, "  %s %v %q %q\n", sym, v.Type, pkg.SymbolDoc(sym), fn)
		}
	}
	return sb.String()
}

// Iteration over a VM with frozen packages matches a cold environment
// loaded from the same program, before and after unfrozen writes.
func TestTemplateFrozenPackageIterationParity(t *testing.T) {
	_, tmpl := frozenTemplate(t)
	cold := templateTestEnv(t)
	frozenEval(t, cold, frozenProgram)
	vm := frozenVM(t, tmpl)
	if !vm.Runtime.Registry.Package("frozen-lib").Frozen() || vm.Runtime.Registry.Package("user").Frozen() {
		t.Fatal("frozen set not applied")
	}
	if got, want := packageView(vm), packageView(cold); got != want {
		t.Fatalf("VM iteration differs from cold env:\n%s\nwant\n%s", got, want)
	}
	const writes = `(set 'zz 1) (set 'aa 2 "doc") (set! greeting "x") (defun greet () 1) (export 'aa)`
	frozenEval(t, vm, writes)
	frozenEval(t, cold, writes)
	if got, want := packageView(vm), packageView(cold); got != want {
		t.Fatalf("written VM iteration differs from cold env:\n%s\nwant\n%s", got, want)
	}
}

// Concurrent VMs share frozen tables; -race reports any write to them.
func TestTemplateFrozenPackageConcurrentVMs(t *testing.T) {
	_, tmpl := frozenTemplate(t)
	var wg sync.WaitGroup
	for n := range 8 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			vm, err := tmpl.NewVM()
			if err != nil {
				t.Error(err)
				return
			}
			src := fmt.Sprintf(`(set 'mine %d) (bump) (bump) (set! greeting "g%d")
(ignore-errors (set 'frozen-lib:counter %d))
(list mine (get frozen-lib:state "n") frozen-lib:counter (greet))`, n, n, n)
			got := vm.LoadString("race.lisp", src)
			if want := fmt.Sprintf(`'(%d 2 1 "g%d")`, n, n); got.String() != want {
				t.Errorf("vm %d: got %v want %s", n, got, want)
			}
		}()
	}
	wg.Wait()
	if got := frozenEval(t, frozenVM(t, tmpl), `(list frozen-lib:counter (get frozen-lib:state "n") greeting)`); got != `'(1 0 "hello")` {
		t.Fatalf("template changed under concurrent VMs: %s", got)
	}
}

// A qualified set with a docstring documents the TARGET package's symbol,
// and a frozen current package does not stop it.
func TestTemplateFrozenQualifiedSetDoc(t *testing.T) {
	_, tmpl := frozenTemplate(t)
	vm := frozenVM(t, tmpl)
	got := vm.LoadString("doc.lisp", `(in-package 'frozen-lib) (lisp:set 'user:qx 1 "the doc") (in-package 'user) qx`)
	if got.Type == lisp.LError || lisp.IsInternalPanic(got) || got.String() != "1" {
		t.Fatalf("qualified set from a frozen package: %v", got)
	}
	if doc := vm.Runtime.Registry.Package("user").SymbolDoc("qx"); doc != "the doc" {
		t.Fatalf("doc on target package = %q", doc)
	}
	// A refused target is refused before anything is written.
	got = vm.LoadString("doc.lisp", `(lisp:set 'frozen-lib:counter 5 "d")`)
	if got.Type != lisp.LError || !strings.Contains(got.String(), "cannot modify frozen package frozen-lib: symbol counter") {
		t.Fatalf("want frozen error, got %v", got)
	}
	if v := frozenEval(t, vm, `frozen-lib:counter`); v != "1" {
		t.Fatalf("counter changed: %s", v)
	}
	// A bad docstring is rejected before the binding is written.
	if got := vm.LoadString("doc.lisp", `(set 'badoc 1 2)`); got.Type != lisp.LError {
		t.Fatalf("want docstring error, got %v", got)
	}
	if v := vm.LoadString("doc.lisp", `badoc`); v.Type != lisp.LError {
		t.Fatalf("binding written before docstring validation: %v", v)
	}
	// Cold envs document the target package too.
	cold := templateTestEnv(t)
	frozenEval(t, cold, frozenProgram+`(in-package 'frozen-lib) (set 'user:qx 1 "cold doc") (in-package 'user)`)
	if doc := cold.Runtime.Registry.Package("user").SymbolDoc("qx"); doc != "cold doc" {
		t.Fatalf("cold doc on target package = %q", doc)
	}
}

// use-package into a frozen package succeeds when it would import nothing
// new (as in a cold env) and errors only when it would add bindings.
func TestTemplateFrozenNoOpUsePackage(t *testing.T) {
	_, tmpl := frozenTemplate(t)
	vm := frozenVM(t, tmpl)
	for _, src := range []string{
		`(in-package 'frozen-lib) (use-package 'lisp)`,
		`(in-package 'frozen-lib) (use-package 'frozen-lib)`,
	} {
		if got := vm.LoadString("use.lisp", src); got.Type == lisp.LError {
			t.Fatalf("%s: no-op use-package failed: %v", src, got)
		}
	}
	frozenEval(t, vm, `(in-package 'user) (in-package 'fresh-pkg) (export 'extra) (set 'extra 1) (in-package 'user)`)
	got := vm.LoadString("use.lisp", `(in-package 'frozen-lib) (use-package 'fresh-pkg)`)
	if got.Type != lisp.LError || !strings.Contains(got.String(), "cannot modify frozen package frozen-lib: symbol extra") {
		t.Fatalf("importing new bindings into a frozen package: %v", got)
	}
}
