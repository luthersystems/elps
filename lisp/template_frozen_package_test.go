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

// frozenWrites are programs that write a frozen package in every way Lisp
// can. Before thawing existed each one failed with a frozen-package error;
// each must now behave exactly as in a cold environment.
var frozenWrites = []string{
	`(in-package 'frozen-lib) (set 'counter 99) (in-package 'user)`, // a phylum overriding a library constant at load
	`(set 'frozen-lib:counter 2)`,
	`(set! frozen-lib:counter 2)`,
	`(in-package 'frozen-lib) (set! counter 2) (in-package 'user)`,
	`(in-package 'frozen-lib) (set 'fresh 2 "fresh doc") (in-package 'user)`,
	`(in-package 'frozen-lib) (defun bump () 0) (in-package 'user)`,
	`(in-package 'frozen-lib) (defmacro mac () 0) (in-package 'user)`,
	`(in-package 'frozen-lib) (export 'extra) (set 'extra 3) (in-package 'user)`,
	`(in-package 'frozen-lib) (use-package 'user) (in-package 'user)`,
	`(in-package 'frozen-lib) (use-package 'lisp) (in-package 'user)`,
	`(in-package 'frozen-lib "new doc") (in-package 'user)`,
	`(in-package 'fresh-pkg) (export 'e) (set 'e 1) (in-package 'frozen-lib) (use-package 'fresh-pkg) (in-package 'user)`,
}

// A write to a frozen package thaws a private copy for that VM: the write
// succeeds, the VM then matches a cold environment given the same program,
// and siblings, later VMs and the template are untouched.
func TestTemplateFrozenPackageThawOnWrite(t *testing.T) {
	source, tmpl := frozenTemplate(t)
	pristine := packageView(frozenVM(t, tmpl))
	for _, src := range frozenWrites {
		t.Run(src, func(t *testing.T) {
			vm, sibling := frozenVM(t, tmpl), frozenVM(t, tmpl)
			cold := templateTestEnv(t)
			frozenEval(t, cold, frozenProgram)
			if got, want := vm.LoadString("write.lisp", src).String(), cold.LoadString("write.lisp", src).String(); got != want {
				t.Fatalf("write result %s, cold env %s", got, want)
			}
			if got, want := packageView(vm), packageView(cold); got != want {
				t.Fatalf("thawed VM differs from cold env:\n%s\nwant\n%s", got, want)
			}
			const probe = `(list frozen-lib:counter (frozen-lib:bump) (greet))`
			if got, want := vm.LoadString("p.lisp", probe).String(), cold.LoadString("p.lisp", probe).String(); got != want {
				t.Fatalf("thawed VM evaluates %s, cold env %s", got, want)
			}
			if !vm.Runtime.Registry.Package("lisp").Frozen() {
				t.Fatal("an unrelated frozen package thawed")
			}
			for name, env := range map[string]*lisp.LEnv{"sibling": sibling, "later": frozenVM(t, tmpl)} {
				if got := packageView(env); got != pristine {
					t.Fatalf("%s observed another VM's write:\n%s", name, got)
				}
				if !env.Runtime.Registry.Package("frozen-lib").Frozen() {
					t.Fatalf("%s thawed", name)
				}
			}
			if got := frozenEval(t, source, `frozen-lib:counter`); got != "1" {
				t.Fatalf("source changed: %s", got)
			}
		})
	}
}

// Reads do not thaw; a mutation of a bound VALUE is per-VM without thawing.
func TestTemplateFrozenPackageReadsStayShared(t *testing.T) {
	_, tmpl := frozenTemplate(t)
	a, b := frozenVM(t, tmpl), frozenVM(t, tmpl)
	frozenEval(t, a, `(list frozen-lib:counter (frozen-lib:bump) (frozen-lib:bump))`)
	if !a.Runtime.Registry.Package("frozen-lib").Frozen() {
		t.Fatal("reads or value mutation thawed the package")
	}
	if got := frozenEval(t, b, `(get frozen-lib:state "n")`); got != "0" {
		t.Fatalf("value mutation leaked: %s", got)
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
(set 'frozen-lib:counter %d)
(list mine (get frozen-lib:state "n") frozen-lib:counter (greet))`, n, n, n+10)
			got := vm.LoadString("race.lisp", src)
			if want := fmt.Sprintf(`'(%d 2 %d "g%d")`, n, n+10, n); got.String() != want {
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
// from a frozen current package and into a frozen target alike.
func TestTemplateFrozenQualifiedSetDoc(t *testing.T) {
	_, tmpl := frozenTemplate(t)
	vm := frozenVM(t, tmpl)
	got := vm.LoadString("doc.lisp", `(in-package 'frozen-lib) (lisp:set 'user:qx 1 "the doc") (in-package 'user) qx`)
	if got.Type == lisp.LError || got.String() != "1" {
		t.Fatalf("qualified set from a frozen package: %v", got)
	}
	if doc := vm.Runtime.Registry.Package("user").SymbolDoc("qx"); doc != "the doc" {
		t.Fatalf("doc on target package = %q", doc)
	}
	if !vm.Runtime.Registry.Package("frozen-lib").Frozen() {
		t.Fatal("a write to user thawed the current package")
	}
	frozenEval(t, vm, `(lisp:set 'frozen-lib:counter 5 "d")`)
	if v, doc := frozenEval(t, vm, `frozen-lib:counter`), vm.Runtime.Registry.Package("frozen-lib").SymbolDoc("counter"); v != "5" || doc != "d" {
		t.Fatalf("frozen target: %s %q", v, doc)
	}
	// A bad docstring is rejected before the binding is written.
	if got := vm.LoadString("doc.lisp", `(set 'badoc 1 2)`); got.Type != lisp.LError {
		t.Fatalf("want docstring error, got %v", got)
	}
	if v := vm.LoadString("doc.lisp", `badoc`); v.Type != lisp.LError {
		t.Fatalf("binding written before docstring validation: %v", v)
	}
	cold := templateTestEnv(t)
	frozenEval(t, cold, frozenProgram+`(in-package 'frozen-lib) (set 'user:qx 1 "cold doc") (in-package 'user)`)
	if doc := cold.Runtime.Registry.Package("user").SymbolDoc("qx"); doc != "cold doc" {
		t.Fatalf("cold doc on target package = %q", doc)
	}
}
