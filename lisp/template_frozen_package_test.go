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

// A set/set! of a name a frozen package already binds writes only this VM's
// slot: the package stays frozen, the write is visible here and nowhere else.
func TestFrozenPackageSlotWriteDoesNotThaw(t *testing.T) {
	_, tmpl := frozenTemplate(t)
	a, b := frozenVM(t, tmpl), frozenVM(t, tmpl)
	frozenEval(t, a, `(set 'frozen-lib:counter 41)`)
	frozenEval(t, a, `(in-package 'frozen-lib) (set! counter (+ counter 1)) (in-package 'user)`)
	pkgA := a.Runtime.Registry.Package("frozen-lib")
	if !pkgA.Frozen() {
		t.Fatal("slot-only write thawed the package")
	}
	if got := frozenEval(t, a, `frozen-lib:counter`); got != "42" {
		t.Fatalf("writer VM sees %s", got)
	}
	if got := frozenEval(t, b, `frozen-lib:counter`); got != "1" {
		t.Fatalf("other VM sees %s", got)
	}
	if got := frozenEval(t, frozenVM(t, tmpl), `frozen-lib:counter`); got != "1" {
		t.Fatalf("fresh VM sees %s", got)
	}
	if doc := pkgA.SymbolDoc("counter"); doc != "a documented counter" {
		t.Fatalf("doc changed: %q", doc)
	}
	// Rebinding the same function value under its own name keeps funNames
	// unchanged, so it needs no thaw either.
	frozenEval(t, a, `(set 'frozen-lib:bump frozen-lib:bump)`)
	if !pkgA.Frozen() {
		t.Fatal("same-function rebind thawed the package")
	}
	// A new function value is a slot write too: its name goes to this VM's
	// overlay, not the shared funNames.
	frozenEval(t, a, `(set 'frozen-lib:bump (lambda () 7))`)
	if !pkgA.Frozen() {
		t.Fatal("function rebind with a new FID thawed the package")
	}
	if got := frozenEval(t, a, `(frozen-lib:bump)`); got != "7" {
		t.Fatalf("writer VM bump = %s", got)
	}
	if got := frozenEval(t, b, `(frozen-lib:bump)`); got != "1" {
		t.Fatalf("other VM bump = %s", got)
	}
	if !b.Runtime.Registry.Package("frozen-lib").Frozen() {
		t.Fatal("other VM thawed")
	}
	// A new name thaws.
	frozenEval(t, b, `(set 'frozen-lib:fresh 1)`)
	if b.Runtime.Registry.Package("frozen-lib").Frozen() {
		t.Fatal("new name did not thaw")
	}
}

// phylumProgram stands in for an application package whose endpoints rebind
// its own globals on every transaction: strings, maps, functions, closures.
const phylumProgram = `
(in-package 'app)
(set 'last-caller "none")
(set 'cache (sorted-map))
(set 'n 0)
(defun handler () "base")
(set 'hook (lambda () "base-hook"))
(defun txn (who)
  (set 'last-caller who)
  (set! cache (sorted-map "who" who))
  (set! n (+ n 1))
  (let ([tag who])
    (set 'hook (lambda () (concat 'string "hook-" tag))))
  (set 'handler (lambda () (error 'boom (concat 'string "from " who))))
  (list last-caller (get cache "who") n (hook)))
(defun explode () (handler))
(in-package 'user)
`

// A fully frozen VM (user and the application package too) running
// per-transaction set/set! on the application's own globals never thaws,
// sees its own writes, leaks nothing to other VMs or the template, and
// renders exactly what a cold environment renders, stack-trace names
// included.
func TestFrozenPackagePerTransactionGlobalsDoNotThaw(t *testing.T) {
	env := templateTestEnv(t)
	if rc := env.LoadString("app.lisp", phylumProgram); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy(), lisp.TemplateWithFrozenPackages(env.Runtime.Registry.PackageNames()...))
	if err != nil {
		t.Fatal(err)
	}
	cold := func() *lisp.LEnv {
		c := templateTestEnv(t)
		if rc := c.LoadString("app.lisp", phylumProgram); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		return c
	}
	run := func(e *lisp.LEnv, src string) string {
		v := e.LoadString("txn.lisp", src)
		if v.Type == lisp.LError {
			var b strings.Builder
			_, _ = (*lisp.ErrorVal)(v).WriteTrace(&b)
			return "ERR " + b.String()
		}
		return v.String()
	}
	frozenAll := func(vm *lisp.LEnv) {
		t.Helper()
		for _, name := range vm.Runtime.Registry.PackageNames() {
			if !vm.Runtime.Registry.Package(name).Frozen() {
				t.Fatalf("package %s thawed", name)
			}
		}
	}
	a, b, c := frozenVM(t, tmpl), frozenVM(t, tmpl), cold()
	frozenAll(a)
	for i, who := range []string{"alice", "bob", "carol"} {
		src := fmt.Sprintf("(app:txn %q)", who)
		got, want := run(a, src), run(c, src)
		if got != want {
			t.Fatalf("txn %d: frozen %s, cold %s", i, got, want)
		}
		if got, want := run(a, "(app:explode)"), run(c, "(app:explode)"); got != want {
			t.Fatalf("txn %d error: frozen %q, cold %q", i, got, want)
		} else if !strings.Contains(got, "app:handler") {
			t.Fatalf("txn %d error trace does not name the rebound function: %q", i, got)
		}
		frozenAll(a)
	}
	if got := run(a, "(list app:last-caller app:n (app:hook))"); got != `'("carol" 3 "hook-carol")` {
		t.Fatalf("writer VM sees %s", got)
	}
	base := `'("none" 0 "base-hook")`
	if got := run(b, "(list app:last-caller app:n (app:hook))"); got != base {
		t.Fatalf("other VM sees %s", got)
	}
	frozenAll(b)
	if got := run(env, "(list app:last-caller app:n (app:hook))"); got != base {
		t.Fatalf("template env sees %s", got)
	}
	if got := run(frozenVM(t, tmpl), "(list app:last-caller app:n (app:hook))"); got != base {
		t.Fatalf("fresh VM sees %s", got)
	}
}

// TestTemplateThawHook pins TemplateWithThawHook: a slot write (rebinding an
// existing name) does not thaw, and exporting a new name thaws the package
// exactly once however many further table writes follow.
func TestTemplateThawHook(t *testing.T) {
	env := templateTestEnv(t)
	if rc := env.LoadString("frozen.lisp", frozenProgram); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	var mu sync.Mutex
	thaws := map[string]int{}
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy(),
		lisp.TemplateWithFrozenPackages("lisp", "frozen-lib"),
		lisp.TemplateWithThawHook(func(pkg string) {
			mu.Lock()
			thaws[pkg]++
			mu.Unlock()
		}))
	if err != nil {
		t.Fatal(err)
	}
	vm := frozenVM(t, tmpl)
	frozenEval(t, vm, `(in-package 'frozen-lib) (set 'counter 42) (set! counter 43) (defun bump () 0) (in-package 'user)`)
	if n := thaws["frozen-lib"]; n != 0 {
		t.Fatalf("slot writes thawed frozen-lib %d times, want 0", n)
	}
	frozenEval(t, vm, `(in-package 'frozen-lib) (set 'fresh 1) (export 'fresh) (set 'fresh2 2) (export 'fresh2) (in-package 'user)`)
	if n := thaws["frozen-lib"]; n != 1 {
		t.Fatalf("new exported names thawed frozen-lib %d times, want 1", n)
	}
	vm2 := frozenVM(t, tmpl)
	frozenEval(t, vm2, `(in-package 'frozen-lib) (export 'counter2) (in-package 'user)`)
	if n := thaws["frozen-lib"]; n != 2 {
		t.Fatalf("second VM: frozen-lib thaws = %d, want 2", n)
	}
}
