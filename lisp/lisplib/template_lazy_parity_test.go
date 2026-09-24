// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"bytes"
	"fmt"
	"sort"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
	"github.com/luthersystems/elps/lisp/x/debugger"
)

// lazyParitySource exercises every representation lazy instantiation
// defers: package slots in frozen and unfrozen packages, sorted maps whose
// entries share a value, nested maps, JSON maps, byte views, closures over
// package state, and a thawed package.
const lazyParitySource = `
(in-package 'lib)
(export 'table 'jm 'mk 'shared 'counter 'bump)
(set 'shared (list 1 2 3))
(set 'table (sorted-map "a" shared "b" shared "n" (sorted-map "deep" (list shared "x")) "bytes" (to-bytes "hi")))
(set 'jm (json:load-string "{\"k\":[1,2],\"s\":\"t\"}"))
(set 'counter 0)
(defun mk (x) (lambda (y) (+ x y (length shared))))
(defun bump () (set! counter (+ counter 1)))
(in-package 'user)
(use-package 'lib)
(set 'adder (mk 10))
(set 'local (sorted-map "k" (sorted-map "v" shared) "s" "str"))
(defun pick (m k) (get m k))
`

// lazyParityProbe is a transaction touching the deferred state through the
// evaluator, including a write that thaws the phylum-style user package.
const lazyParityProbe = `
(set 'fresh 1)
(list (adder 5) (pick lib:table "a") (keys local) (get (get local "k") "v")
      (json:dump-string lib:jm) (lib:bump) (lib:bump) fresh
      (sorted-map "copy" (get lib:table "n")))
`

func lazyParityTemplates(t *testing.T) map[string]*lisp.Template {
	t.Helper()
	out := map[string]*lisp.Template{}
	for name, opts := range map[string][]lisp.TemplateOption{
		"eager":       {templateFixturePolicy(), lisp.TemplateWithEagerInstantiation()},
		"lazy":        {templateFixturePolicy()},
		"lazy-frozen": {templateFixturePolicy(), lisp.TemplateWithFrozenPackages("lib")},
		"prewarm":     {templateFixturePolicy(), lisp.TemplateWithFrozenPackages("lib")},
	} {
		env := loadTemplateFixture(t, lazyParitySource)
		tmpl, err := lisp.NewTemplate(env, opts...)
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		out[name] = tmpl
	}
	return out
}

// TestTemplateLazyWalkParity runs every path that walks a whole value graph
// or package table on a FRESH VM, so each starts with every entry still
// pending, and requires the result to equal eager instantiation's.
func TestTemplateLazyWalkParity(t *testing.T) {
	templates := lazyParityTemplates(t)
	paths := map[string]func(t *testing.T, vm *lisp.LEnv) string{
		"eval": func(t *testing.T, vm *lisp.LEnv) string {
			return vm.LoadString("probe.lisp", lazyParityProbe).String()
		},
		"copy": func(t *testing.T, vm *lisp.LEnv) string {
			return vm.GetGlobal(lisp.Symbol("lib:table")).Copy().String() + " " +
				vm.GetGlobal(lisp.Symbol("local")).Copy().String()
		},
		"json": func(t *testing.T, vm *lisp.LEnv) string {
			return vm.LoadString("json.lisp", `(list (json:dump-string (sorted-map "t" lib:table "l" local)) (json:dump-string lib:jm))`).String()
		},
		"help": func(t *testing.T, vm *lisp.LEnv) string {
			var buf bytes.Buffer
			if err := libhelp.RenderPkgExported(&buf, vm, "lib"); err != nil {
				t.Fatal(err)
			}
			doc, err := libhelp.QueryPackage(vm, "user")
			if err != nil {
				t.Fatal(err)
			}
			return buf.String() + fmt.Sprintf("%+v %d", *doc, len(libhelp.CheckMissing(vm)))
		},
		"debugger": func(t *testing.T, vm *lisp.LEnv) string {
			var parts []string
			for _, c := range debugger.CompleteInContext(vm, "lib:") {
				parts = append(parts, fmt.Sprintf("%+v", c))
			}
			for _, c := range debugger.CompleteInContext(vm, "") {
				parts = append(parts, fmt.Sprintf("%+v", c))
			}
			sort.Strings(parts)
			for _, b := range debugger.InspectScope(vm) {
				parts = append(parts, b.Name+"="+debugger.FormatValue(b.Value))
			}
			parts = append(parts, debugger.FormatValue(vm.GetGlobal(lisp.Symbol("lib:table"))))
			parts = append(parts, debugger.EvalInContext(vm, `(pick local "s")`).String())
			return strings.Join(parts, "\n")
		},
		"republish": func(t *testing.T, vm *lisp.LEnv) string {
			// Thaw user first, so republication reads a package with pending
			// bindings as well as untouched maps.
			vm.LoadString("thaw.lisp", `(set 'thawed 1)`)
			again, err := lisp.NewTemplate(vm, templateFixturePolicy())
			if err != nil {
				t.Fatal(err)
			}
			next := forkTemplateFixture(t, again)
			return next.LoadString("probe.lisp", lazyParityProbe).String()
		},
		"symbols": func(t *testing.T, vm *lisp.LEnv) string {
			var parts []string
			reg := vm.Runtime.Registry
			for _, name := range reg.PackageNames() {
				pkg := reg.Package(name)
				for _, sym := range pkg.SymbolNames() {
					v, ok := pkg.Symbol(sym)
					if !ok || v == nil {
						t.Fatalf("%s:%s has no value", name, sym)
					}
					parts = append(parts, name+":"+sym+"="+v.Type.String())
				}
			}
			return strings.Join(parts, "\n")
		},
	}
	for name, path := range paths {
		t.Run(name, func(t *testing.T) {
			want := path(t, forkTemplateFixture(t, templates["eager"]))
			if strings.Contains(want, "lazy-pending") {
				t.Fatalf("eager result mentions the marker: %s", want)
			}
			// Run the probe once on the prewarm template so its hot set is
			// populated; the compared VM then starts partly built.
			forkTemplateFixture(t, templates["prewarm"]).LoadString("probe.lisp", lazyParityProbe)
			for _, mode := range []string{"lazy", "lazy-frozen", "prewarm"} {
				vm := forkTemplateFixture(t, templates[mode])
				if mode == "prewarm" {
					var err error
					if vm, err = templates[mode].NewVM(lisp.VMWithPrewarm()); err != nil {
						t.Fatal(err)
					}
				}
				got := path(t, vm)
				if got != want {
					t.Errorf("%s differs from eager:\n got: %s\nwant: %s", mode, got, want)
				}
			}
		})
	}
}

// TestTemplateLazyConcurrentVMs is the supported concurrency pattern under
// -race: many goroutines mint VMs from one template at once, each VM is used
// by one goroutine at a time, and a VM may move between goroutines across a
// channel. Every VM starts lazy, so the materialization is concurrent across
// VMs but never within one.
func TestTemplateLazyConcurrentVMs(t *testing.T) {
	tmpl := lazyParityTemplates(t)["lazy-frozen"]
	want := forkTemplateFixture(t, tmpl).LoadString("probe.lisp", lazyParityProbe).String()
	handoff := make(chan *lisp.LEnv)
	var wg sync.WaitGroup
	errs := make(chan string, 64)
	for range 8 {
		wg.Add(2)
		go func() {
			defer wg.Done()
			for i := range 4 {
				var opts []lisp.VMOption
				if i%2 == 0 {
					opts = append(opts, lisp.VMWithPrewarm()) // reads the hot set other VMs are writing
				}
				vm, err := tmpl.NewVM(opts...)
				if err != nil {
					errs <- err.Error()
					return
				}
				vm.GetGlobal(lisp.Symbol("lib:table")) // start materializing here...
				handoff <- vm                          // ...finish on another goroutine
			}
		}()
		go func() {
			defer wg.Done()
			for range 4 {
				vm := <-handoff
				if got := vm.LoadString("probe.lisp", lazyParityProbe).String(); got != want {
					errs <- got
				}
			}
		}()
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}
}
