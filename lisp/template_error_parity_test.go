package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Definition sites are program metadata, not the transient evaluator register.
// Cancelling exactly at a body entry must report the same location after a
// cold load or Template instantiation. Sweep every early cancellation point
// and require that the sweep actually reaches a function definition.
func TestTemplateCancellationPreservesDefinitionSites(t *testing.T) {
	load := func() *lisp.LEnv {
		env := templateTestEnv(t)
		if got := env.LoadString("definitions.lisp", "(defun step (x) (+ x 1))\n(defun entry (x) (step x))"); got.Type == lisp.LError {
			t.Fatal(got)
		}
		return env
	}
	tmpl, err := lisp.NewTemplate(load(), templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	definitions := 0
	for point := range 24 {
		cold := load()
		fork, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		// Start the call after the same request in both VMs, independently of
		// constructor-time transient source-location registers.
		for _, vm := range []*lisp.LEnv{cold, fork} {
			if got := vm.LoadString("previous.lisp", "()"); !got.IsNil() {
				t.Fatal(got)
			}
		}
		want := cold.LoadStringContext(&countdownContext{n: point}, "request.lisp", "(entry 3)")
		got := fork.LoadStringContext(&countdownContext{n: point}, "request.lisp", "(entry 3)")
		if strings.HasPrefix(want.String(), "definitions.lisp:") {
			definitions++
		}
		if got.Type != want.Type || got.String() != want.String() {
			t.Errorf("cancel point %d: cold=%s; template=%s", point, want, got)
		}
	}
	if definitions == 0 {
		t.Fatal("cancellation sweep never reached a definition-site error")
	}
}
