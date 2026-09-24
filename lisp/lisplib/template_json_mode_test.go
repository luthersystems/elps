// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// JSON serializer modes are per-VM state: a VM whose json package is frozen
// can still switch modes, and the switch reaches neither its siblings nor
// the template. A mode set before publication is inherited by every VM.
func TestTemplateJSONModesWithFrozenJSONPackage(t *testing.T) {
	const probe = `(list (json:string-numbers?) (json:load-string "1.5") (json:load-string "12345678901234567890"))`
	for _, tc := range []struct {
		name, setup, want string
	}{
		{"default", ``, `'(false 1.5 12345678901234567890.0)`},
		{"published", `(json:use-string-numbers true) (json:use-exact-integers true)`, `'(true "1.5" "12345678901234567890")`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := loadTemplateFixture(t, tc.setup+` (set 'x 1)`)
			want := env.LoadString("probe.lisp", probe).String()
			tmpl, err := lisp.NewTemplate(env, templateFixturePolicy(), lisp.TemplateWithFrozenPackages("json", "lisp"))
			if err != nil {
				t.Fatal(err)
			}
			a, _ := tmpl.NewVM()
			b, _ := tmpl.NewVM()
			if !a.Runtime.Registry.Package("json").Frozen() {
				t.Fatal("json not frozen")
			}
			flip := `(json:use-string-numbers true) (json:use-exact-integers true)`
			if tc.setup != "" {
				flip = `(json:use-string-numbers false) (json:use-exact-integers false)`
			}
			if got := a.LoadString("flip.lisp", flip); got.Type == lisp.LError {
				t.Fatalf("mode switch in a frozen-json VM failed: %v", got)
			}
			if got := a.LoadString("probe.lisp", probe).String(); got == want {
				t.Fatalf("mode switch had no effect in its own VM: %s", got)
			}
			for name, e := range map[string]*lisp.LEnv{"sibling": b, "source": env} {
				if got := e.LoadString("probe.lisp", probe).String(); got != want {
					t.Fatalf("%s: got %s, want %s", name, got, want)
				}
			}
			c, _ := tmpl.NewVM()
			if got := c.LoadString("probe.lisp", probe).String(); got != want {
				t.Fatalf("later VM: got %s, want %s", got, want)
			}
		})
	}
}
