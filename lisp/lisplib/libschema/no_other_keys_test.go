package libschema_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
)

// TestNoOtherKeysNonMap checks that s:no-other-keys raises wrong-type on a
// value that is not a sorted-map, as s:has-key does. Before the fix it
// raised internal-panic, which handler-bind cannot catch
// (luthersystems/elps#989).
func TestNoOtherKeysNonMap(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"no children", `(s:validate (s:no-other-keys) 1)`},
		{"under s:any", `(s:validate (s:make-validator "T" s:any (s:no-other-keys)) 1)`},
		{"child that accepts an int", `(s:validate (s:no-other-keys s:int) 1)`},
		{"string input", `(s:validate (s:no-other-keys (s:has-key "a" s:int)) "a")`},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			env := newSchemaEnv(t)
			got := env.LoadStringContext(context.Background(), "test", c.src)
			if got.Type != lisp.LError {
				t.Fatalf("want a wrong-type error, got %v", got)
			}
			if got.Str != libschema.WrongType {
				t.Fatalf("want condition %q, got %q: %v", libschema.WrongType, got.Str, got)
			}
		})
	}
}
