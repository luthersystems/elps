package libschema_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Issue #620: schema constructors used Go closures whose captured LVals stayed
// attached to the template. A fork must retain the cold VM's alias between a
// constraint's allowed value and the program's mutable global.
func TestSchemaMutableCaptureColdForkParity(t *testing.T) {
	cases := []struct {
		name, constraint, input, rejected string
	}{
		{"in", `(s:in allowed)`, `allowed`, `(sorted-map "k" -1)`},
		{"any", `(s:make-validator "T" s:any (s:in allowed))`, `allowed`, `(sorted-map "k" -1)`},
		{"map", `(s:make-validator "T" s:sorted-map (s:in allowed))`, `allowed`, `(sorted-map "k" -1)`},
		{"array", `(s:make-validator "T" s:array (s:of (s:in allowed)))`, `(vector allowed)`, `(vector (sorted-map "k" -1))`},
		{"has-key", `(s:has-key "v" (s:in allowed))`, `(sorted-map "v" allowed)`, `(sorted-map "v" (sorted-map "k" -1))`},
		{"may-have-key", `(s:may-have-key "v" (s:in allowed))`, `(sorted-map "v" allowed)`, `(sorted-map "v" (sorted-map "k" -1))`},
		{"no-other-keys", `(s:no-other-keys (s:has-key "v" (s:in allowed)))`, `(sorted-map "v" allowed)`, `(sorted-map "v" allowed "extra" 0)`},
		{"when-result", `(s:when "guard" (s:in true) "v" (s:in allowed))`, `(sorted-map "guard" true "v" allowed)`, `(sorted-map "guard" true "v" (sorted-map "k" -1))`},
		{"when-guard", `(s:when "guard" (s:in allowed) "v" (s:gt 0))`, `(sorted-map "guard" allowed "v" 1)`, `(sorted-map "guard" allowed "v" -1)`},
		{"not", `(s:not (s:in allowed))`, `(sorted-map "k" -1)`, `allowed`},
		{"tagged", `(progn (deftype Box (x) x) (s:make-validator Box s:any (s:in allowed)))`, `(new Box allowed)`, `(new Box (sorted-map "k" -1))`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			setup := `(set 'allowed (sorted-map "k" 1)) (set 'check ` + tc.constraint + `)`
			load := func() *lisp.LEnv {
				env := newSchemaForkEnv(t)
				if got := env.LoadString("schema.lisp", setup); got.Type == lisp.LError {
					t.Fatal(got)
				}
				return env
			}
			fork := func(env *lisp.LEnv) *lisp.LEnv {
				return mustForkEnv(t, env)
			}
			template := load()
			plan := schemaTemplate(t, template)
			first, sibling := forkSchemaTemplate(t, plan), forkSchemaTemplate(t, plan)
			arms := []struct {
				name string
				env  *lisp.LEnv
			}{
				{"cold-1", load()}, {"cold-2", load()},
				{"fork-1", first}, {"fork-2", sibling}, {"grandchild", fork(first)},
			}
			var coldError string
			for i, arm := range arms {
				t.Run(arm.name, func(t *testing.T) {
					if got := arm.env.LoadString("mutate.lisp", fmt.Sprintf(`(assoc! allowed "k" %d)`, i+2)); got.Type == lisp.LError {
						t.Fatal(got)
					}
					want := `()`
					if tc.name == "has-key" || tc.name == "may-have-key" {
						want = `"v"`
					}
					if got := arm.env.LoadString("accepted.lisp", `(s:validate check `+tc.input+`)`); got.String() != want {
						t.Fatalf("own mutated capture: got %v, want %s", got, want)
					}
					wantCondition := "failed-constraint"
					if tc.name == "array" || tc.name == "has-key" || tc.name == "may-have-key" {
						wantCondition = "wrong-type"
					}
					got := arm.env.LoadString("rejected.lisp", `(s:validate check `+tc.rejected+`)`)
					if got.Type != lisp.LError || got.Str != wantCondition {
						t.Fatalf("invalid value: got %v, want %s", got, wantCondition)
					}
					if i == 0 {
						coldError = got.String()
					} else if got.String() != coldError {
						t.Fatalf("rejection differs from cold VM: got %v, want %s", got, coldError)
					}
				})
			}
			for i, arm := range arms {
				if got := arm.env.LoadString("observe.lisp", `(get allowed "k")`); got.Type != lisp.LInt || got.Int != i+2 {
					t.Errorf("%s changed through another VM: %v", arm.name, got)
				}
			}
			if got := template.LoadString("observe.lisp", `(get allowed "k")`); got.Type != lisp.LInt || got.Int != 1 {
				t.Fatalf("template changed through a fork: %v", got)
			}
		})
	}
}
