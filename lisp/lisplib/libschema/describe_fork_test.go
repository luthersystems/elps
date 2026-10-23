// Copyright © 2026 The ELPS authors

package libschema_test

import "testing"

// TestDescribeSurvivesFork checks that a validator a template holds still
// describes itself, and still validates, in a VM forked from the template:
// the definition rides in the validator's immutable marker payload.
func TestDescribeSurvivesFork(t *testing.T) {
	env := newSchemaForkEnv(t)
	mustLoad(t, env, "schema", `
(set 'line (s:make-validator "line" s:sorted-map
  (s:no-other-keys (s:has-key "sku" s:string) (s:has-key "qty" s:int))))
(set 'lines (s:make-validator "lines" s:sequence (s:of line)))
(set 'want (s:describe lines))`)
	vm, err := schemaTemplate(t, env).NewVM()
	if err != nil {
		t.Fatal(err)
	}
	res := vm.LoadString("fork", `
(assert (equal? want (s:describe lines)))
(s:validate lines (list (sorted-map "sku" "a" "qty" 1)))`)
	if !res.IsNil() {
		t.Fatalf("fork: %v", res)
	}
}
