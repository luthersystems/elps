// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestUsePackageKeepsCoreSetBang pins elps#903: the record field write must
// not be exported as set!, because (use-package 's) copies the exports of s
// over the caller's bindings, and the core set! would then fail.
func TestUsePackageKeepsCoreSetBang(t *testing.T) {
	env := newSchemaForkEnv(t)
	mustLoad(t, env, "use-s.lisp", `
(in-package 'use-s-test)
(use-package 's)
(set 'x 1)
(set! x 2)
(set 'acct (s:make-record "acct" (s:has-key "n" s:int) (s:method put (o v) (s:set-field! o 'n v))))
; s:sorted-map hides lisp:sorted-map after use-package.
(set 'a (acct (lisp:sorted-map "n" 1)))
(a 'put 5)
`)
	got := env.LoadString("check.lisp", `(list use-s-test:x (use-s-test:a 'n))`)
	if got.Type == lisp.LError {
		t.Fatal(got)
	}
	if got.String() != "'(2 5)" {
		t.Fatalf("got %v, want '(2 5)", got)
	}
}
