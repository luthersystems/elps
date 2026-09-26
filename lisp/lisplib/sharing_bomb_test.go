// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"context"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
)

// The Lisp-level half of the sharing-bomb regression net (lisp/sharing.go;
// the Go-level half is lisp's TestSharingBombEveryWalker).  x and y are
// distinct 40-level sharing chains -- 40 steps and 40 two-cell lists each,
// 2^40 paths -- and every row is an operation a program can apply to one.
// Each must come back under a 1s deadline (scaled under -race; the unfixed
// walks take hours) and a step budget: with an
// answer, or with an ordinary error such as an allocation cap, but never by
// walking every path inside one step.
func TestSharingBombLibraries(t *testing.T) {
	setup := `
(set 'x 1) (set 'y 1)
(dotimes (i 40) (set! x (list x x)) (set! y (list y y)))
(set 'doc (sorted-map "a" 1 "big" x))
(set 'Q (car '(quote)))
(set 'QQ (car '(quasiquote)))
(defmacro embed () (list Q x))
()`
	for _, src := range []string{
		`(equal? x y)`,
		`(copy x)`,
		`(to-string x)`,
		`(format-string "{}" x)`,
		`(json:dump-string x)`,
		`(json:dump-bytes x)`,
		`(embed)`,
		`(macroexpand '(embed))`,
		`(eval (list QQ x))`,
		`(elpspath:? doc "a")`,
		`(elpspath:?set doc "a" 2)`,
		`(elpspath:?del doc "a")`,
		`(elpspath:?nil doc "a")`,
		`(s:deftype "shared" "any" (s:in x)) (s:validate shared y)`,
		`(sorted-map "k" x)`,
		`(vector x x)`,
	} {
		t.Run(src, func(t *testing.T) {
			env := newStdlibEnv(t, lisp.WithMaxSteps(1_000_000))
			if rc := env.LoadString("setup.lisp", setup); rc.Type == lisp.LError {
				t.Fatalf("setup: %v", rc)
			}
			var rc *lisp.LVal
			testdeadline.Watch(src, 20*time.Second, 1<<30, func() {
				ctx, cancel := context.WithTimeout(context.Background(), testdeadline.Scale(time.Second))
				defer cancel()
				rc = env.LoadStringContext(ctx, "probe.lisp", src)
			})
			if lisp.IsInternalPanic(rc) {
				t.Fatalf("internal panic: %v", rc)
			}
			t.Logf("%s -> %v", src, rc.Type)
		})
	}
}
