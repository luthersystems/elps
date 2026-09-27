// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"context"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// The Lisp-level half of the sharing-bomb regression net (lisp/sharing.go;
// the Go-level half is lisp's TestSharingBombEveryWalker, plus
// TestSharingBombLibraryGoAPIs below for the libraries' Go APIs).  x and y are
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
(set 'stars (map 'list (lambda (i) '*) (make-sequence 0 40)))
(defun star-path (&rest tail) (concat 'list (list doc "big") stars tail))
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
		// Iterators through every level enumerate all 2^40 paths; the
		// output really is that large, so no memo applies and the work is
		// charged in steps past an allowance instead (issue #722).
		`(apply elpspath:? (star-path))`,
		`(apply elpspath:?set (star-path 2))`,
		`(apply elpspath:?del (star-path))`,
		`(apply elpspath:?nil (star-path))`,
		`(apply elpspath:?set! (star-path 2))`,
		`(apply elpspath:?del! (star-path 0))`,
		`(apply elpspath:?nil! (star-path 0))`,
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

// The Go-API rows of the net: library conversions an embedder calls on a
// program's value, over the same 40-level sharing chain.  Each must come
// back rather than walk every path.
func TestSharingBombLibraryGoAPIs(t *testing.T) {
	bombOf := func() *lisp.LVal {
		v := lisp.Int(1)
		for range 40 {
			v = lisp.SExpr([]*lisp.LVal{v, v})
		}
		return v
	}
	s := libjson.DefaultSerializer()
	for _, row := range []struct {
		name string
		run  func(v *lisp.LVal)
	}{
		{"json Serializer.GoValue", func(v *lisp.LVal) { _ = s.GoValue(v, false) }},
		{"json Serializer.GoSlice", func(v *lisp.LVal) { _, _ = s.GoSlice(v, true) }},
		{"json Serializer.GoMap", func(v *lisp.LVal) {
			m := lisp.SortedMap()
			m.Map().Set(lisp.String("bomb"), v)
			_, _ = s.GoMap(m, false)
		}},
	} {
		t.Run(row.name, func(t *testing.T) {
			v := bombOf()
			testdeadline.Watch(row.name+" over a 40-level sharing bomb", 20*time.Second, 1<<30, func() { row.run(v) })
		})
	}
}
