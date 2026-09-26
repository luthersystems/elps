// Copyright © 2026 The ELPS authors

package libelpspath_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/stdlib"
	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Sharing bombs (lisp/sharing.go) in elpspath documents.  Every elpspath
// builtin, even read-only ?, first validated the WHOLE document as a tree,
// and ?set/?del/?nil deep-copied the off-path part as a tree: a document
// holding a D-level sharing chain -- D steps and D two-cell lists to build
// -- cost 2^D work, and for the copies 2^D allocation, inside one step.

const sharingBombDepth = 40

// sharingBombDoc returns an environment in which doc is a map holding the
// scalar "a" and, under "big", a sharingBombDepth-level sharing chain x.
func sharingBombDoc(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	lisp.WithMaxSteps(1_000_000)(env)
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := stdlib.Load(env, false); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	setup := fmt.Sprintf(`(set 'x 1)
(dotimes (i %d) (set! x (list x x)))
(set 'doc (sorted-map "a" 1 "big" x "v" (vector x x)))
()`, sharingBombDepth)
	if rc := env.LoadString("setup.lisp", setup); rc.Type == lisp.LError {
		t.Fatalf("setup: %v", rc)
	}
	return env
}

// runBomb evaluates src under a 1s deadline (scaled for the race detector;
// the unfixed walks take hours) and the step budget, behind a watchdog that ends the test binary if it does not come back or allocates
// without bound.
func runBomb(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	var rc *lisp.LVal
	testdeadline.Watch(src, 20*time.Second, 1<<30, func() {
		ctx, cancel := context.WithTimeout(context.Background(), testdeadline.Scale(time.Second))
		defer cancel()
		rc = env.LoadStringContext(ctx, "probe.lisp", src)
	})
	if rc.Type == lisp.LError {
		t.Fatalf("%s: %v", src, rc)
	}
	return rc
}

// sharedChain checks that v is a depth-level chain of two-cell lists ending
// in 1 whose two cells are the same node at every level the copy finished
// after its memo switched on: the copy keeps the document's sharing.
func sharedChain(t *testing.T, v *lisp.LVal, depth int) {
	t.Helper()
	for v.Type == lisp.LQuote {
		v = v.Cells[0]
	}
	for i := range depth {
		if v.Type != lisp.LSExpr || len(v.Cells) != 2 {
			t.Fatalf("level %d: got %v with %d cells", i, v.Type, len(v.Cells))
		}
		if i < depth-13 && v.Cells[0] != v.Cells[1] {
			t.Fatalf("level %d: the copy unshared the document", i)
		}
		v = v.Cells[0]
	}
	if v.Type != lisp.LInt || v.Int != 1 {
		t.Fatalf("leaf: got %v", v)
	}
}

func TestSharingBombElpspathGet(t *testing.T) {
	env := sharingBombDoc(t)
	if rc := runBomb(t, env, `(elpspath:? doc "a")`); rc.Type != lisp.LInt || rc.Int != 1 {
		t.Fatalf("got %v, want 1", rc)
	}
}

func TestSharingBombElpspathCopies(t *testing.T) {
	for _, src := range []string{
		`(elpspath:?set doc "a" 2)`,
		`(elpspath:?del doc "a")`,
		`(elpspath:?nil doc "a")`,
	} {
		t.Run(src, func(t *testing.T) {
			env := sharingBombDoc(t)
			out := runBomb(t, env, src)
			big := out.MapGet(lisp.String("big"))
			sharedChain(t, big, sharingBombDepth)
			// The copy is a copy: it shares nothing with the document.
			x := env.LoadString("x.lisp", "x")
			if big == x || big.Cells[0] == x.Cells[0] {
				t.Fatal("the copy shares a container with the document")
			}
			// The vector's two cells are the same x, so their copies are
			// the same copy.
			v := out.MapGet(lisp.String("v"))
			if cells := v.Cells[1].Cells; cells[0] != cells[1] || cells[0] != big {
				t.Fatal("the copy did not keep the document's sharing across containers")
			}
		})
	}
}

// Mutating operations still validate the replacement value too.
func TestSharingBombElpspathSetBombValue(t *testing.T) {
	env := sharingBombDoc(t)
	runBomb(t, env, `(elpspath:?set (sorted-map "a" 1) "a" x)`)
}

// A wide list shared along many paths: both memos count cells, so they
// switch on after a few revisits of the wide list rather than thousands.
func TestSharingBombElpspathWideList(t *testing.T) {
	env := sharingBombDoc(t)
	setup := `(set 'w (make-sequence 0 100000)) (dotimes (i 40) (set! w (list w w))) (set 'wdoc (sorted-map "a" 1 "w" w)) ()`
	if rc := env.LoadString("setup.lisp", setup); rc.Type == lisp.LError {
		t.Fatalf("setup: %v", rc)
	}
	for _, src := range []string{`(elpspath:? wdoc "a")`, `(elpspath:?set wdoc "a" 2)`} {
		var rc *lisp.LVal
		testdeadline.Watch(src, 20*time.Second, 1<<30, func() { rc = env.LoadString("probe.lisp", src) })
		if rc.Type == lisp.LError {
			t.Fatalf("%s: %v", src, rc)
		}
	}
}

// One operation copies in many walks -- one per iterator element, one per
// level of a chained path -- and they share one memo, so a shared value is
// copied once per operation rather than up to the budget once per walk.
func TestSharingBombElpspathOperationSharesMemo(t *testing.T) {
	// Each probe checks something cheap about its result: the iterator's
	// length, or the value the chain set.
	for name, src := range map[string]string{
		// 4096 references to one bomb, each copied by the iterator.
		"iterator": `(set 'y 1) (dotimes (i 40) (set! y (vector y y)))
(set 'v (vector y)) (dotimes (i 12) (set! v (concat 'vector v v)))
(length (elpspath:?set v '* "k" 1))`,
		// The same, where the path succeeds on every element.
		"iterator-maps": `(set 'm (sorted-map "a" x "b" x "k" 0))
(set 'v (vector m)) (dotimes (i 12) (set! v (concat 'vector v v)))
(length (elpspath:?set v '* "k" 1))`,
		// A 300-step key path through nested maps, each holding the bomb as
		// a sibling that every level's copy must copy.
		"chain": `(set 'd (sorted-map "s" x))
(dotimes (i 300) (set! d (sorted-map "n" d "s" x)))
(set 'keys (map 'list (lambda (i) "n") (make-sequence 0 300)))
(set 'd2 (apply elpspath:?set (concat 'list (list d) keys (list 1))))
(apply elpspath:? (cons d2 keys))`,
	} {
		t.Run(name, func(t *testing.T) {
			env := sharingBombDoc(t)
			lisp.WithMaxAlloc(64 << 20)(env)
			var rc *lisp.LVal
			testdeadline.Watch(name, 20*time.Second, 1<<30, func() { rc = env.LoadString("probe.lisp", src) })
			want := int64(4096)
			if name == "chain" {
				want = 1
			}
			if rc.Type != lisp.LInt || int64(rc.Int) != want {
				t.Fatalf("%s: got %v, want %d", name, rc, want)
			}
		})
	}
}
