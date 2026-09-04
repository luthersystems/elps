// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Fork must reproduce the template's SLOT aliasing, not only its element
// sharing.
//
// `cdr` (and `rest`, and `slice`) return a VIEW: a second header over the
// first list's backing array, sharing its elements by construction.  A
// destructive in-place operation through one header -- stable-sort writes
// slots through lvalByFun.Swap -- is therefore visible through the other.
// That is a documented property of the language, and docs/fork.md promises
// a fork reproduces it ("aliasing and cycles preserved").
//
// It did not.  forker.val allocated a fresh backing array per LVal header,
// so the two headers that shared one array in the template came out of the
// fork over two arrays: the elements stayed shared (the value memo), the
// slots did not.  The program below then answered '(20 30) in the template
// and in a cold environment, and '(10 20) in a fork.
const forkCellAliasProgram = `
(set 'l (list 30 10 20))
(set 'tail (cdr l))
`

const forkCellAliasTx = `(stable-sort < l) tail`

// loadForkCellAliasTemplate builds an environment holding the program
// above.
func loadForkCellAliasTemplate(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
		t.Fatalf("init: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); !rc.IsNil() {
		t.Fatalf("in-package: %v", rc)
	}
	if rc := env.LoadString("cellalias.lisp", forkCellAliasProgram); rc.Type == lisp.LError {
		t.Fatalf("program: %v", rc)
	}
	return env
}

// runForkCellAliasTx sorts `l` in place and renders `tail`, the view that
// must see the sorted slots.
func runForkCellAliasTx(t *testing.T, env *lisp.LEnv) string {
	t.Helper()
	rc := env.LoadString("cellalias_tx.lisp", forkCellAliasTx)
	if rc.Type == lisp.LError {
		t.Fatalf("transaction: %v", rc)
	}
	return rc.String()
}

func TestForkPreservesCellSlotAliasing(t *testing.T) {
	// The reference answer, from an environment that was never forked.
	want := runForkCellAliasTx(t, loadForkCellAliasTemplate(t))
	if want != "'(20 30)" {
		t.Fatalf("cold environment answered %s, want '(20 30) -- the premise of this test is wrong", want)
	}

	template := loadForkCellAliasTemplate(t)
	fork, err := template.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	if got := runForkCellAliasTx(t, fork); got != want {
		t.Errorf("fork answered %s, want %s: the fork gave `l` and its cdr view separate backing arrays", got, want)
	}
	// The template still answers for itself, on its own untouched slots.
	if got := runForkCellAliasTx(t, template); got != want {
		t.Errorf("template answered %s after forking, want %s", got, want)
	}
}

// TestForkCellSlotAliasingSurvivesWalkOrder pins the property that made a
// map-keyed memo impossible: the fork walk reaches headers in Go map order,
// so the view may be walked BEFORE the list it views, or after, and the
// grouping has to be order-independent.  Ten forks of one template is a
// cheap way to sample both orders.
func TestForkCellSlotAliasingSurvivesWalkOrder(t *testing.T) {
	template := loadForkCellAliasTemplate(t)
	for i := range 10 {
		fork, err := template.Fork()
		if err != nil {
			t.Fatalf("fork %d: %v", i, err)
		}
		if got := runForkCellAliasTx(t, fork); got != "'(20 30)" {
			t.Fatalf("fork %d answered %s, want '(20 30)", i, got)
		}
	}
}

// TestForkCellSlotAliasingIsolatesFromTemplate is the other half: the slots
// a fork shares INTERNALLY must still be fork-owned.  A write through the
// fork's `l` must not reach the template's `tail`, and vice versa.
func TestForkCellSlotAliasingIsolatesFromTemplate(t *testing.T) {
	template := loadForkCellAliasTemplate(t)
	fork, err := template.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	if rc := fork.LoadString("mutate.lisp", `(stable-sort < l)`); rc.Type == lisp.LError {
		t.Fatalf("fork mutation: %v", rc)
	}
	// The fork's own view sees the sort.
	if got := fork.LoadString("read.lisp", `tail`).String(); got != "'(20 30)" {
		t.Errorf("fork's view = %s, want '(20 30)", got)
	}
	// The template's does not.
	if got := template.LoadString("read.lisp", `tail`).String(); got != "'(10 20)" {
		t.Errorf("template's view = %s after the fork sorted its own list, want '(10 20)", got)
	}
}
