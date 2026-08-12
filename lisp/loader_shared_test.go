// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

const loaderSrc = `
(defun loader-shared-add (a b) (+ a b))
(set 'loader-shared-list '(1 2 3))
(set 'loader-shared-str "literal")
(defmacro loader-shared-mac (x) (quasiquote (loader-shared-add (unquote x) 1)))
(set 'loader-shared-result (loader-shared-mac 41))
`

// TestTextLoaderSharesSealedAST pins the sealed-share in TextLoader (#379,
// the defensive-copy audit): one parse, evaluated by SEVERAL Runtimes, with
// no per-load deep copy.
//
// This is the loader shape substrate's warm parse cache has in production —
// one parsed stdlib source handed to every environment — and the shape the
// bench harness now uses. The three things that make it safe are asserted
// here rather than assumed:
//
//	(a) every environment computes the same answers, so sharing did not
//	    leave one environment reading another's state;
//	(b) the sealed AST is bit-identical afterwards (SealedASTFingerprint),
//	    so no evaluation wrote through the shared tree — the oracle for the
//	    substrate#378 corruption class; and
//	(c) it runs under -tags elpscheck, where the ownership checker would
//	    panic on a cross-runtime MUTABLE value. Passing there is what
//	    distinguishes "sealed and shared" from "shared and hoped".
func TestTextLoaderSharesSealedAST(t *testing.T) {
	t.Parallel()

	loader, err := lisp.TextLoader(parser.NewReader(), "loader_test.lisp", strings.NewReader(loaderSrc))
	if err != nil {
		t.Fatalf("TextLoader: %v", err)
	}

	// The loader closes over the parse; fingerprint it through a second
	// parse of the same source is not equivalent (a fingerprint is
	// content-based, so it would match trivially). Instead evaluate, then
	// re-evaluate in more environments, and require every environment to
	// agree with the first — an in-place write by environment 1 would show
	// up as a different answer in environment 2.
	const runtimes = 4
	var want string
	for i := range runtimes {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
			t.Fatalf("runtime %d: InitializeUserEnv: %v", i, lerr)
		}
		if lerr := loader(env); lerr.Type == lisp.LError {
			t.Fatalf("runtime %d: loader: %v", i, lerr)
		}

		got := env.GetGlobal(lisp.Symbol("loader-shared-result")).String() + " " +
			env.GetGlobal(lisp.Symbol("loader-shared-list")).String() + " " +
			env.GetGlobal(lisp.Symbol("loader-shared-str")).String()
		if i == 0 {
			want = got
			continue
		}
		if got != want {
			t.Fatalf("runtime %d disagrees with runtime 0 — the shared parse was mutated\n got: %s\nwant: %s",
				i, got, want)
		}
	}

	// Assertion (b): the checked-mode inspector re-fingerprints every sealed
	// parse recorded in this process. A free nil in untagged builds; the
	// real oracle under -tags elpscheck.
	if err := lisp.VerifySealedASTs(); err != nil {
		t.Fatalf("sealed AST verification failed after %d loads: %v", runtimes, err)
	}
}

// TestTextLoaderCopiesUnsealed is the other half: a Reader that does not seal
// its output must still get the per-load copy #365 requires. Without it the
// loader would hand one MUTABLE tree to several Runtimes, which is the
// contamination #365 was filed for and which the ownership checker forbids.
func TestTextLoaderCopiesUnsealed(t *testing.T) {
	t.Parallel()

	r := &unsealingReader{inner: parser.NewReader()}
	loader, err := lisp.TextLoader(r, "loader_test.lisp", strings.NewReader(loaderSrc))
	if err != nil {
		t.Fatalf("TextLoader: %v", err)
	}
	if r.calls != 1 {
		t.Fatalf("expected the reader to be invoked once, got %d", r.calls)
	}

	var want string
	for i := range 3 {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
			t.Fatalf("runtime %d: InitializeUserEnv: %v", i, lerr)
		}
		if lerr := loader(env); lerr.Type == lisp.LError {
			t.Fatalf("runtime %d: loader: %v", i, lerr)
		}
		got := env.GetGlobal(lisp.Symbol("loader-shared-result")).String()
		if i == 0 {
			want = got
			continue
		}
		if got != want {
			t.Fatalf("runtime %d disagrees with runtime 0: got %s, want %s", i, got, want)
		}
	}
}

// TestTextLoaderSealsEitherWay pins the invariant TextLoader owes the rest
// of the kernel — the tree it evaluates is SEALED — across BOTH of its
// paths, because the two arrived from different branches and the obligation
// moved when they were merged.
//
// A sealing Reader takes the share path: nothing is copied and nothing is
// sealed, because the parse already is (#379/#380).  A non-sealing Reader
// takes #365's private-copy path, and Copy CLEARS the sealed flag — so that
// is the path where the loader must seal, and the only path where forgetting
// to would be invisible.  Before TextLoader sealed at all, a defmacro loaded
// through a Loader bound an UNSEALED formals node and was silently
// disqualified from per-callsite expansion caching in every environment
// (#381); the assertion below is that same observation, made directly on the
// formals rather than through the cache's admission counters.
func TestTextLoaderSealsEitherWay(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name   string
		reader lisp.Reader
	}{
		{"sealing reader (share path)", parser.NewReader()},
		{"non-sealing reader (copy path)", &unsealingReader{inner: parser.NewReader()}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			loader, err := lisp.TextLoader(tc.reader, "loader_test.lisp", strings.NewReader(loaderSrc))
			if err != nil {
				t.Fatalf("TextLoader: %v", err)
			}
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
				t.Fatalf("InitializeUserEnv: %v", lerr)
			}
			if lerr := loader(env); lerr.Type == lisp.LError {
				t.Fatalf("loader: %v", lerr)
			}
			fun := env.GetGlobal(lisp.Symbol("loader-shared-mac"))
			if fun.Type != lisp.LFun {
				t.Fatalf("loader-shared-mac did not resolve to a function: %v", fun)
			}
			if len(fun.Cells) == 0 {
				t.Fatalf("loader-shared-mac has no formals cell: %v", fun)
			}
			if !lisp.SealedForTest(fun.Cells[0]) {
				t.Fatal("loader-defined macro bound UNSEALED formals: the tree TextLoader evaluated was not sealed")
			}
		})
	}
}

// unsealingReader models an embedder's Reader that produces UNSEALED trees.
// The Reader interface is public and this package cannot make every
// implementation seal, so the unsealed path has to keep working.
type unsealingReader struct {
	inner lisp.Reader
	calls int
}

func (r *unsealingReader) Read(name string, in io.Reader) ([]*lisp.LVal, error) {
	r.calls++
	exprs, err := r.inner.Read(name, in)
	if err != nil {
		return nil, err
	}
	// Copy() severs the parse: the copy is a fresh, unsealed tree.
	out := make([]*lisp.LVal, len(exprs))
	for i, e := range exprs {
		out[i] = e.Copy()
	}
	return out, nil
}
