// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// partialSealReader seals its roots and nothing else — the shape SealAST
// itself produces for a tree containing a value it declines to mark.
// SealAST stops WITHOUT DESCENDING at anything that is not a
// parser-producible type, and checkLoaderExpr explicitly admits two such
// types into a cached loader (LFun and LTaggedVal).  So "the roots report
// IsSealed()" does not imply "the tree is frozen", and TextLoader's
// share-vs-copy decision has to ask the deep question.
type partialSealReader struct{ payload *lisp.LVal }

func (r *partialSealReader) Read(name string, _ io.Reader) ([]*lisp.LVal, error) {
	root := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("user-data"),
		lisp.Quote(r.payload),
	})
	root.SealAST()
	return []*lisp.LVal{root}, nil
}

func newPartialSealEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	if rc := lisp.InitializeUserEnv(env,
		lisp.WithReader(parser.NewReader()),
		lisp.WithLoader(lisplib.LoadLibrary),
	); rc.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	return env
}

// TestTextLoaderCopiesPartiallySealedAST is the regression test for the
// shallow seal gate: a root-sealed tree with unsealed storage underneath
// must take the per-load copy, so one environment's in-place mutation of the
// loaded value cannot be observed by another.
//
// Red-proof: with the gate asking only `expr.IsSealed()` of the roots, the
// loader hands both environments the SAME *LVal and environment A's
// stable-sort rewrites what environment B holds.
func TestTextLoaderCopiesPartiallySealedAST(t *testing.T) {
	seed := newPartialSealEnv(t)
	payload := seed.TaggedValue(lisp.Symbol("payload"),
		lisp.QExpr([]*lisp.LVal{lisp.Int(3), lisp.Int(1), lisp.Int(2)}))
	if payload.Type == lisp.LError {
		t.Fatalf("TaggedValue: %v", payload)
	}

	reader := &partialSealReader{payload: payload}
	roots, err := reader.Read("partial", strings.NewReader(""))
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if !roots[0].IsSealed() {
		t.Fatal("the reader's root is not sealed; this test would prove nothing")
	}
	if payload.IsSealed() {
		t.Fatal("SealAST sealed the tagged value; this test would prove nothing")
	}

	loader, err := lisp.TextLoader(reader, "partial", strings.NewReader(""))
	if err != nil {
		t.Fatalf("TextLoader: %v", err)
	}

	envA, envB := newPartialSealEnv(t), newPartialSealEnv(t)
	a := loader(envA)
	if a.Type == lisp.LError {
		t.Fatalf("load into environment A: %v", a)
	}
	b := loader(envB)
	if b.Type == lisp.LError {
		t.Fatalf("load into environment B: %v", b)
	}
	if a == b {
		t.Fatalf("both environments received the same *LVal (%p) from a tree that is"+
			" only root-sealed; the loader must copy what it cannot prove frozen", a)
	}

	before := b.String()
	sortFn := envA.GetGlobal(lisp.Symbol("stable-sort"))
	if sortFn.Type == lisp.LError {
		t.Fatalf("stable-sort: %v", sortFn)
	}
	lt := envA.GetGlobal(lisp.Symbol("<"))
	if rc := envA.FunCall(sortFn, lisp.SExpr([]*lisp.LVal{lt, a})); rc.Type == lisp.LError {
		t.Fatalf("stable-sort: %v", rc)
	}
	if after := b.String(); after != before {
		t.Errorf("environment A's in-place sort of the loaded value changed what"+
			" environment B holds: %s -> %s", before, after)
	}
}
