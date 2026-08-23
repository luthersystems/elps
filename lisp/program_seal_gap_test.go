// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/astraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// This file covers issue #394: the Program constructors accepting reader
// output WITHOUT the seal admission, so a Program could hand one unsealed
// tree to every environment that loaded it.
//
// Program's doc comment promises that an embedder's parse cache "cannot
// leak *LVal pointers between environments by construction".  Before the
// fix that held only when the Reader happened to seal: the standard parser
// does, but parser.NewReader(parser.WithFormatPreserving()) deliberately
// does not (a supported public option), and a caller-written Reader may
// return anything, including the same tree on every Read.  One environment
// evaluating (stable-sort > literal) then rewrote the program literal for
// every other environment sharing the Program — the substrate#378 class,
// silent in a production build.
//
// Layers of coverage, mirroring libelpspath/path_seal_test.go:
//
//	(1) TestProgramFormatPreservingParseIsSealed — issue reproduction A:
//	    the one-line public-API misuse, one Program, three environments,
//	    each reporting the literal BEFORE it mutates anything.
//	(2) TestProgramCopiesAliasedReaderOutput — issue reproduction B: a
//	    Reader that retains (and re-serves) its output; the Program must
//	    own a private sealed copy.
//	(3) TestProgramSharesSealedParserOutput — the fast path pin: already-
//	    sealed parser output is admitted as-is, not copied, so the fix
//	    cannot silently change the parse cache's cost model.
//	(4) Rejection tests for output the seal cannot protect, mirroring
//	    (and one step past) TextLoader's checkLoaderExpr.
//
// The checked-mode half of the red proof lives in
// program_seal_gap_elpscheck_test.go: pre-fix, the cross-runtime evaluation
// below panicked a -tags elpscheck binary with "ownership violation: LVal
// used by two Runtimes".

// sealGapSrc is the reproduction program.  The let materializes the
// literal's head BEFORE the sort touches anything, so the returned value is
// an immutable snapshot of what this environment saw: 10 from a pristine
// literal, 30 from the wreckage a previous environment left behind.
const sealGapSrc = `(defun limits () '(10 20 30))
(let ([pre (car (limits))]) (stable-sort > (limits)) pre)
`

// sealGapWant is what every load must return: the pristine literal's head.
const sealGapWant = "10"

// programSealedThroughout walks every node of the program's expressions and
// returns the first unsealed one, or nil.  A root-only check would pass a
// sealed header over unsealed storage, which is one of the shapes the
// admission must catch.
func programSealedThroughout(p lisp.Program) *lisp.LVal {
	var walk func(v *lisp.LVal) *lisp.LVal
	walk = func(v *lisp.LVal) *lisp.LVal {
		if !v.IsSealed() {
			return v
		}
		for _, c := range v.Cells {
			if u := walk(c); u != nil {
				return u
			}
		}
		return nil
	}
	for _, expr := range astraw.Exprs(p) {
		if u := walk(expr); u != nil {
			return u
		}
	}
	return nil
}

// formatPreservingProgram parses src through the format-preserving reader —
// the supported public option whose documented behaviour is to skip the
// parser's seal — via ParseProgram, the constructor an embedder wiring
// that reader into a runtime actually hits.
func formatPreservingProgram(t *testing.T, src string) lisp.Program {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader(parser.WithFormatPreserving())
	p, err := env.ParseProgram("gap.lisp", "gap.lisp", strings.NewReader(src))
	if err != nil {
		t.Fatalf("ParseProgram: %v", err)
	}
	return p
}

// TestProgramFormatPreservingParseIsSealed is issue #394's reproduction A.
//
// One Program built through the format-preserving reader, three fresh
// environments.  Each environment snapshots the literal's head before
// sorting it; every snapshot must be the pristine one, and must agree with
// a freshly-parsed baseline.  On the unfixed tree env2 and env3 read env1's
// write before running anything of their own.
func TestProgramFormatPreservingParseIsSealed(t *testing.T) {
	p := formatPreservingProgram(t, sealGapSrc)

	if u := programSealedThroughout(p); u != nil {
		t.Errorf("a Program built from format-preserving reader output holds an unsealed %v node;"+
			" the constructors did not establish the seal", u.Type)
	}

	want := programTestEnv(t).LoadString("gap.lisp", sealGapSrc)
	if want.Type == lisp.LError {
		t.Fatalf("baseline load: %v", want)
	}
	if want.String() != sealGapWant {
		t.Fatalf("baseline = %v, want %s; the reproduction program no longer probes the literal", want, sealGapWant)
	}

	for i := range 3 {
		got := programTestEnv(t).LoadProgram(p)
		if got.Type == lisp.LError {
			t.Fatalf("environment %d: %v", i+1, got)
		}
		if got.String() != want.String() {
			t.Errorf("environment %d read another environment's write out of the shared Program:"+
				" got %v, want %v (the pristine literal)", i+1, got, want)
		}
	}
}

// aliasingReader is reproduction B's Reader: it retains its expression
// slice and hands the SAME slice to every Read call, the way a caller-side
// cache would.  Program must not admit those nodes by reference.
type aliasingReader struct {
	exprs []*lisp.LVal
	err   error
}

func (r *aliasingReader) Read(name string, _ io.Reader) ([]*lisp.LVal, error) {
	return r.exprs, r.err
}

// unsealedExprs parses src into an UNSEALED tree (via the format-preserving
// parser) for use as hostile reader output, with an anti-vacuity check that
// it really is unsealed — otherwise every assertion downstream is about the
// parser, not about the admission.
func unsealedExprs(t *testing.T, src string) []*lisp.LVal {
	t.Helper()
	exprs, err := parser.NewReader(parser.WithFormatPreserving()).Read("gap.lisp", strings.NewReader(src))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	for i, expr := range exprs {
		if expr.IsSealed() {
			t.Fatalf("anti-vacuity: format-preserving expr %d is sealed; the hostile-reader fixture is broken", i)
		}
	}
	return exprs
}

// TestProgramCopiesAliasedReaderOutput is issue #394's reproduction B, plus
// the ownership assertions that make "copied" checkable: two Programs built
// from one retained slice must not share nodes with the reader or with each
// other, and vandalizing the reader's tree after construction must not
// change what either Program evaluates to.
func TestProgramCopiesAliasedReaderOutput(t *testing.T) {
	reader := &aliasingReader{exprs: unsealedExprs(t, sealGapSrc)}

	p1, err := lisp.ReadProgram(reader, "gap.lisp", strings.NewReader(""))
	if err != nil {
		t.Fatalf("ReadProgram 1: %v", err)
	}
	p2, err := lisp.ReadProgram(reader, "gap.lisp", strings.NewReader(""))
	if err != nil {
		t.Fatalf("ReadProgram 2: %v", err)
	}

	for _, p := range []lisp.Program{p1, p2} {
		if u := programSealedThroughout(p); u != nil {
			t.Errorf("a Program built from unsealed reader output holds an unsealed %v node", u.Type)
		}
	}
	raw1, raw2 := astraw.Exprs(p1), astraw.Exprs(p2)
	for i := range reader.exprs {
		if raw1[i] == reader.exprs[i] || raw2[i] == reader.exprs[i] {
			t.Errorf("expr %d: the Program admitted the reader's retained node by reference", i)
		}
		if raw1[i] == raw2[i] {
			t.Errorf("expr %d: two Programs built from one retained slice share a node", i)
		}
	}

	// Vandalize every node the reader still holds.  A Program that copied
	// is unaffected; a Program that aliased now evaluates garbage.
	var vandalize func(v *lisp.LVal)
	vandalize = func(v *lisp.LVal) {
		v.Int = 999
		v.Str = "vandalized"
		for _, c := range v.Cells {
			vandalize(c)
		}
	}
	for _, expr := range reader.exprs {
		vandalize(expr)
	}

	for i, p := range []lisp.Program{p1, p2} {
		got := programTestEnv(t).LoadProgram(p)
		if got.Type == lisp.LError {
			t.Fatalf("program %d: %v", i+1, got)
		}
		if got.String() != sealGapWant {
			t.Errorf("program %d = %v, want %s: writes through the reader's retained tree reached the Program",
				i+1, got, sealGapWant)
		}
	}
}

// recordingReader wraps the standard sealing parser and records the slice
// it returned, so the test can compare the Program's nodes against the
// parser's by pointer.
type recordingReader struct {
	inner lisp.Reader
	last  []*lisp.LVal
}

func (r *recordingReader) Read(name string, stream io.Reader) ([]*lisp.LVal, error) {
	exprs, err := r.inner.Read(name, stream)
	r.last = exprs
	return exprs, err
}

// TestProgramSharesSealedParserOutput pins the fast path against the
// alternative fix that was considered and rejected (copying always).
// Already-sealed parser output is the sanctioned cross-environment share —
// sealed nodes are frozen storage under copy-on-write — and substrate's
// parse cache builds a Program per phylum on the transaction path.  If
// someone later swaps the admission for an unconditional copy, this fails
// and they have to say so.
func TestProgramSharesSealedParserOutput(t *testing.T) {
	reader := &recordingReader{inner: parser.NewReader()}
	p, err := lisp.ReadProgram(reader, "fast.lisp", strings.NewReader(sealGapSrc))
	if err != nil {
		t.Fatalf("ReadProgram: %v", err)
	}
	if len(reader.last) != p.Len() {
		t.Fatalf("recorded %d exprs, Program has %d", len(reader.last), p.Len())
	}
	for i, raw := range astraw.Exprs(p) {
		if raw != reader.last[i] {
			t.Errorf("expr %d: sealed parser output was copied on admission;"+
				" that is a deliberate change to the parse-cache cost model, not a refactor", i)
		}
	}
}

// TestProgramRejectsReferenceTypes mirrors TextLoader's checkLoaderExpr at
// the Program boundary: reference types share mutable state through every
// copy and every evaluation, the seal cannot mark them, so a Reader that
// emits one cannot be cached.  The nested case pins that the check walks
// the tree rather than glancing at roots.
func TestProgramRejectsReferenceTypes(t *testing.T) {
	for _, tc := range []struct {
		name string
		expr *lisp.LVal
	}{
		{"vector-root", lisp.Vector([]*lisp.LVal{lisp.Int(1)})},
		{"native-nested", lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), lisp.Native(t)})},
	} {
		t.Run(tc.name, func(t *testing.T) {
			reader := &aliasingReader{exprs: []*lisp.LVal{tc.expr}}
			_, err := lisp.ReadProgram(reader, "ref.lisp", strings.NewReader(""))
			if err == nil {
				t.Fatal("a reference type was admitted into a Program")
			}
			if !strings.Contains(err.Error(), "cannot cache reference type") {
				t.Errorf("error should use TextLoader's reference-type report; got: %v", err)
			}
		})
	}
}

// TestProgramRejectsUnsealableTypes covers the residue past TextLoader's
// denylist: a value type that SealAST declines to mark (a function value —
// no parser emits one, but the Reader interface cannot promise that) would
// ride into every environment unsealed, reopening exactly the hole this
// admission closes.  Program rejects it rather than admitting a tree its
// seal does not cover.
func TestProgramRejectsUnsealableTypes(t *testing.T) {
	fn := lisp.Fun("gap-test-fun", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Nil() })
	reader := &aliasingReader{exprs: []*lisp.LVal{fn}}
	_, err := lisp.ReadProgram(reader, "fun.lisp", strings.NewReader(""))
	if err == nil {
		t.Fatal("an unsealable function value was admitted into a Program")
	}
	if !strings.Contains(err.Error(), "cannot seal expression") {
		t.Errorf("error should report the unsealable expression; got: %v", err)
	}
}
