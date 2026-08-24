// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"errors"
	"fmt"
	"io"

	"github.com/luthersystems/elps/internal/astraw/hook"
	"github.com/luthersystems/elps/parser/token"
)

// Program is an opaque sequence of parsed top-level expressions — the sealed
// form of a parser's []*LVal output.  Its purpose is boundary control: code
// outside this module can hold, cache, and evaluate a Program but can never
// reach the raw AST nodes inside it, so an embedder's parse cache cannot leak
// *LVal pointers between environments by construction.  The class of bug is
// eliminated at compile time (there is no accessor to misuse) and at zero
// runtime cost (Program is a slice header behind a struct; sealing copies
// nothing).
//
// Producers live where the parse happens so raw-slice custody never leaves
// this package: ReadProgram, ReadLocationProgram, and (*LEnv).ParseProgram.
// The consumer is (*LEnv).LoadProgram / LoadProgramContext, which evaluates
// the sealed expressions exactly as (*LEnv).Load evaluates a Reader's output.
// There is no exported exit: the in-kernel detach machinery (lisp/detach.go)
// can produce hermetic deep copies of the sealed expressions, but it stays
// unexported until a real embedder consumer materializes.  A reflection test
// (program_seal_test.go) guards the surface: no exported method may expose
// *LVal.
//
// Scope of the guarantee: Program seals the parse/cache boundary in both
// directions.  Outward, it stops raw AST nodes from ESCAPING to embedders
// (the compile-time seal above).  Inward, every constructor establishes the
// hermetic seal (lisp/seal.go) on the expressions it admits — reader output
// that is not already sealed throughout is privately copied and sealed, and
// output the seal cannot cover is rejected (see newProgram, issue #394) —
// so the sharing a parse cache does is always the sanctioned kind: sealed
// nodes are frozen storage under the seal's write protection, and evaluating
// one Program from many environments cannot corrupt it for the others.
// Concurrency is unchanged by any of this: a Runtime serves one goroutine,
// so concurrent evaluation still means one environment per goroutine, all
// of them free to share the sealed Program.
//
// The zero Program is valid, empty, and evaluates to nil.
type Program struct {
	exprs []*LVal
}

// Len returns the number of top-level expressions in the program.
func (p Program) Len() int { return len(p.exprs) }

// String returns a short debugging description.  It deliberately does not
// render the program's expressions.
func (p Program) String() string {
	return fmt.Sprintf("<program %d exprs>", len(p.exprs))
}

// detach returns hermetic deep copies of the program's expressions, sharing
// no memory with the sealed AST (see (*LVal).detach for exactly what is
// copied).  It is the escape hatch for tooling and transfer — code that
// genuinely needs AST nodes, e.g. to analyze or serialize them outside this
// module.  Evaluation does not need it: pass the Program itself to
// (*LEnv).LoadProgram.  Like (*LVal).detach it is unexported until a real
// consumer appears; re-exporting is additive and easy.
//
// Parser output contains only syntax types, so detach on a Program produced
// by ReadProgram, ReadLocationProgram, or ParseProgram always succeeds; the
// error mirrors (*LVal).detach's contract for completeness.
func (p Program) detach() ([]*LVal, error) {
	if len(p.exprs) == 0 {
		return nil, nil
	}
	// One detacher across all expressions so any aliasing between top-level
	// expressions is preserved in the copies.
	d := &detacher{seen: make(map[*LVal]*LVal)}
	out := make([]*LVal, len(p.exprs))
	for i, expr := range p.exprs {
		cp, err := d.detach(expr)
		if err != nil {
			return nil, prependPath(err, fmt.Sprintf("Expr[%d]", i))
		}
		out[i] = cp
	}
	return out, nil
}

// ReadProgram parses the contents of r using reader and seals the result as
// a Program.  The parsed expression slice never leaves this package: it goes
// from the reader's return value through newProgram's seal admission into
// the sealed Program.  Reader output that is not already hermetically sealed
// (a format-preserving parser, a caller-written Reader) is privately copied
// and sealed; output the seal cannot protect — reference types, function
// values — is rejected with an error.  See newProgram.
func ReadProgram(reader Reader, name string, r io.Reader) (Program, error) {
	if reader == nil {
		return Program{}, errors.New("nil reader")
	}
	exprs, err := reader.Read(name, r)
	if err != nil {
		return Program{}, err
	}
	return newProgram(exprs)
}

// ReadLocationProgram is ReadProgram for a LocationReader, assigning physical
// location loc to the parsed tokens.
func ReadLocationProgram(reader LocationReader, name, loc string, r io.Reader) (Program, error) {
	if reader == nil {
		return Program{}, errors.New("nil reader")
	}
	exprs, err := reader.ReadLocation(name, loc, r)
	if err != nil {
		return Program{}, err
	}
	return newProgram(exprs)
}

// ParseProgram parses the contents of r using env.Runtime.Reader and seals
// the result as a Program, without evaluating anything.  Like LoadLocation,
// it uses ReadLocation when the runtime's reader supports locations and
// falls back to Read (with loc as the stream name) otherwise.  An error is
// returned if env.Runtime.Reader has not been set.
func (env *LEnv) ParseProgram(name, loc string, r io.Reader) (Program, error) {
	if env.Runtime.Reader == nil {
		return Program{}, errors.New("no reader for environment runtime")
	}
	if reader, ok := env.Runtime.Reader.(LocationReader); ok {
		return ReadLocationProgram(reader, name, loc, r)
	}
	return ReadProgram(env.Runtime.Reader, loc, r)
}

// newProgram is the single admission point for every Program constructor:
// reader output becomes a Program only after the hermetic seal that makes
// Program's boundary guarantee true is actually established (issue #394).
//
// Program's premise is that a cached parse "cannot leak *LVal pointers
// between environments by construction", and before this check the premise
// held only when the Reader happened to seal.  The standard parser does
// (rdparser.ParseExpression calls SealAST on each completed top-level
// expression); a format-preserving parser deliberately does not (its Meta
// construction continues past the parser's seal point); and a
// caller-supplied Reader may do anything at all, including handing the same
// tree to every Read call.  Wrapping such output unchecked shared one
// unsealed, mutable tree among every environment that loaded the Program —
// the substrate#378 corruption class, with no diagnostic in a production
// build.
//
// The admission asks TextLoader's questions, in TextLoader's order, adapted
// to the different mechanism (TextLoader hands each load a private copy;
// Program hands every load the same sealed tree):
//
//   - checkLoaderExpr runs first, verbatim.  Reference types (bytes, map,
//     array, native) share mutable state through every copy AND every
//     evaluation — SealAST declines to mark them and Copy preserves their
//     reference semantics — so no admission can make them safe to share and
//     they are rejected with TextLoader's error.
//   - Output that is already sealed throughout is admitted as-is.  This is
//     the standard-parser fast path, and the sharing it takes is the
//     sanctioned kind: sealed nodes are frozen storage under the seal's write
//     protection, shared across runtimes by design (lisp/seal.go;
//     elpstest.RunBenchmark takes the same share).  The check is a deep
//     walk, not a root check: SealAST stops without descending at anything
//     it declines to mark, so a Reader can hand back a sealed root over
//     unsealed storage.  (RunBenchmark's root-only check is sound only
//     because it constructed the parser itself; a Program's Reader belongs
//     to the caller.)
//   - Anything else gets the private-copy-and-seal treatment: Copy severs
//     every alias the Reader may have retained (cells, locations, and
//     format metadata are all detached), and SealAST freezes the copy.
//     Sealing here is safe even for a format-preserving parse — the parser
//     skips SealAST because Meta construction continues past its per-
//     expression seal point, but Read has returned by now, so construction
//     is complete.
//   - A node that even the fresh copy cannot seal (a function value, say —
//     no parser produces one, but the Reader interface cannot promise
//     that) would reopen the same hole one Reader further out, so it is
//     rejected.  This is the one place the admission is deliberately
//     stricter than TextLoader: TextLoader's per-load copies mean its
//     cached tree is never itself shared between environments, but for
//     Program the seal is the only thing standing between environments, so
//     "cannot seal" has to mean "cannot admit".
func newProgram(exprs []*LVal) (Program, error) {
	for _, expr := range exprs {
		if err := checkLoaderExpr(expr); err != nil {
			lerr := Error(err)
			// Copied, not aliased: the error escapes to the caller through
			// GoError while expr remains the Reader's property, so the two
			// must not share a *token.Location (TextLoader's rule; cold
			// path, the copy is free in practice).
			lerr.source = copyLocation(expr.source)
			return Program{}, GoError(lerr)
		}
	}
	allSealed := true
	for _, expr := range exprs {
		if firstUnsealed(expr) != nil {
			allSealed = false
			break
		}
	}
	if allSealed {
		return Program{exprs: exprs}, nil
	}
	sealed := make([]*LVal, len(exprs))
	for i, expr := range exprs {
		cp := expr.Copy()
		cp.SealAST()
		if u := firstUnsealed(cp); u != nil {
			lerr := Error(fmt.Errorf("cannot seal expression of type %v into a program", u.Type))
			// Copied, not aliased, as above — u's location was already
			// privately copied from the Reader's tree, but the program copy
			// is discarded on this path while the error escapes, and the
			// rule is cheaper to follow than to prove unnecessary.
			lerr.source = copyLocation(u.source)
			return Program{}, GoError(lerr)
		}
		sealed[i] = cp
	}
	return Program{exprs: sealed}, nil
}

// firstUnsealed returns the first node reachable through v's Cells that is
// not sealed, or nil when the tree is sealed throughout.  The trees it
// walks were admitted by checkLoaderExpr, whose recursion already assumes
// finite, non-nil parser-shaped nodes; the same custody assumption applies
// here.
func firstUnsealed(v *LVal) *LVal {
	if !v.IsSealed() {
		return v
	}
	for _, c := range v.Cells {
		if u := firstUnsealed(c); u != nil {
			return u
		}
	}
	return nil
}

// LoadProgram evaluates the program's expressions as if in a progn, exactly
// as Load evaluates the expressions returned by env.Runtime.Reader.  The
// value of the last expression is returned.  After evaluation the current
// package is restored, in case the program made calls to "in-package".
//
// Deprecated: Use LoadProgramContext for cancellation and timeout support.
func (env *LEnv) LoadProgram(p Program) *LVal {
	return env.load(env.evalCtx, p.exprs)
}

// LoadProgramContext evaluates the program's expressions with the given
// context.  See LoadProgram.
func (env *LEnv) LoadProgramContext(ctx context.Context, p Program) *LVal {
	return env.load(ctx, p.exprs)
}

func init() {
	// Inject the zero-copy Program accessor for in-repo tooling.  The typed
	// surface lives in internal/astraw; the untyped slot in internal/astraw/
	// hook exists only to break the import cycle (astraw needs lisp's types,
	// so lisp cannot import astraw).  This is deliberately the ONLY way to
	// reach a Program's sealed expressions without copying, and internal/
	// visibility limits it to this module.
	hook.ProgramExprs = func(p Program) []*LVal { return p.exprs }

	// Inject the by-reference location accessor, for the same audience and
	// under the same internal/ visibility rule.  Source() hands out a value
	// copy so that nobody can write through a shared Location (issue #362);
	// astraw.SourceRef exists so in-repo checks can still ASK whether two
	// nodes share one, which is the invariant behind #426 and #446.  See its
	// doc comment.
	hook.SourceRef = func(v *LVal) *token.Location {
		if v == nil {
			return nil
		}
		return v.source
	}
}
