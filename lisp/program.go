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
// Scope of the guarantee: Program seals the parse/cache boundary — it stops
// raw AST nodes from ESCAPING to embedders.  It does not, by itself, make one
// Program safe to evaluate in multiple runtimes concurrently: evaluation can
// still alias parts of the AST into runtime values through quote and macro
// paths inside eval.  Sealing those eval-side leak points is separate work
// (the exp-ast-leakpoints line); full hermetic sealing is the composition of
// the two — Program at the boundary, leak seals inside eval.  Until then,
// treat a shared Program like any shared AST: reuse within one runtime is
// fine, cross-runtime reuse has the aliasing caveats of issues #288/#362,
// and the in-kernel detach machinery is what a sanctioned hand-off of a
// private copy would use (unexported until a consumer appears).
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
// directly from the reader's return value into the sealed Program.
func ReadProgram(reader Reader, name string, r io.Reader) (Program, error) {
	if reader == nil {
		return Program{}, errors.New("nil reader")
	}
	exprs, err := reader.Read(name, r)
	if err != nil {
		return Program{}, err
	}
	return Program{exprs: exprs}, nil
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
	return Program{exprs: exprs}, nil
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
