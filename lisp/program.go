// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"errors"
	"fmt"
	"io"

	"github.com/luthersystems/elps/internal/astraw/hook"
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

// newProgram wraps a reader's output in a Program, taking responsibility for
// the seal the reader may not have applied.  Every Program constructor goes
// through here; nothing else may build a non-zero Program.
//
// Why the constructors must ask this at all (issue #394).  Program's promise
// is that an embedder's parse cache cannot leak *LVal pointers between
// environments.  Read literally that is a promise about SHARING, not a
// precondition on the caller: "by construction" names the mechanism (there
// is no accessor to misuse), not an obligation the caller has to discharge.
// Reader, however, is a public interface this package cannot constrain, and
// one supported reader in this very repo does not seal — rdparser skips
// SealAST in format-preserving mode (parser/rdparser/parser.go), which is
// deliberate and documented there.  An embedder who writes
//
//	env.Runtime.Reader = parser.NewReader(parser.WithFormatPreserving())
//
// and then caches a Program used to get an unsealed tree shared by every
// environment: the first environment's stable-sort rewrote the program
// literal's cells in place and every later environment read the mutated
// literal before evaluating anything of its own.
//
// Why repair rather than reject.  Rejecting an unsealed tree would be the
// stronger-sounding rule, but it is the wrong one here, for three reasons.
//
//   - It inverts the promise.  A type whose premise is "sharing this is safe
//     by construction" keeps that premise by MAKING it true, not by pushing
//     the obligation back onto a caller who has no way to discharge it: the
//     seal is unexported behaviour of somebody else's Reader.
//   - It would contradict the rest of the file family.  TextLoader, forty
//     lines away in loader.go, asks exactly this question and answers it with
//     a private copy plus SealAST.  env.LoadString and env.Load accept a
//     format-preserving runtime reader today and work.  Making only
//     ParseProgram fail on the same runtime reader would be an incoherent
//     surface, and it would break a working embedder configuration whose
//     seal-skipping is documented, supported behaviour.
//   - The predicate cannot support a rejection.  sealedThroughout is
//     deliberately CONSERVATIVE: it is bounded by sealFPMaxDepth and answers
//     "not sealed throughout" for a cyclic or pathologically deep tree that
//     may in fact be perfectly sealed.  A conservative predicate may trigger
//     a conservative REPAIR, whose only cost is a copy; wiring it to a hard
//     error would reject valid, already-safe input.
//
// What is rejected is the case repair cannot fix.  SealAST marks
// parser-producible shapes only, so a tree carrying a reference type
// (LBytes, LSortMap, LArray, LNative) cannot be sealed and cannot be shared:
// Copy preserves an array's backing storage and a native payload by design,
// so one copy at construction would still hand every environment the same
// mutable buffer.  checkLoaderExpr is the codified form of that question and
// is reused verbatim.  The check costs nothing on the normal path and can
// never fire there: a tree containing a reference type is never sealed
// throughout (SealAST refuses to mark those types), so it always reaches the
// repair path first.  No parser in this repo can produce one — TextLoader
// has applied the same rule to every lisplib load for years.
//
// Residual: checkLoaderExpr admits LFun and LTaggedVal, which SealAST also
// declines to descend into, so a Reader synthesising those would still share
// their storage.  That is TextLoader's existing cacheability contract, not
// something #394 changes; no parser produces either from source text.
func newProgram(exprs []*LVal) (Program, error) {
	sealed := true
	for _, expr := range exprs {
		if !sealedThroughout(expr, 0) {
			sealed = false
			break
		}
	}
	if sealed {
		// Already frozen by the reader: share as-is, which is the whole
		// point of the seal and costs nothing.
		return Program{exprs: exprs}, nil
	}
	for _, expr := range exprs {
		if err := checkLoaderExpr(expr); err != nil {
			return Program{}, fmt.Errorf("cannot seal program: %w", err)
		}
	}
	// The copy is what makes the seal ours to apply: the reader still holds
	// its tree (a format-preserving reader hands the same nodes to a
	// formatter, which goes on writing their metadata), so sealing in place
	// would freeze a value the caller owns.  Copy clears the sealed flag on
	// the fresh storage it creates, so SealAST must follow it, not precede
	// it.  This happens once per Program, not once per load.
	out := make([]*LVal, len(exprs))
	for i, expr := range exprs {
		cp := expr.Copy()
		cp.SealAST()
		out[i] = cp
	}
	return Program{exprs: out}, nil
}

// ReadProgram parses the contents of r using reader and seals the result as
// a Program.  The parsed expression slice never leaves this package: it goes
// directly from the reader's return value into the sealed Program.
//
// A reader that does not seal its own output (the format-preserving reader,
// or any implementation outside this module) gets a private, sealed copy —
// see newProgram for why the unsealed tree is repaired rather than refused.
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
}
