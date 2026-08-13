// Copyright © 2026 The ELPS authors

// The elpsseal analyzer is the third elpsvet rule: seal provenance for LVal
// values minted over BORROWED backing storage.
//
// The invariant.  A *lisp.LVal that is (or shares storage with) parser
// output carries the unexported `sealed` flag: the node may be aliased by
// every environment evaluating the same parse — substrate's parse cache
// shares one tree process-wide — so nothing may write it in place.  The flag
// travels with the STORAGE, not with the header.  Any code that mints a new
// LVal header over an existing LVal's cells (or byte) backing array
// therefore has to carry the constraint across, because the new header is
// a second, independent handle on the same memory:
//
//	r := QExpr(v.Cells[1:])   // fresh header, sealed == false by construction
//	r.sealed = v.sealed       // ... unless you say otherwise
//
// Drop the second line and a sealed program literal becomes reachable
// through a mutable value.  A single lisp-level write then corrupts the
// parse for every environment sharing it — silently, permanently, from pure
// lisp, with no Go code in sight.
//
// This has now shipped twice.
//
//   - #369 mechanism 2, in lisp/builtins.go builtinAppend: `cells :=
//     seqCells(seq)` followed by `Array(nil, append(cells, vals...))` wrote
//     into the spare capacity of a sealed list's backing array, so
//     (append 'vector (slice 'list '(1 2 3) 0 1) 99) overwrote the literal's
//     second element.  Fixed by a copy-on-write guard on seq.sealed.
//   - #392, in lisp/lisplib/libelpspath/path.go rangePath.Get: `cells =
//     cells[from:to]` followed by `toList(cells)` — a fresh unsealed header
//     over a possibly-sealed list's live backing.
//
// Both were found by a human or an agent reading code.  The kernel already
// HAS the discipline — builtinCdr, builtinRest and builtinSlice all
// propagate the flag explicitly — which is exactly why it needs a machine
// check: a hand-maintained invariant fails at the one producer who did not
// know about it.
//
// # The rule
//
// Flag a call that constructs a *lisp.LVal over a slice argument whose
// backing storage is borrowed from another LVal, unless the enclosing
// function is SEAL-AWARE with respect to that construction.
//
// "Constructs over a borrowed slice" has two halves, and both are computed
// rather than assumed:
//
//   - BORROWING CONSTRUCTOR.  The callee must be known to retain the caller's
//     slice as backing storage.  The kernel's allocation primitives
//     (lisp.SExpr, lisp.QExpr, lisp.Vector, lisp.Array, lisp.Bytes — the five
//     whose doc comments say "are not copied") are seeded by name, and
//     wrappers are INFERRED to fixpoint and exported as facts, so
//     libelpspath's `func toList(cells []*lisp.LVal) *lisp.LVal { return
//     lisp.SExpr(cells) }` is recognised as borrowing without being named
//     anywhere in this analyzer.  A callee that is not provably borrowing is
//     not flagged: see "What it cannot catch" below.
//
//   - BORROWED BACKING.  The argument must alias another LVal's backing
//     ARRAY.  This reuses the taint tracker built for #371 (alias.go) —
//     .Cells selections, seqCells/toCells results, .Bytes(), slice
//     expressions and slice conversions over any of those, tracked
//     transitively through local assignments.
//
// That second half is where the boundary sits, and it is the difference
// between a rule and a nuisance.  Sharing element POINTERS is normal and
// safe: the elements of a sealed list are themselves sealed, and a new list
// holding the same pointers cannot write through them.  Sharing the ARRAY is
// the bug, because a write through either header lands in the other's cells.
// So libelpspath's iterPath.Get —
//
//	var results []*lisp.LVal
//	for ... { results = append(results, in) }
//	newVal = toList(results)
//
// — is clean and stays clean: `results` grows from a nil base, so it is the
// function's own storage no matter where the elements came from.  Change it
// to `results := toCells(in)` and it flags.
//
// # Discharge
//
// Three ways out, matching what the tree actually does.
//
// PROPAGATE — the constructed value's `sealed` field is assigned anywhere in
// the function, or (*LVal).InheritSeal is called on it.  Position-insensitive,
// because the idiom writes it after the construction:
//
//	r := QExpr(v.Cells[1:])   // builtinCdr, builtinRest, inside package lisp
//	r.sealed = v.sealed
//
//	out := toList(cells)      // libelpspath, outside it
//	out.InheritSeal(in)
//
// CARRIED BY THE CALLEE — the borrowing constructor propagates the seal
// itself, so its call sites need no discharge at all.  libelpspath's #392 fix
// collapsed six hand-written borrow sites into one `alias(in, cells)` helper
// ending `out.InheritSeal(in)`; that property is inferred and travels as part
// of the borrowsFact, so factoring the discipline into a helper makes the
// diagnostics disappear instead of multiplying them.
//
// GUARD — the source LVal's seal is READ before the construction, so the code
// had the chance to copy:
//
//	if seq.sealed { fresh := make(...); copy(fresh, cells); cells = fresh }
//	return Array(nil, append(cells, vals...))     // builtinAppend
//
//	if in.IsSealed() { cells = clone(cells) }     // the out-of-package form,
//	return toList(cells)                          // where `sealed` is private
//
// The guard's BEFORE is load-bearing.  builtinSlice reads list.sealed near
// the end of the function, in its "vector" branch; a position-insensitive
// guard would let that one read excuse every earlier borrow in the same
// function, including the one whose propagation you just deleted.  Measured:
// with the guard order-insensitive, deleting builtinSlice's seal propagation
// produced NO diagnostic.  With it ordered, deleting any of the four kernel
// guards (builtinCdr, builtinRest, builtinSlice, builtinAppend) fires.
//
// This remains a DISCIPLINE check, not a proof.  It demands that the author
// demonstrably reasoned about this value's seal; it does not prove they got
// the branch conditions right.  A rule that tried to prove correctness would
// need full path-sensitive analysis and would pay for it in false positives,
// which is the failure mode that kills rules.  What this catches is the
// failure mode that actually shipped, twice: a producer who never thought
// about the seal at all.
//
// # Suppression
//
// //elps:unsealed on the flagged line, the line above it, or the enclosing
// function's doc comment.  It asserts THE BORROWED BACKING CAN NEVER BELONG
// TO A SEALED VALUE — that the source's dynamic type is outside SealAST's
// type switch, or that it is storage this code allocated by a route the
// tracker cannot see.  Same audit rules as //elps:mutates and
// //elpsvet:allow: every one carries a justification a reader can check.
//
// # What it cannot catch
//
//   - A borrowing constructor reached through an interface, a function value,
//     or a package outside the analysis (facts only flow along the import
//     graph elpsvet is given).
//   - A []*lisp.LVal PARAMETER as a taint source in the callee: the tracker
//     is intraprocedural on the taint side, so a helper that takes borrowed
//     cells and constructs from them is only flagged at its call site, and
//     only if it is itself inferred as borrowing.
//   - Storage laundered through a field of some other struct
//     (s.buf = v.Cells; ...; QExpr(s.buf)): alias.go follows composite-literal
//     capture only, one level.
//   - Whether the seal-awareness it demands is CORRECT.  See "Discharge".
//   - Loop-carried flow: the body of a for/range is walked once from the
//     pre-loop state, so taint created late in an iteration is not seen by
//     reads early in it.
//   - A source that is not a nameable local: `QExpr(someStruct.field.Cells)`
//     flags (there is no root to consult a seal on), which is the safe
//     direction, but the diagnostic cannot name the source.
package main

import (
	"go/ast"
	"go/token"
	"go/types"
	"sort"
	"strconv"
	"strings"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/types/typeutil"
)

var sealAnalyzer = &analysis.Analyzer{
	Name:      "elpsseal",
	Doc:       "flag lisp.LVal values constructed over backing storage borrowed from another LVal without carrying its sealed constraint (the seal-laundering class behind issues #369 and #392); suppress with //elps:unsealed",
	Run:       runSeal,
	FactTypes: []analysis.Fact{(*borrowsFact)(nil)},
}

const unsealedMarker = "elps:unsealed"

// sealFieldName is the LVal field that carries the constraint.  IsSealed is
// its exported reader — the only handle out-of-package code has.
const (
	sealFieldName   = "sealed"
	sealReaderName  = "IsSealed"
	sealInheritName = "InheritSeal"
)

// seedBorrowers are the kernel's LVal allocation primitives that retain the
// caller's slice as backing storage.  Their doc comments all say so
// ("Provided cells are used as backing storage ... and are not copied").
//
// Seeded by NAME rather than inferred, because inferring them means reading
// `&LVal{Cells: cells}` composite literals and `Native: &b` through a
// pointer-to-parameter — real dataflow for five functions that have not
// changed shape in the life of the package.  Wrappers ARE inferred (see
// inferBorrowers), which is where the churn actually is.  checkSeeds below
// fails loudly if one of these names ever stops existing, so the shortcut
// cannot rot silently.
//
// Bytes is included even though it is vacuous TODAY: SealAST's type switch
// marks only LSExpr/LQuote/LSymbol/LQSymbol/LString/LInt/LFloat, and
// (*LVal).Bytes panics on anything but LBytes, so a byte backing array can
// never belong to a sealed value.  It costs nothing (its one in-tree call
// site is seal-aware anyway) and it is already armed if LBytes ever becomes
// parser-producible.
var seedBorrowers = map[string]bool{
	"SExpr": true, "QExpr": true, "Vector": true, "Array": true, "Bytes": true,
}

// borrowsFact marks a function as a borrowing LVal constructor: calling it
// yields a *lisp.LVal whose backing storage IS the caller's slice argument at
// each recorded parameter index.
//
// Carries records that the function ALSO propagates the seal onto the header
// it returns — it calls (*LVal).InheritSeal or assigns .sealed on a value it
// returns.  Such a constructor discharges the rule at every call site,
// because the discipline has been moved inside it.  libelpspath's `alias(in,
// cells)` is the motivating case: the #392 fix collapsed six hand-written
// borrow sites into one helper that ends `out.InheritSeal(in)`, and a rule
// that could not see that would report all six and be deleted for it.
type borrowsFact struct {
	Params  []int
	Carries bool
}

func (*borrowsFact) AFact() {}

func (f *borrowsFact) String() string {
	ps := make([]string, len(f.Params))
	for i, p := range f.Params {
		ps[i] = strconv.Itoa(p)
	}
	out := "borrowsLValBacking(" + strings.Join(ps, ",") + ")"
	if f.Carries {
		out += "+carriesSeal"
	}
	return out
}

func runSeal(pass *analysis.Pass) (interface{}, error) {
	inferBorrowers(pass)
	for _, file := range pass.Files {
		ann := unsealedLines(pass.Fset, file)
		for _, decl := range file.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				if d.Body == nil || hasUnsealed(d.Doc) {
					continue
				}
				checkSealProvenance(pass, d.Type, d.Body, ann)
			case *ast.GenDecl:
				ast.Inspect(d, func(n ast.Node) bool {
					if lit, ok := n.(*ast.FuncLit); ok {
						checkSealProvenance(pass, lit.Type, lit.Body, ann)
						return false
					}
					return true
				})
			}
		}
	}
	return nil, nil
}

// ---------------------------------------------------------------------------
// Which callees borrow.
// ---------------------------------------------------------------------------

// inferBorrowers seeds the kernel primitives (when analysing package lisp)
// and then propagates the property to wrappers within this package, to
// fixpoint.  Facts on imported functions are already available from the
// driver, so a wrapper in libelpspath sees lisp.SExpr's fact on the first
// iteration.
func inferBorrowers(pass *analysis.Pass) {
	decls := funcDecls(pass)
	if pass.Pkg.Path() == lispPkgPath {
		seedBorrowFacts(pass, decls)
	}
	for changed := true; changed; {
		changed = false
		for fn, decl := range decls {
			if _, done := lookupBorrows(pass, fn); done {
				continue
			}
			params, carries := inferBorrowParams(pass, fn, decl)
			if len(params) > 0 {
				pass.ExportObjectFact(fn, &borrowsFact{Params: params, Carries: carries})
				changed = true
			}
		}
	}
}

func funcDecls(pass *analysis.Pass) map[*types.Func]*ast.FuncDecl {
	out := make(map[*types.Func]*ast.FuncDecl)
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			fd, ok := decl.(*ast.FuncDecl)
			if !ok || fd.Body == nil {
				continue
			}
			if fn, ok := pass.TypesInfo.Defs[fd.Name].(*types.Func); ok {
				out[fn] = fd
			}
		}
	}
	return out
}

// seedBorrowFacts exports the primitives' facts and fails loudly on drift.
func seedBorrowFacts(pass *analysis.Pass, decls map[*types.Func]*ast.FuncDecl) {
	found := make(map[string]bool)
	for fn := range decls {
		if fn.Pkg() == nil || !seedBorrowers[fn.Name()] {
			continue
		}
		sig, ok := fn.Type().(*types.Signature)
		if !ok || sig.Recv() != nil || !resultHasLValPtr(sig) {
			continue
		}
		params := sliceParamIndexes(sig)
		if len(params) == 0 {
			continue
		}
		found[fn.Name()] = true
		pass.ExportObjectFact(fn, &borrowsFact{Params: params})
	}
	checkSeeds(pass, found)
}

// checkSeeds reports a seeded constructor that no longer exists with the
// expected shape.  A silently stale seed would disarm the whole rule, which
// is the one failure mode a gate must never have.
func checkSeeds(pass *analysis.Pass, found map[string]bool) {
	var missing []string
	for name := range seedBorrowers {
		if !found[name] {
			missing = append(missing, name)
		}
	}
	if len(missing) == 0 || len(pass.Files) == 0 {
		return
	}
	sort.Strings(missing)
	pass.Reportf(pass.Files[0].Package,
		"elpsseal seed list is stale: %s no longer exist(s) in package lisp as a"+
			" slice-taking *LVal constructor; update seedBorrowers in"+
			" cmd/elpsvet/seal.go or the seal-laundering rule is disarmed",
		strings.Join(missing, ", "))
}

// inferBorrowParams reports the parameter indexes of fn whose slice backing
// reaches a borrowing constructor whose result fn returns, and whether fn
// carries the seal onto that result itself.
func inferBorrowParams(pass *analysis.Pass, fn *types.Func, decl *ast.FuncDecl) ([]int, bool) {
	sig, ok := fn.Type().(*types.Signature)
	if !ok || !resultHasLValPtr(sig) {
		return nil, false
	}
	candidates := sliceParamIndexes(sig)
	if len(candidates) == 0 {
		return nil, false
	}
	returned := returnedIdents(pass, decl.Body)
	var params []int
	for _, i := range candidates {
		p := sig.Params().At(i)
		if paramReachesBorrowingCall(pass, decl.Body, p, returned) {
			params = append(params, i)
		}
	}
	if len(params) == 0 {
		return nil, false
	}
	return params, carriesSeal(pass, decl.Body, returned)
}

// carriesSeal reports whether the function propagates a seal onto a value it
// returns — `out.InheritSeal(in)` or `out.sealed = in.sealed`.  Such a
// borrowing constructor has absorbed the discipline, so its call sites need
// no discharge of their own: that is the whole point of factoring the borrow
// into one helper, and the rule has to be able to see it or the refactor
// makes the diagnostics worse instead of better.
func carriesSeal(pass *analysis.Pass, body *ast.BlockStmt, returned map[types.Object]bool) bool {
	facts := collectSealFacts(pass, body)
	for obj := range returned {
		if facts.assigned[obj] {
			return true
		}
	}
	return false
}

// paramReachesBorrowingCall reports whether the body passes param (possibly
// re-sliced, converted or appended to) at a borrowing position of a borrowing
// constructor whose result leaves the function.
func paramReachesBorrowingCall(pass *analysis.Pass, body *ast.BlockStmt, param *types.Var, returned map[types.Object]bool) bool {
	// Statements whose call results escape: `return C(p)` directly, or
	// `v := C(p)` where v is returned.
	escapes := make(map[*ast.CallExpr]bool)
	ast.Inspect(body, func(n ast.Node) bool {
		switch s := n.(type) {
		case *ast.ReturnStmt:
			for _, r := range s.Results {
				markCalls(r, escapes)
			}
		case *ast.AssignStmt:
			for i, lhs := range s.Lhs {
				id, ok := ast.Unparen(lhs).(*ast.Ident)
				if !ok || !returned[pass.TypesInfo.ObjectOf(id)] {
					continue
				}
				if len(s.Rhs) == 1 {
					markCalls(s.Rhs[0], escapes)
				} else if i < len(s.Rhs) {
					markCalls(s.Rhs[i], escapes)
				}
			}
		}
		return true
	})
	hit := false
	ast.Inspect(body, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok || !escapes[call] {
			return true
		}
		bpos, _ := borrowPositions(pass, call)
		for _, pos := range bpos {
			if pos < len(call.Args) && sliceRootsAtParam(pass, call.Args[pos], param) {
				hit = true
			}
		}
		return true
	})
	return hit
}

// markCalls records the call expressions directly producing e's value.
func markCalls(e ast.Expr, out map[*ast.CallExpr]bool) {
	if call, ok := ast.Unparen(e).(*ast.CallExpr); ok {
		out[call] = true
	}
}

func returnedIdents(pass *analysis.Pass, body *ast.BlockStmt) map[types.Object]bool {
	out := make(map[types.Object]bool)
	ast.Inspect(body, func(n ast.Node) bool {
		ret, ok := n.(*ast.ReturnStmt)
		if !ok {
			return true
		}
		for _, r := range ret.Results {
			if id, ok := ast.Unparen(r).(*ast.Ident); ok {
				if obj := pass.TypesInfo.ObjectOf(id); obj != nil {
					out[obj] = true
				}
			}
		}
		return true
	})
	return out
}

// sliceRootsAtParam reports whether e is param, or a slice/conversion/append
// over it — the shapes that keep the parameter's backing array.
func sliceRootsAtParam(pass *analysis.Pass, e ast.Expr, param *types.Var) bool {
	e = ast.Unparen(e)
	switch x := e.(type) {
	case *ast.Ident:
		return pass.TypesInfo.ObjectOf(x) == param
	case *ast.SliceExpr:
		if zeroCapRebase(x) {
			return false
		}
		return sliceRootsAtParam(pass, x.X, param)
	case *ast.CallExpr:
		if id, ok := ast.Unparen(x.Fun).(*ast.Ident); ok && id.Name == "append" {
			if _, isBuiltin := pass.TypesInfo.Uses[id].(*types.Builtin); isBuiltin && len(x.Args) > 0 {
				return sliceRootsAtParam(pass, x.Args[0], param)
			}
		}
		if tv, ok := pass.TypesInfo.Types[x.Fun]; ok && tv.IsType() && len(x.Args) == 1 {
			if _, isSlice := tv.Type.Underlying().(*types.Slice); isSlice {
				return sliceRootsAtParam(pass, x.Args[0], param)
			}
		}
	}
	return false
}

// borrowPositions returns the argument positions of call that the callee
// retains as LVal backing storage, or nil when the callee is not a known
// borrowing constructor.
func borrowPositions(pass *analysis.Pass, call *ast.CallExpr) ([]int, bool) {
	fn, ok := typeutil.Callee(pass.TypesInfo, call).(*types.Func)
	if !ok {
		return nil, false
	}
	fact, ok := lookupBorrows(pass, fn)
	if !ok {
		return nil, false
	}
	return fact.Params, fact.Carries
}

func lookupBorrows(pass *analysis.Pass, fn *types.Func) (*borrowsFact, bool) {
	var f borrowsFact
	if pass.ImportObjectFact(fn, &f) {
		return &f, true
	}
	return nil, false
}

func sliceParamIndexes(sig *types.Signature) []int {
	var out []int
	for i := range sig.Params().Len() {
		if _, ok := sig.Params().At(i).Type().Underlying().(*types.Slice); ok {
			out = append(out, i)
		}
	}
	return out
}

func resultHasLValPtr(sig *types.Signature) bool {
	for i := range sig.Results().Len() {
		if isLValPtr(sig.Results().At(i).Type()) {
			return true
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// The check.
// ---------------------------------------------------------------------------

// sealChecker walks one function body, BRANCH-SCOPED.
//
// The freshness and taint state is snapshotted around every construct with
// mutually exclusive paths (if/else, switch, type switch, select) and merged
// afterwards in the safe direction — fresh only if fresh everywhere, tainted
// if tainted anywhere.  The two sibling rules in this package are
// path-insensitive and say so; this one cannot be, and the reason is
// concrete.  lisp/builtins.go builtinSlice reads
//
//	switch list.Type {
//	case LString: list = String(list.Str[i:j])   // marks `list` fresh
//	case LBytes:  list = Bytes(list.Bytes()[i:j])
//	default:      list = QExpr(seqCells(list)[i:j])   // the borrow
//	}
//
// Walked flat and in source order, the LString case marks `list` as storage
// this function constructed, and the default case — the one that actually
// borrows a sealed list's backing — is then invisible.  Deleting the seal
// guard from builtinSlice produced NO diagnostic before this change: the
// rule was silently vacuous over the exact function whose discipline it
// exists to protect.
type sealChecker struct {
	pass    *analysis.Pass
	aliases *aliasTracker
	facts   *sealFacts
	ann     map[int]bool
	// bound maps a construction call to the variable it is assigned to, so a
	// later `v.sealed = ...` can discharge it.
	bound map[*ast.CallExpr]types.Object
	// borrowed records the calls that turned out to mint a header over
	// FOREIGN backing.  Their result is not fresh storage however fresh the
	// header is -- the constraint travels with the array -- so a further
	// borrow from it is still a borrow.  Without this, builtinSlice's
	// `list = QExpr(seqCells(list)[i:j])` would launder `list` into a value
	// this function constructed and hide every later borrow from it.
	borrowed map[*ast.CallExpr]bool
}

func checkSealProvenance(pass *analysis.Pass, ftype *ast.FuncType, body *ast.BlockStmt, ann map[int]bool) {
	fresh := make(map[types.Object]bool)
	seedArglistFresh(pass, ftype, fresh)
	aliases := newAliasTracker(pass, fresh, nil)
	aliases.quiet = true // elpsfreshness owns the mutation diagnostics
	c := &sealChecker{
		pass:     pass,
		aliases:  aliases,
		facts:    collectSealFacts(pass, body),
		ann:      ann,
		bound:    make(map[*ast.CallExpr]types.Object),
		borrowed: make(map[*ast.CallExpr]bool),
	}
	c.stmt(body)
}

// branches walks each alternative from the same starting state and merges the
// results.  exhaustive says whether the alternatives cover every path; when
// they do not (an `if` with no `else`, a `switch` with no `default`) the
// fall-through state is one more alternative.
func (c *sealChecker) branches(exhaustive bool, walk ...func()) {
	pre := c.aliases.snapshot()
	outs := make([]aliasSnapshot, 0, len(walk)+1)
	for _, w := range walk {
		c.aliases.restore(pre)
		w()
		outs = append(outs, c.aliases.snapshot())
	}
	if !exhaustive {
		outs = append(outs, pre)
	}
	c.aliases.restore(mergeSnapshots(outs))
}

func (c *sealChecker) stmt(s ast.Stmt) {
	switch st := s.(type) {
	case nil, *ast.BranchStmt, *ast.EmptyStmt:
		return
	case *ast.BlockStmt:
		for _, x := range st.List {
			c.stmt(x)
		}
	case *ast.AssignStmt:
		// Order matters: the RHS is evaluated in the state BEFORE the
		// binding lands, or `list = QExpr(seqCells(list)[i:j])` would read
		// `list` as already freshened by its own right-hand side.
		c.record(st)
		for _, r := range st.Rhs {
			c.expr(r)
		}
		for _, l := range st.Lhs {
			if _, isIdent := ast.Unparen(l).(*ast.Ident); !isIdent {
				c.expr(l)
			}
		}
		trackFresh(c.pass, st, c.aliases.fresh)
		c.unfreshenBorrowed(st)
		c.aliases.handleAssign(st)
	case *ast.DeclStmt:
		gd, ok := st.Decl.(*ast.GenDecl)
		if !ok {
			return
		}
		for _, spec := range gd.Specs {
			vs, ok := spec.(*ast.ValueSpec)
			if !ok {
				continue
			}
			recordSpecBindings(c.pass, vs, c.bound)
			for _, v := range vs.Values {
				c.expr(v)
			}
			c.trackSpecFresh(vs)
			c.aliases.handleValueSpec(vs)
		}
	case *ast.ExprStmt:
		c.expr(st.X)
	case *ast.SendStmt:
		c.expr(st.Chan)
		c.expr(st.Value)
	case *ast.IncDecStmt:
		c.expr(st.X)
	case *ast.GoStmt:
		c.expr(st.Call)
	case *ast.DeferStmt:
		c.expr(st.Call)
	case *ast.ReturnStmt:
		for _, r := range st.Results {
			c.expr(r)
		}
	case *ast.LabeledStmt:
		c.stmt(st.Stmt)
	case *ast.IfStmt:
		c.stmt(st.Init)
		c.expr(st.Cond)
		if st.Else == nil {
			c.branches(false, func() { c.stmt(st.Body) })
			return
		}
		c.branches(true,
			func() { c.stmt(st.Body) },
			func() { c.stmt(st.Else) })
	case *ast.SwitchStmt:
		c.stmt(st.Init)
		c.expr(st.Tag)
		c.cases(st.Body)
	case *ast.TypeSwitchStmt:
		c.stmt(st.Init)
		c.stmt(st.Assign)
		c.cases(st.Body)
	case *ast.SelectStmt:
		c.comms(st.Body)
	case *ast.ForStmt:
		c.stmt(st.Init)
		c.expr(st.Cond)
		// The body may run zero times, so merge it against the pre-state.
		// Loop-carried flow (iteration N's taint reaching iteration 1's
		// reads) is not modelled — a documented residual, and strictly
		// tighter than the flat walk this replaced.
		c.branches(false, func() { c.stmt(st.Body); c.stmt(st.Post) })
	case *ast.RangeStmt:
		c.expr(st.X)
		c.branches(false, func() { c.stmt(st.Body) })
	case *ast.CaseClause:
		for _, x := range st.Body {
			c.stmt(x)
		}
	case *ast.CommClause:
		c.stmt(st.Comm)
		for _, x := range st.Body {
			c.stmt(x)
		}
	default:
		// Any statement shape not named above: walk its expressions without
		// scoping rather than skipping it.
		ast.Inspect(s, func(n ast.Node) bool {
			if e, ok := n.(ast.Expr); ok {
				c.expr(e)
				return false
			}
			return true
		})
	}
}

func (c *sealChecker) cases(body *ast.BlockStmt) {
	if body == nil {
		return
	}
	walks := make([]func(), 0, len(body.List))
	exhaustive := false
	for _, cl := range body.List {
		cc, ok := cl.(*ast.CaseClause)
		if !ok {
			continue
		}
		if cc.List == nil {
			exhaustive = true // default clause
		}
		walks = append(walks, func() { c.stmt(cc) })
	}
	c.branches(exhaustive, walks...)
}

func (c *sealChecker) comms(body *ast.BlockStmt) {
	if body == nil {
		return
	}
	walks := make([]func(), 0, len(body.List))
	for _, cl := range body.List {
		cc, ok := cl.(*ast.CommClause)
		if !ok {
			continue
		}
		walks = append(walks, func() { c.stmt(cc) })
	}
	c.branches(true, walks...) // select always takes exactly one clause
}

// expr visits an expression's calls in the current state, checking each
// construction.  Function literals are walked as statements so their bodies
// get the same branch scoping, with the enclosing function's state — the
// intraprocedural approximation the sibling rules also make.
func (c *sealChecker) expr(e ast.Expr) {
	if e == nil {
		return
	}
	ast.Inspect(e, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.CallExpr:
			c.check(x)
		case *ast.FuncLit:
			c.stmt(x.Body)
			return false
		}
		return true
	})
}

func (c *sealChecker) check(call *ast.CallExpr) {
	if checkConstruction(c.pass, c.aliases, call, c.bound[call], c.facts, c.ann) {
		c.borrowed[call] = true
	}
}

// unfreshenBorrowed undoes trackFresh for a variable bound to a construction
// over foreign backing.  lisp.QExpr IS a fresh-header constructor, which is
// why the freshness rule lists it; but a fresh header over someone else's
// array is not fresh STORAGE, and the seal is a property of the storage.
func (c *sealChecker) unfreshenBorrowed(stmt *ast.AssignStmt) {
	if len(stmt.Rhs) != len(stmt.Lhs) {
		return
	}
	for i, lhs := range stmt.Lhs {
		id, ok := ast.Unparen(lhs).(*ast.Ident)
		if !ok || id.Name == "_" {
			continue
		}
		call, ok := ast.Unparen(stmt.Rhs[i]).(*ast.CallExpr)
		if !ok || !c.borrowed[call] {
			continue
		}
		if obj := c.pass.TypesInfo.ObjectOf(id); obj != nil {
			c.aliases.fresh[obj] = false
		}
	}
}

func (c *sealChecker) record(stmt *ast.AssignStmt) {
	recordBindings(c.pass, stmt, c.bound)
}

// trackSpecFresh is trackFresh for `var x = ...` declarations.
func (c *sealChecker) trackSpecFresh(spec *ast.ValueSpec) {
	if len(spec.Values) != len(spec.Names) {
		for _, name := range spec.Names {
			if obj := c.pass.TypesInfo.Defs[name]; obj != nil {
				c.aliases.fresh[obj] = false
			}
		}
		return
	}
	for i, name := range spec.Names {
		obj := c.pass.TypesInfo.Defs[name]
		if obj == nil || name.Name == "_" {
			continue
		}
		c.aliases.fresh[obj] = isFreshExpr(c.pass, spec.Values[i], c.aliases.fresh)
	}
}

// seedArglistFresh marks the per-call argument list of a builtin-shaped
// function — `func(*lisp.LEnv, *lisp.LVal) *lisp.LVal`, the lisp.LBuiltin
// signature — as storage the function owns.
//
// This is THE narrowing that makes the rule usable, and it is a contract, not
// a guess.  Every builtin, macro and special operator is entered through
// LEnv.evalSExprCells, which builds the arglist with `newCells := make([]*LVal,
// 1, len(s.Cells))` and returns SExpr(newCells): fresh backing, allocated per
// call, never the program's.  The tree already depends on this in writing —
// several //elps:mutates justifications in lisp/op.go and lisp/env.go cite
// "evalSExprCells builds fresh backing per call" as their reason.  Without it
// the rule reports every `SExpr(args.Cells[1:])` in the kernel, ~20 hits, all
// wrong, and dies of noise on arrival.
//
// It is deliberately SHALLOW: only args.Cells itself is the function's own
// array.  args.Cells[i] is a caller's value — for a special operator it is a
// sealed subform straight out of the parse — so `SExpr(branch.Cells[1:])`
// where `branch := args.Cells[i]` still flags, which is correct and is where
// the interesting kernel findings are.
//
// The residual false NEGATIVE, recorded honestly: a helper that merely shares
// the builtin signature and is called directly with borrowed cells (lisp/op.go
// calls opProgn(env, SExpr(branch.Cells[1:])) that way) treats its own
// parameter as fresh.  The construction at the CALL site still flags, which is
// where the fix belongs; a second construction inside the callee would not.
func seedArglistFresh(pass *analysis.Pass, ftype *ast.FuncType, fresh map[types.Object]bool) {
	if ftype == nil || ftype.Params == nil || ftype.Results == nil {
		return
	}
	params := flattenFields(ftype.Params)
	results := flattenFields(ftype.Results)
	if len(params) != 2 || len(results) != 1 {
		return
	}
	if !isLEnvPtrExpr(pass, params[0].typ) || !isLValPtr(pass.TypesInfo.TypeOf(params[1].typ)) {
		return
	}
	if !isLValPtr(pass.TypesInfo.TypeOf(results[0].typ)) {
		return
	}
	if params[1].name == nil {
		return
	}
	if obj := pass.TypesInfo.Defs[params[1].name]; obj != nil {
		fresh[obj] = true
	}
}

type fieldSlot struct {
	name *ast.Ident
	typ  ast.Expr
}

// flattenFields expands a parameter/result list into one slot per name, so
// that `func(env *LEnv, args *LVal)` and `func(a, b *LVal)` both count
// correctly.
func flattenFields(fl *ast.FieldList) []fieldSlot {
	var out []fieldSlot
	for _, f := range fl.List {
		if len(f.Names) == 0 {
			out = append(out, fieldSlot{typ: f.Type})
			continue
		}
		for _, n := range f.Names {
			out = append(out, fieldSlot{name: n, typ: f.Type})
		}
	}
	return out
}

func isLEnvPtrExpr(pass *analysis.Pass, e ast.Expr) bool {
	t := pass.TypesInfo.TypeOf(e)
	if t == nil {
		return false
	}
	p, ok := types.Unalias(t).Underlying().(*types.Pointer)
	return ok && isLispNamed(p.Elem(), "LEnv")
}

// recordBindings maps a construction call to the variable it is assigned to,
// so that a later `v.sealed = ...` can discharge it.
func recordBindings(pass *analysis.Pass, stmt *ast.AssignStmt, bound map[*ast.CallExpr]types.Object) {
	if len(stmt.Rhs) != len(stmt.Lhs) {
		return
	}
	for i, lhs := range stmt.Lhs {
		id, ok := ast.Unparen(lhs).(*ast.Ident)
		if !ok || id.Name == "_" {
			continue
		}
		call, ok := ast.Unparen(stmt.Rhs[i]).(*ast.CallExpr)
		if !ok {
			continue
		}
		if obj := pass.TypesInfo.ObjectOf(id); obj != nil {
			bound[call] = obj
		}
	}
}

func recordSpecBindings(pass *analysis.Pass, spec *ast.ValueSpec, bound map[*ast.CallExpr]types.Object) {
	if len(spec.Values) != len(spec.Names) {
		return
	}
	for i, name := range spec.Names {
		if name.Name == "_" {
			continue
		}
		call, ok := ast.Unparen(spec.Values[i]).(*ast.CallExpr)
		if !ok {
			continue
		}
		if obj := pass.TypesInfo.Defs[name]; obj != nil {
			bound[call] = obj
		}
	}
}

// trackFresh maintains the freshness map without emitting the freshness
// rule's diagnostics — elpsseal needs to know which LVals this function
// constructed (aliasing your own storage is your own business) but must not
// double-report what elpsfreshness owns.
func trackFresh(pass *analysis.Pass, stmt *ast.AssignStmt, fresh map[types.Object]bool) {
	if len(stmt.Rhs) == 1 && len(stmt.Lhs) > 1 {
		rhsFresh := isFreshExpr(pass, stmt.Rhs[0], fresh)
		for _, lhs := range stmt.Lhs {
			bindFresh(pass, lhs, rhsFresh, fresh)
		}
		return
	}
	for i, lhs := range stmt.Lhs {
		rhsFresh := false
		if i < len(stmt.Rhs) {
			rhsFresh = isFreshExpr(pass, stmt.Rhs[i], fresh)
		}
		bindFresh(pass, lhs, rhsFresh, fresh)
	}
}

// checkConstruction reports call when it mints an LVal over borrowed backing
// and nothing in the function carries the seal across.  It reports whether
// the construction borrowed at all, so the caller can record that the result
// is a header over FOREIGN storage rather than storage this function owns.
func checkConstruction(pass *analysis.Pass, aliases *aliasTracker, call *ast.CallExpr, result types.Object, facts *sealFacts, ann map[int]bool) bool {
	positions, carries := borrowPositions(pass, call)
	if len(positions) == 0 {
		return false
	}
	if carries {
		// The callee propagates the seal itself.  Still a borrow (the result
		// is a header over foreign storage, so a further borrow from it is
		// one too), but not a defect.
		return true
	}
	for _, pos := range positions {
		if pos >= len(call.Args) {
			continue
		}
		arg := call.Args[pos]
		if !aliases.sliceTaint(arg) {
			continue
		}
		roots := aliases.sliceOrigin(arg)
		line := pass.Fset.Position(call.Pos()).Line
		switch {
		case ann[line] || ann[line-1]:
		case facts.propagates(result):
		case facts.guarded(roots, call.Pos()):
		default:
			pass.Reportf(call.Pos(),
				"%s constructs a lisp.LVal over backing storage borrowed from %s"+
					" without carrying its sealed constraint"+
					" (the seal-laundering class behind issues #369 and #392);"+
					" copy the backing, propagate the flag from the source,"+
					" or annotate //elps:unsealed with a justification",
				calleeName(pass, call), sourceName(pass, roots))
		}
		return true
	}
	return false
}

func calleeName(pass *analysis.Pass, call *ast.CallExpr) string {
	fn, ok := typeutil.Callee(pass.TypesInfo, call).(*types.Func)
	if !ok {
		return "call"
	}
	if fn.Pkg() != nil && fn.Pkg() != pass.Pkg {
		return fn.Pkg().Name() + "." + fn.Name()
	}
	return fn.Name()
}

func sourceName(pass *analysis.Pass, roots []types.Object) string {
	if len(roots) == 0 {
		return "another lisp.LVal"
	}
	names := make([]string, 0, len(roots))
	seen := make(map[string]bool)
	for _, r := range roots {
		if !seen[r.Name()] {
			seen[r.Name()] = true
			names = append(names, r.Name())
		}
	}
	sort.Strings(names)
	return "`" + strings.Join(names, "`/`") + "`"
}

// sealFacts records where a function touches the seal, split by the two
// discharges the kernel actually uses.
//
// PROPAGATE (assigned): `r.sealed = v.sealed` — the flag is written onto the
// new header.  Position-insensitive, because the idiom writes it AFTER the
// construction (builtinCdr, builtinRest, builtinSlice all do).
//
// GUARD (read): `if seq.sealed { ...copy... }` or `in.IsSealed()` — the
// source's seal is consulted and the code branches on it.  Position-SENSITIVE:
// a guard only protects what comes after it.  Without that ordering the rule
// goes vacuous on builtinSlice, whose "vector" branch reads list.sealed near
// the end of the function and would otherwise excuse every earlier borrow —
// including the one whose guard you just deleted.
type sealFacts struct {
	assigned map[types.Object]bool
	reads    map[types.Object][]token.Pos
}

func (f *sealFacts) propagates(obj types.Object) bool {
	return obj != nil && f.assigned[obj]
}

// guarded reports whether any of roots had its seal consulted before pos.
func (f *sealFacts) guarded(roots []types.Object, pos token.Pos) bool {
	for _, r := range roots {
		for _, p := range f.reads[r] {
			if p < pos {
				return true
			}
		}
	}
	return false
}

// collectSealFacts scans a function body for every mention of the seal: a
// `x.sealed` selection (only possible inside package lisp) or an
// `x.IsSealed()` call.  A pre-pass, because a propagation lands after the
// construction it discharges.
func collectSealFacts(pass *analysis.Pass, body *ast.BlockStmt) *sealFacts {
	f := &sealFacts{
		assigned: make(map[types.Object]bool),
		reads:    make(map[types.Object][]token.Pos),
	}
	// Selectors appearing as an assignment LHS are writes, not reads.
	written := make(map[*ast.SelectorExpr]bool)
	ast.Inspect(body, func(n ast.Node) bool {
		as, ok := n.(*ast.AssignStmt)
		if !ok {
			return true
		}
		for _, lhs := range as.Lhs {
			if sel, ok := ast.Unparen(lhs).(*ast.SelectorExpr); ok {
				written[sel] = true
			}
		}
		return true
	})
	ast.Inspect(body, func(n ast.Node) bool {
		sel, ok := n.(*ast.SelectorExpr)
		if !ok {
			return true
		}
		var roots []types.Object
		write := written[sel]
		switch sel.Sel.Name {
		case sealFieldName:
			if name, ok := lvalFieldSelection(pass, sel); !ok || name != sealFieldName {
				return true
			}
			roots = lvalRoot(pass, sel.X)
		case sealReaderName:
			fn, ok := pass.TypesInfo.Uses[sel.Sel].(*types.Func)
			if !ok || !recvIsLisp(fn, "LVal") {
				return true
			}
			roots = lvalRoot(pass, sel.X)
		case sealInheritName:
			return true // handled at the call node below
		default:
			return true
		}
		for _, obj := range roots {
			if write {
				f.assigned[obj] = true
			} else {
				f.reads[obj] = append(f.reads[obj], sel.Pos())
			}
		}
		return true
	})
	// dst.InheritSeal(src) IS the propagation, spelled for code outside
	// package lisp where `sealed` is unreachable: a write to dst's seal and a
	// read of src's.  Missing it would report every caller of the very helper
	// the #392 fix introduced.
	ast.Inspect(body, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		sel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
		if !ok || sel.Sel.Name != sealInheritName {
			return true
		}
		fn, ok := pass.TypesInfo.Uses[sel.Sel].(*types.Func)
		if !ok || !recvIsLisp(fn, "LVal") {
			return true
		}
		for _, obj := range lvalRoot(pass, sel.X) {
			f.assigned[obj] = true
		}
		for _, arg := range call.Args {
			for _, src := range lvalRoot(pass, arg) {
				f.reads[src] = append(f.reads[src], call.Pos())
			}
		}
		return true
	})
	return f
}

// ---------------------------------------------------------------------------
// Annotation.
// ---------------------------------------------------------------------------

func unsealedLines(fset *token.FileSet, file *ast.File) map[int]bool {
	lines := make(map[int]bool)
	for _, cg := range file.Comments {
		for _, c := range cg.List {
			if commentIsUnsealed(c.Text) {
				lines[fset.Position(c.Pos()).Line] = true
			}
		}
	}
	return lines
}

func hasUnsealed(cg *ast.CommentGroup) bool {
	if cg == nil {
		return false
	}
	for _, c := range cg.List {
		if commentIsUnsealed(c.Text) {
			return true
		}
	}
	return false
}

func commentIsUnsealed(text string) bool {
	text = strings.TrimPrefix(text, "//")
	text = strings.TrimPrefix(text, "/*")
	return strings.HasPrefix(strings.TrimSpace(text), unsealedMarker)
}
