// Copyright © 2026 The ELPS authors

// Slice-alias tracking for the elpsfreshness analyzer (issue #371).
//
// The freshness rule in freshness.go flags writes whose LHS is syntactically
// rooted at lisp.LVal storage (v.Cells[i] = x).  Both #369 corruption bugs
// were invisible to it because the write was laundered through a local slice
// alias first:
//
//	cells := seqCells(list)     // aliases list's backing array
//	cells[i] = x                // mutates list, no LVal in the LHS
//
// aliasTracker closes that gap.  It tracks, per function body, which local
// slice variables alias LVal backing storage ("tainted") and flags the
// mutating operations on them:
//
//   - index assignment (cells[i] = x, cells[i]++);
//   - append with a tainted base (append(cells, ...) may write vals into
//     spare capacity of the shared backing array — #369 mechanism 2 — and
//     its result may remain a mutable window onto it);
//   - copy with a tainted destination (copy(cells, ...));
//   - the sort mutators (sort.Sort/Stable/Slice/SliceStable, slices.Sort/
//     SortFunc/SortStableFunc/Reverse) applied to a tainted slice, or to a
//     sort.Interface value built from one (the #369 mechanism 1 shape:
//     sort.Stable(&lvalByFun{cells: cells}) — composite literals capturing a
//     tainted slice mark their variable as a "carrier").
//
// Taint sources: a selection of a slice-typed lisp.LVal field (v.Cells), a
// slice-returning method on *lisp.LVal (v.Bytes()), and any call returning
// []*lisp.LVal that is fed an LVal-typed argument or an already-tainted
// slice (seqCells(v) and friends, in any package).  A source whose receiver
// or argument LVal is provably fresh in the same function does not taint —
// aliasing storage this function allocated is this function's business.
//
// Depth: taint is tracked TRANSITIVELY through local identifier assignments
// (a := v.Cells; b := a; b[i] = x flags at any depth), through plain var
// declarations (var a = v.Cells is a ValueSpec, not an AssignStmt), through
// slice type conversions (cellSlice(a) shares backing — the
// sort.Sort(mapEntriesByKey(buf)) shape from lisp/maps.go), and through slice
// expressions (b := a[1:3] shares backing, so it inherits taint).  Tracking
// is last-assignment-wins in source order and path-insensitive, exactly like
// the freshness map: reassigning the variable to provably fresh storage
// (make, append from nil, a []*LVal composite literal) clears its taint,
// even when the reassignment sits inside a conditional (the copy-on-write
// guards added for #369 read as freshening, deliberately).  The append
// rebase idiom append(x[:0:0], x...) is recognised as fresh: a zero-capacity
// base forces reallocation.
//
// Out of scope (documented limitations, consistent with the freshness rule's
// intraprocedural approximation): []*LVal function parameters are not taint
// sources in the callee; storing a tainted slice into a field of a
// pre-existing struct escapes tracking (only composite-literal capture is
// followed, one level, for the sort-carrier shape); a value-typed LVal
// variable is treated as a fresh root even though its Cells still alias
// shared backing (the freshness rule's existing convention).
//
// Escape hatch: //elps:mutates on the flagged line, the line above it, or
// the enclosing function's doc comment — same annotation, same audit rules
// as the field-write half of the analyzer.
package main

import (
	"go/ast"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/types/typeutil"
)

// sortMutators are the standard-library functions that reorder or rewrite a
// slice in place.  Values name the argument that must not be a tainted
// slice (or, for sort.Sort/Stable, a carrier of one).
var sortMutators = map[string]bool{
	"sort.Sort": true, "sort.Stable": true,
	"sort.Slice": true, "sort.SliceStable": true,
	"slices.Sort": true, "slices.SortFunc": true,
	"slices.SortStableFunc": true, "slices.Reverse": true,
}

// aliasTracker holds the per-function-body taint state.  It shares the
// freshness map with checkFreshness so that aliases of locally constructed
// LVals do not taint.
type aliasTracker struct {
	pass    *analysis.Pass
	fresh   map[types.Object]bool
	tainted map[types.Object]bool // slice vars aliasing LVal backing storage
	carrier map[types.Object]bool // vars whose composite literal captured a tainted slice
	covered map[*ast.CallExpr]bool
	ann     map[int]bool

	// quiet silences this tracker's own diagnostics.  The elpsseal rule
	// (seal.go) drives a second tracker purely for its taint state; the
	// mutation findings belong to elpsfreshness and must be reported once,
	// by it, with its annotation map.
	quiet bool

	// origin records, for each tainted slice variable, the LVal variables
	// whose backing storage it may alias.  The mutation rules in this file
	// do not need provenance — a write through a tainted alias is a write
	// whoever owns the storage — but the seal rule (seal.go) does: its
	// discharge condition is "the function consulted the seal of THIS
	// source", so it has to know which source.
	origin map[types.Object][]types.Object
}

func newAliasTracker(pass *analysis.Pass, fresh map[types.Object]bool, ann map[int]bool) *aliasTracker {
	return &aliasTracker{
		pass:    pass,
		fresh:   fresh,
		tainted: make(map[types.Object]bool),
		carrier: make(map[types.Object]bool),
		covered: make(map[*ast.CallExpr]bool),
		ann:     ann,
		origin:  make(map[types.Object][]types.Object),
	}
}

// aliasSnapshot is a copy of a tracker's whole state, for the branch-scoped
// walk in seal.go.  The mutation rules in this file are deliberately
// path-insensitive (see the header comment), so they never take one; the
// seal rule cannot afford to be, because path-insensitive FRESHNESS lets an
// assignment in one switch case ("case LString: list = String(...)") mark a
// variable fresh for a sibling case that never ran it — which silently
// disarmed the rule over builtinSlice, the very function whose discipline it
// exists to protect.
type aliasSnapshot struct {
	fresh   map[types.Object]bool
	tainted map[types.Object]bool
	carrier map[types.Object]bool
	origin  map[types.Object][]types.Object
}

func (t *aliasTracker) snapshot() aliasSnapshot {
	s := aliasSnapshot{
		fresh:   make(map[types.Object]bool, len(t.fresh)),
		tainted: make(map[types.Object]bool, len(t.tainted)),
		carrier: make(map[types.Object]bool, len(t.carrier)),
		origin:  make(map[types.Object][]types.Object, len(t.origin)),
	}
	for k, v := range t.fresh {
		s.fresh[k] = v
	}
	for k, v := range t.tainted {
		s.tainted[k] = v
	}
	for k, v := range t.carrier {
		s.carrier[k] = v
	}
	for k, v := range t.origin {
		s.origin[k] = append([]types.Object(nil), v...)
	}
	return s
}

// restore refills the tracker's existing maps in place, so callers holding a
// reference to t.fresh (the freshness map elpsseal shares) keep seeing it.
func (t *aliasTracker) restore(s aliasSnapshot) {
	clear(t.fresh)
	for k, v := range s.fresh {
		t.fresh[k] = v
	}
	clear(t.tainted)
	for k, v := range s.tainted {
		t.tainted[k] = v
	}
	clear(t.carrier)
	for k, v := range s.carrier {
		t.carrier[k] = v
	}
	clear(t.origin)
	for k, v := range s.origin {
		t.origin[k] = v
	}
}

// mergeSnapshots joins the states of mutually exclusive paths in the safe
// direction: a value is FRESH only if the function constructed it on every
// path, and TAINTED if it may alias foreign backing on any path.
func mergeSnapshots(paths []aliasSnapshot) aliasSnapshot {
	out := aliasSnapshot{
		fresh:   make(map[types.Object]bool),
		tainted: make(map[types.Object]bool),
		carrier: make(map[types.Object]bool),
		origin:  make(map[types.Object][]types.Object),
	}
	if len(paths) == 0 {
		return out
	}
	for obj, v := range paths[0].fresh {
		all := v
		for _, p := range paths[1:] {
			all = all && p.fresh[obj]
		}
		out.fresh[obj] = all
	}
	for _, p := range paths {
		for obj, v := range p.tainted {
			out.tainted[obj] = out.tainted[obj] || v
		}
		for obj, v := range p.carrier {
			out.carrier[obj] = out.carrier[obj] || v
		}
		for obj, roots := range p.origin {
			out.origin[obj] = appendUnique(out.origin[obj], roots)
		}
	}
	return out
}

func appendUnique(dst []types.Object, src []types.Object) []types.Object {
	for _, s := range src {
		found := false
		for _, d := range dst {
			if d == s {
				found = true
				break
			}
		}
		if !found {
			dst = append(dst, s)
		}
	}
	return dst
}

func (t *aliasTracker) report(pos token.Pos, op string) {
	if t.quiet {
		return
	}
	line := t.pass.Fset.Position(pos).Line
	if t.ann[line] || t.ann[line-1] {
		return
	}
	t.pass.Reportf(pos,
		"%s a local slice alias of lisp.LVal backing storage"+
			" (the alias-laundering gap behind issue #369);"+
			" give the slice fresh backing (make/copy) first,"+
			" or annotate //elps:mutates with a justification", op)
}

// handleAssign tracks taint through identifier assignments and flags index
// writes whose base is a tainted slice.  It also marks the self-append grow
// idiom (x.Cells = append(x.Cells, ...)) as covered: the field-write half of
// the analyzer already polices that LHS, and a second diagnostic on the same
// line would double-count the census.
func (t *aliasTracker) handleAssign(stmt *ast.AssignStmt) {
	single := len(stmt.Rhs) == 1
	var rhsTaint, rhsCarrier bool
	if single {
		rhsTaint = t.sliceTaint(stmt.Rhs[0])
		rhsCarrier = t.carrierTaint(stmt.Rhs[0])
	}
	flagged := false
	for i, lhs := range stmt.Lhs {
		lhs = ast.Unparen(lhs)
		taint, carry := rhsTaint, rhsCarrier
		if !single {
			taint, carry = false, false
			if i < len(stmt.Rhs) {
				taint = t.sliceTaint(stmt.Rhs[i])
				carry = t.carrierTaint(stmt.Rhs[i])
			}
		}
		if single && i < len(stmt.Lhs) {
			t.coverSelfAppend(lhs, stmt.Rhs[0])
		} else if !single && i < len(stmt.Rhs) {
			t.coverSelfAppend(lhs, stmt.Rhs[i])
		}
		if id, ok := lhs.(*ast.Ident); ok {
			if id.Name == "_" {
				continue
			}
			obj := t.pass.TypesInfo.Defs[id]
			if obj == nil {
				obj = t.pass.TypesInfo.Uses[id]
			}
			if obj == nil {
				continue
			}
			if _, isSlice := obj.Type().Underlying().(*types.Slice); isSlice {
				t.tainted[obj] = taint
				t.recordOrigin(obj, taint, stmt, i, single)
			} else {
				t.carrier[obj] = carry
			}
			continue
		}
		if !flagged && t.isAliasIndexWrite(lhs) {
			t.report(lhs.Pos(), "index write through")
			flagged = true
		}
	}
}

// handleValueSpec tracks taint through plain var declarations
// (var cells = list.Cells), which reach the walker as ValueSpecs rather
// than AssignStmts.  Only the one-value-per-name form can name a slice
// alias; tuple declarations (var a, b = f()) cannot, because a
// multi-result function is not a taint source (resultIsLValSlice requires
// a single result).
func (t *aliasTracker) handleValueSpec(spec *ast.ValueSpec) {
	if len(spec.Values) != len(spec.Names) {
		return
	}
	for i, name := range spec.Names {
		if name.Name == "_" {
			continue
		}
		obj := t.pass.TypesInfo.Defs[name]
		if obj == nil {
			continue
		}
		if _, isSlice := obj.Type().Underlying().(*types.Slice); isSlice {
			taint := t.sliceTaint(spec.Values[i])
			t.tainted[obj] = taint
			if taint {
				t.origin[obj] = t.sliceOrigin(spec.Values[i])
			} else {
				delete(t.origin, obj)
			}
		} else {
			t.carrier[obj] = t.carrierTaint(spec.Values[i])
		}
	}
}

// recordOrigin attaches (or clears) the provenance of a slice variable that
// handleAssign has just re-bound.  Reassignment to fresh storage clears it,
// exactly as it clears the taint bit — last-assignment-wins, in source order.
func (t *aliasTracker) recordOrigin(obj types.Object, taint bool, stmt *ast.AssignStmt, i int, single bool) {
	if !taint {
		delete(t.origin, obj)
		return
	}
	rhs := stmt.Rhs[0]
	if !single {
		if i >= len(stmt.Rhs) {
			delete(t.origin, obj)
			return
		}
		rhs = stmt.Rhs[i]
	}
	t.origin[obj] = t.sliceOrigin(rhs)
}

// handleIncDec flags cells[i]++ / b[i]-- through a tainted alias.
func (t *aliasTracker) handleIncDec(stmt *ast.IncDecStmt) {
	if x := ast.Unparen(stmt.X); t.isAliasIndexWrite(x) {
		t.report(x.Pos(), "index write through")
	}
}

// isAliasIndexWrite reports an index write whose base is a tainted slice.
// A write the field-write half already classifies (v.Cells[0] = x,
// v.Bytes()[0] = 1) is NOT an alias write — checkWrite owns it, and a second
// diagnostic would double-count the same line.
func (t *aliasTracker) isAliasIndexWrite(lhs ast.Expr) bool {
	if kind, _ := classifyLValWrite(t.pass, lhs); kind != "" {
		return false
	}
	idx, ok := lhs.(*ast.IndexExpr)
	return ok && t.sliceTaint(idx.X)
}

// coverSelfAppend marks rhs as covered when it is append(X, ...) and lhs is
// the same X written back — the canonical grow idiom, whose LHS the
// field-write half of the analyzer already classifies.
func (t *aliasTracker) coverSelfAppend(lhs, rhs ast.Expr) {
	call, ok := ast.Unparen(rhs).(*ast.CallExpr)
	if !ok || !t.isBuiltin(call, "append") || len(call.Args) == 0 {
		return
	}
	if _, isIdent := lhs.(*ast.Ident); isIdent {
		return // ident LHS has no field-write diagnostic; do not cover
	}
	if t.sameChain(lhs, call.Args[0]) {
		t.covered[call] = true
	}
}

// checkCall flags the mutating calls: append with a tainted base, copy with
// a tainted destination, and the sort mutators over a tainted slice or a
// carrier of one.
func (t *aliasTracker) checkCall(call *ast.CallExpr) {
	if t.isBuiltin(call, "append") {
		if !t.covered[call] && len(call.Args) > 0 && t.sliceTaint(call.Args[0]) {
			t.report(call.Pos(), "append into")
		}
		return
	}
	if t.isBuiltin(call, "copy") {
		if len(call.Args) == 2 && t.sliceTaint(call.Args[0]) {
			t.report(call.Pos(), "copy into")
		}
		return
	}
	fn, ok := typeutil.Callee(t.pass.TypesInfo, call).(*types.Func)
	if !ok || fn.Pkg() == nil || len(call.Args) == 0 {
		return
	}
	name := fn.Pkg().Path() + "." + fn.Name()
	if !sortMutators[name] {
		return
	}
	if t.sliceTaint(call.Args[0]) || t.carrierTaint(call.Args[0]) {
		t.report(call.Pos(), name+" over")
	}
}

func (t *aliasTracker) isBuiltin(call *ast.CallExpr, name string) bool {
	id, ok := ast.Unparen(call.Fun).(*ast.Ident)
	if !ok || id.Name != name {
		return false
	}
	_, ok = t.pass.TypesInfo.Uses[id].(*types.Builtin)
	return ok
}

// sliceTaint reports whether e evaluates to a slice that may alias the
// backing storage of a lisp.LVal this function did not construct.
func (t *aliasTracker) sliceTaint(e ast.Expr) bool {
	e = ast.Unparen(e)
	switch x := e.(type) {
	case *ast.Ident:
		obj := t.pass.TypesInfo.Uses[x]
		if obj == nil {
			obj = t.pass.TypesInfo.Defs[x]
		}
		return obj != nil && t.tainted[obj]
	case *ast.SelectorExpr:
		// v.Cells (any slice-typed LVal field): tainted unless v is fresh.
		if _, ok := lvalFieldSelection(t.pass, x); ok {
			if _, isSlice := t.pass.TypesInfo.TypeOf(x).Underlying().(*types.Slice); isSlice {
				return !rootIsFresh(t.pass, x.X, t.fresh)
			}
		}
		return false
	case *ast.SliceExpr:
		if zeroCapRebase(x) {
			return false // append(x[:0:0], ...) must reallocate
		}
		return t.sliceTaint(x.X)
	case *ast.CallExpr:
		return t.callTaint(x)
	}
	return false
}

// callTaint decides taint for a call expression appearing where a slice is
// used: append propagates its base's taint; a slice-returning method on
// *lisp.LVal aliases its receiver; any function returning []*lisp.LVal fed
// an LVal (or an already-tainted slice) is assumed to return an alias.
func (t *aliasTracker) callTaint(call *ast.CallExpr) bool {
	if t.isBuiltin(call, "append") {
		return len(call.Args) > 0 && t.sliceTaint(call.Args[0])
	}
	// A slice-to-slice type conversion (cellSlice(cells), []*lisp.LVal(cs))
	// shares the operand's backing array: it inherits taint rather than
	// laundering it.  Conversions to non-slice types (string(b)) copy.
	if tv, ok := t.pass.TypesInfo.Types[call.Fun]; ok && tv.IsType() && len(call.Args) == 1 {
		if _, isSlice := tv.Type.Underlying().(*types.Slice); isSlice {
			return t.sliceTaint(call.Args[0])
		}
		return false
	}
	fn, ok := typeutil.Callee(t.pass.TypesInfo, call).(*types.Func)
	if !ok {
		return false
	}
	sig, ok := fn.Type().(*types.Signature)
	if !ok {
		return false
	}
	if fn := methodCallee(t.pass, call); fn != nil && recvIsLisp(fn, "LVal") {
		// v.Bytes() and any future slice-returning LVal method: the result
		// aliases the receiver's storage.  Fresh receiver, fresh alias.
		if !resultIsSlice(sig) {
			return false
		}
		if sel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr); ok {
			return !rootIsFresh(t.pass, sel.X, t.fresh)
		}
		return true
	}
	// Plain function (or non-LVal method) returning []*lisp.LVal: tainted
	// when fed an LVal it does not own or a tainted slice (seqCells shape).
	if !resultIsLValSlice(sig) {
		return false
	}
	for _, arg := range call.Args {
		at := t.pass.TypesInfo.TypeOf(arg)
		if isLValValue(at) || isLValPtr(at) {
			if !isFreshExpr(t.pass, arg, t.fresh) && !rootIsFresh(t.pass, arg, t.fresh) {
				return true
			}
			continue
		}
		if t.sliceTaint(arg) {
			return true
		}
	}
	return false
}

// carrierTaint reports whether e is (or holds) a value whose composite
// literal captured a tainted slice — the sort.Stable(&lvalByFun{cells:
// cells}) shape.
func (t *aliasTracker) carrierTaint(e ast.Expr) bool {
	e = ast.Unparen(e)
	switch x := e.(type) {
	case *ast.Ident:
		obj := t.pass.TypesInfo.Uses[x]
		if obj == nil {
			obj = t.pass.TypesInfo.Defs[x]
		}
		return obj != nil && t.carrier[obj]
	case *ast.UnaryExpr:
		return x.Op == token.AND && t.carrierTaint(x.X)
	case *ast.CompositeLit:
		for _, elt := range x.Elts {
			if kv, ok := elt.(*ast.KeyValueExpr); ok {
				elt = kv.Value
			}
			if t.sliceTaint(elt) {
				return true
			}
		}
	}
	return false
}

// sameChain reports whether a and b are the same identifier/selector/index
// chain (v.Cells[1].Cells vs v.Cells[1].Cells) — used only to recognise the
// self-append idiom, so index expressions match on identical identifiers or
// integer literals.
func (t *aliasTracker) sameChain(a, b ast.Expr) bool {
	a, b = ast.Unparen(a), ast.Unparen(b)
	switch ax := a.(type) {
	case *ast.Ident:
		bx, ok := b.(*ast.Ident)
		if !ok {
			return false
		}
		ao := t.pass.TypesInfo.ObjectOf(ax)
		return ao != nil && ao == t.pass.TypesInfo.ObjectOf(bx)
	case *ast.BasicLit:
		bx, ok := b.(*ast.BasicLit)
		return ok && ax.Kind == bx.Kind && ax.Value == bx.Value
	case *ast.SelectorExpr:
		bx, ok := b.(*ast.SelectorExpr)
		return ok && ax.Sel.Name == bx.Sel.Name && t.sameChain(ax.X, bx.X)
	case *ast.IndexExpr:
		bx, ok := b.(*ast.IndexExpr)
		return ok && t.sameChain(ax.X, bx.X) && t.sameChain(ax.Index, bx.Index)
	}
	return false
}

// zeroCapRebase recognises x[:0:0] (and x[0:0:0]): a zero-capacity rebase
// whose append always reallocates.
func zeroCapRebase(s *ast.SliceExpr) bool {
	if !s.Slice3 {
		return false
	}
	zero := func(e ast.Expr) bool {
		if e == nil {
			return false
		}
		lit, ok := ast.Unparen(e).(*ast.BasicLit)
		return ok && lit.Kind == token.INT && lit.Value == "0"
	}
	return zero(s.High) && zero(s.Max) && (s.Low == nil || zero(s.Low))
}

func resultIsSlice(sig *types.Signature) bool {
	if sig.Results().Len() != 1 {
		return false
	}
	_, ok := sig.Results().At(0).Type().Underlying().(*types.Slice)
	return ok
}

// resultIsLValSlice reports whether sig returns []*lisp.LVal in ANY result
// position.  Multi-result is deliberate: libelpspath's toCells has signature
// `([]*lisp.LVal, error)` and is the taint source for the whole path package
// — restricting this to single-result signatures (as it originally did) made
// every `cells, err := toCells(in)` launder the alias, which is precisely how
// issue #392 stayed invisible to the rule.
func resultIsLValSlice(sig *types.Signature) bool {
	for i := range sig.Results().Len() {
		s, ok := sig.Results().At(i).Type().Underlying().(*types.Slice)
		if ok && isLValPtr(s.Elem()) {
			return true
		}
	}
	return false
}

// sliceOrigin returns the LVal variables whose backing storage e may alias.
// It mirrors sliceTaint's structure exactly — same cases, same order — and
// returns the roots rather than a bit.  A nil result means "tainted, but the
// source is not a variable this function can name" (a field of some other
// struct, a call with no LVal argument); the seal rule treats that as
// undischargeable by a seal consultation, because there is nothing to
// consult.
func (t *aliasTracker) sliceOrigin(e ast.Expr) []types.Object {
	e = ast.Unparen(e)
	switch x := e.(type) {
	case *ast.Ident:
		if obj := t.objectOf(x); obj != nil {
			return t.origin[obj]
		}
	case *ast.SelectorExpr:
		// v.Cells: the root is whatever v roots at.
		if _, ok := lvalFieldSelection(t.pass, x); ok {
			return lvalRoot(t.pass, x.X)
		}
	case *ast.SliceExpr:
		return t.sliceOrigin(x.X)
	case *ast.CallExpr:
		return t.callOrigin(x)
	}
	return nil
}

func (t *aliasTracker) callOrigin(call *ast.CallExpr) []types.Object {
	if t.isBuiltin(call, "append") {
		if len(call.Args) > 0 {
			return t.sliceOrigin(call.Args[0])
		}
		return nil
	}
	if tv, ok := t.pass.TypesInfo.Types[call.Fun]; ok && tv.IsType() && len(call.Args) == 1 {
		return t.sliceOrigin(call.Args[0]) // slice conversion shares backing
	}
	if fn := methodCallee(t.pass, call); fn != nil && recvIsLisp(fn, "LVal") {
		// v.Bytes(): the backing belongs to the receiver.
		if sel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr); ok {
			return lvalRoot(t.pass, sel.X)
		}
		return nil
	}
	// seqCells(v) / toCells(v): the backing belongs to the LVal arguments,
	// or to whatever an already-tainted slice argument came from.
	var roots []types.Object
	for _, arg := range call.Args {
		at := t.pass.TypesInfo.TypeOf(arg)
		if isLValValue(at) || isLValPtr(at) {
			roots = append(roots, lvalRoot(t.pass, arg)...)
			continue
		}
		if t.sliceTaint(arg) {
			roots = append(roots, t.sliceOrigin(arg)...)
		}
	}
	return roots
}

func (t *aliasTracker) objectOf(id *ast.Ident) types.Object {
	if obj := t.pass.TypesInfo.Uses[id]; obj != nil {
		return obj
	}
	return t.pass.TypesInfo.Defs[id]
}

// lvalRoot walks an LVal expression down to the variable it is rooted at.
// v, v.Cells[0], (*v), v.Copy() all root at v.  A chain that does not bottom
// out in an identifier (a package-level selector, a call with no receiver)
// has no nameable root and returns nil.
func lvalRoot(pass *analysis.Pass, e ast.Expr) []types.Object {
	for {
		e = ast.Unparen(e)
		switch x := e.(type) {
		case *ast.Ident:
			obj := pass.TypesInfo.Uses[x]
			if obj == nil {
				obj = pass.TypesInfo.Defs[x]
			}
			if obj == nil {
				return nil
			}
			return []types.Object{obj}
		case *ast.SelectorExpr:
			e = x.X
		case *ast.IndexExpr:
			e = x.X
		case *ast.StarExpr:
			e = x.X
		case *ast.CallExpr:
			if sel, ok := ast.Unparen(x.Fun).(*ast.SelectorExpr); ok && methodCallee(pass, x) != nil {
				e = sel.X
				continue
			}
			return nil
		default:
			return nil
		}
	}
}
