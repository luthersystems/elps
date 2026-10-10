// Copyright © 2026 The ELPS authors

package idiom

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"
)

// This file holds the parts of elpsidiom that run inside package lisp.
//
// In package lisp the rules match unqualified names (QExpr, LError, ...),
// because they resolve each name through the type checker.  A fix there
// writes the helper without the "lisp." qualifier.  A fix that calls a
// helper is not made in a function that the helper reaches through its own
// calls, because the rewrite would make the helper call itself.  For
// example, the body of (*LVal).IsError keeps v.Type == LError, and the body
// of Vector keeps Array(nil, cells).

// collectUses records each function body of package lisp and the functions
// and package-level variables that it names.
func (s *state) collectUses() {
	s.uses = make(map[types.Object]map[types.Object]bool)
	s.reach = make(map[string]map[types.Object]bool)
	scope := s.pass.Pkg.Scope()
	add := func(obj types.Object, body ast.Node) {
		s.units = append(s.units, unit{obj: obj, pos: body.Pos(), end: body.End()})
		named := s.uses[obj]
		if named == nil {
			named = make(map[types.Object]bool)
			s.uses[obj] = named
		}
		ast.Inspect(body, func(n ast.Node) bool {
			id, ok := n.(*ast.Ident)
			if !ok {
				return true
			}
			switch u := s.pass.TypesInfo.Uses[id].(type) {
			case *types.Func:
				named[u.Origin()] = true
			case *types.Var:
				if u.Parent() == scope {
					named[u] = true
				}
			}
			return true
		})
	}
	for _, file := range s.pass.Files {
		for _, decl := range file.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				fn, ok := s.pass.TypesInfo.Defs[d.Name].(*types.Func)
				if ok && d.Body != nil {
					add(fn, d)
				}
			case *ast.GenDecl:
				if d.Tok != token.VAR {
					continue
				}
				for _, spec := range d.Specs {
					vs, ok := spec.(*ast.ValueSpec)
					if !ok {
						continue
					}
					for i, name := range vs.Names {
						obj := s.pass.TypesInfo.Defs[name]
						if obj == nil || i >= len(vs.Values) {
							continue
						}
						add(obj, vs.Values[i])
					}
				}
			}
		}
	}
}

// helperObj returns the object of a helper of package lisp: "Vector" for a
// function, or "LVal.IsError" for a method.
func (s *state) helperObj(helper string) types.Object {
	scope := s.pass.Pkg.Scope()
	recv, name, isMethod := strings.Cut(helper, ".")
	if !isMethod {
		return scope.Lookup(helper)
	}
	tn, ok := scope.Lookup(recv).(*types.TypeName)
	if !ok {
		return nil
	}
	named, ok := types.Unalias(tn.Type()).(*types.Named)
	if !ok {
		return nil
	}
	for m := range named.Methods() {
		if m.Name() == name {
			return m
		}
	}
	return nil
}

// reaches returns the units that helper reaches through the functions and
// variables that each body names, the helper itself included.
func (s *state) reaches(helper string) map[types.Object]bool {
	if r, ok := s.reach[helper]; ok {
		return r
	}
	r := make(map[types.Object]bool)
	s.reach[helper] = r
	start := s.helperObj(helper)
	if start == nil {
		return r
	}
	work := []types.Object{start}
	r[start] = true
	for len(work) > 0 {
		obj := work[len(work)-1]
		work = work[:len(work)-1]
		for next := range s.uses[obj] {
			if !r[next] {
				r[next] = true
				work = append(work, next)
			}
		}
	}
	return r
}

// enclosing returns the innermost unit that holds pos, or nil.
func (s *state) enclosing(pos token.Pos) types.Object {
	var best *unit
	for i := range s.units {
		u := &s.units[i]
		if u.pos <= pos && pos < u.end && (best == nil || u.pos >= best.pos) {
			best = u
		}
	}
	if best == nil {
		return nil
	}
	return best.obj
}

// fixAllowed reports whether a fix at n may call the helpers.  It is false
// on a line that a keepMarker comment covers.  Otherwise, outside package
// lisp, it is true.  In package lisp it is false when a
// helper reaches the function that holds n, or when the unqualified name of
// a package-level helper (Vector, Cells, MapOf) does not resolve to that
// helper at n.
func (s *state) fixAllowed(n ast.Node, helpers ...string) bool {
	if s.kept(n.Pos()) {
		return false
	}
	if !s.inLisp {
		return true
	}
	at := s.enclosing(n.Pos())
	for _, h := range helpers {
		recv, _, isMethod := strings.Cut(h, ".")
		name := h
		if isMethod {
			name = recv
		}
		if !s.resolvesToPkg(n.Pos(), name) {
			return false
		}
		if at != nil && s.reaches(h)[at] {
			return false
		}
	}
	return true
}

// resolvesToPkg reports whether the unqualified name at pos resolves to the
// package-level object of package lisp with that name.  A method helper on
// LVal or LEnv checks its type name, which a rewrite does not write; such a
// name passes when no local declaration shadows it.
func (s *state) resolvesToPkg(pos token.Pos, name string) bool {
	pkgObj := s.pass.Pkg.Scope().Lookup(name)
	if pkgObj == nil {
		return false
	}
	inner := s.pass.Pkg.Scope().Innermost(pos)
	if inner == nil {
		return true
	}
	_, obj := inner.LookupParent(name, pos)
	return obj == pkgObj
}

// qualifier returns the prefix that names a lisp function at the call
// target fun: "lisp." (or the file's name for the lisp import) for a
// qualified target, and "" for an unqualified target in package lisp.
func (s *state) qualifier(fun ast.Expr) (string, bool) {
	switch f := ast.Unparen(fun).(type) {
	case *ast.SelectorExpr:
		id, ok := f.X.(*ast.Ident)
		if !ok {
			return "", false
		}
		return id.Name + ".", true
	case *ast.Ident:
		return "", s.inLisp
	}
	return "", false
}

// isCellSliceLit reports whether e is a composite literal of type []*LVal
// with its type written out.
func (s *state) isCellSliceLit(e ast.Expr) (*ast.CompositeLit, bool) {
	lit, ok := ast.Unparen(e).(*ast.CompositeLit)
	if !ok || lit.Type == nil {
		return nil, false
	}
	sl, ok := types.Unalias(s.pass.TypesInfo.TypeOf(lit)).(*types.Slice)
	if !ok {
		return nil, false
	}
	p, ok := types.Unalias(sl.Elem()).(*types.Pointer)
	return lit, ok && isLispNamed(p.Elem(), "LVal")
}

// checkCellsKeyed: in package lisp, &LVal{..., Cells: []*LVal{...}} becomes
// &LVal{..., Cells: Cells{...}}.  Cells is []*LVal, so the field takes the
// value with no conversion.
func (s *state) checkCellsKeyed(lit *ast.CompositeLit) {
	if !s.inLisp || !isLispNamed(s.pass.TypesInfo.TypeOf(lit), "LVal") {
		return
	}
	for _, elt := range lit.Elts {
		kv, ok := elt.(*ast.KeyValueExpr)
		if !ok {
			continue
		}
		if key, isID := kv.Key.(*ast.Ident); !isID || key.Name != "Cells" {
			continue
		}
		s.cellsFieldFix(kv.Value)
	}
}

// checkCellsAssign: in package lisp, x.Cells = []*LVal{...} becomes
// x.Cells = Cells{...}.
func (s *state) checkCellsAssign(assign *ast.AssignStmt) {
	if !s.inLisp || assign.Tok != token.ASSIGN || len(assign.Lhs) != len(assign.Rhs) {
		return
	}
	for i, l := range assign.Lhs {
		sel, ok := ast.Unparen(l).(*ast.SelectorExpr)
		if !ok || sel.Sel.Name != "Cells" || s.typeFieldOwner(sel) == nil {
			continue
		}
		s.cellsFieldFix(assign.Rhs[i])
	}
}

func (s *state) cellsFieldFix(value ast.Expr) {
	lit, ok := s.isCellSliceLit(value)
	if !ok || !s.fixAllowed(lit, "Cells") {
		return
	}
	s.report(lit.Type, CategoryInfo, "use Cells{...} for the Cells field, which is the same slice type",
		replace(lit.Type, "Use Cells", "Cells"))
}

// nilTest returns x for x != nil (cmp token.NEQ) or x == nil (token.EQL).
func (s *state) nilTest(e ast.Expr, cmp token.Token) ast.Expr {
	b, ok := ast.Unparen(e).(*ast.BinaryExpr)
	if !ok || b.Op != cmp {
		return nil
	}
	switch {
	case isNilIdent(s, b.Y):
		return b.X
	case isNilIdent(s, b.X):
		return b.Y
	}
	return nil
}

// isErrorTest returns x for x.Type == LError or x.IsError() (cmp
// token.EQL), or for x.Type != LError or !x.IsError() (cmp token.NEQ).
func (s *state) isErrorTest(e ast.Expr, cmp token.Token) ast.Expr {
	e = ast.Unparen(e)
	if b, ok := e.(*ast.BinaryExpr); ok && b.Op == cmp {
		x, other := s.typeField(b.X), b.Y
		if x == nil {
			x, other = s.typeField(b.Y), b.X
		}
		if x != nil && s.isLispConst(other, "LError") {
			return x
		}
		return nil
	}
	if cmp == token.NEQ {
		u, ok := e.(*ast.UnaryExpr)
		if !ok || u.Op != token.NOT {
			return nil
		}
		e = ast.Unparen(u.X)
	}
	x, _ := s.methodTest(e, "IsError")
	return x
}

// isSymbolCall returns x and name for x.IsSymbol(name) (cmp token.EQL) or
// !x.IsSymbol(name) (cmp token.NEQ).
func (s *state) isSymbolCall(e ast.Expr, cmp token.Token) (ast.Expr, ast.Expr) {
	e = ast.Unparen(e)
	if cmp == token.NEQ {
		u, ok := e.(*ast.UnaryExpr)
		if !ok || u.Op != token.NOT {
			return nil, nil
		}
		e = ast.Unparen(u.X)
	}
	return s.methodTest(e, "IsSymbol")
}

// methodTest returns the receiver and the first argument of a call of the
// LVal method name.
func (s *state) methodTest(e ast.Expr, name string) (ast.Expr, ast.Expr) {
	call, ok := e.(*ast.CallExpr)
	if !ok || s.lispMethod(call, "LVal") == nil || s.callee(call).Name() != name {
		return nil, nil
	}
	sel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
	if !ok {
		return nil, nil
	}
	var arg ast.Expr
	if len(call.Args) > 0 {
		arg = call.Args[0]
	}
	return sel.X, arg
}

// nilGuardFix checks the terms of an && chain (cmp token.EQL) or an || chain
// (cmp token.NEQ) that start at terms[i] with a nil test of x.  IsError and
// IsSymbol return false for a nil x, so the nil test and the compare after
// it become one call.  It returns the count of terms that the fix replaces,
// or 0.
func (s *state) nilGuardFix(terms []ast.Expr, i int, cmp token.Token, neg string) int {
	nilCmp := token.NEQ
	if cmp == token.NEQ {
		nilCmp = token.EQL
	}
	x := s.nilTest(terms[i], nilCmp)
	if x == nil || !s.pure(x) || i+1 >= len(terms) {
		return 0
	}
	xs := s.text(x)
	emit := func(n int, text, helper string) int {
		last := terms[i+n-1]
		if !s.fixAllowed(terms[i], helper) {
			return 0
		}
		for _, t := range terms[i : i+n] {
			ast.Inspect(t, func(m ast.Node) bool {
				s.covered[m] = true
				return true
			})
		}
		s.reportRange(terms[i], last, "use "+text+", which is the same test: it is false for a nil value", text)
		return n
	}
	if y := s.isErrorTest(terms[i+1], cmp); y != nil && s.text(y) == xs {
		return emit(2, neg+s.operand(x)+".IsError()", "LVal.IsError")
	}
	if y, name := s.isSymbolCall(terms[i+1], cmp); y != nil && name != nil && s.text(y) == xs {
		return emit(2, neg+s.operand(x)+".IsSymbol("+s.text(name)+")", "LVal.IsSymbol")
	}
	if i+2 < len(terms) {
		t1, t2 := terms[i+1], terms[i+2]
		y := s.symbolTypeTest(t1, cmp)
		ny, name := s.symbolNameTest(t2, cmp)
		if y == nil || ny == nil {
			ny, name = s.symbolNameTest(t1, cmp)
			y = s.symbolTypeTest(t2, cmp)
		}
		if y != nil && ny != nil && s.text(y) == xs && s.text(ny) == xs {
			return emit(3, neg+s.operand(x)+".IsSymbol("+s.text(name)+")", "LVal.IsSymbol")
		}
	}
	return 0
}

// keepMarker, followed by a reason, on the line of a fix or on the line
// above it, keeps the code as it is.  Use it at a hot site where the helper
// is measurably slower, for example the nil test that IsError adds.
const keepMarker = "//elpsvet:keep-idiom"

// kept reports whether a keepMarker comment with a reason covers the line of
// pos.
func (s *state) kept(pos token.Pos) bool {
	if s.keepLines == nil {
		s.keepLines = make(map[string]map[int]bool)
		for _, f := range s.pass.Files {
			for _, cg := range f.Comments {
				for _, c := range cg.List {
					reason, ok := strings.CutPrefix(c.Text, keepMarker)
					if !ok || strings.TrimSpace(reason) == "" || !strings.HasPrefix(reason, " ") {
						continue
					}
					p := s.pass.Fset.Position(c.Slash)
					lines := s.keepLines[p.Filename]
					if lines == nil {
						lines = make(map[int]bool)
						s.keepLines[p.Filename] = lines
					}
					lines[p.Line] = true
					lines[p.Line+1] = true
				}
			}
		}
	}
	p := s.pass.Fset.Position(pos)
	return s.keepLines[p.Filename][p.Line]
}
