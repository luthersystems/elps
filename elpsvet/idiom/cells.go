// Copyright © 2026 The ELPS authors

package idiom

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
)

// This file holds the rules for the lisp.Cells methods Map, Clone, Append
// and MapIfChanged.
//
// A fix changes the static type of its result from []*lisp.LVal to
// lisp.Cells.  Go assigns one to the other with no conversion, so the change
// is invisible except where the value meets an interface or a type
// parameter: there the dynamic type differs (a type switch, fmt's %T,
// reflection).  cellsSafe checks every place the result goes, and a fix is
// made only when each place takes a []*lisp.LVal.

// isCellSlice reports whether t is []*lisp.LVal or lisp.Cells.
func isCellSlice(t types.Type) bool {
	if t == nil {
		return false
	}
	if isLispNamed(t, "Cells") {
		return true
	}
	sl, ok := types.Unalias(t).(*types.Slice)
	if !ok {
		return false
	}
	if _, named := types.Unalias(t).(*types.Named); named {
		return false
	}
	p, ok := types.Unalias(sl.Elem()).(*types.Pointer)
	return ok && isLispNamed(p.Elem(), "LVal")
}

// cellSliceExpr reports whether e has type []*lisp.LVal or lisp.Cells.
func (s *state) cellSliceExpr(e ast.Expr) bool {
	return isCellSlice(s.pass.TypesInfo.TypeOf(e))
}

// isCellsType reports whether e has type lisp.Cells.
func (s *state) isCellsType(e ast.Expr) bool {
	return isLispNamed(s.pass.TypesInfo.TypeOf(e), "Cells")
}

// lispQual returns the prefix that names package lisp at pos: "" in package
// lisp, else the name of the file's import of it.  It is false when the
// file does not import lisp by a name that resolves at pos.
func (s *state) lispQual(pos token.Pos) (string, bool) {
	if s.inLisp {
		return "", s.resolvesToPkg(pos, "Cells") && s.resolvesToPkg(pos, "LVal")
	}
	for _, f := range s.pass.Files {
		if pos < f.FileStart || pos > f.FileEnd {
			continue
		}
		for _, imp := range f.Imports {
			if strings.Trim(imp.Path.Value, `"`) != lispPkgPath {
				continue
			}
			name := "lisp"
			if imp.Name != nil {
				name = imp.Name.Name
			}
			if name == "_" || name == "." {
				return "", false
			}
			inner := s.pass.Pkg.Scope().Innermost(pos)
			if inner == nil {
				return "", false
			}
			_, obj := inner.LookupParent(name, pos)
			pn, ok := obj.(*types.PkgName)
			if !ok || pn.Imported().Path() != lispPkgPath {
				return "", false
			}
			return name + ".", true
		}
	}
	return "", false
}

// cellsOf returns the text of a lisp.Cells receiver over e: e itself when it
// is already a lisp.Cells, else the conversion q+"Cells(e)".
func (s *state) cellsOf(q string, e ast.Expr) string {
	if s.isCellsType(e) {
		return s.operand(e)
	}
	return q + "Cells(" + s.text(e) + ")"
}

// parent returns the node that holds n.
func (s *state) parent(n ast.Node) ast.Node {
	if s.parents == nil {
		s.parents = make(map[ast.Node]ast.Node)
		for _, f := range s.pass.Files {
			var stack []ast.Node
			ast.Inspect(f, func(n ast.Node) bool {
				if n == nil {
					stack = stack[:len(stack)-1]
					return false
				}
				if len(stack) > 0 {
					s.parents[n] = stack[len(stack)-1]
				}
				stack = append(stack, n)
				return true
			})
		}
	}
	return s.parents[n]
}

// typeSensitive reports whether a value converted to t keeps its own type
// as its dynamic type: t is an interface or a type parameter, or unknown.
func typeSensitive(t types.Type) bool {
	if t == nil {
		return true
	}
	switch types.Unalias(t).Underlying().(type) {
	case *types.Interface:
		return true
	}
	_, isParam := types.Unalias(t).(*types.TypeParam)
	return isParam
}

// cellsSafe reports whether the value of e, of type []*lisp.LVal, may
// become a lisp.Cells with no visible change.  depth bounds the variables
// it follows.
func (s *state) cellsSafe(e ast.Expr, depth int) bool {
	switch p := s.parent(e).(type) {
	case *ast.ParenExpr:
		return s.cellsSafe(p, depth)
	case *ast.CallExpr:
		return p.Fun != e && s.argSafe(p, e, depth)
	case *ast.AssignStmt:
		for _, l := range p.Lhs {
			if l == e {
				return true // a store into the variable
			}
		}
		if len(p.Lhs) != len(p.Rhs) {
			return false
		}
		for i, r := range p.Rhs {
			if r != e {
				continue
			}
			if id, ok := p.Lhs[i].(*ast.Ident); ok && id.Name == "_" {
				return true
			}
			if id, ok := p.Lhs[i].(*ast.Ident); ok && p.Tok == token.DEFINE {
				if obj := s.pass.TypesInfo.Defs[id]; obj != nil {
					return depth < 2 && s.varSafe(obj, depth+1)
				}
			}
			return !typeSensitive(s.pass.TypesInfo.TypeOf(p.Lhs[i]))
		}
		return false
	case *ast.ValueSpec:
		if p.Type != nil {
			return !typeSensitive(s.pass.TypesInfo.TypeOf(p.Type))
		}
		for i, v := range p.Values {
			if v == e && i < len(p.Names) && len(p.Names) == len(p.Values) {
				obj := s.pass.TypesInfo.Defs[p.Names[i]]
				return obj != nil && depth < 2 && s.varSafe(obj, depth+1)
			}
		}
		return false
	case *ast.ReturnStmt:
		sig := s.enclosingSig(p)
		if sig == nil || sig.Results().Len() != len(p.Results) {
			return false
		}
		for i, r := range p.Results {
			if r == e {
				return !typeSensitive(sig.Results().At(i).Type())
			}
		}
		return false
	case *ast.CompositeLit:
		return !typeSensitive(elemType(s.pass.TypesInfo.TypeOf(p), ""))
	case *ast.KeyValueExpr:
		if p.Value != e {
			return false
		}
		lit, ok := s.parent(p).(*ast.CompositeLit)
		if !ok {
			return false
		}
		key := ""
		if id, ok := p.Key.(*ast.Ident); ok {
			key = id.Name
		}
		return !typeSensitive(elemType(s.pass.TypesInfo.TypeOf(lit), key))
	case *ast.SendStmt:
		ch, ok := types.Unalias(s.pass.TypesInfo.TypeOf(p.Chan)).Underlying().(*types.Chan)
		return ok && p.Value == e && !typeSensitive(ch.Elem())
	case *ast.IndexExpr:
		return p.X == e
	case *ast.SliceExpr:
		return p.X == e && s.cellsSafe(p, depth)
	case *ast.RangeStmt:
		return p.X == e
	case *ast.BinaryExpr:
		return true // a compare with nil
	}
	return false
}

// argSafe reports whether arg, an argument of call, may become a lisp.Cells.
func (s *state) argSafe(call *ast.CallExpr, arg ast.Expr, depth int) bool {
	tv := s.pass.TypesInfo.Types[call.Fun]
	if tv.IsType() {
		return !typeSensitive(tv.Type)
	}
	if tv.IsBuiltin() {
		id, ok := ast.Unparen(call.Fun).(*ast.Ident)
		if !ok {
			return false
		}
		switch id.Name {
		case "len", "cap", "copy", "clear":
			return true
		case "append":
			if len(call.Args) > 0 && call.Args[0] == arg {
				return s.cellsSafe(call, depth) // append returns the first argument's type
			}
			return true
		}
		return false
	}
	sig, ok := types.Unalias(tv.Type).(*types.Signature)
	if !ok {
		return false
	}
	idx := -1
	for i, a := range call.Args {
		if a == arg {
			idx = i
		}
	}
	if idx < 0 {
		return false
	}
	paramType := func(sig *types.Signature) types.Type {
		n := sig.Params().Len()
		if sig.Variadic() && idx >= n-1 {
			last := sig.Params().At(n - 1).Type()
			if call.Ellipsis.IsValid() {
				return last
			}
			if sl, ok := types.Unalias(last).(*types.Slice); ok {
				return sl.Elem()
			}
			return nil
		}
		if idx >= n {
			return nil
		}
		return sig.Params().At(idx).Type()
	}
	if fn := s.callee(call); fn != nil && fn.Origin() != fn {
		// A generic callee: a type parameter keeps the argument's type.
		if _, isParam := types.Unalias(paramType(fn.Origin().Signature())).(*types.TypeParam); isParam {
			return fn.Pkg() != nil && fn.Pkg().Path() == "slices"
		}
	}
	return !typeSensitive(paramType(sig))
}

// elemType returns the type a composite literal of type t gives the
// element with the field name key ("" for a positional element).
func elemType(t types.Type, key string) types.Type {
	if t == nil {
		return nil
	}
	switch u := types.Unalias(t).Underlying().(type) {
	case *types.Slice:
		return u.Elem()
	case *types.Array:
		return u.Elem()
	case *types.Map:
		return u.Elem()
	case *types.Struct:
		for f := range u.Fields() {
			if f.Name() == key {
				return f.Type()
			}
		}
	case *types.Pointer:
		return elemType(u.Elem(), key)
	}
	return nil
}

// enclosingSig returns the signature of the function that holds n.
func (s *state) enclosingSig(n ast.Node) *types.Signature {
	for p := s.parent(n); p != nil; p = s.parent(p) {
		switch f := p.(type) {
		case *ast.FuncLit:
			sig, _ := s.pass.TypesInfo.TypeOf(f).(*types.Signature)
			return sig
		case *ast.FuncDecl:
			if fn, ok := s.pass.TypesInfo.Defs[f.Name].(*types.Func); ok {
				return fn.Signature()
			}
			return nil
		}
	}
	return nil
}

// varSafe reports whether each use of the variable obj may see a lisp.Cells.
func (s *state) varSafe(obj types.Object, depth int) bool {
	for id, o := range s.pass.TypesInfo.Uses {
		if o == obj && !s.cellsSafe(id, depth) {
			return false
		}
	}
	return true
}

// commentsInside reports whether a comment lies in [from, to) outside
// [keepFrom, keepTo).
func (s *state) commentsInside(from, to, keepFrom, keepTo token.Pos) bool {
	for _, f := range s.pass.Files {
		if from < f.FileStart || from > f.FileEnd {
			continue
		}
		for _, cg := range f.Comments {
			if cg.Pos() >= from && cg.End() <= to && (cg.Pos() < keepFrom || cg.End() > keepTo) {
				return true
			}
		}
	}
	return false
}

// reportPair reports an info idiom over two adjacent statements with a fix
// that replaces the first with text and deletes the second.  A comment at
// the end of the first statement's line stays.
func (s *state) reportPair(first, last ast.Stmt, msg, text string) {
	file := s.pass.Fset.File(first.End())
	line := file.Line(first.End())
	if line >= file.LineCount() {
		return
	}
	eol := file.LineStart(line+1) - 1
	s.pass.Report(analysis.Diagnostic{
		Pos:      first.Pos(),
		End:      last.End(),
		Category: CategoryInfo,
		Message:  msg,
		SuggestedFixes: []analysis.SuggestedFix{{
			Message: "Use the helper",
			TextEdits: []analysis.TextEdit{
				{Pos: first.Pos(), End: first.End(), NewText: []byte(text)},
				{Pos: eol, End: last.End()},
			},
		}},
	})
}

// pairComments reports whether a comment lies inside first, or between the
// line after first and the end of last outside [keepFrom, keepTo).
func (s *state) pairComments(first, last ast.Stmt, keepFrom, keepTo token.Pos) bool {
	file := s.pass.Fset.File(first.End())
	line := file.Line(first.End())
	if line >= file.LineCount() {
		return true
	}
	return s.commentsInside(first.Pos(), first.End(), first.End(), first.End()) ||
		s.commentsInside(file.LineStart(line+1), last.End(), keepFrom, keepTo)
}

// indentOf returns the leading white space of the line that holds pos.
func (s *state) indentOf(pos token.Pos) string {
	file := s.pass.Fset.File(pos)
	if file == nil {
		return ""
	}
	start := file.LineStart(file.Line(pos))
	text, ok := s.source(start, pos)
	if !ok {
		return ""
	}
	return text[:len(text)-len(strings.TrimLeft(text, " \t"))]
}

// sliceMake returns the length argument of make(T, n), for a T of
// []*lisp.LVal or lisp.Cells and no capacity argument.
func (s *state) sliceMake(e ast.Expr) ast.Expr {
	call, ok := ast.Unparen(e).(*ast.CallExpr)
	if !ok || len(call.Args) != 2 {
		return nil
	}
	id, ok := ast.Unparen(call.Fun).(*ast.Ident)
	if !ok || id.Name != "make" {
		return nil
	}
	if _, ok := s.pass.TypesInfo.Uses[id].(*types.Builtin); !ok || !s.cellSliceExpr(call) {
		return nil
	}
	return call.Args[1]
}

// lenOf returns x for len(x), for an x of []*lisp.LVal or lisp.Cells.
func (s *state) lenOf(e ast.Expr) ast.Expr {
	call, ok := ast.Unparen(e).(*ast.CallExpr)
	if !ok || len(call.Args) != 1 {
		return nil
	}
	id, ok := ast.Unparen(call.Fun).(*ast.Ident)
	if !ok || id.Name != "len" {
		return nil
	}
	if _, ok := s.pass.TypesInfo.Uses[id].(*types.Builtin); !ok || !s.cellSliceExpr(call.Args[0]) {
		return nil
	}
	return call.Args[0]
}

// makeSite is a statement out := make(T, len(xs)) or out = make(T, len(xs)).
type makeSite struct {
	out      *ast.Ident
	xs       ast.Expr
	declares bool // the statement declares out
}

// makeTarget returns the makeSite of st, or the zero makeSite (out nil).
func (s *state) makeTarget(st ast.Stmt) makeSite {
	assign, ok := st.(*ast.AssignStmt)
	if !ok || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 || (assign.Tok != token.DEFINE && assign.Tok != token.ASSIGN) {
		return makeSite{}
	}
	out, ok := assign.Lhs[0].(*ast.Ident)
	if !ok || out.Name == "_" {
		return makeSite{}
	}
	n := s.sliceMake(assign.Rhs[0])
	if n == nil {
		return makeSite{}
	}
	xs := s.lenOf(n)
	if xs == nil || !s.pure(xs) {
		return makeSite{}
	}
	return makeSite{out: out, xs: xs, declares: assign.Tok == token.DEFINE && s.pass.TypesInfo.Defs[out] != nil}
}

// usesObj reports whether n names obj.
func (s *state) usesObj(n ast.Node, obj types.Object) bool {
	found := false
	ast.Inspect(n, func(m ast.Node) bool {
		if id, ok := m.(*ast.Ident); ok && s.pass.TypesInfo.Uses[id] == obj {
			found = true
		}
		return !found
	})
	return found
}

// checkMapLoop: out := make([]*lisp.LVal, len(xs)), then
// for i, x := range xs { out[i] = expr }, where expr does not name out or i.
// lisp.Cells(xs).Map runs expr for each cell in the same order.
func (s *state) checkMapLoop(block *ast.BlockStmt) {
	for i := 0; i+1 < len(block.List); i++ {
		site := s.makeTarget(block.List[i])
		out, xs, declares := site.out, site.xs, site.declares
		if out == nil {
			continue
		}
		outObj := s.pass.TypesInfo.ObjectOf(out)
		r, ok := block.List[i+1].(*ast.RangeStmt)
		if !ok || r.Tok != token.DEFINE || r.Key == nil || r.Value == nil || len(r.Body.List) != 1 ||
			s.text(r.X) != s.text(xs) {
			continue
		}
		key, kok := r.Key.(*ast.Ident)
		val, vok := r.Value.(*ast.Ident)
		if !kok || !vok {
			continue
		}
		keyObj := s.pass.TypesInfo.Defs[key]
		body, ok := r.Body.List[0].(*ast.AssignStmt)
		if !ok || body.Tok != token.ASSIGN || len(body.Lhs) != 1 || len(body.Rhs) != 1 {
			continue
		}
		idx, ok := body.Lhs[0].(*ast.IndexExpr)
		if !ok {
			continue
		}
		if x, xok := ast.Unparen(idx.X).(*ast.Ident); !xok || s.pass.TypesInfo.Uses[x] != outObj {
			continue
		}
		if k, iok := ast.Unparen(idx.Index).(*ast.Ident); !iok || keyObj == nil || s.pass.TypesInfo.Uses[k] != keyObj {
			continue
		}
		expr := body.Rhs[0]
		if s.usesObj(expr, outObj) || (keyObj != nil && s.usesObj(expr, keyObj)) {
			continue
		}
		if declares && !s.varSafe(outObj, 1) {
			continue
		}
		first, last := block.List[i], block.List[i+1]
		if s.pairComments(first, last, expr.Pos(), expr.End()) ||
			!s.fixAllowed(first, "Cells", "Cells.Map") {
			continue
		}
		q, ok := s.lispQual(first.Pos())
		if !ok {
			continue
		}
		exprText, ok := s.source(expr.Pos(), expr.End())
		if !ok {
			continue
		}
		var fn string
		if f := s.plainMapper(expr, val); f != "" {
			fn = f
		} else {
			indent := s.indentOf(first.Pos())
			fn = "func(" + val.Name + " *" + q + "LVal) *" + q + "LVal {\n" + indent + "\treturn " + exprText + "\n" + indent + "}"
		}
		tok := " = "
		if declares {
			tok = " := "
		}
		text := out.Name + tok + s.cellsOf(q, xs) + ".Map(" + fn + ")"
		s.reportPair(first, last, "use "+s.cellsOf(q, xs)+".Map, which builds the same slice; "+
			"it returns nil for a nil "+s.text(xs)+" where make returns an empty slice", text)
		i++
	}
}

// plainMapper returns the name of f when expr is f(x), for a declared
// function f of type func(*lisp.LVal) *lisp.LVal and the range value x.
func (s *state) plainMapper(expr ast.Expr, val *ast.Ident) string {
	call, ok := ast.Unparen(expr).(*ast.CallExpr)
	if !ok || len(call.Args) != 1 || call.Ellipsis.IsValid() {
		return ""
	}
	if a, aok := ast.Unparen(call.Args[0]).(*ast.Ident); !aok || s.pass.TypesInfo.Uses[a] != s.pass.TypesInfo.Defs[val] {
		return ""
	}
	var id *ast.Ident
	switch f := ast.Unparen(call.Fun).(type) {
	case *ast.Ident:
		id = f
	case *ast.SelectorExpr:
		if _, isPkg := s.pass.TypesInfo.Uses[identOf(f.X)].(*types.PkgName); !isPkg {
			return ""
		}
		id = f.Sel
	default:
		return ""
	}
	fn, ok := s.pass.TypesInfo.Uses[id].(*types.Func)
	if !ok || fn.Signature().Recv() != nil || fn.Signature().TypeParams().Len() > 0 {
		return ""
	}
	sig := fn.Signature()
	if sig.Variadic() || sig.Params().Len() != 1 || sig.Results().Len() != 1 {
		return ""
	}
	if !isLValPtr(sig.Params().At(0).Type()) || !isLValPtr(sig.Results().At(0).Type()) {
		return ""
	}
	return s.text(call.Fun)
}

func identOf(e ast.Expr) *ast.Ident {
	id, _ := ast.Unparen(e).(*ast.Ident)
	return id
}

func isLValPtr(t types.Type) bool {
	p, ok := types.Unalias(t).(*types.Pointer)
	return ok && isLispNamed(p.Elem(), "LVal")
}

// checkMakeCopy: x := make([]*lisp.LVal, len(s)), then copy(x, s).
// lisp.Cells(s).Clone() makes the same copy.
func (s *state) checkMakeCopy(block *ast.BlockStmt) {
	for i := 0; i+1 < len(block.List); i++ {
		site := s.makeTarget(block.List[i])
		out, xs, declares := site.out, site.xs, site.declares
		if out == nil {
			continue
		}
		es, ok := block.List[i+1].(*ast.ExprStmt)
		if !ok {
			continue
		}
		call, ok := ast.Unparen(es.X).(*ast.CallExpr)
		if !ok || len(call.Args) != 2 {
			continue
		}
		if id, idok := ast.Unparen(call.Fun).(*ast.Ident); !idok || id.Name != "copy" {
			continue
		} else if _, bok := s.pass.TypesInfo.Uses[id].(*types.Builtin); !bok {
			continue
		}
		outObj := s.pass.TypesInfo.ObjectOf(out)
		if dst, dok := ast.Unparen(call.Args[0]).(*ast.Ident); !dok || s.pass.TypesInfo.Uses[dst] != outObj {
			continue
		}
		if s.text(call.Args[1]) != s.text(xs) {
			continue
		}
		if declares && !s.varSafe(outObj, 1) {
			continue
		}
		first, last := block.List[i], block.List[i+1]
		if s.pairComments(first, last, last.End(), last.End()) || !s.fixAllowed(first, "Cells", "Cells.Clone") {
			continue
		}
		q, ok := s.lispQual(first.Pos())
		if !ok {
			continue
		}
		tok := " = "
		if declares {
			tok = " := "
		}
		s.reportPair(first, last, "use "+s.cellsOf(q, xs)+".Clone(), which makes the same copy; "+
			"it returns nil for a nil "+s.text(xs)+" where make returns an empty slice",
			out.Name+tok+s.cellsOf(q, xs)+".Clone()")
		i++
	}
}

// cloneSource returns s for a clone of a cell slice s: slices.Clone(s),
// append([]*lisp.LVal(nil), s...) or append([]*lisp.LVal{}, s...).  The
// string names the form for a message.
func (s *state) cloneSource(e ast.Expr) (ast.Expr, string) {
	call, ok := ast.Unparen(e).(*ast.CallExpr)
	if !ok {
		return nil, ""
	}
	if fn := s.callee(call); fn != nil && fn.Pkg() != nil && fn.Pkg().Path() == "slices" && fn.Name() == "Clone" &&
		len(call.Args) == 1 && s.cellSliceExpr(call.Args[0]) {
		return call.Args[0], "slices.Clone"
	}
	if !isBuiltinAppend(s, call) || len(call.Args) != 2 || !call.Ellipsis.IsValid() || !s.cellSliceExpr(call.Args[1]) {
		return nil, ""
	}
	switch base := ast.Unparen(call.Args[0]).(type) {
	case *ast.CompositeLit:
		if len(base.Elts) == 0 && s.cellSliceExpr(base) {
			return call.Args[1], "empty"
		}
	case *ast.CallExpr:
		if tv := s.pass.TypesInfo.Types[base.Fun]; tv.IsType() && isCellSlice(tv.Type) && len(base.Args) == 1 &&
			isNilIdent(s, base.Args[0]) {
			return call.Args[1], "nil"
		}
	}
	return nil, ""
}

// cloneNote is the difference a Clone fix makes for each form.
var cloneNote = map[string]string{
	"slices.Clone": "its capacity is exactly the length",
	"empty":        "its capacity is exactly the length, and it returns nil for a nil source",
	"nil":          "its capacity is exactly the length, and it returns an empty slice for an empty source",
}

// resultSafe reports whether the call e, whose type is a cell slice, may
// become a lisp.Cells.
func (s *state) resultSafe(e ast.Expr) bool {
	return s.isCellsType(e) || s.cellsSafe(e, 0)
}

// checkCloneCall: a clone of a cell slice becomes lisp.Cells(s).Clone().
func (s *state) checkCloneCall(call *ast.CallExpr) {
	if s.covered[call] {
		return
	}
	src, form := s.cloneSource(call)
	if src == nil || !s.resultSafe(call) || !s.fixAllowed(call, "Cells", "Cells.Clone") {
		return
	}
	q, ok := s.lispQual(call.Pos())
	if !ok {
		return
	}
	text := s.cellsOf(q, src) + ".Clone()"
	s.report(call, CategoryInfo, "use "+text+", which makes the same copy; "+cloneNote[form],
		replace(call, "Use Cells.Clone", text))
}

// checkAppendCall: append(clone of s, xs...) and append([]*lisp.LVal{a, b},
// s...) become lisp.Cells(s).Append(xs...) and lisp.Cells{a, b}.Append(s...).
func (s *state) checkAppendCall(call *ast.CallExpr) {
	if !isBuiltinAppend(s, call) || len(call.Args) < 2 || !s.cellSliceExpr(call) {
		return
	}
	var recv string
	q, ok := s.lispQual(call.Pos())
	if !ok {
		return
	}
	rest := call.Args[1:]
	note := "the same cells in one allocation of exact capacity"
	if src, _ := s.cloneSource(call.Args[0]); src != nil {
		recv = s.cellsOf(q, src)
		note += "; it returns an empty slice where append returns nil"
	} else if lit, ok := ast.Unparen(call.Args[0]).(*ast.CompositeLit); ok && len(lit.Elts) > 0 &&
		s.cellSliceExpr(lit) && len(rest) == 1 && call.Ellipsis.IsValid() {
		for _, e := range lit.Elts {
			if _, kv := e.(*ast.KeyValueExpr); kv {
				return
			}
		}
		elts, ok := s.source(lit.Lbrace+1, lit.Rbrace)
		if !ok {
			return
		}
		recv = q + "Cells{" + elts + "}"
	} else {
		return
	}
	if call.Ellipsis.IsValid() && !s.cellSliceExpr(rest[len(rest)-1]) {
		return
	}
	if !s.resultSafe(call) || !s.fixAllowed(call, "Cells", "Cells.Append") {
		return
	}
	args := make([]string, len(rest))
	for i, a := range rest {
		args[i] = s.text(a)
	}
	text := recv + ".Append(" + strings.Join(args, ", ")
	if call.Ellipsis.IsValid() {
		text += "..."
	}
	text += ")"
	ast.Inspect(call.Args[0], func(n ast.Node) bool {
		s.covered[n] = true
		return true
	})
	s.report(call, CategoryInfo, "use "+recv+".Append, which builds "+note, replace(call, "Use Cells.Append", text))
}

// checkCopyOnChange: a loop over a cell slice xs that leaves out nil until
// an element changes, then copies xs[:i] into out.  It is
// lisp.Cells(xs).MapIfChanged(f).
func (s *state) checkCopyOnChange(r *ast.RangeStmt) {
	key, ok := r.Key.(*ast.Ident)
	if !ok || !s.cellSliceExpr(r.X) {
		return
	}
	keyObj := s.pass.TypesInfo.ObjectOf(key)
	xs := s.text(r.X)
	var outObj types.Object
	ast.Inspect(r.Body, func(n ast.Node) bool {
		ifs, ok := n.(*ast.IfStmt)
		if !ok || outObj != nil {
			return outObj == nil
		}
		var cand types.Object
		ast.Inspect(ifs.Cond, func(m ast.Node) bool {
			if b, ok := m.(*ast.BinaryExpr); ok && b.Op == token.EQL && isNilIdent(s, b.Y) {
				if id, ok := ast.Unparen(b.X).(*ast.Ident); ok && s.cellSliceExpr(id) {
					cand = s.pass.TypesInfo.Uses[id]
				}
			}
			return cand == nil
		})
		if cand == nil {
			return true
		}
		ast.Inspect(ifs.Body, func(m ast.Node) bool {
			se, ok := m.(*ast.SliceExpr)
			if ok && se.Low == nil && se.High != nil && s.text(se.X) == xs {
				if h, ok := ast.Unparen(se.High).(*ast.Ident); ok && s.pass.TypesInfo.Uses[h] == keyObj {
					outObj = cand
				}
			}
			return outObj == nil
		})
		return outObj == nil
	})
	if outObj == nil {
		return
	}
	s.report(r, CategoryInfo, "this copy-on-change loop is "+s.cellsLabel(r.X)+".MapIfChanged(f): it returns "+xs+
		" itself and false when f returns every cell unchanged, and otherwise a fresh slice of exact length and true")
}

func (s *state) cellsLabel(e ast.Expr) string {
	if s.isCellsType(e) {
		return s.operand(e)
	}
	return "lisp.Cells(" + s.text(e) + ")"
}
