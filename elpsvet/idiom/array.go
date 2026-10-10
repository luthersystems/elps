// Copyright © 2026 The ELPS authors

package idiom

import (
	"fmt"
	"go/ast"
	"go/constant"
)

// The array layout (Cells[0] is the dimension list, Cells[1] is the data
// list) belongs to package lisp.  Outside it, these hints name the methods
// that read and write the layout.

// checkArrayCells: x.Cells[0] or x.Cells[1] after an x.Type == lisp.LArray
// test.
func (s *state) checkArrayCells(idx *ast.IndexExpr, stack []ast.Node) {
	if s.inLisp {
		return
	}
	sel, ok := ast.Unparen(idx.X).(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Cells" || s.typeFieldOwner(sel) == nil {
		return
	}
	tv, ok := s.pass.TypesInfo.Types[idx.Index]
	if !ok || tv.Value == nil || tv.Value.Kind() != constant.Int {
		return
	}
	n, exact := constant.Int64Val(tv.Value)
	if !exact || (n != 0 && n != 1) {
		return
	}
	x := s.text(sel.X)
	if !s.typeGuarded(stack, x, "LArray") {
		return
	}
	part := "dims"
	if n == 1 {
		part = "data"
	}
	s.report(idx, CategoryInfo, fmt.Sprintf("dims, data := %s.ArrayParts() reads the array's lists without the cell "+
		"layout; use %s for %s.Cells[%d], and SetArrayData to replace the data; this is a hint, not a fix",
		s.operand(sel.X), part, x, n))
}

// checkArrayLit: a lisp.LVal literal with Type: lisp.LArray.
func (s *state) checkArrayLit(lit *ast.CompositeLit) {
	if s.inLisp || !isLispNamed(s.pass.TypesInfo.TypeOf(lit), "LVal") {
		return
	}
	for _, elt := range lit.Elts {
		kv, ok := elt.(*ast.KeyValueExpr)
		if !ok {
			continue
		}
		if key, isID := kv.Key.(*ast.Ident); isID && key.Name == "Type" && s.isLispConst(kv.Value, "LArray") {
			s.report(lit, CategoryInfo, "build an array with lisp.Vector or lisp.Array; for an array that must exist "+
				"before its contents (a back-reference), use lisp.Vector(nil) or lisp.Array(nil, nil) and fill it with "+
				"SetArrayData; this is a hint, not a fix")
			return
		}
	}
}
