package idiomcase

import "github.com/luthersystems/elps/lisp"

func strList(ss []string) *lisp.LVal {
	var cells []*lisp.LVal
	for _, s := range ss { // want `lisp.StringList\(ss\) builds the same list of strings in one call`
		cells = append(cells, lisp.String(s))
	}
	return lisp.QExpr(cells)
}

func strListIndex(ss []string) *lisp.LVal {
	cells := make([]*lisp.LVal, len(ss))
	for i, s := range ss { // want `lisp.StringList\(ss\)`
		cells[i] = lisp.String(s)
	}
	return lisp.Cells(cells).List()
}

// The slice becomes a vector, or the loop does more: no hint.
func strListOther(ss []string) (*lisp.LVal, *lisp.LVal) {
	var cells, more []*lisp.LVal
	for _, s := range ss {
		cells = append(cells, lisp.String(s))
	}
	for _, s := range ss {
		more = append(more, lisp.String(s+"!"))
	}
	return lisp.Vector(cells), lisp.QExpr(more)
}
