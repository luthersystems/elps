package idiomcase

import "github.com/luthersystems/elps/lisp"

func arrayLen(v *lisp.LVal) int {
	if v.Type == lisp.LArray {
		return len(v.Cells[1].Cells) // want `dims, data := v.ArrayParts\(\) reads the array's lists`
	}
	return 0
}

func arrayRank(v *lisp.LVal) int {
	switch v.Type {
	case lisp.LArray:
		return v.Cells[0].Len() // want `use dims for v.Cells\[0\]`
	}
	return 0
}

func arrayData(v *lisp.LVal) *lisp.LVal {
	if v.Type != lisp.LArray {
		return nil
	}
	return v.Cells[1] // want `use data for v.Cells\[1\]`
}

func newArray(dims, data *lisp.LVal) *lisp.LVal {
	return &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{dims, data}} // want `build an array with lisp.Vector or lisp.Array`
}

// Not reported: a list read, a read with no array test, and a third cell.
func notArray(v *lisp.LVal) *lisp.LVal {
	if v.Type == lisp.LSExpr {
		return v.Cells[1]
	}
	if v.Type == lisp.LArray {
		return v.Cells[2]
	}
	return v.Cells[0]
}
