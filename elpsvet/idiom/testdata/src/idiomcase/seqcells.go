package idiomcase

import "github.com/luthersystems/elps/lisp"

func cellsSwitch(v *lisp.LVal) []*lisp.LVal {
	switch { // want `v.SeqCells\(\) \(or lisp.SeqOf\[T\]\(v\)\) returns the cells of a list or a one-dimensional vector`
	case v == nil:
		return nil
	case v.Type == lisp.LSExpr:
		return v.Cells
	case v.Type == lisp.LArray:
		out := make([]*lisp.LVal, v.Len())
		for i := range out {
			out[i] = v.ArrayIndex(lisp.Int(i))
		}
		return out
	}
	return nil
}

func cellsTagSwitch(params *lisp.LVal) []*lisp.LVal {
	var pos []*lisp.LVal
	switch params.Type { // want `params.SeqCells\(\)`
	case lisp.LSExpr:
		pos = append(pos, params.Cells...)
	case lisp.LArray:
		for i := range params.Len() {
			pos = append(pos, params.ArrayIndex(lisp.Int(i)))
		}
	}
	return pos
}

func cellsIfChain(v *lisp.LVal) []*lisp.LVal {
	var cells []*lisp.LVal
	if v.Type == lisp.LSExpr { // want `v.SeqCells\(\)`
		cells = v.Cells
	} else if v.Type == lisp.LArray && v.Len() > 0 {
		for i := 0; i < v.Len(); i++ {
			cells = append(cells, v.ArrayIndex(lisp.Int(i)))
		}
	}
	return cells
}

// An array arm alone is not the pattern.
func arrayOnly(v *lisp.LVal) []*lisp.LVal {
	var cells []*lisp.LVal
	if v.Type == lisp.LArray {
		for i := range v.Len() {
			cells = append(cells, v.ArrayIndex(lisp.Int(i)))
		}
	}
	return cells
}

// The arms read different values.
func otherValues(v, w *lisp.LVal) []*lisp.LVal {
	var cells []*lisp.LVal
	switch {
	case v.Type == lisp.LSExpr:
		cells = v.Cells
	case w.Type == lisp.LArray:
		for i := range w.Len() {
			cells = append(cells, w.ArrayIndex(lisp.Int(i)))
		}
	}
	return cells
}

// A test under || does not select the type.
func orTest(v *lisp.LVal, ok bool) []*lisp.LVal {
	var cells []*lisp.LVal
	switch {
	case v.Type == lisp.LSExpr || ok:
		cells = v.Cells
	case v.Type == lisp.LArray:
		for i := range v.Len() {
			cells = append(cells, v.ArrayIndex(lisp.Int(i)))
		}
	}
	return cells
}
