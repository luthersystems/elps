package idiomcase

import "github.com/luthersystems/elps/lisp"

func entriesIf(v *lisp.LVal) (out []*lisp.LVal) {
	if v.Type == lisp.LSortMap {
		for _, kv := range v.MapEntries().Cells { // want `for k, v := range v.All\(\) walks the entries`
			out = append(out, kv.Cells[1])
		}
	}
	return out
}

func entriesShape(v *lisp.LVal) (n int) {
	switch lisp.ShapeOf(v.Type) {
	case lisp.ShapeMap:
		for range v.MapEntries().Cells { // want `range v.All\(\)`
			n++
		}
	}
	return n
}

func entriesCase(v *lisp.LVal) (n int) {
	switch v.Type {
	case lisp.LSortMap:
		for range v.MapEntries().Cells { // want `range v.All\(\)`
			n++
		}
	}
	return n
}

// Without a map check, All would hide MapEntries' panic: no hint.
func entriesNoCheck(v *lisp.LVal) (n int) {
	for range v.MapEntries().Cells {
		n++
	}
	switch v.Type {
	case lisp.LString:
		for range v.MapEntries().Cells {
			n++
		}
	}
	return n
}
