package idiomcase

import (
	"fmt"
	"slices"

	"github.com/luthersystems/elps/lisp"
)

func expand(v *lisp.LVal) *lisp.LVal { return v }

func expandN(v *lisp.LVal, n int) *lisp.LVal { return v }

func mapLoops(form *lisp.LVal, c lisp.Cells, n int) []*lisp.LVal {
	out := make([]*lisp.LVal, len(form.Cells)) // want `use lisp.Cells\(form.Cells\).Map, which builds the same slice; it returns nil for a nil form.Cells`
	for i, x := range form.Cells {
		out[i] = expandN(x, n)
	}
	plain := make([]*lisp.LVal, len(form.Cells)) // want `use lisp.Cells\(form.Cells\).Map`
	for i, x := range form.Cells {
		plain[i] = expand(x)
	}
	_ = plain
	fromCells := make([]*lisp.LVal, len(c)) // want `use c.Map`
	for i, x := range c {
		fromCells[i] = expand(x)
	}
	out = make([]*lisp.LVal, len(c)) // want `use c.Map`
	for i, x := range c {
		out[i] = lisp.String(x.Str)
	}
	return lisp.SExpr(fromCells).Cells
}

// Each of these loops does more than map, so it is not reported.
func mapLoopsKept(form *lisp.LVal, n int) any {
	usesIndex := make([]*lisp.LVal, len(form.Cells))
	for i, x := range form.Cells {
		usesIndex[i] = expandN(x, i)
	}
	readsOut := make([]*lisp.LVal, len(form.Cells))
	for i, x := range form.Cells {
		if i > 0 {
			readsOut[i] = readsOut[i-1]
			continue
		}
		readsOut[i] = x
	}
	readsOut2 := make([]*lisp.LVal, len(form.Cells))
	for i, x := range form.Cells {
		readsOut2[i] = expandN(x, len(readsOut2))
	}
	twoStatements := make([]*lisp.LVal, len(form.Cells))
	for i, x := range form.Cells {
		n++
		twoStatements[i] = x
	}
	otherSource := make([]*lisp.LVal, len(form.Cells))
	for i, x := range usesIndex {
		otherSource[i] = x
	}
	withCap := make([]*lisp.LVal, len(form.Cells), len(form.Cells)+1)
	for i, x := range form.Cells {
		withCap[i] = x
	}
	// An interface sees the slice's dynamic type, which a fix would change.
	asAny := make([]*lisp.LVal, len(form.Cells))
	for i, x := range form.Cells {
		asAny[i] = x
	}
	fmt.Println(asAny)
	return []any{usesIndex, readsOut, readsOut2, twoStatements, otherSource, withCap}
}

func clones(s []*lisp.LVal, c lisp.Cells) [][]*lisp.LVal {
	a := slices.Clone(s)                 // want `use lisp.Cells\(s\).Clone\(\), which makes the same copy; its capacity is exactly the length`
	b := append([]*lisp.LVal(nil), s...) // want `use lisp.Cells\(s\).Clone\(\), which makes the same copy; its capacity is exactly the length, and it returns an empty slice for an empty source`
	d := append([]*lisp.LVal{}, c...)    // want `use c.Clone\(\), which makes the same copy; its capacity is exactly the length, and it returns nil for a nil source`
	e := make([]*lisp.LVal, len(s))      // want `use lisp.Cells\(s\).Clone\(\), which makes the same copy; it returns nil for a nil s`
	copy(e, s)
	return [][]*lisp.LVal{a, b, d, e}
}

// Lookalikes that are not a plain clone.
func clonesKept(s []*lisp.LVal, ss []string) any {
	strs := slices.Clone(ss)
	prefix := append([]*lisp.LVal(nil), s[0])
	short := make([]*lisp.LVal, len(s))
	copy(short, s[1:])
	var boxed any = slices.Clone(s)
	return []any{strs, prefix, short, boxed}
}

func appends(s []*lisp.LVal, v, w *lisp.LVal) [][]*lisp.LVal {
	a := append(slices.Clone(s), v)                 // want `use lisp.Cells\(s\).Append, which builds the same cells in one allocation of exact capacity; it returns an empty slice where append returns nil`
	b := append(append([]*lisp.LVal{}, s...), s...) // want `use lisp.Cells\(s\).Append`
	c := append([]*lisp.LVal{v, w}, s...)           // want `use lisp.Cells\{v, w\}.Append, which builds the same cells in one allocation of exact capacity$`
	return [][]*lisp.LVal{a, b, c}
}

// Lookalikes that are not reported: an append onto a slice the code owns,
// and a literal prefix without a spread.
func appendsKept(s []*lisp.LVal, v *lisp.LVal) []*lisp.LVal {
	s = append(s, v)
	return append([]*lisp.LVal{v}, v, v)
}

func copyOnChange(form *lisp.LVal) *lisp.LVal {
	var out []*lisp.LVal
	for i, x := range form.Cells { // want `this copy-on-change loop is lisp.Cells\(form.Cells\).MapIfChanged\(f\)`
		y := expand(x)
		if y != x && out == nil {
			out = append([]*lisp.LVal{}, form.Cells[:i]...) // want `use lisp.Cells\(form.Cells\[:i\]\).Clone\(\)`
		}
		if out != nil {
			out = append(out, y)
		}
	}
	if out == nil {
		return form
	}
	return lisp.SExpr(out)
}
