// Package idiomfixonly is the -fixonly fixture: only a diagnostic that
// carries a fix is reported.
package idiomfixonly

import "github.com/luthersystems/elps/lisp"

func isErr(v *lisp.LVal) bool {
	return v.Type == lisp.LError // want `use v.IsError\(\)`
}

// A hint is not reported under -fixonly.
func keys(m *lisp.LVal) int {
	n := 0
	if m.Type == lisp.LSortMap {
		for range m.MapKeys().Cells {
			n++
		}
	}
	return n
}

// A mistake has no fix and is not reported under -fixonly.
func errorVal() *lisp.ErrorVal {
	return nil
}

// A Cells fix is reported under -fixonly; the copy-on-change hint is not.
func cells(form *lisp.LVal) *lisp.LVal {
	out := make([]*lisp.LVal, len(form.Cells)) // want `use lisp.Cells\(form.Cells\).Map`
	for i, x := range form.Cells {
		out[i] = lisp.String(x.Str)
	}
	var changed []*lisp.LVal
	for i, x := range out {
		if x != form && changed == nil {
			changed = append([]*lisp.LVal(nil), out[:i]...) // want `use lisp.Cells\(out\[:i\]\).Clone\(\)`
		}
	}
	return lisp.SExpr(changed)
}
