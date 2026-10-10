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
