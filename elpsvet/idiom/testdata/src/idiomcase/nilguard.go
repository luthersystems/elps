package idiomcase

import "github.com/luthersystems/elps/lisp"

// IsError and IsSymbol return false for a nil value, so a nil test before
// the compare joins the call.
func nilGuards(v, args *lisp.LVal, ok bool) bool {
	a := v != nil && v.Type == lisp.LError                         // want `use v.IsError\(\), which is the same test: it is false for a nil value`
	b := v != nil && v.IsError()                                   // want `use v.IsError\(\), which is the same test`
	c := v == nil || v.Type != lisp.LError                         // want `use !v.IsError\(\), which is the same test`
	d := v == nil || !v.IsError()                                  // want `use !v.IsError\(\), which is the same test`
	e := ok && v != nil && v.Type == lisp.LSymbol && v.Str == "x"  // want `use v.IsSymbol\("x"\), which is the same test`
	f := v != nil && v.IsSymbol(lisp.TrueSymbol)                   // want `use v.IsSymbol\(lisp.TrueSymbol\), which is the same test`
	g := v == nil || v.Type != lisp.LSymbol || v.Str != "y"        // want `use !v.IsSymbol\("y"\), which is the same test`
	h := nil != args.Cells[0] && args.Cells[0].Type == lisp.LError // want `use args.Cells\[0\].IsError\(\), which is the same test`
	return a || b || c || d || e || f || g || h
}

// Not reported: a nil test of another value, a call operand, and a nil test
// that does not guard the compare.
func nilGuardsNoFix(v, w *lisp.LVal) bool {
	return (v != nil && w.IsError()) ||
		(first(v) != nil && first(v).IsError()) ||
		(v == nil && v.IsError()) ||
		(v != nil || v.IsError())
}
