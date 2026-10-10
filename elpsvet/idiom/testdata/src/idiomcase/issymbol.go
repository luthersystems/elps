package idiomcase

import "github.com/luthersystems/elps/lisp"

const quoteName = "quote"

func isTrue(v *lisp.LVal) bool {
	return v.Type == lisp.LSymbol && v.Str == lisp.TrueSymbol // want `use v.IsSymbol\(lisp.TrueSymbol\), which is the same compare`
}

func isQuote(args *lisp.LVal, ok bool) bool {
	head := args
	return ok && head.Type == lisp.LSymbol && head.Str == quoteName && len(args.Cells) > 1 // want `use head.IsSymbol\(quoteName\)`
}

func isNamed(v *lisp.LVal) bool {
	return v.Str == "x" && v.Type == lisp.LSymbol // want `use v.IsSymbol\("x"\)`
}

// A selector chain reads the same value twice.
func isFirstSym(args *lisp.LVal) bool {
	return args.Cells != nil && args.Type == lisp.LSymbol && args.Str == "y" // want `use args.IsSymbol\("y"\)`
}

func indexed(args *lisp.LVal, name string) bool {
	return args.Cells[0].Type == lisp.LSymbol && args.Cells[0].Str == name // want `use args.Cells\[0\].IsSymbol\(name\)`
}

func notSymbol(v *lisp.LVal) bool {
	return v.Type != lisp.LSymbol || v.Str != "in-package" // want `use !v.IsSymbol\("in-package"\)`
}

func first(v *lisp.LVal) *lisp.LVal { return v }

func symName() string { return "x" }

// Not reported: a call operand or name, a variable index, two values, a
// test split by parentheses, a mixed chain and another type.
func symbolNoFix(args *lisp.LVal, i int, w *lisp.LVal, ok bool) bool {
	return (first(args).Type == lisp.LSymbol && first(args).Str == "x") ||
		(args.Type == lisp.LSymbol && args.Str == symName()) ||
		(args.Cells[i].Type == lisp.LSymbol && args.Cells[i].Str == "x") ||
		(args.Type == lisp.LSymbol && w.Str == "x") ||
		((ok && args.Type == lisp.LSymbol) && args.Str == "x") ||
		(args.Type != lisp.LSymbol && args.Str != "x") ||
		(args.Type == lisp.LString && args.Str == "x")
}
