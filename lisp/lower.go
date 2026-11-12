// Copyright © 2026 The ELPS authors

package lisp

import "strings"

// specialOpLowerings maps a special operator of package lisp to its
// lowering: the same form written in other forms, as the macro that the
// operator replaced expanded it.
//
// Why: a tool that compiles ELPS code (substrate's defflow compiler, which
// hoists each await out of a call's arguments) models a fixed set of forms.
// When elps turns a macro into a special operator, such a tool sees an
// unknown form where it used to see the macro's expansion, and must either
// learn the new form or refuse it.  A lowering lets the tool ask for the
// old expansion instead (CodeWalker.Lower, macroexpand-all :lower).
//
// Add a lowering when a macro becomes a special operator that evaluates an
// argument only in some cases.  Each one takes the operator's argument list
// and returns the expansion, or an LError for a wrong argument count.
var specialOpLowerings = map[string]func(args *LVal) *LVal{
	"get-default": lowerGetDefault,
}

// lowerGetDefault is get-default as the macro of elps v1.96.0 expanded it:
// the map and the key are bound once, and the default is evaluated only
// when the key is missing.  The generated names come from the arguments
// (NewGenSyms), so they are the macro's names.
func lowerGetDefault(args *LVal) *LVal {
	if len(args.Cells) != 3 {
		return Errorf("lisp:get-default: invalid number of arguments: %d", len(args.Cells))
	}
	g := NewGenSyms(args)
	mapSym, keySym := g.Symbol("map"), g.Symbol("key")
	return Cells{
		Symbol("lisp:let"),
		Cells{
			Cells{mapSym, args.Cells[0]}.SExpr(),
			Cells{keySym, args.Cells[1]}.SExpr(),
		}.SExpr(),
		Cells{
			Symbol("lisp:if"),
			Cells{
				Symbol("lisp:if"),
				Cells{Symbol("lisp:nil?"), mapSym}.SExpr(),
				Symbol("lisp:false"),
				Cells{Symbol("lisp:key?"), mapSym, keySym}.SExpr(),
			}.SExpr(),
			Cells{Symbol("lisp:get"), mapSym, keySym}.SExpr(),
			args.Cells[2],
		}.SExpr(),
	}.SExpr()
}

// HasLowering reports whether the special operator of package lisp named
// name has a lowering.  name may carry the lisp: prefix, as a form's head
// may.
func HasLowering(name string) bool {
	return specialOpLowerings[strings.TrimPrefix(name, DefaultLangPackage+":")] != nil
}
