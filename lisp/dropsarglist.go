// Copyright © 2026 The ELPS authors

package lisp

// DropsArgList marks a Go builtin whose argument list is dead once it
// returns, so the evaluator can reuse the list of each direct call
// (luthersystems/elps#944).  It is opt-in, per builtin, and changes no
// result, step count or error text: it only saves the allocation of the
// list.
//
// The evaluator builds one list for each call form: a header and the cells
// that hold the evaluated arguments.  A builtin may keep that list (list
// returns it), so without the mark the list is left to the collector.  With
// the mark, the evaluator gives the list back to the VM when the call
// returns, and the next call form of a marked or kernel builtin uses the
// same storage.
//
// A marked builtin must keep neither the list nor a slice of its Cells past
// the call.  It may read, evaluate, return or store single cells: a cell is
// a value the list does not own.  It must not return the list, store it or a
// slice of its cells, capture either in a closure, or hand either to code
// that may keep it.  elpsvet's elpscalllist rule checks each builtin that
// elps itself marks.  A host that marks its own builtin makes the same
// promise; a build with the elpscheck tag fills each returned list with
// invalid values, so a test of a builtin that broke the promise reads them.
//
// The mark applies to a builtin or special operator registered through
// AddBuiltins, BindBuiltins, RegisterBuiltins or AddSpecialOps.  A macro's
// list is never reused.  It composes with FreeKeywords in either order.
//
// The returned definition wraps def, as FreeKeywords does: a type assertion
// to def's concrete type fails on it.  Name, Formals, Eval and Docstring
// forward to def.
func DropsArgList(def LBuiltinDef) LBuiltinDef {
	return dropsArgListDef{def}
}

type dropsArgListDef struct {
	LBuiltinDef
}

// Docstring forwards the wrapped definition's docstring.
func (d dropsArgListDef) Docstring() string {
	return builtinDocstring(d.LBuiltinDef)
}

// dropsArgListMarker is unexported so only DropsArgList can opt a builtin
// in.
func (dropsArgListDef) dropsArgListMarker() {}

func (d dropsArgListDef) wrappedDef() LBuiltinDef { return d.LBuiltinDef }

type dropsArgListBuiltin interface {
	dropsArgListMarker()
}

// wrappingDef is a definition made by DropsArgList or FreeKeywords.  Each
// mark is found through the other wrapper, so the two compose in either
// order.
type wrappingDef interface {
	wrappedDef() LBuiltinDef
}

// markedDef reports whether def, or a definition it wraps, satisfies has.
func markedDef(def LBuiltinDef, has func(LBuiltinDef) bool) bool {
	for def != nil {
		if has(def) {
			return true
		}
		w, ok := def.(wrappingDef)
		if !ok {
			return false
		}
		def = w.wrappedDef()
	}
	return false
}

func isDropsArgListDef(def LBuiltinDef) bool {
	_, ok := def.(dropsArgListBuiltin)
	return ok
}
