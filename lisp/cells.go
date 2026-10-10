// Copyright © 2026 The ELPS authors

package lisp

// Cells is a slice of values.  Go assigns it to and from []*LVal with no
// conversion, and it costs nothing at run time.  It lets Go code build a
// list or a vector as one expression:
//
//	lisp.Cells{lisp.String("info"), merged, lisp.String("generating exception")}.List()
//
// List, SExpr and Vector use the receiver as the new value's storage, as
// QExpr, SExpr and Vector do: they do not copy it.  So
// lisp.Cells(v.Cells).List() shares v's cells.  Build the slice fresh, or
// copy it first, when the result must not alias another value.
type Cells []*LVal

// List returns a list (a quoted s-expression) whose cells are c.  It is
// QExpr(c).
func (c Cells) List() *LVal {
	return QExpr(c)
}

// SExpr returns an unquoted s-expression whose cells are c.  It is
// SExpr(c).
func (c Cells) SExpr() *LVal {
	return SExpr(c)
}

// Vector returns a vector (a one-dimensional array) whose cells are c.  It
// is Vector(c).
func (c Cells) Vector() *LVal {
	return Vector(c)
}

// StringList returns a fresh list of the strings ss.  Each element is a new
// string value.  StringList makes no check; a builtin that sizes the list
// from its input checks the allocation cap first (see LEnv.CheckAlloc).
func StringList(ss []string) *LVal {
	cells := make([]*LVal, len(ss))
	vals := make([]LVal, len(ss))
	for i, s := range ss {
		vals[i] = LVal{Type: LString, Str: s}
		cells[i] = &vals[i]
	}
	return QExpr(cells)
}
