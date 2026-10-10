// Copyright © 2026 The ELPS authors

package lisp

// Cells is a slice of values.  Go assigns it to and from []*LVal with no
// conversion, and it costs nothing at run time.  It lets Go code build a
// list or a vector as one expression:
//
//	lisp.Cells{lisp.String("info"), merged, lisp.String("generating exception")}.List()
//
// List and Vector use the receiver as the new value's storage, as QExpr and
// Vector do: they do not copy it.  So lisp.Cells(v.Cells).List() shares v's
// cells.  Build the slice fresh, or copy it first, when the result must not
// alias another value.
type Cells []*LVal

// List returns a list (a quoted s-expression) whose cells are c.  It is
// QExpr(c).
func (c Cells) List() *LVal {
	return QExpr(c)
}

// Vector returns a vector (a one-dimensional array) whose cells are c.  It
// is Vector(c).
func (c Cells) Vector() *LVal {
	return Vector(c)
}
