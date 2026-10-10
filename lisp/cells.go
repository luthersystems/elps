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
// copy it first, when the result must not alias another value.  Map, Clone
// and Append always return fresh storage, and MapIfChanged does when it
// reports a change, so their results chain into List, SExpr and Vector:
//
//	lisp.Cells(args).Append(bookmark).List()
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

// Map returns a fresh slice holding f(x) for each cell x of c, in order.  Its
// length and capacity are len(c).  A nil c gives nil, and an empty non-nil c
// gives an empty non-nil slice.  Map never aliases c, so the result may
// become a new value's storage:
//
//	lisp.Cells(form.Cells).Map(expand).SExpr()
//
// Map makes no allocation check and charges no step; f does what it does.
func (c Cells) Map(f func(*LVal) *LVal) Cells {
	if c == nil {
		return nil
	}
	out := make(Cells, len(c))
	for i, x := range c {
		out[i] = f(x)
	}
	return out
}

// MapIfChanged is Map that copies only when it must.  When f returns each
// cell itself (pointer-equal), MapIfChanged returns c, the original slice,
// and false, having allocated nothing.  Otherwise it returns a fresh slice
// of length and capacity len(c), with the unchanged prefix copied from c,
// and true.  The input is never written.  This is the copy-on-change loop a
// code walker writes by hand:
//
//	out, changed := lisp.Cells(form.Cells).MapIfChanged(rewrite)
//	if !changed {
//		return form
//	}
//	return out.SExpr()
//
// When it reports false the result is c itself, so do not use it as the
// storage of a new value then.
func (c Cells) MapIfChanged(f func(*LVal) *LVal) (Cells, bool) {
	for i, x := range c {
		y := f(x)
		if y == x {
			continue
		}
		out := make(Cells, len(c))
		copy(out, c[:i])
		out[i] = y
		for j := i + 1; j < len(c); j++ {
			out[j] = f(c[j])
		}
		return out, true
	}
	return c, false
}

// Clone returns a fresh copy of c whose length and capacity are both len(c).
// A nil c gives nil, and an empty non-nil c gives an empty non-nil slice.
// It copies the cell pointers, not the values they point to.
//
// slices.Clone and append([]*LVal(nil), c...) may round the capacity up, so
// a later append to their result can write past len(c) without a copy;
// Clone's exact capacity makes any append copy.
func (c Cells) Clone() Cells {
	if c == nil {
		return nil
	}
	out := make(Cells, len(c))
	copy(out, c)
	return out
}

// Append returns a fresh slice holding the cells of c followed by xs, of
// length and capacity len(c)+len(xs).  Unlike the append builtin it never
// writes c's backing array, even when c has spare capacity, so c and any
// value that shares it are unchanged.  It is one allocation, where
// append(slices.Clone(c), xs...) is two.  A nil c with no xs gives an empty
// non-nil slice.
func (c Cells) Append(xs ...*LVal) Cells {
	out := make(Cells, len(c)+len(xs))
	copy(out, c)
	copy(out[len(c):], xs)
	return out
}

// Strings returns the text of every cell of c, which must all be strings,
// in a fresh slice of exact length and capacity.  When a cell is not a
// string it returns nil and that cell, so the caller writes its own message:
//
//	parts, bad := lisp.Cells(args.Cells[1:]).Strings()
//	if bad != nil {
//		return env.Errorf("docstring argument is not a string: %v", bad.Type)
//	}
//
// A symbol is not a string here.  An empty c gives nil and nil.  It makes
// no check beyond the types and charges no step.
func (c Cells) Strings() ([]string, *LVal) {
	if len(c) == 0 {
		return nil, nil
	}
	for _, x := range c {
		if x.Type != LString {
			return nil, x
		}
	}
	out := make([]string, len(c))
	for i, x := range c {
		out[i] = x.Str
	}
	return out, nil
}
