// Copyright © 2026 The ELPS authors

package lisp

import "strconv"

// CellReader reads a builtin's arguments in order, with no index and no
// subject string (luthersystems/elps#829).  Get one from Cells.Read:
//
//	r := lisp.Cells(args.Cells).Read(env)
//	collection, key := r.Name(), r.Name()
//	limit := r.OptInt(100)
//	if lerr := r.Err(); lerr.IsError() {
//		return lerr
//	}
//
// Each read takes the next cell.  The subject of a failure comes from its
// position, as Func1E, Func2E and Func3E name it: "argument" when there is
// one cell, otherwise "first argument", "second argument" and so on.  A
// failure raises condition argument-error (CondArgumentError), a child of
// error.  The first failure sticks: every later read returns a zero value
// without checking, and Err returns that failure.  A read past the last cell
// fails with "invalid number of arguments: <n>".
//
// A CellReader is a small value meant to live in the builtin's frame.  It
// allocates nothing and charges no step unless a read fails.  Values it
// returns that are slices ([]byte, Cells) share the argument's storage, so
// treat them as read-only, except Text of a string, which is a copy.
//
// Use Func1E, Func2E or Func3E for one to three required typed arguments,
// CellReader for optional or rest arguments, a raw *LVal or a receiver
// method, and ArgReader for a custom message per argument.
type CellReader struct {
	env   *LEnv
	err   *LVal
	cells Cells
	i     int
}

// Read returns a CellReader over the cells c, which are a builtin's
// arguments.
func (c Cells) Read(env *LEnv) CellReader {
	return CellReader{env: env, cells: c}
}

// Err returns the first failure, or Nil() when every read succeeded.
func (r *CellReader) Err() *LVal {
	if r.err == nil {
		return Nil()
	}
	return r.err
}

// ordinals are the subjects Func*E uses, by position.
var ordinals = [...]string{"first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth"}

// subject names argument i of n: "argument" when n is 1, else "first
// argument", ... "tenth argument", then "argument 11" and so on.
func subject(i, n int) string {
	if n == 1 {
		return "argument"
	}
	if i < len(ordinals) {
		return ordinals[i] + " argument"
	}
	return "argument " + strconv.Itoa(i+1)
}

// next returns the next cell, or nil after a failure or past the end.  When
// opt is false, a read past the end is a failure.
func (r *CellReader) next(opt bool) *LVal {
	if r.err != nil {
		return nil
	}
	if r.i >= len(r.cells) {
		if !opt {
			r.err = r.env.ErrorConditionf(CondArgumentError, "invalid number of arguments: %d", len(r.cells))
		}
		return nil
	}
	v := r.cells[r.i]
	r.i++
	return v
}

// fail records "<subject> is not <noun>: <type>" for v, the cell just read.
// A nil v (no cell: a failure is already recorded, or an optional read past
// the end) records nothing.
func (r *CellReader) fail(noun string, v *LVal) {
	if v == nil {
		return
	}
	r.err = r.env.ErrorConditionf(CondArgumentError, "%s is not %s: %v", subject(r.i-1, len(r.cells)), noun, v.Type)
}

// Each typed read below checks v.Type itself in its common case, a next cell
// of the right type, and leaves every other case to miss or optMiss.  It
// used to call valueAs, a generic type switch that does not inline, which
// cost several times the check on a hot builtin such as substrate's
// cc:storage-put (luthersystems/elps#841, BenchmarkCellReader).  The checks
// and conversions are valueAs's; TestCellReaderMatchesValueAs holds them
// equal.

// miss reads the next cell for a required read whose common case failed and
// records why: a failure already recorded, no cell left, or a cell that is
// not <noun>.  The error path stays out of each read's body.
func (r *CellReader) miss(noun string) {
	r.fail(noun, r.next(false))
}

// optMiss is miss for an optional read: an absent cell is no failure.  The
// read returns its default after it.
func (r *CellReader) optMiss(noun string) {
	if v := r.next(true); !absent(v) {
		r.fail(noun, v)
	}
}

// absent reports whether v, the cell next(true) returned, is absent: no
// cell, or ().
func absent(v *LVal) bool {
	return v == nil || v.IsNil()
}

// Value returns the next argument unchecked.
func (r *CellReader) Value() *LVal {
	v := r.next(false)
	if v == nil {
		return Nil()
	}
	return v
}

// Str returns the next argument, which must be a string.  It is not named
// String, so a CellReader is not a fmt.Stringer: printing a reader must
// not consume an argument.
func (r *CellReader) Str() string {
	if r.err == nil && r.i < len(r.cells) {
		if v := r.cells[r.i]; v.Type == LString {
			r.i++
			return v.Str
		}
	}
	r.miss("a string")
	return ""
}

// Name returns the text of the next argument, which must be a string or a
// symbol.
func (r *CellReader) Name() string {
	if r.err == nil && r.i < len(r.cells) {
		if v := r.cells[r.i]; v.Type == LString || v.Type == LSymbol {
			r.i++
			return v.Str
		}
	}
	r.miss(nameNoun)
	return ""
}

// Text returns the bytes of the next argument, which must be a string (the
// bytes are a copy) or bytes (the argument's own storage).
func (r *CellReader) Text() []byte {
	if r.err == nil && r.i < len(r.cells) {
		switch v := r.cells[r.i]; v.Type {
		case LString:
			r.i++
			return []byte(v.Str)
		case LBytes:
			r.i++
			return v.Bytes()
		}
	}
	r.miss(textNoun)
	return nil
}

// Int returns the next argument, which must be an integer.
func (r *CellReader) Int() int {
	if r.err == nil && r.i < len(r.cells) {
		if v := r.cells[r.i]; v.Type == LInt {
			r.i++
			return v.Int
		}
	}
	r.miss("an integer")
	return 0
}

// Float returns the next argument, which must be a number; an integer is
// converted.
func (r *CellReader) Float() float64 {
	if r.err == nil && r.i < len(r.cells) {
		switch v := r.cells[r.i]; v.Type {
		case LFloat:
			r.i++
			return v.Float
		case LInt:
			r.i++
			return float64(v.Int)
		}
	}
	r.miss("a number")
	return 0
}

// Bytes returns the next argument, which must be bytes.
func (r *CellReader) Bytes() []byte {
	if r.err == nil && r.i < len(r.cells) {
		if v := r.cells[r.i]; v.Type == LBytes {
			r.i++
			return v.Bytes()
		}
	}
	r.miss("bytes")
	return nil
}

// Map returns the next argument, which must be a sorted-map, as a MapView.
// After a failure it returns the zero MapView.
func (r *CellReader) Map() MapView {
	v := r.typed(LSortMap, "a sorted-map")
	if v.Type != LSortMap {
		return MapView{}
	}
	return MapView{v: v}
}

// Fun returns the next argument, which must be a function.
func (r *CellReader) Fun() *LVal { return r.typed(LFun, "a function") }

// typed reads the next cell, which must have type t.
func (r *CellReader) typed(t LType, noun string) *LVal {
	v := r.next(false)
	if v == nil {
		return Nil()
	}
	if v.Type != t {
		r.fail(noun, v)
		return Nil()
	}
	return v
}

// Seq returns the cells of the next argument, which must be a list or a
// one-dimensional vector.
func (r *CellReader) Seq() Cells {
	v := r.next(false)
	if v == nil {
		return nil
	}
	if !isSeq(v) {
		r.fail("a proper sequence", v)
		return nil
	}
	return seqCells(v)
}

// OptValue returns the next argument unchecked, or nil when it is absent.
func (r *CellReader) OptValue() *LVal {
	v := r.next(true)
	if v == nil {
		return Nil()
	}
	return v
}

// OptStr returns the next argument, which must be a string, or def when
// it is absent.
func (r *CellReader) OptStr(def string) string {
	if r.err == nil && r.i < len(r.cells) {
		if v := r.cells[r.i]; v.Type == LString {
			r.i++
			return v.Str
		}
	}
	r.optMiss("a string")
	return def
}

// OptName returns the text of the next argument, which must be a string or
// a symbol, or def when it is absent.
func (r *CellReader) OptName(def string) string {
	if r.err == nil && r.i < len(r.cells) {
		if v := r.cells[r.i]; v.Type == LString || v.Type == LSymbol {
			r.i++
			return v.Str
		}
	}
	r.optMiss(nameNoun)
	return def
}

// OptInt returns the next argument, which must be an integer, or def when it
// is absent.
func (r *CellReader) OptInt(def int) int {
	if r.err == nil && r.i < len(r.cells) {
		if v := r.cells[r.i]; v.Type == LInt {
			r.i++
			return v.Int
		}
	}
	r.optMiss("an integer")
	return def
}

// OptFloat returns the next argument, which must be a number, or def when it
// is absent.
func (r *CellReader) OptFloat(def float64) float64 {
	if r.err == nil && r.i < len(r.cells) {
		switch v := r.cells[r.i]; v.Type {
		case LFloat:
			r.i++
			return v.Float
		case LInt:
			r.i++
			return float64(v.Int)
		}
	}
	r.optMiss("a number")
	return def
}

// Rest returns the cells not read yet, which share the argument list's
// storage.  After a failure it returns nil.
func (r *CellReader) Rest() Cells {
	if r.err != nil || r.i >= len(r.cells) {
		return nil
	}
	rest := r.cells[r.i:]
	r.i = len(r.cells)
	return rest
}
