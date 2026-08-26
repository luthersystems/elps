// Copyright © 2018 The ELPS authors

package lisp

import "reflect"

// True interprets v as a boolean and returns the result.
//
// NOTE:  I don't like this name, really.  But I can't think of a better one.
func True(v *LVal) bool {
	if v.IsNil() {
		return false
	}
	if v.Type != LSymbol {
		return true
	}
	return v.Str != FalseSymbol
}

// Not interprets v as a boolean value and returns its negation.
func Not(v *LVal) bool {
	return !True(v)
}

// GoValue converts v to its natural representation in Go.  Quotes are ignored
// and all lists are turned into slices.  Symbols are converted to strings.
// The value Nil() is converted to nil.  Functions are returned as is.
//
// A bytes value is returned as a []byte that COPIES the lisp value's storage,
// so writing to it cannot be observed through the original (issue #548).  The
// cost is proportional to the length; see the LBytes arm of goValueNode for
// why the copy is not optional.  A native value is the opposite case and is
// returned BY REFERENCE: the payload is the embedder's own, so GoValue hands
// back what the caller already owns.
//
// NOTE:  These semantics may change.  It's unclear what the exact need is in
// corner cases.
func GoValue(v *LVal) interface{} {
	var st cycleState
	x := goValue(v, cycleGuard{state: &st})
	if st.cyclic {
		// A value that contains itself has no Go representation -- every
		// conversion of one is infinite -- so it is returned as the *LVal it
		// is, which is already what GoValue does for the types with no
		// natural Go form.  Without the bound the walk would overflow the
		// goroutine stack and kill the process; see lisp/cycle.go and issue
		// #390.  GoValue is not reachable from lisp, but an embedder holding
		// a value a program built is exactly the case #390 is about.
		return v
	}
	return x
}

func goValue(v *LVal, g cycleGuard) interface{} {
	if v.IsNil() {
		return nil
	}
	if !v.mayNest() {
		return v.goValueNode(g)
	}
	if g.abandoned() {
		// The result is discarded; GoValue is about to return v itself.
		return nil
	}
	g, cyclic := g.descend(v)
	if cyclic {
		return v
	}
	x := v.goValueNode(g)
	if g.tracking() {
		g.ascend(v)
	}
	return x
}

func (v *LVal) goValueNode(g cycleGuard) interface{} {
	switch v.Type {
	case LError:
		return (error)((*ErrorVal)(v))
	case LSymbol, LString:
		return v.Str
	case LBytes:
		// v.Bytes is a METHOD, not a field (lisp/lisp.go), so the obvious
		// `return v.Bytes` handed the embedder a func() []byte where every
		// other arm of this switch returns plain data.  It type-checks,
		// because the arm's result is interface{}, and only fails at use --
		// which is why it survived (issue #548).  libjson's Serializer had
		// the identical arm and the identical bug; both are fixed.
		//
		// The result is a COPY, and the reason is WHOSE DATA IT IS rather
		// than any blanket no-aliasing rule: GoValue's doc comment does not
		// promise one, and the LNative arm below hands its payload straight
		// back uncopied.  That is right for LNative -- the payload is the
		// EMBEDDER's, so returning it shares what they already own.  An
		// LBytes is the other case: its bytes live in a *[]byte under Native
		// so append! can grow them in place, so the storage is the
		// INTERPRETER's and lisp code observes writes through it.
		//
		// Two concrete failures, neither of which needs that framing to bite:
		//
		//   - append! REPLACES the slice header (builtinAppendMutate ->
		//     appendMutateBytes assigns through the *[]byte), so a live slice
		//     handed out before an append is silently STALE afterwards:
		//     right contents, wrong length, no error.
		//   - (slice 'bytes ...) mints a distinct LBytes over the SAME
		//     backing array, so a write through one live result corrupts its
		//     siblings.
		//
		// Ownership, not seal: sealableNodeType (lisp/seal.go) does not
		// include LBytes, so a bytes value is never sealed and only the
		// ownership invariant is in play.
		//
		// COST: O(len).  This is the only LEAF arm here that is unbounded --
		// NOT the only unbounded arm, which an earlier draft of this comment
		// claimed and which is wrong: LSExpr, LSortMap and LArray all scale
		// with their input and cost far more (a 65k-entry sorted map runs to
		// ~150ms and tens of megabytes).  What is unusual here is copying
		// PAYLOAD rather than building a container spine.
		//
		// Magnitudes on the authoring machine: tens of nanoseconds at 16
		// bytes; hundreds of microseconds and a megabyte allocated at a
		// megabyte; against single-digit nanoseconds and no allocation for
		// an LNative carrying the same megabyte.  Magnitudes rather than
		// figures on purpose -- the absolute numbers move by ~2x run to run
		// on a shared box -- so BenchmarkGoValueBytes is where they live.
		//
		// One amplification worth knowing: N references to ONE LBytes inside
		// a converted structure produce N copies, since each is converted
		// where it is reached.  Strings do not do this (v.Str is shared).
		//
		// The cost is affordable because of who calls this: GoValue has no
		// caller inside elps at all, so it is purely an embedder API, and a
		// copy is what an embedder handing the result to something that
		// outlives the call -- a logger, a queue, a serializer -- needs.
		b := v.Bytes()
		out := make([]byte, len(b))
		copy(out, b)
		return out
	case LInt:
		return v.Int
	case LFloat:
		return v.Float
	case LQuote:
		return goValue(v.Cells[0], g)
	case LSExpr:
		s, _ := goSlice(v, g)
		return s
	case LSortMap:
		m, _ := goMap(v, g)
		return m
	case LArray:
		dims, storage := v.Cells[0], v.Cells[1]
		s, _ := goSlice(SExpr(storage.Cells), g)
		switch dims.Len() {
		case 0:
			return s[0]
		case 1:
			return s
		default:
			// TODO:  Slice up the backing storage s to create a multidimensional
			// slice (e.g.  [][]interface{}).
			return v
		}
	case LNative:
		return v.Native
	case LInvalid, LQSymbol, LFun, LTaggedVal,
		LMarkTerminal, LMarkTailRec, LMarkMacExpand, LTypeMax:
		// Returned as the *LVal itself, which is the documented behaviour for
		// functions and the de-facto behaviour for everything else here.
		// Enumerated so a new LType has to decide whether it has a natural Go
		// representation instead of inheriting this one by accident.
		//
		// LTaggedVal is the interesting one: libjson's encoder unwraps a
		// tagged value to its user-data (encodeTaggedVal), so GoValue and the
		// JSON encoder disagree about it.  Left alone deliberately -- GoValue
		// is an exported API with outside callers.
		return v
	}
	return v
}

// GoError returns an error that represents v.  If v is not LError then nil is
// returned.
func GoError(v *LVal) error {
	if v.Type != LError {
		return nil
	}
	return (*ErrorVal)(v)
}

// GoString returns the string that v represents and the value true.  If v does
// not represent a string GoString returns a false second argument
func GoString(v *LVal) (string, bool) {
	if v.Type != LString {
		return "", false
	}
	return v.Str, true
}

// SymbolName returns the name of the symbol that v represents and the value
// true.  If v does not represent a symbol SymbolName returns a false second
// argument
func SymbolName(v *LVal) (string, bool) {
	if v.Type != LSymbol {
		return "", false
	}
	return v.Str, true
}

// GoInt converts the numeric value that v represents to and int and returns it
// with the value true.  If v does not represent a number GoInt returns a
// false second argument
func GoInt(v *LVal) (int, bool) {
	if !v.IsNumeric() {
		return 0, false
	}
	if v.Type == LFloat {
		return int(v.Float), true
	}
	return v.Int, true
}

// GoFloat64 converts the numeric value that v represents to a float64 and
// returns it with the value true.  If v does not represent a number GoFloat64
// returns a false second argument
func GoFloat64(v *LVal) (float64, bool) {
	if !v.IsNumeric() {
		return 0, false
	}
	if v.Type == LFloat {
		return v.Float, true
	}
	return float64(v.Int), true
}

// GoSlice returns the string that v represents and the value true.  If v does
// not represent a string GoSlice returns a false second argument
func GoSlice(v *LVal) ([]interface{}, bool) {
	var st cycleState
	vs, ok := goSlice(v, cycleGuard{state: &st})
	if st.cyclic {
		// See GoValue: a value that contains itself has no Go representation,
		// and "not representable" is what a false second argument means.
		return nil, false
	}
	return vs, ok
}

func goSlice(v *LVal, g cycleGuard) ([]interface{}, bool) {
	if v.Type != LSExpr {
		return nil, false
	}
	vs := make([]interface{}, len(v.Cells))
	for i := range vs {
		vs[i] = goValue(v.Cells[i], g)
	}
	return vs, true
}

// GoMap converts an LSortMap to its Go equivalent and returns it with a true
// second argument.  If v does not represent a map GoMap returns a false second
// argument.  Application's using custom Map implementations which allow
// arbitrary keys may not be able to construct a native Go map, in which case
// GoMap returns (nil, true).
func GoMap(v *LVal) (map[interface{}]interface{}, bool) {
	var st cycleState
	m, ok := goMap(v, cycleGuard{state: &st})
	if st.cyclic {
		// See GoValue: a value that contains itself has no Go representation.
		return nil, false
	}
	return m, ok
}

func goMap(v *LVal, g cycleGuard) (map[interface{}]interface{}, bool) {
	if v.Type != LSortMap {
		return nil, false
	}
	data := v.Map()
	m := make(gomap, data.Len())
	for _, pair := range sortedMapEntries(data).Cells {
		if !checkGoMapInsert(m, pair.Cells[0], pair.Cells[1], g) {
			return nil, true
		}
	}
	return m, true
}

func checkGoMapInsert(m gomap, lk, lv *LVal, g cycleGuard) (ok bool) {
	// k is definitely assignable to m's key type (interface{}) but map keys
	// must also be comparable which is not known without reflection on k's
	// type (or through recovering a failed map assignment).
	k := goValue(lk, g)
	if k == nil {
		// Either the walk has been abandoned -- goValue returns nil rather
		// than descending, and the caller is about to discard this map -- or
		// the key really converts to nil, which no Go map can hold as a key
		// either.  reflect.TypeOf(nil) is nil and panics on Comparable, so
		// this has to be checked before the reflection below.
		return false
	}
	if reflect.TypeOf(k).Comparable() {
		m[k] = goValue(lv, g)
		return true
	}
	return false
}

type gomap = map[interface{}]interface{}
