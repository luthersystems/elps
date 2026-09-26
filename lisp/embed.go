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
// cost is proportional to the length; see the LBytes arm of conversionLeaf for
// why the copy is not optional.  A native value is the opposite case and is
// returned BY REFERENCE: the payload is the embedder's own, so GoValue hands
// back what the caller already owns.
//
// Excessive nesting returns an ordinary *ErrorVal implementing error, using
// MaxValueDepth. No partial converted container is returned. Cycles discovered
// within the cap retain the historical return of the original *LVal.
//
// GoValue takes no Runtime, so it cannot read a WithMaxValueDepth override and
// always walks to MaxValueDepth. A caller holding the runtime whose values
// these are uses GoValueWithRuntime instead.
//
// NOTE:  These semantics may change.  It's unclear what the exact need is in
// corner cases.
func GoValue(v *LVal) any {
	// Same body as GoValueWithRuntime with the default limit spelled as the
	// constant: this is the hot conversion entry point (BenchmarkGoValueBytes
	// measures it at tens of nanoseconds), and routing it through the
	// runtime-taking form costs two call frames and a nil-receiver method
	// call on every leaf.
	if v != nil && v.Type == LNative {
		return v.Native
	}
	out, ok := convertValue(v, MaxValueDepth)
	if !ok {
		return v
	}
	return out
}

// GoValueWithRuntime is GoValue bounded by rt's configured value-depth limit
// (Runtime.ValueDepthLimit). A nil runtime is MaxValueDepth, which is what
// GoValue passes.
//
// The runtime is a parameter rather than a field read off the value because a
// *LVal carries no runtime: the same value is reachable from every environment
// that was handed it, and only the caller knows which runtime's budget applies.
func GoValueWithRuntime(rt *Runtime, v *LVal) any {
	// Opaque native payloads are already Go values; even leaf conversion
	// dispatch is unnecessary here.
	if v != nil && v.Type == LNative {
		return v.Native
	}
	out, ok := convertValue(v, rt.ValueDepthLimit())
	if !ok {
		return v
	}
	return out
}

func convertValue(v *LVal, limit int) (any, bool) {
	if v.IsNil() {
		return nil, true
	}
	switch v.Type {
	case LQuote, LSExpr, LArray, LSortMap:
		return convertContainer(v, limit)
	default:
		// Leaves must not create an addressable result slot or walk state.
		return conversionLeaf(v), true
	}
}

// conversionFrame holds output under construction, not a memo of source
// identities.  height is the number of levels the walk has gone below v so
// far, for the sharing memo below.
type conversionFrame struct {
	v        *LVal
	mapping  map[any]any
	kv       [2]any
	children []*LVal
	values   []any
	index    int
	height   int
	invalid  bool
}

// conversionMemo is one container's entry in convertContainer's sharing
// memo: its conversion, and the levels the conversion walked below it, so
// that a hit at depth d fails the depth limit exactly where re-converting
// would -- when d+height reaches it.
type conversionMemo struct {
	out    any
	height int
}

// convertContainer converts v and everything under it.
//
// SHARING (lisp/sharing.go).  A value built as (set! x (list x x)) D times
// has D containers and 2^D paths, and converting it as a tree built 2^D Go
// containers.  The walk counts its work -- containers plus the children
// they hold -- and past sharedWalkBudget it memoises each container it
// finishes, by identity, and hands back the same conversion when the
// container is reached again: the Go value then shares what the LVal
// shared.  A tree never reaches a container twice, so its conversion is
// unchanged, down to every slice and map being distinct.
func convertContainer(v *LVal, limit int) (any, bool) {
	// One reusable continuation per ancestor, never one per sibling.
	pending := make([]conversionFrame, 0, 16)
	var path map[*LVal]bool
	var memo map[*LVal]conversionMemo
	work := 0
walk:
	for {
		if len(pending) >= limit {
			return (*ErrorVal)(Error(ValueDepthError(limit))), true
		}
		var out any
		// outHeight is the levels converted below out's source: 0 for a
		// leaf or an empty container.
		outHeight := 0
		f := conversionFrame{v: v}
		container := true
		if m, ok := memo[v]; ok {
			// Converted already, reached again through sharing.
			if len(pending)+m.height >= limit {
				return (*ErrorVal)(Error(ValueDepthError(limit))), true
			}
			out, outHeight = m.out, m.height
		} else if !v.IsNil() {
			switch v.Type {
			case LQuote:
				f.children = v.Cells[:1]
			case LSExpr:
				f.children = v.Cells
			case LArray:
				if v.Cells[0].Len() > 1 {
					container = false
					out = v
					break
				}
				f.children = v.Cells[1].Cells
			case LSortMap:
				md := v.Map()
				if md.mapBacking == nil {
					// Degenerate MapData with no implementation (possible
					// via SortedMapFromData(NewMapData(nil))).  The other
					// two value walkers each carry this arm -- copier.mapData
					// has `case nil:` and detachMapData checks
					// md.mapBacking == nil -- and without it the walk called
					// sortedMapEntries, whose first act is a Len() method
					// call on the nil Map, so GoValue panicked with a nil
					// pointer dereference.  A backing-less map holds no
					// entries, so it converts to the same empty Go map an
					// ordinary empty sorted-map converts to.
					f.mapping = make(map[any]any)
					out = f.mapping
					break
				}
				entries := sortedMapEntries(md)
				if entries.Type == LError {
					return (*ErrorVal)(entries), true
				}
				for _, pair := range entries.Cells {
					if len(pair.Cells) != 2 {
						return nil, false
					}
				}
				f.children = entries.Cells
				f.mapping = make(map[any]any, len(entries.Cells))
				out = f.mapping
			default:
				container = false
				out = conversionLeaf(v)
			}
			if container {
				work += 1 + len(f.children)
				if memo == nil && work > sharedWalkBudget {
					memo = make(map[*LVal]conversionMemo)
				}
				if len(pending) >= 64 {
					if path == nil {
						path = make(map[*LVal]bool)
					}
					if path[v] {
						return nil, false
					}
					path[v] = true
				}
				if v.Type != LSortMap && v.Type != LQuote && (v.Type != LArray || v.Cells[0].Len() != 0) {
					// f.values doubles as the SNAPSHOT of this container's
					// children: each slot starts out holding the source
					// child and is replaced by that child's conversion as
					// the walk passes it (a slot is read immediately before
					// it is written, below), so the walk never reads a
					// child back out of the source's own backing array.
					//
					// f.children used to serve that purpose, and it is a
					// slice header over the source's cells, not a copy of
					// them.  The walk runs host code -- a custom Map's
					// Entries, through sortedMapEntries -- so a hook that
					// wrote into a cell the walk had not reached yet had
					// that write picked up, and the conversion was neither
					// the container as it was nor as the hook left it.  A
					// *LVal stored in an interface costs no allocation, so
					// the snapshot is free (TestGoValueContainerAllocations
					// pins that).  The copier and the detacher agree:
					// a walk converts the children a container held when
					// the walker entered it.
					f.values = make([]any, len(f.children))
					for i, child := range f.children {
						f.values[i] = child
					}
				}
				if len(f.children) > 0 {
					pending = append(pending, f)
					if f.values != nil {
						v = f.values[0].(*LVal)
					} else {
						v = f.children[0]
						if f.mapping != nil {
							v = v.Cells[0]
						}
					}
					continue
				}
				// Only an empty container reaches here; a container with
				// children is boxed once, in the pop arm below. Boxing it
				// here as well cost one extra allocation per container.
				if f.values != nil {
					out = f.values
				}
				delete(path, v)
			}
		}
		for len(pending) > 0 {
			f := &pending[len(pending)-1]
			if outHeight >= f.height {
				f.height = outHeight + 1
			}
			switch {
			case f.mapping != nil:
				f.kv[f.index%2] = out
				f.index++
				if f.index%2 == 0 {
					kv, m := f.kv, f.mapping
					if kv[0] != nil && reflect.ValueOf(kv[0]).Comparable() {
						m[kv[0]] = kv[1]
					} else {
						f.invalid = true
					}
				}
				if f.index < 2*len(f.children) {
					v = f.children[f.index/2].Cells[f.index%2]
					continue walk
				}
				if f.invalid {
					out = map[any]any(nil)
				} else {
					out = f.mapping
				}
			case f.values != nil:
				// Read the next child out of the snapshot the slot still
				// holds, then overwrite that slot with this child's
				// conversion; f.values is the output once the cursor runs
				// off the end.
				f.values[f.index] = out
				f.index++
				if f.index < len(f.values) {
					v = f.values[f.index].(*LVal)
					continue walk
				}
				out = f.values
			}
			outHeight = f.height
			if memo != nil {
				memo[f.v] = conversionMemo{out: out, height: outHeight}
			}
			delete(path, f.v)
			*f = conversionFrame{}
			pending = pending[:len(pending)-1]
		}
		return out, true
	}
}

func conversionLeaf(v *LVal) any {
	switch v.Type {
	case LError:
		return (*ErrorVal)(v)
	case LSymbol, LString:
		return v.Str
	case LBytes:
		b := v.Bytes()
		out := make([]byte, len(b))
		copy(out, b)
		return out
	case LInt:
		return v.Int
	case LFloat:
		return v.Float
	case LNative:
		return v.Native
	default:
		return v
	}
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

// GoSlice converts a list to a Go slice. Non-lists, cycles and walks exceeding
// MaxValueDepth return (nil, false).  Like GoValue it has no runtime and so
// cannot read a WithMaxValueDepth override; GoSliceWithRuntime does.
func GoSlice(v *LVal) ([]any, bool) {
	return GoSliceWithRuntime(nil, v)
}

// GoSliceWithRuntime is GoSlice bounded by rt's configured value-depth limit.
func GoSliceWithRuntime(rt *Runtime, v *LVal) ([]any, bool) {
	if v.Type != LSExpr {
		return nil, false
	}
	out, ok := convertValue(v, rt.ValueDepthLimit())
	if !ok {
		return nil, false
	}
	if v.IsNil() {
		return []any{}, true
	}
	values, ok := out.([]any)
	return values, ok
}

// GoMap converts an LSortMap to its Go equivalent and returns it with a true
// second argument.  If v does not represent a map GoMap returns a false second
// argument.  Application's using custom Map implementations which allow
// arbitrary keys may not be able to construct a native Go map, in which case
// GoMap returns (nil, true). Cycles and excessive nesting return (nil, false).
// Like GoValue it has no runtime and so cannot read a WithMaxValueDepth
// override; GoMapWithRuntime does.
func GoMap(v *LVal) (map[any]any, bool) {
	return GoMapWithRuntime(nil, v)
}

// GoMapWithRuntime is GoMap bounded by rt's configured value-depth limit.
func GoMapWithRuntime(rt *Runtime, v *LVal) (map[any]any, bool) {
	if v.Type != LSortMap {
		return nil, false
	}
	out, ok := convertValue(v, rt.ValueDepthLimit())
	if !ok {
		return nil, false
	}
	values, ok := out.(map[any]any)
	return values, ok
}

// GoSliceOf is GoSlice with every element asserted to T (elps#690).  It
// returns (nil, false) wherever GoSlice does, and also when any element's
// GoValue conversion is not a T; there is no partial result.
//
// T is matched against what GoValue produces for each element, not against
// the lisp type: a string or symbol is a string, an int an int, a float a
// float64, a nested list a []interface{}, a sorted-map a
// map[interface{}]interface{}, a native value its own payload, and nil (the
// empty list) the untyped nil, which only an interface T accepts.  A value
// GoValue returns as is -- a function, for example -- is a *LVal, so
// GoSliceOf[*LVal] collects those and nothing else; it is not a way to get
// the list's cells back unconverted.
//
// Like GoSlice it has no runtime and walks to MaxValueDepth;
// GoSliceOfWithRuntime reads a WithMaxValueDepth override.
func GoSliceOf[T any](v *LVal) ([]T, bool) {
	return GoSliceOfWithRuntime[T](nil, v)
}

// GoSliceOfWithRuntime is GoSliceOf bounded by rt's configured value-depth
// limit.
func GoSliceOfWithRuntime[T any](rt *Runtime, v *LVal) ([]T, bool) {
	values, ok := GoSliceWithRuntime(rt, v)
	if !ok {
		return nil, false
	}
	out := make([]T, len(values))
	for i, x := range values {
		t, ok := assertConverted[T](x)
		if !ok {
			return nil, false
		}
		out[i] = t
	}
	return out, true
}

// GoMapOf is GoMap with every key asserted to K and every value to V
// (elps#690).  Keys and values are matched against what GoValue produces, as
// for GoSliceOf; a sorted-map's keys are strings.  It returns (nil, false)
// wherever GoMap does and on the first key or value of the wrong type, and
// (nil, true) where GoMap does, for a custom map with no Go equivalent.
//
// Like GoMap it has no runtime and walks to MaxValueDepth; GoMapOfWithRuntime
// reads a WithMaxValueDepth override.
func GoMapOf[K comparable, V any](v *LVal) (map[K]V, bool) {
	return GoMapOfWithRuntime[K, V](nil, v)
}

// GoMapOfWithRuntime is GoMapOf bounded by rt's configured value-depth limit.
func GoMapOfWithRuntime[K comparable, V any](rt *Runtime, v *LVal) (map[K]V, bool) {
	values, ok := GoMapWithRuntime(rt, v)
	if !ok {
		return nil, false
	}
	if values == nil {
		return nil, true
	}
	out := make(map[K]V, len(values))
	for k, x := range values {
		kt, ok := assertConverted[K](k)
		if !ok {
			return nil, false
		}
		vt, ok := assertConverted[V](x)
		if !ok {
			return nil, false
		}
		out[kt] = vt
	}
	return out, true
}

// assertConverted asserts a GoValue result to T.  A nil result (the empty
// list) is accepted only by an interface T, whose zero value is that same
// nil; a plain x.(T) refuses nil for every T, interfaces included.
func assertConverted[T any](x interface{}) (T, bool) {
	if x == nil {
		var zero T
		return zero, interface{}(zero) == nil
	}
	t, ok := x.(T)
	return t, ok
}
