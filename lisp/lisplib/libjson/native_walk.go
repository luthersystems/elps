// Copyright © 2026 The ELPS authors

package libjson

import (
	"encoding"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"math"
	"math/big"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"sync"

	"github.com/luthersystems/elps/lisp"
)

// nativeNestLimit is the deepest encoding/json may recurse into a native,
// counted in the levels it recurses through: each value reached through a
// pointer, an interface, or a struct, map, slice or array element is one
// level deeper than its parent.
//
// encoding/json recurses on the Go stack, and a goroutine that outgrows its
// maximum stack kills the process (luthersystems/elps#802). Measured on Go
// 1.26, a level costs 430 to 710 bytes of stack, so this limit holds a native
// encode to about 35 MB of stack. It is far above anything a native that loads
// back can reach: the decoder refuses JSON nested past jsonNestingLimit, and
// a JSON level is at most three of these levels (an interface holding a
// pointer to a container).
const nativeNestLimit = 50_000

// jsonNestingLimit is the nesting depth encoding/json's decoder refuses past,
// which makes it the deepest native checkLoadable accepts.
const jsonNestingLimit = 10_000

// jsonCycleCheckAfter is encoding/json's startDetectingCyclesAfter: it starts
// to record pointers, maps and slices once it is this many of them deep.
const jsonCycleCheckAfter = 1000

// nativeRefScan is how many open references the walk searches linearly for a
// cycle before it indexes them in a map.
const nativeRefScan = 32

// nativeWalkEvent is why a nativeWalker stopped or flagged a value.
type nativeWalkEvent uint8

const (
	nativeWalkOK nativeWalkEvent = iota
	// nativeWalkJSONError marks a value encoding/json refuses with an error
	// of its own: an unsupported type, a NaN, an invalid json.Number, a map
	// key it cannot name, a failing marshaler, or a cycle.
	nativeWalkJSONError
	// nativeWalkTooBig marks a native whose output would pass the
	// allocation cap.
	nativeWalkTooBig
	// nativeWalkTooDeep marks a native that nests past nativeNestLimit.
	nativeWalkTooDeep
	// nativeWalkCycle marks a cycle encoding/json would only report after
	// nesting past nativeNestLimit or writing past the cap.
	nativeWalkCycle
)

// nativeWalkKey identifies a pointer, map or slice as encoding/json's cycle
// check does: a pointer by its type and address, a map by its address, and a
// slice by the address and length of its elements.
type nativeWalkKey struct {
	typ reflect.Type
	ptr uintptr
	len int
}

// nativeWalkRef is one pointer, map or slice open on the walk's current path.
type nativeWalkRef struct {
	typ   reflect.Type
	key   nativeWalkKey
	depth int
	est   int
}

// nativeFrameKind is the kind of container a nativeFrame works through.
type nativeFrameKind uint8

const (
	frameArray    nativeFrameKind = iota + 1 // v is a slice or array
	frameStruct                              // v is a struct, written field by field
	frameMap                                 // v is a map, written entry by entry
	frameAnySlice                            // anys is a []any
	frameAnyMap                              // anyMap is a map[string]any
)

// nativeMapEntry is one entry of a map written by reflection.
type nativeMapEntry struct {
	key, val reflect.Value
	name     string // the key's name; resolved only by the ordered walk
}

// nativeFrame is one container the walk is inside of.  levels and refs are
// the levels and references the walk opened to reach and enter it, closed
// again when it is finished.
type nativeFrame struct {
	v       reflect.Value
	anys    []any
	anyMap  map[string]any
	fields  []nativeField
	entries []nativeMapEntry
	keys    []string // frameAnyMap: its keys, in the order to write them
	i       int
	levels  int
	refs    int
	kind    nativeFrameKind
	wrote   bool // frameStruct: a member has been written
}

// nativeWalker bounds a native before encoding/json marshals it
// (luthersystems/elps#802).
//
// It visits the values encoding/json would visit, by the same rules: the same
// Marshaler and TextMarshaler dispatch (including addressability), the same
// struct fields (nativeTypeFields), the same omitempty and omitzero tests and
// the same cycle detection. It writes nothing, and it computes:
//
//   - est, a lower bound on the bytes encoding/json writes. A Marshaler's
//     output is unknown until it runs, so it counts as its smallest possible
//     form, except for the types whose size is known (see marshaler).
//   - the nesting depth, against nativeNestLimit.
//
// So a native is refused BEFORE encoding/json allocates its output or recurses
// into it, when its output cannot fit the cap or its nesting could exhaust the
// stack. Every other native, including every native encoding/json refuses
// with an error of its own, goes to encoding/json as before, so its bytes and
// error text do not change.
//
// The walk does not recurse: it keeps the containers it is inside of on an
// explicit stack, so its own stack does not grow with the native.  It charges
// steps per KiB of est as it goes, and polls the context, so a step budget or
// a cancellation stops it partway.
//
// There are two walks, and both stop at nativeNestLimit.  The bounding walk
// (ordered false) runs on every native.  It visits map entries in any order
// and calls no marshaler.  When it finds a reason to refuse the native, the
// ordered walk follows encoding/json's own order instead, to find what
// encoding/json would have met first: an error of its own, which encoding/json
// then reports, or the reason to refuse.
type nativeWalker struct {
	enc      *encoder
	err      error
	cycleErr error
	budget   encodeBudget

	// The containers the walk is inside of: the first len(framesArr) in
	// framesArr, the rest in framesMore.  Indexed rather than sliced, so the
	// walker holds no pointer into itself and stays on the stack.
	framesMore []nativeFrame

	// The pointers, maps and slices open on the current path, stored the
	// same way.  refIndex indexes them all once there are more than
	// nativeRefScan.
	refsMore []nativeWalkRef
	refIndex map[nativeWalkKey]int
	refsArr  [8]nativeWalkRef

	framesArr [8]nativeFrame
	nframes   int
	nrefs     int

	// base is the output length when the walk started; est counts from it.
	base  int
	est   int
	limit int // est past limit passes the cap
	poll  int // est at which to charge steps and poll the context next
	nodes int // values visited, to poll the context between bytes

	depth     int
	maxDepth  int
	jsonDepth int

	// deepBracket is the bracket that opens the first JSON container nested
	// past jsonNestingLimit, in document order.  It names the character in
	// the decoder's error, which checkLoadable would have reported.
	deepBracket byte

	event nativeWalkEvent

	// ordered walks map keys in encoding/json's sorted order, calls the
	// marshalers it passes, and stops at the first event.
	ordered bool

	// mayFailFirst records that the bounding walk passed a value encoding/json
	// refuses, or a marshaler that may fail, either of which may come before
	// a later event in encoding/json's order.
	mayFailFirst bool
}

// nativeWalkPollBytes is how many bytes of est the walk covers between
// charges and context polls.
const nativeWalkPollBytes = 1024

func (w *nativeWalker) init(enc *encoder, b encodeBudget, ordered bool) {
	w.enc, w.budget, w.ordered = enc, b, ordered
	w.base = enc.buf.Len()
	w.limit = math.MaxInt
	if b.maxBytes > 0 {
		w.limit = b.maxBytes - w.base
	}
	w.poll = nativeWalkPollBytes
}

// stop reports whether the walk must end now.
func (w *nativeWalker) stop() bool {
	return w.err != nil || (w.event != nativeWalkOK && (w.ordered || w.event != nativeWalkJSONError))
}

// setEvent records e.  The ordered walk keeps its first event, which is the
// first problem in encoding/json's order.  The bounding walk lets a reason to
// refuse replace a JSON error, which may not come first.
func (w *nativeWalker) setEvent(e nativeWalkEvent) {
	if w.event == nativeWalkOK || (!w.ordered && w.event == nativeWalkJSONError) {
		w.event = e
	}
}

// add counts n more output bytes.
func (w *nativeWalker) add(n int) {
	w.est += n
	if w.est > w.limit {
		w.setEvent(nativeWalkTooBig)
		return
	}
	if w.est >= w.poll {
		w.poll = w.est + nativeWalkPollBytes
		w.pollContext()
		if w.err == nil {
			if err := w.enc.chargeKiB(w.budget, w.base+w.est); err != nil {
				w.err = err
			}
		}
	}
}

func (w *nativeWalker) pollContext() {
	if w.budget.ctx != nil {
		if err := w.budget.ctx.Err(); err != nil {
			w.err = encodeCancelledError{err}
		}
	}
}

// jsonError records a value encoding/json refuses.
func (w *nativeWalker) jsonError() {
	w.mayFailFirst = true
	if w.event == nativeWalkOK {
		w.event = nativeWalkJSONError
	}
}

// descend enters one level deeper, or records that the native nests past
// nativeNestLimit.
func (w *nativeWalker) descend() bool {
	if w.depth >= nativeNestLimit {
		w.setEvent(nativeWalkTooDeep)
		return false
	}
	w.depth++
	w.maxDepth = max(w.maxDepth, w.depth)
	if w.nodes++; w.nodes%encodeContextInterval == 0 {
		w.pollContext()
	}
	return true
}

// release closes levels levels and refs references.
func (w *nativeWalker) release(levels, refs int) {
	w.depth -= levels
	for range refs {
		w.leaveRef()
	}
}

// run walks root, the value of a native LVal, at the top level.
func (w *nativeWalker) run(root any) {
	if w.descend() {
		w.anyValue(root, 1, 0)
	}
	for w.nframes > 0 && !w.stop() {
		w.step()
	}
}

// step writes the next member of the innermost container, or finishes it.
func (w *nativeWalker) step() {
	f := w.frame(w.nframes - 1)
	switch f.kind {
	case frameArray:
		if f.i == f.v.Len() {
			w.pop()
			return
		}
		elem := f.v.Index(f.i)
		f.i++
		if w.descend() {
			w.value(elem, false, 1, 0)
		}
	case frameAnySlice:
		if f.i == len(f.anys) {
			w.pop()
			return
		}
		elem := f.anys[f.i]
		f.i++
		w.anyElem(elem)
	case frameAnyMap:
		if f.i == len(f.keys) {
			w.pop()
			return
		}
		k := f.keys[f.i]
		f.i++
		w.add(jsonStringLen(k) + 1)
		w.anyElem(f.anyMap[k])
	case frameMap:
		if f.i == len(f.entries) {
			w.pop()
			return
		}
		e := f.entries[f.i]
		f.i++
		if w.ordered {
			w.add(jsonStringLen(e.name) + 1)
		} else {
			w.add(nativeKeyLen(e.key) + 1)
		}
		if w.descend() {
			w.value(e.val, false, 1, 0)
		}
	case frameStruct:
		for f.i < len(f.fields) {
			fd := &f.fields[f.i]
			f.i++
			fv, ok := nativeFieldValue(f.v, fd)
			if !ok {
				continue
			}
			if f.wrote {
				w.add(1)
			}
			f.wrote = true
			w.add(fd.nameLen)
			if w.descend() {
				w.value(fv, fd.quoted, 1, 0)
			}
			return
		}
		w.pop()
	}
}

// nativeFieldValue is the value of field f of struct v, and whether
// encoding/json writes it: not when an embedded pointer on its path is nil,
// and not when omitempty or omitzero omits it.
func nativeFieldValue(v reflect.Value, f *nativeField) (reflect.Value, bool) {
	fv := v
	for _, i := range f.index {
		if fv.Kind() == reflect.Pointer {
			if fv.IsNil() {
				return reflect.Value{}, false
			}
			fv = fv.Elem()
		}
		fv = fv.Field(i)
	}
	if (f.omitEmpty && isNativeEmptyValue(fv)) ||
		(f.omitZero && (f.isZero == nil && fv.IsZero() || (f.isZero != nil && f.isZero(fv)))) {
		return reflect.Value{}, false
	}
	return fv, true
}

// push enters container f.  It opens a JSON container, so its bracket and
// its first byte of punctuation are counted here.
func (w *nativeWalker) push(f nativeFrame, bracket byte) {
	w.jsonDepth++
	if w.jsonDepth == jsonNestingLimit+1 && w.deepBracket == 0 {
		w.deepBracket = bracket
	}
	w.add(2)
	if w.nframes < len(w.framesArr) {
		w.framesArr[w.nframes] = f
	} else {
		w.framesMore = append(w.framesMore, f)
	}
	w.nframes++
}

// pop finishes the innermost container.
func (w *nativeWalker) pop() {
	w.nframes--
	var f nativeFrame
	if w.nframes < len(w.framesArr) {
		f, w.framesArr[w.nframes] = w.framesArr[w.nframes], nativeFrame{}
	} else {
		f = w.framesMore[w.nframes-len(w.framesArr)]
		w.framesMore = w.framesMore[:w.nframes-len(w.framesArr)]
	}
	w.jsonDepth--
	w.release(f.levels, f.refs)
}

func (w *nativeWalker) frame(i int) *nativeFrame {
	if i < len(w.framesArr) {
		return &w.framesArr[i]
	}
	return &w.framesMore[i-len(w.framesArr)]
}

// anyElem visits e, a []any element or map[string]any value: the interface
// is one level, and the value it holds the next.
func (w *nativeWalker) anyElem(e any) {
	if !w.descend() {
		return
	}
	if e == nil {
		w.add(4)
		w.release(1, 0)
		return
	}
	if w.descend() {
		w.anyValue(e, 2, 0)
	} else {
		w.release(1, 0)
	}
}

// anyValue visits x, whose level the caller has entered.  It handles the
// types encoding/json's own decoder produces without reflection, which is
// what most natives are made of, and counts them exactly as value would.
// levels and refs are what this visit has opened so far.
func (w *nativeWalker) anyValue(x any, levels, refs int) {
	switch t := x.(type) {
	case nil:
		w.add(4)
	case string:
		w.add(jsonStringLen(t))
	case float64:
		if math.IsInf(t, 0) || math.IsNaN(t) {
			w.jsonError()
		} else {
			w.add(nativeFloatLen(t, 64))
		}
	case bool:
		w.add(boolLen(t))
	case map[string]any:
		if t == nil {
			w.add(4)
			break
		}
		if !w.enterRef(reflect.TypeOf(x), nativeWalkKey{ptr: reflect.ValueOf(x).Pointer()}) {
			break
		}
		keys := make([]string, 0, len(t))
		for k := range t {
			keys = append(keys, k)
		}
		if w.ordered {
			slices.Sort(keys)
		}
		w.add(max(0, len(t)-1))
		w.push(nativeFrame{kind: frameAnyMap, anyMap: t, keys: keys, levels: levels, refs: refs + 1}, '{')
		return
	case []any:
		if t == nil {
			w.add(4)
			break
		}
		if !w.enterRef(reflect.TypeOf(x), nativeWalkKey{ptr: reflect.ValueOf(x).Pointer(), len: len(t)}) {
			break
		}
		w.add(max(0, len(t)-1))
		w.push(nativeFrame{kind: frameAnySlice, anys: t, levels: levels, refs: refs + 1}, '[')
		return
	default:
		w.value(reflect.ValueOf(x), false, levels, refs)
		return
	}
	w.release(levels, refs)
}

// value visits v, whose level the caller has entered.  It counts a leaf, or
// enters a container for step to work through; either way the levels and
// references it opened are closed when v is finished.
func (w *nativeWalker) value(v reflect.Value, quoted bool, levels, refs int) {
	for {
		if !v.IsValid() {
			w.add(4) // null
			break
		}
		info := nativeInfo(v.Type())
		allowAddr := true
		if info.ptrMarshaler {
			if v.CanAddr() {
				w.marshaler(v, true, false)
				break
			}
			allowAddr = false
		}
		if info.marshaler {
			w.marshaler(v, false, false)
			break
		}
		if allowAddr && info.ptrTextMarshaler && v.CanAddr() {
			w.marshaler(v, true, true)
			break
		}
		if info.textMarshaler {
			w.marshaler(v, false, true)
			break
		}
		q := 0
		if quoted {
			q = 2
		}
		switch v.Kind() {
		case reflect.Bool:
			w.add(boolLen(v.Bool()) + q)
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			var b [24]byte
			w.add(len(strconv.AppendInt(b[:0], v.Int(), 10)) + q)
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
			var b [24]byte
			w.add(len(strconv.AppendUint(b[:0], v.Uint(), 10)) + q)
		case reflect.Float32, reflect.Float64:
			f := v.Float()
			if math.IsInf(f, 0) || math.IsNaN(f) {
				w.jsonError()
				break
			}
			w.add(nativeFloatLen(f, v.Type().Bits()) + q)
		case reflect.String:
			s := v.String()
			if v.Type() != jsonNumberType {
				w.add(jsonStringLen(s) + q)
				break
			}
			if s == "" {
				s = "0"
			}
			if !isValidJSONNumber(s) {
				w.jsonError()
				break
			}
			w.add(len(s) + q)
		case reflect.Interface:
			if v.IsNil() {
				w.add(4)
				break
			}
			if !w.descend() {
				break
			}
			if !quoted && v.CanInterface() {
				w.anyValue(v.Interface(), levels+1, refs)
				return
			}
			levels++
			v = v.Elem()
			continue
		case reflect.Pointer:
			if v.IsNil() {
				w.add(4)
				break
			}
			if !w.enterRef(v.Type(), nativeWalkKey{typ: v.Type(), ptr: v.Pointer()}) {
				break
			}
			refs++
			if !w.descend() {
				break
			}
			levels++
			v = v.Elem()
			continue
		case reflect.Struct:
			w.push(nativeFrame{kind: frameStruct, v: v, fields: info.fields, levels: levels, refs: refs}, '{')
			return
		case reflect.Map:
			if !info.mapKeyOK {
				// encoding/json refuses the map's type, even when the map is
				// nil.
				w.jsonError()
				break
			}
			if v.IsNil() {
				w.add(4)
				break
			}
			if !w.enterRef(v.Type(), nativeWalkKey{ptr: v.Pointer()}) {
				break
			}
			entries, ok := w.mapEntries(v)
			if !ok {
				w.leaveRef()
				break
			}
			w.add(max(0, len(entries)-1))
			w.push(nativeFrame{kind: frameMap, v: v, entries: entries, levels: levels, refs: refs + 1}, '{')
			return
		case reflect.Slice:
			if v.IsNil() {
				w.add(4)
				break
			}
			if info.byteSlice {
				w.add(base64.StdEncoding.EncodedLen(v.Len()) + 2)
				break
			}
			if !w.enterRef(v.Type(), nativeWalkKey{ptr: v.Pointer(), len: v.Len()}) {
				break
			}
			w.add(max(0, v.Len()-1))
			w.push(nativeFrame{kind: frameArray, v: v, levels: levels, refs: refs + 1}, '[')
			return
		case reflect.Array:
			w.add(max(0, v.Len()-1))
			w.push(nativeFrame{kind: frameArray, v: v, levels: levels, refs: refs}, '[')
			return
		default:
			// Complex numbers, channels, functions and unsafe pointers.
			w.jsonError()
		}
		break
	}
	w.release(levels, refs)
}

func boolLen(b bool) int {
	if b {
		return 4
	}
	return 5
}

// mapEntries returns the entries of map v.  The ordered walk names each key,
// which may call its MarshalText, and sorts them by name as encoding/json
// does; it reports false at a key encoding/json cannot name.
func (w *nativeWalker) mapEntries(v reflect.Value) ([]nativeMapEntry, bool) {
	entries := make([]nativeMapEntry, 0, v.Len())
	it := v.MapRange()
	for it.Next() {
		e := nativeMapEntry{key: it.Key(), val: it.Value()}
		if w.ordered {
			name, err := resolveNativeKeyName(e.key)
			if err != nil {
				w.jsonError()
				return nil, false
			}
			e.name = name
		}
		entries = append(entries, e)
	}
	if w.ordered {
		slices.SortFunc(entries, func(a, b nativeMapEntry) int { return strings.Compare(a.name, b.name) })
	}
	return entries, true
}

// resolveNativeKeyName is encoding/json's resolveKeyName.
func resolveNativeKeyName(k reflect.Value) (string, error) {
	if k.Kind() == reflect.String {
		return k.String(), nil
	}
	if tm, ok := k.Interface().(encoding.TextMarshaler); ok {
		if k.Kind() == reflect.Pointer && k.IsNil() {
			return "", nil
		}
		buf, err := tm.MarshalText()
		return string(buf), err
	}
	switch k.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return strconv.FormatInt(k.Int(), 10), nil
	default:
		return strconv.FormatUint(k.Uint(), 10), nil
	}
}

// nativeKeyLen is a lower bound on a map key's quoted name, without calling
// MarshalText.
func nativeKeyLen(k reflect.Value) int {
	if k.Kind() == reflect.String {
		return jsonStringLen(k.String())
	}
	if nativeInfo(k.Type()).textMarshaler {
		return 2
	}
	var b [24]byte
	switch k.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return len(strconv.AppendInt(b[:0], k.Int(), 10)) + 2
	default:
		return len(strconv.AppendUint(b[:0], k.Uint(), 10)) + 2
	}
}

// enterRef opens the pointer, map or slice of type typ identified by key on
// the current path.  It returns false, and opens nothing, when the value is
// already open: it contains itself.  leaveRef closes the last one opened.
func (w *nativeWalker) enterRef(typ reflect.Type, key nativeWalkKey) bool {
	if i := w.findRef(key); i >= 0 {
		w.cycle(i)
		return false
	}
	r := nativeWalkRef{typ: typ, key: key, depth: w.depth, est: w.est}
	if w.nrefs < len(w.refsArr) {
		w.refsArr[w.nrefs] = r
	} else {
		w.refsMore = append(w.refsMore, r)
	}
	w.nrefs++
	if w.nrefs > nativeRefScan {
		if w.refIndex == nil {
			w.refIndex = make(map[nativeWalkKey]int, 2*nativeRefScan)
			for i := range w.nrefs - 1 {
				w.refIndex[w.ref(i).key] = i
			}
		}
		w.refIndex[key] = w.nrefs - 1
	}
	return true
}

func (w *nativeWalker) leaveRef() {
	w.nrefs--
	if w.refIndex != nil {
		delete(w.refIndex, w.ref(w.nrefs).key)
	}
	if w.nrefs >= len(w.refsArr) {
		w.refsMore = w.refsMore[:w.nrefs-len(w.refsArr)]
	}
}

// ref returns the i-th open reference, counting from the root.
func (w *nativeWalker) ref(i int) nativeWalkRef {
	if i < len(w.refsArr) {
		return w.refsArr[i]
	}
	return w.refsMore[i-len(w.refsArr)]
}

func (w *nativeWalker) findRef(key nativeWalkKey) int {
	if w.refIndex != nil {
		if i, ok := w.refIndex[key]; ok {
			return i
		}
		return -1
	}
	for i := range min(w.nrefs, len(w.refsArr)) {
		if w.refsArr[i].key == key {
			return i
		}
	}
	return -1
}

// cycle handles a value that contains itself: w.ref(i) is about to be
// entered again.
//
// encoding/json does not see the cycle here.  It records references only
// once more than jsonCycleCheckAfter are open, so it goes around the cycle
// until then, writing every trip, and reports the cycle one trip after it
// starts recording.  The walk stops at the first repeat instead, and works out
// from the path how deep and how large encoding/json gets before it reports
// the cycle, and through which value.  When both fit, encoding/json is left to
// report it.  Otherwise the walk reports the same error without running it.
func (w *nativeWalker) cycle(i int) {
	n := w.nrefs
	period := n - i // references per trip around the cycle
	// The n-th reference encoding/json opens is the one about to repeat.
	// It reports the cycle at the first reference it opens twice after it
	// starts recording, at the jsonCycleCheckAfter-th.
	at := max(jsonCycleCheckAfter, i) + period
	trips := (at - i) / period
	r := w.ref(i + (at-i)%period)
	tripDepth := w.depth - w.ref(i).depth
	tripEst := w.est - w.ref(i).est
	depth := r.depth + trips*tripDepth + max(0, w.maxDepth-w.depth)
	est := r.est + trips*tripEst
	if est <= w.limit && depth <= nativeNestLimit {
		w.jsonError()
		return
	}
	w.mayFailFirst = true
	if w.event == nativeWalkOK || (!w.ordered && w.event == nativeWalkJSONError) {
		w.event = nativeWalkCycle
		w.cycleErr = &json.UnsupportedValueError{Str: "encountered a cycle via " + r.typ.String()}
	}
}

// marshaler counts a value encoding/json hands to MarshalJSON (text false)
// or MarshalText (text true), with v.Addr() as the receiver when addr is set.
//
// The bounding walk does not call marshalers.  Most marshalers' output is
// unknown until they run, so they count as their smallest output: one byte of
// JSON, or an empty string.  The few whose size is known from the value --
// this package's own messages, json.RawMessage and math/big's numbers --
// count as that size.  math/big also does work out of proportion to its
// output, a decimal conversion whose intermediate grows with the exponent,
// and that is charged and capped here before it runs.
//
// The ordered walk runs only when the native is about to be refused, and must
// know whether encoding/json would fail at a marshaler first.  So it calls
// each marshaler it passes, as encoding/json would have, and counts the
// output it returns.  math/big's never fail and are still not called.
func (w *nativeWalker) marshaler(v reflect.Value, addr, text bool) {
	m := v
	if addr {
		m = v.Addr()
	}
	if (m.Kind() == reflect.Pointer || m.Kind() == reflect.Interface) && m.IsNil() {
		w.add(4) // null
		return
	}
	if m.Kind() == reflect.Interface {
		m = m.Elem()
	}
	minLen := 1
	if text {
		minLen = 2
	}
	if !sizedMarshalerTypes[m.Type()] {
		w.mayFailFirst = true
		if w.ordered {
			w.callMarshaler(m, text)
			return
		}
		w.add(minLen)
		return
	}
	switch x := m.Interface().(type) {
	case *ownMessage:
		w.add(max(len(x.msg), 1))
	case *json.RawMessage:
		w.rawMessage(*x)
	case json.RawMessage:
		w.rawMessage(x)
	case *big.Int:
		w.convert(bigIntLen(x))
		w.add(bigIntLen(x))
	case *big.Float:
		w.convert(bigFloatWork(x))
		w.add(minLen)
	case *big.Rat:
		n := bigIntLen(x.Num())
		if !x.IsInt() {
			n += 1 + bigIntLen(x.Denom())
		}
		w.convert(n)
		w.add(n + 2)
	default:
		w.add(minLen)
	}
}

// sizedMarshalerTypes are the marshalers whose output size, or conversion
// cost, marshaler reads from the value.  Other marshalers are not converted
// to an interface, which would allocate a copy of a value receiver.
var sizedMarshalerTypes = map[reflect.Type]bool{
	reflect.TypeFor[*ownMessage]():      true,
	reflect.TypeFor[*json.RawMessage](): true,
	reflect.TypeFor[json.RawMessage]():  true,
	reflect.TypeFor[*big.Int]():         true,
	reflect.TypeFor[*big.Float]():       true,
	reflect.TypeFor[*big.Rat]():         true,
}

// rawMessage counts a json.RawMessage, whose MarshalJSON returns it as is.
// encoding/json fails on one that is not valid JSON, which the ordered walk
// checks.
func (w *nativeWalker) rawMessage(raw json.RawMessage) {
	w.mayFailFirst = true
	if w.ordered && raw != nil && !json.Valid(raw) {
		w.jsonError()
		return
	}
	w.add(rawMessageLen(raw))
}

// callMarshaler calls m's MarshalJSON or MarshalText.  It records a JSON
// error where encoding/json would report one: an error from the method, or
// MarshalJSON output that is not valid JSON.
func (w *nativeWalker) callMarshaler(m reflect.Value, text bool) {
	if text {
		tm, ok := m.Interface().(encoding.TextMarshaler)
		if !ok {
			w.add(4) // encoding/json writes null
			return
		}
		b, err := tm.MarshalText()
		if err != nil {
			w.jsonError()
			return
		}
		w.add(jsonStringLen(string(b)))
		return
	}
	jm, ok := m.Interface().(json.Marshaler)
	if !ok {
		w.add(4)
		return
	}
	b, err := jm.MarshalJSON()
	if err != nil || !json.Valid(b) {
		w.jsonError()
		return
	}
	w.add(rawMessageLen(b))
}

// convert refuses, and charges for, a conversion that allocates n bytes of
// intermediate work that are not output.
func (w *nativeWalker) convert(n int) {
	if n > w.limit-w.est {
		w.setEvent(nativeWalkTooBig)
		return
	}
	if n >= 1024 && w.enc.env != nil {
		if lerr := w.enc.env.ChargeSteps(int64(n >> 10)); lerr.Type == lisp.LError {
			w.err = encodeStepError{lerr}
		}
	}
}

// rawMessageLen is a lower bound on raw once encoding/json has compacted it:
// compacting removes whitespace between tokens and nothing else.
func rawMessageLen(raw json.RawMessage) int {
	if raw == nil {
		return 4 // RawMessage.MarshalJSON writes null
	}
	n := 0
	for _, c := range raw {
		if c != ' ' && c != '\t' && c != '\n' && c != '\r' {
			n++
		}
	}
	return max(n, 1)
}

// bigIntLen is a lower bound on the length of x in decimal:
// floor((BitLen-1) * log10(2)) + 1 digits, and a sign.
func bigIntLen(x *big.Int) int {
	n := x.BitLen()
	if n == 0 {
		return 1
	}
	d := (n-1)/10*3 + 1 // log10(2) > 0.3: a lower bound on the digits
	if x.Sign() < 0 {
		d++
	}
	return d
}

// bigFloatWork is the size of the decimal big.Float.Text builds for x before
// it shortens it: the exact decimal expansion of mantissa * 2^exp, which has
// about 0.3 digits per bit for a positive exponent and one digit per bit for
// a negative one.
func bigFloatWork(x *big.Float) int {
	if x.IsInf() || x.Sign() == 0 {
		return 0
	}
	exp := int64(x.MantExp(nil))
	prec := int64(min(x.MinPrec(), big.MaxPrec)) // MinPrec <= MaxPrec, a uint32
	var digits int64
	if exp > 0 {
		digits = (exp + prec) / 10 * 3
	} else {
		digits = -exp + prec
	}
	if digits > math.MaxInt32 {
		return math.MaxInt32
	}
	return int(digits)
}

// nativeFloatLen is the length of encoding/json's text for f.
func nativeFloatLen(f float64, bits int) int {
	var b [32]byte
	abs := math.Abs(f)
	format := byte('f')
	if abs != 0 {
		if bits == 64 && (abs < 1e-6 || abs >= 1e21) || bits == 32 && (float32(abs) < 1e-6 || float32(abs) >= 1e21) {
			format = 'e'
		}
	}
	out := strconv.AppendFloat(b[:0], f, format, -1, bits)
	n := len(out)
	if format == 'e' && n >= 4 && out[n-4] == 'e' && out[n-3] == '-' && out[n-2] == '0' {
		n--
	}
	return n
}

// isValidJSONNumber is encoding/json's isValidNumber: the JSON number grammar.
func isValidJSONNumber(s string) bool {
	if s == "" {
		return false
	}
	if s[0] == '-' {
		s = s[1:]
		if s == "" {
			return false
		}
	}
	switch {
	default:
		return false
	case s[0] == '0':
		s = s[1:]
	case '1' <= s[0] && s[0] <= '9':
		s = s[1:]
		for len(s) > 0 && '0' <= s[0] && s[0] <= '9' {
			s = s[1:]
		}
	}
	if len(s) >= 2 && s[0] == '.' && '0' <= s[1] && s[1] <= '9' {
		s = s[2:]
		for len(s) > 0 && '0' <= s[0] && s[0] <= '9' {
			s = s[1:]
		}
	}
	if len(s) >= 2 && (s[0] == 'e' || s[0] == 'E') {
		s = s[1:]
		if s[0] == '+' || s[0] == '-' {
			s = s[1:]
			if s == "" {
				return false
			}
		}
		for len(s) > 0 && '0' <= s[0] && s[0] <= '9' {
			s = s[1:]
		}
	}
	return s == ""
}

// nativeTypeInfo is what the walk needs to know about a type, computed once
// per type: encoding/json's encoder choice for it.
type nativeTypeInfo struct {
	fields           []nativeField
	marshaler        bool // t implements json.Marshaler
	ptrMarshaler     bool // t is not a pointer and *t implements json.Marshaler
	textMarshaler    bool
	ptrTextMarshaler bool
	byteSlice        bool // a slice encoding/json writes as base64
	mapKeyOK         bool // a map whose key type encoding/json accepts
}

var nativeInfoCache sync.Map // reflect.Type -> *nativeTypeInfo

var textMarshalerType = reflect.TypeFor[encoding.TextMarshaler]()

func nativeInfo(t reflect.Type) *nativeTypeInfo {
	if i, ok := nativeInfoCache.Load(t); ok {
		info, _ := i.(*nativeTypeInfo)
		return info
	}
	info := &nativeTypeInfo{
		marshaler:     t.Implements(jsonMarshalerType),
		textMarshaler: t.Implements(textMarshalerType),
	}
	if t.Kind() != reflect.Pointer {
		pt := reflect.PointerTo(t)
		info.ptrMarshaler = pt.Implements(jsonMarshalerType)
		info.ptrTextMarshaler = pt.Implements(textMarshalerType)
	}
	switch t.Kind() {
	case reflect.Struct:
		info.fields = cachedNativeFields(t)
	case reflect.Slice:
		if t.Elem().Kind() == reflect.Uint8 {
			p := reflect.PointerTo(t.Elem())
			info.byteSlice = !p.Implements(jsonMarshalerType) && !p.Implements(textMarshalerType)
		}
	case reflect.Map:
		switch t.Key().Kind() {
		case reflect.String,
			reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
			reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
			info.mapKeyOK = true
		default:
			info.mapKeyOK = t.Key().Implements(textMarshalerType)
		}
	default:
	}
	i, _ := nativeInfoCache.LoadOrStore(t, info)
	info, _ = i.(*nativeTypeInfo)
	return info
}

// errNativeTooDeep reports a native nested past nativeNestLimit whose JSON
// nesting alone would still load.
var errNativeTooDeep = fmt.Errorf("value nests more than %d levels deep", nativeNestLimit)

// boundNative walks native, the value of a native LVal, and returns the error
// that refuses it, or nil when encoding/json may marshal it.
func (enc *encoder) boundNative(native any, b encodeBudget) error {
	var w nativeWalker
	w.init(enc, b, false)
	w.run(native)
	if w.err != nil {
		return w.err
	}
	switch w.event {
	case nativeWalkTooBig:
		if !w.mayFailFirst {
			return encodeSizeError(b.maxBytes)
		}
	case nativeWalkTooDeep, nativeWalkCycle:
	default:
		return nil
	}
	// Too big, too deep, or a cycle encoding/json reports too late to run
	// it.  When encoding/json would have stopped at an error of its own
	// first, that error is the result, so walk again in encoding/json's own
	// order to learn what it meets first.  The second walk charges only
	// what the first one did not reach.
	var ordered nativeWalker
	ordered.init(enc, b, true)
	ordered.run(native)
	switch {
	case ordered.err != nil:
		return ordered.err
	case ordered.event == nativeWalkTooBig:
		return encodeSizeError(b.maxBytes)
	case ordered.event == nativeWalkCycle:
		return ordered.cycleErr
	case ordered.event != nativeWalkTooDeep:
		// encoding/json fails at an error of its own before it is too deep
		// or too big, so it is safe to run, and it says how it fails.
		return nil
	case ordered.deepBracket != 0:
		// The text checkLoadable reports for these bytes: the decoder's error
		// at the first container past its nesting limit.
		return encodeUnloadableNativeError{err: errors.New("invalid character '" + string(ordered.deepBracket) + "' exceeded max depth")}
	default:
		return encodeUnloadableNativeError{err: errNativeTooDeep}
	}
}
