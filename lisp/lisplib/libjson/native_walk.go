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
	"time"

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

// str counts a native's string, or a map key when key is set, as
// encoding/json writes it, quotes included, plus extra bytes.
//
// It refuses a string that is not valid UTF-8.  The v1 engine writes each
// bad byte as the escape \ufffd and the v2 engine, Go 1.27's default, as a
// raw U+FFFD, so the bytes of the dump would depend on the engine
// (luthersystems/elps#928).  Either form also changes the string, without
// a word to the program.  The measuring pass finds the bad bytes, so the
// check costs no pass of its own.
func (w *nativeWalker) str(s string, extra int, key bool) {
	n, invalid := escapedStringLen(s)
	if invalid == 0 {
		w.add(n + extra)
		return
	}
	if key {
		w.fail(encodeUnloadableNativeError{err: errNativeKeyUTF8})
		return
	}
	w.fail(encodeUnloadableNativeError{err: errNativeStringUTF8})
}

var (
	errNativeStringUTF8 = errors.New("a string is not valid UTF-8")
	errNativeKeyUTF8    = errors.New("a map key is not valid UTF-8")
)

// nativeRefScan is how many open references the walk searches linearly for a
// cycle before it indexes them in a map.
const nativeRefScan = 32

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
	typ reflect.Type
	key nativeWalkKey
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
	name     string // encoding/json's resolved key name
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

// nativeWalker decides whether this package may write a native, and then
// writes it (luthersystems/elps#802, luthersystems/elps#928,
// luthersystems/elps#950).
//
// It visits the values the JSON v1 engine visits, by the same rules: the same
// Marshaler and TextMarshaler dispatch (including addressability), the same
// struct fields (nativeTypeFields) and the same omitempty and omitzero
// tests.  It makes two passes.  The measuring pass (boundNative) writes
// nothing, and it computes:
//
//   - est, a lower bound on the bytes encoding/json writes. A Marshaler's
//     output is unknown until it runs, so it counts as its smallest possible
//     form, except for the types whose size is known (see marshaler).
//   - the nesting depth, against nativeNestLimit and jsonNestingLimit.
//
// The walk refuses a native, with an error of this package, when:
//
//   - its output cannot fit the cap, or its nesting could exhaust the stack.
//     So a native is refused BEFORE encoding/json allocates its output or
//     recurses into it.
//   - the v1 engine refuses it for a reason the value itself shows: a type
//     with no JSON form, a map key type with no name, a NaN, an invalid
//     json.Number, a time.Time with no RFC 3339 form, a failing key
//     MarshalText, JSON nested past the decoder's limit, or a value that
//     contains itself.
//   - the two engines write it differently: a string or a map key that is
//     not valid UTF-8 (see str), or a struct with a json tag they read
//     differently (see nativeTypeFields).
//
// The second group is why the walk decides and encoding/json does not
// (luthersystems/elps#928).  The v2 engine, Go 1.27's default, writes some
// of these values (a map with float keys, a nil map with bool keys) and
// reports the others with different text.  substrate endorses the result
// of a dump, so it must not move with the engine.  So the walk's errors are
// the same on both engines.
//
// The writing pass (writeNative) runs only on a native the measuring pass
// has passed.  It writes the bytes the v1 engine writes, on both engines,
// and gives only a MarshalJSON value to encoding/json (see writeMarshaler).
//
// The walk stops at the first problem it meets.  It visits a map's entries
// sorted by their JSON key names, so that problem is the same on every run
// (luthersystems/elps#807).  Sorting can call a key's MarshalText, in each
// pass.  The measuring pass calls no value marshaler: the writing pass calls
// each one once, and reports a failure there.  So a problem the measuring
// pass can see wins over a failing marshaler, wherever the two are in the
// value.
//
// The walk does not recurse: it keeps the containers it is inside of on an
// explicit stack, so its own stack does not grow with the native.  It charges
// steps per KiB of est as it goes, and polls the context, so a step budget or
// a cancellation stops it partway.
type nativeWalker struct {
	enc *encoder
	// err is why the walk stopped: the refusal, or a step or context error.
	err    error
	budget encodeBudget

	// The containers the walk is inside of: the first len(framesArr) in
	// framesArr, the rest in framesMore.  Indexed rather than sliced, so the
	// walker holds no pointer into itself and stays on the stack.
	framesMore []nativeFrame

	// keySlab holds the sorted keys of the open map[string]any frames,
	// one run after another, innermost last.
	keySlab []string

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
	jsonDepth int

	// write selects the writing pass (see writeNative): the walk writes
	// the native to enc.buf, and counts, charges and polls nothing.
	write bool
}

// nativeWalkPollBytes is how many bytes of est the walk covers between
// charges and context polls.
const nativeWalkPollBytes = 1024

func (w *nativeWalker) init(enc *encoder, b encodeBudget) {
	w.enc, w.budget = enc, b
	w.base = enc.buf.Len()
	w.limit = math.MaxInt
	if b.maxBytes > 0 {
		w.limit = b.maxBytes - w.base
	}
	w.poll = nativeWalkPollBytes
}

// stop reports whether the walk must end now.
func (w *nativeWalker) stop() bool { return w.err != nil }

// fail stops the walk with err, unless it has stopped already: the first
// problem in the walk's order is the one reported.
func (w *nativeWalker) fail(err error) {
	if w.err == nil {
		w.err = err
	}
}

// refuse stops the walk at a value this package does not encode.
func (w *nativeWalker) refuse(format string, args ...any) {
	if w.err == nil {
		w.err = encodeUnloadableNativeError{err: fmt.Errorf(format, args...)}
	}
}

// add counts n more output bytes.  The writing pass counts nothing: the
// measuring pass has charged the native already.
func (w *nativeWalker) add(n int) {
	if w.err != nil || w.write {
		return
	}
	w.est += n
	if w.est > w.limit {
		w.err = encodeSizeError(w.budget.maxBytes)
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
			w.fail(encodeCancelledError{err})
		}
	}
}

// descend enters one level deeper, or refuses a native that nests past
// nativeNestLimit.
func (w *nativeWalker) descend() bool {
	if w.depth >= nativeNestLimit {
		w.fail(encodeUnloadableNativeError{err: errNativeTooDeep})
		return false
	}
	w.depth++
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
		w.comma(f.i)
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
		w.comma(f.i)
		elem := f.anys[f.i]
		f.i++
		w.anyElem(elem)
	case frameAnyMap:
		if f.i == len(f.keys) {
			w.pop()
			return
		}
		w.comma(f.i)
		k := f.keys[f.i]
		f.i++
		w.key(k)
		w.anyElem(f.anyMap[k])
	case frameMap:
		if f.i == len(f.entries) {
			w.pop()
			return
		}
		w.comma(f.i)
		e := f.entries[f.i]
		f.i++
		w.key(e.name)
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
				w.writeByte(',')
			}
			f.wrote = true
			w.add(len(fd.nameJSON))
			w.writeString(fd.nameJSON)
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

// push enters container f.  It opens a JSON container, so its brackets are
// counted here.
//
// A container past jsonNestingLimit is refused here.  The v1 engine writes
// it, and this package then refuses bytes it cannot load back.  The v2
// engine refuses to write it.  Refusing it in the walk gives one error on
// both, before either recurses that deep (luthersystems/elps#928).
func (w *nativeWalker) push(f nativeFrame) {
	w.jsonDepth++
	if w.jsonDepth > jsonNestingLimit {
		w.fail(encodeUnloadableNativeError{err: errNativeJSONTooDeep})
	}
	w.add(2)
	w.writeByte(f.open())
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
	if f.kind == frameAnyMap {
		// The map's keys are the last ones on keySlab.
		end := len(w.keySlab)
		clear(w.keySlab[end-len(f.keys) : end])
		w.keySlab = w.keySlab[:end-len(f.keys)]
	}
	w.writeByte(f.open() + 2) // ] after [, } after {
	w.release(f.levels, f.refs)
}

// open is the byte that opens f's JSON container.  The byte two above it
// in ASCII closes it.
func (f *nativeFrame) open() byte {
	if f.kind == frameArray || f.kind == frameAnySlice {
		return '['
	}
	return '{'
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
		w.null()
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
		w.null()
	case string:
		w.strValue(t, false)
	case float64:
		if math.IsInf(t, 0) || math.IsNaN(t) {
			w.refuseFloat(t)
		} else {
			w.float(t, 64, false)
		}
	case bool:
		w.boolean(t, false)
	case map[string]any:
		if t == nil {
			w.null()
			break
		}
		if !w.enterRef(reflect.TypeOf(x), nativeWalkKey{ptr: reflect.ValueOf(x).Pointer()}) {
			break
		}
		// The keys of every open map share keySlab, so the walk does not
		// allocate a slice per map in each of its two passes.
		start := len(w.keySlab)
		for k := range t {
			w.keySlab = append(w.keySlab, k)
		}
		keys := w.keySlab[start:len(w.keySlab):len(w.keySlab)]
		slices.Sort(keys)
		w.add(max(0, len(t)-1))
		w.push(nativeFrame{kind: frameAnyMap, anyMap: t, keys: keys, levels: levels, refs: refs + 1})
		return
	case []any:
		if t == nil {
			w.null()
			break
		}
		if !w.enterRef(reflect.TypeOf(x), nativeWalkKey{ptr: reflect.ValueOf(x).Pointer(), len: len(t)}) {
			break
		}
		w.add(max(0, len(t)-1))
		w.push(nativeFrame{kind: frameAnySlice, anys: t, levels: levels, refs: refs + 1})
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
			w.null()
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
		switch v.Kind() {
		case reflect.Bool:
			w.boolean(v.Bool(), quoted)
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			var b [24]byte
			w.number(strconv.AppendInt(b[:0], v.Int(), 10), quoted)
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
			var b [24]byte
			w.number(strconv.AppendUint(b[:0], v.Uint(), 10), quoted)
		case reflect.Float32, reflect.Float64:
			f := v.Float()
			if math.IsInf(f, 0) || math.IsNaN(f) {
				w.refuseFloat(f)
				break
			}
			w.float(f, v.Type().Bits(), quoted)
		case reflect.String:
			s := v.String()
			if v.Type() != jsonNumberType {
				w.strValue(s, quoted)
				break
			}
			if s == "" {
				s = "0"
			}
			if !isValidJSONNumber(s) {
				w.refuse("invalid json.Number %q", s)
				break
			}
			w.number([]byte(s), quoted)
		case reflect.Interface:
			if v.IsNil() {
				w.null()
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
				w.null()
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
			if info.badTag != "" {
				w.refuse("%s", info.badTag)
				break
			}
			w.push(nativeFrame{kind: frameStruct, v: v, fields: info.fields, levels: levels, refs: refs})
			return
		case reflect.Map:
			if !info.mapKeyOK {
				// The v1 engine refuses the map's type, even when the map is
				// nil.  The v2 engine writes some of these maps.
				w.refuseType(v.Type())
				break
			}
			if v.IsNil() {
				w.null()
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
			w.push(nativeFrame{kind: frameMap, v: v, entries: entries, levels: levels, refs: refs + 1})
			return
		case reflect.Slice:
			if v.IsNil() {
				w.null()
				break
			}
			if info.byteSlice {
				w.byteSlice(v)
				break
			}
			if !w.enterRef(v.Type(), nativeWalkKey{ptr: v.Pointer(), len: v.Len()}) {
				break
			}
			w.add(max(0, v.Len()-1))
			w.push(nativeFrame{kind: frameArray, v: v, levels: levels, refs: refs + 1})
			return
		case reflect.Array:
			w.add(max(0, v.Len()-1))
			w.push(nativeFrame{kind: frameArray, v: v, levels: levels, refs: refs})
			return
		default:
			// Complex numbers, channels, functions and unsafe pointers.
			w.refuseType(v.Type())
		}
		break
	}
	w.release(levels, refs)
}

// The leaf methods below count a leaf in the measuring pass and write it in
// the writing pass, as the v1 engine's encoder writes it.  quoted is the
// ",string" option of the struct field that holds the leaf.

func (w *nativeWalker) writeByte(c byte) {
	if w.write {
		w.enc.buf.WriteByte(c)
	}
}

func (w *nativeWalker) writeString(s string) {
	if w.write {
		w.enc.buf.WriteString(s)
	}
}

// comma separates the i-th member of a container from the one before it.
// The measuring pass counts the commas when it enters the container.
func (w *nativeWalker) comma(i int) {
	if i > 0 {
		w.writeByte(',')
	}
}

func (w *nativeWalker) null() {
	w.add(4)
	w.writeString("null")
}

func (w *nativeWalker) quote(quoted bool) {
	if quoted {
		w.writeByte('"')
	}
}

func (w *nativeWalker) boolean(b, quoted bool) {
	if !w.write {
		w.add(boolLen(b) + quotedLen(quoted))
		return
	}
	w.quote(quoted)
	w.writeString(strconv.FormatBool(b))
	w.quote(quoted)
}

// number writes text, a JSON number.
func (w *nativeWalker) number(text []byte, quoted bool) {
	if !w.write {
		w.add(len(text) + quotedLen(quoted))
		return
	}
	w.quote(quoted)
	w.enc.buf.Write(text)
	w.quote(quoted)
}

func (w *nativeWalker) float(f float64, bits int, quoted bool) {
	var b [32]byte
	w.number(appendNativeFloat(b[:0], f, bits), quoted)
}

// strValue writes a string.  With quoted it writes the JSON string of the
// string's JSON text, as the v1 engine does.  The inner text has no <, >
// or & left, so escaping it again for HTML changes nothing.
func (w *nativeWalker) strValue(s string, quoted bool) {
	if !w.write {
		w.str(s, quotedLen(quoted), false)
		return
	}
	if quoted {
		w.enc.buf.Write(appendJSONString(w.enc.buf.AvailableBuffer(), appendJSONString(nil, s)))
		return
	}
	w.enc.buf.Write(appendJSONString(w.enc.buf.AvailableBuffer(), s))
}

// key writes a map key and its colon.
func (w *nativeWalker) key(name string) {
	if !w.write {
		w.str(name, 1, true)
		return
	}
	w.enc.buf.Write(append(appendJSONString(w.enc.buf.AvailableBuffer(), name), ':'))
}

// byteSlice writes a []byte as a base64 string.
func (w *nativeWalker) byteSlice(v reflect.Value) {
	if !w.write {
		w.add(base64.StdEncoding.EncodedLen(v.Len()) + 2)
		return
	}
	b := append(w.enc.buf.AvailableBuffer(), '"')
	b = base64.StdEncoding.AppendEncode(b, v.Bytes())
	w.enc.buf.Write(append(b, '"'))
}

func quotedLen(quoted bool) int {
	if quoted {
		return 2
	}
	return 0
}

// writeMarshaler writes a value through its MarshalJSON method (text false)
// or MarshalText method (text true), and calls the method once.
//
// libjson quotes the text of a MarshalText method itself, with
// appendJSONString.  The two engines quote text that is not valid UTF-8
// differently: the v1 engine writes each bad byte as the escape \ufffd, the
// v2 engine as a raw U+FFFD, which a valid text can hold too.  Neither the
// walk nor the output can find the bad bytes without a second call of the
// method (luthersystems/elps#950).  appendJSONString writes the v1 engine's
// bytes on both engines.
//
// encoding/json still writes the output of a MarshalJSON method, because it
// checks and compacts the method's JSON.  It gets the method alone, bound to
// v.Addr() when addr is set: the receiver the v1 engine uses in place.
func (w *nativeWalker) writeMarshaler(v reflect.Value, addr, text bool) {
	m := v
	if addr {
		m = v.Addr()
	}
	if (m.Kind() == reflect.Pointer || m.Kind() == reflect.Interface) && m.IsNil() {
		w.null()
		return
	}
	buf := &w.enc.buf
	if text {
		tm, ok := reflect.TypeAssert[encoding.TextMarshaler](m)
		if !ok {
			w.null()
			return
		}
		b, err := tm.MarshalText()
		if err != nil {
			w.fail(nativeMarshalError(&json.MarshalerError{Type: m.Type(), Err: err}))
			return
		}
		buf.Write(appendJSONString(buf.AvailableBuffer(), b))
		return
	}
	jm, ok := reflect.TypeAssert[json.Marshaler](m)
	if !ok {
		w.null()
		return
	}
	mark := buf.Len()
	// encoding/json gets the method, not the value.  Given an interface
	// that holds a nil pointer, it would write null at the top level,
	// where the v1 engine calls the method in place.  It still checks and
	// compacts what the method returns.
	//
	// The wrapper is the encoder's own, and the json.Encoder stays on the
	// stack, so a MarshalJSON value allocates no more than it did when
	// encoding/json wrote the whole native.  A wrapper made here would be
	// copied to the heap at each call, because Encode keeps its argument.
	wrap := w.enc.marshalJSON
	if wrap == nil {
		wrap = new(nativeMarshalJSON)
		w.enc.marshalJSON = wrap
	}
	wrap.m = jm
	err := json.NewEncoder(buf).Encode(wrap)
	wrap.m = nil
	if err != nil {
		buf.Truncate(mark)
		w.fail(nativeMarshalError(err))
		return
	}
	buf.Truncate(buf.Len() - 1) // drop Encode's trailing newline
}

// nativeMarshalJSON hands one MarshalJSON method to encoding/json.
type nativeMarshalJSON struct{ m json.Marshaler }

func (n *nativeMarshalJSON) MarshalJSON() ([]byte, error) { return n.m.MarshalJSON() }

// refuseType refuses a value whose type has no JSON form.
func (w *nativeWalker) refuseType(t reflect.Type) {
	w.refuse("unsupported type %s", t)
}

// refuseFloat refuses a NaN or an infinity, which JSON cannot write.
func (w *nativeWalker) refuseFloat(f float64) {
	w.refuse("unsupported number %s", strconv.FormatFloat(f, 'g', -1, 64))
}

func boolLen(b bool) int {
	if b {
		return 4
	}
	return 5
}

// mapEntries sorts v's entries by JSON key name.
// TextMarshaler keys are resolved in key order, before visiting any value.
// A key-name error therefore precedes every problem in that map's values.
func (w *nativeWalker) mapEntries(v reflect.Value) ([]nativeMapEntry, bool) {
	entries := make([]nativeMapEntry, 0, v.Len())
	it := v.MapRange()
	for it.Next() {
		entries = append(entries, nativeMapEntry{key: it.Key(), val: it.Value()})
	}
	textKeys := v.Type().Key().Kind() != reflect.String && v.Type().Key().Implements(textMarshalerType)
	if textKeys {
		// Sorting must not call user methods. Resolve each key only after
		// this sort, so multiple failing key hooks choose the same error.
		slices.SortFunc(entries, func(a, b nativeMapEntry) int { return compareNativeKeys(a.key, b.key) })
	}
	for i := range entries {
		if i%encodeContextInterval == 0 {
			w.pollContext()
			if w.err != nil {
				return nil, false
			}
		}
		name, err := resolveNativeKeyName(entries[i].key)
		if err != nil {
			w.refuse("map key MarshalText failed: %w", err)
			return nil, false
		}
		entries[i].name = name
	}
	slices.SortStableFunc(entries, func(a, b nativeMapEntry) int { return strings.Compare(a.name, b.name) })
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

// enterRef opens the pointer, map or slice of type typ identified by key on
// the current path.  It returns false, and opens nothing, when the value is
// already open: it contains itself.  leaveRef closes the last one opened.
func (w *nativeWalker) enterRef(typ reflect.Type, key nativeWalkKey) bool {
	if i := w.findRef(key); i >= 0 {
		// The value contains itself.  The v1 engine goes around the cycle
		// about 1000 times before it reports it.  The v2 engine stops at
		// its nesting limit with another error.  The walk refuses the value
		// at the first repeat, with one error for both
		// (luthersystems/elps#928).
		w.refuse("value of type %s contains itself", w.ref(i).typ)
		return false
	}
	r := nativeWalkRef{typ: typ, key: key}
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

// marshaler counts or writes a value the v1 engine hands to MarshalJSON
// (text false) or MarshalText (text true), with v.Addr() as the receiver
// when addr is set.
//
// The measuring pass does not call value marshalers: the writing pass calls
// each one, once.  Most marshalers' output is unknown until they run, so they count as
// their smallest output: one byte of JSON, or an empty string.  The few whose
// size is known from the value -- this package's own messages,
// json.RawMessage and math/big's numbers -- count as that size.  math/big
// also does work out of proportion to its output, a decimal conversion whose
// intermediate grows with the exponent, and that is charged and capped here
// before it runs.
func (w *nativeWalker) marshaler(v reflect.Value, addr, text bool) {
	if w.write {
		w.writeMarshaler(v, addr, text)
		return
	}
	m := v
	if addr {
		m = v.Addr()
	}
	if (m.Kind() == reflect.Pointer || m.Kind() == reflect.Interface) && m.IsNil() {
		w.null()
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
		w.add(minLen)
		return
	}
	if m.Type() == timeType {
		// TypeAssert does not allocate a copy, as Interface would.
		if t, ok := reflect.TypeAssert[time.Time](m); ok {
			w.time(t)
		}
		w.add(minLen)
		return
	}
	switch x := m.Interface().(type) {
	case *time.Time:
		w.time(*x)
		w.add(minLen)
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
	timeType:                            true,
	reflect.TypeFor[*time.Time]():       true,
	reflect.TypeFor[*ownMessage]():      true,
	reflect.TypeFor[*json.RawMessage](): true,
	reflect.TypeFor[json.RawMessage]():  true,
	reflect.TypeFor[*big.Int]():         true,
	reflect.TypeFor[*big.Float]():       true,
	reflect.TypeFor[*big.Rat]():         true,
}

var timeType = reflect.TypeFor[time.Time]()

// time refuses a time.Time that has no RFC 3339 form: a year outside of 0 to
// 9999, or a zone 24 hours or more from UTC.  These are the times whose
// MarshalJSON fails.  The walk checks them itself because the v2 engine
// does not call that method: it formats a time.Time with its own code, and
// reports the failure in other words (luthersystems/elps#928).
func (w *nativeWalker) time(t time.Time) {
	if y := t.Year(); y < 0 || y > 9999 {
		w.refuse("time.Time has year %d, outside of 0 to 9999", y)
		return
	}
	const day = 24 * 60 * 60
	if _, offset := t.Zone(); offset <= -day || offset >= day {
		w.refuse("time.Time has a zone %d seconds from UTC, which is 24 hours or more", offset)
	}
}

// rawMessage counts a json.RawMessage, whose MarshalJSON returns it as is.
//
// The walk does not check that it is valid JSON.  encoding/json checks each
// marshal method's output anyway, and nativeMarshalError gives its refusal
// one text on both engines.  A json.Valid here would scan every RawMessage
// one more time: 26% of the time of a dump of one 6 KiB RawMessage on the v1
// engine.
func (w *nativeWalker) rawMessage(raw json.RawMessage) {
	w.add(rawMessageLen(raw))
}

// convert refuses, and charges for, a conversion that allocates n bytes of
// intermediate work that are not output.
func (w *nativeWalker) convert(n int) {
	if w.err != nil {
		return
	}
	if n > w.limit-w.est {
		w.err = encodeSizeError(w.budget.maxBytes)
		return
	}
	if n >= 1024 && w.enc.env != nil {
		if err := w.enc.env.ChargeSteps(int64(n >> 10)); err != nil {
			w.err = encodeStepError{lisp.Error(err)}
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

// appendNativeFloat appends encoding/json's text for f, a float of bits
// bits.
func appendNativeFloat(b []byte, f float64, bits int) []byte {
	abs := math.Abs(f)
	format := byte('f')
	if abs != 0 {
		if bits == 64 && (abs < 1e-6 || abs >= 1e21) || bits == 32 && (float32(abs) < 1e-6 || float32(abs) >= 1e21) {
			format = 'e'
		}
	}
	out := strconv.AppendFloat(b, f, format, -1, bits)
	// Shorten e-09 to e-9, as encoding/json does.
	if n := len(out); format == 'e' && n >= 4 && out[n-4] == 'e' && out[n-3] == '-' && out[n-2] == '0' {
		out[n-2] = out[n-1]
		out = out[:n-1]
	}
	return out
}

// isValidJSONNumber reports whether s matches the JSON number grammar.
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
	// badTag is why the walk refuses a struct of this type: a json tag the
	// two JSON engines read differently.  Empty for every other type.
	badTag           string
	fields           []nativeField
	marshaler        bool // t implements json.Marshaler
	ptrMarshaler     bool // t is not a pointer and *t implements json.Marshaler
	textMarshaler    bool
	ptrTextMarshaler bool
	byteSlice        bool // a slice encoding/json writes as base64
	mapKeyOK         bool // a map whose key type the v1 engine accepts
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
		info.fields, info.badTag = cachedNativeFields(t)
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
			// The v2 engine also writes float keys, and a nil map with
			// any key type.  The walk refuses them, as the v1 engine
			// does, so a dump does not depend on the engine
			// (luthersystems/elps#928).
			info.mapKeyOK = t.Key().Implements(textMarshalerType)
		}
	default:
	}
	i, _ := nativeInfoCache.LoadOrStore(t, info)
	info, _ = i.(*nativeTypeInfo)
	return info
}

// errNativeTooDeep reports a native nested past nativeNestLimit.
var errNativeTooDeep = fmt.Errorf("value nests more than %d levels deep", nativeNestLimit)

// errNativeJSONTooDeep reports a native whose JSON nests past
// jsonNestingLimit, which this package would not load back.
var errNativeJSONTooDeep = fmt.Errorf("JSON nests more than %d levels deep", jsonNestingLimit)

// boundNative walks native, the value of a native LVal, in the measuring
// pass.  It returns the size it counted, and the error that refuses the
// native, or nil when writeNative may write it.
func (enc *encoder) boundNative(native any, b encodeBudget) (int, error) {
	var w nativeWalker
	w.init(enc, b)
	w.run(native)
	return w.est, w.err
}

// writeNative writes native, which boundNative has passed, to enc.buf.
//
// It is the walk again, in the writing pass: it writes what the measuring
// pass counted, in the same order.  The measuring pass calls no value
// marshaler, so each one runs once, here.  writeNative charges no steps and
// polls no context, as encoding/json did when it wrote natives: boundNative
// has charged for the output already.
//
// est is the size boundNative counted.  It is exact but for the output of
// marshalers, which it counts at their smallest.  So the buffer grows once
// for most natives, as it did when encoding/json wrote a whole native to
// its own buffer and copied it in.
func (enc *encoder) writeNative(native any, est int) error {
	enc.buf.Grow(est)
	w := nativeWalker{enc: enc, limit: math.MaxInt, write: true}
	w.run(native)
	return w.err
}
