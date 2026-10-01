// Copyright © 2026 The ELPS authors

package libjson

import (
	"encoding"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"math"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"sync"
	"unicode/utf8"

	"github.com/luthersystems/elps/internal/valwalk"
	"github.com/luthersystems/elps/lisp"
)

// Canonize returns a fresh plain JSON image of v, using exact integer types.
// It walks values directly, without encoding and decoding a document.
// For every successful result c, Dump(c, false) == Dump(v, false) ==
// DumpTyped(c); LoadWith(..., LoadOpts{ExactIntegers:true}) and LoadTyped
// return exactly c, and Canonize(c) equals c. Limits and incremental step
// charging use the same TypedOptions as DumpTyped. Options may lower the
// default limits; raising them cannot admit values beyond the default typed
// limits, since such results would violate the invariant.
//
// Leading tilde strings, invalid UTF-8, integers beyond +/-2^53, nonfinite
// floats, negative zero, whole-number floats outside the exact/platform int
// range, int map keys, collisions and changed member order are errors. Opaque
// native encodings and native numbers are refused: their JSON spelling or
// treatment of StringNumbers cannot be preserved by a direct value walk.
// CanonizeBuiltin exposes these data rejections as json:canonize-error,
// with (message, case keyword, path) data for Lisp handlers.
func Canonize(v *lisp.LVal, opts ...TypedOption) (*lisp.LVal, error) {
	w := canonWalkerPool.Get().(*canonWalker)
	w.cfg = newTypedConfig(opts)
	defer func() {
		clear(w.frames[:cap(w.frames)])
		w.frames = w.frames[:0]
		clear(w.nativePath)
		w.cfg = typedConfig{}
		w.walk = nil
		w.values, w.bytes, w.charged = 0, 0, 0
		canonWalkerPool.Put(w)
	}()
	w.cfg.maxDepth = min(w.cfg.maxDepth, DefaultTypedMaxDepth)
	w.cfg.maxBytes = min(w.cfg.maxBytes, DefaultTypedMaxBytes)
	w.cfg.maxValues = min(w.cfg.maxValues, DefaultTypedMaxValues)
	return valwalk.Walk(v, w)
}

var canonWalkerPool = sync.Pool{New: func() any { return &canonWalker{} }}

type canonWalker struct {
	walk       *valwalk.Walker[*lisp.LVal]
	nativePath map[reflect.Value]bool
	frames     []canonFrame
	cfg        typedConfig
	childIndex int
	values     int
	bytes      int
	charged    int
	child      bool
}

// Canonize rejection data is independent of its human-readable message.
// Lisp handlers receive (condition, message, case keyword, path).
type canonizeError struct {
	cause                  error
	caseName, path, detail string
}

func (e *canonizeError) message() string { return e.detail + " at " + e.path }
func (e *canonizeError) Error() string   { return "json:canonize: " + e.message() }
func (e *canonizeError) Unwrap() error   { return e.cause }

func canonizeFailure(caseName, path, detail string, cause error) error {
	return &canonizeError{cause: cause, caseName: caseName, path: path, detail: detail}
}

// fail uses only scalar fields: rendering an arbitrary offending container
// could itself recurse through a cycle or expand a shared graph.
func (w *canonWalker) fail(path, caseName, reason string, v *lisp.LVal) error {
	value := "Go nil"
	if v != nil {
		switch v.Type {
		case lisp.LString, lisp.LSymbol:
			value = strconv.Quote(v.Str[:min(len(v.Str), 128)])
			if len(v.Str) > 128 {
				value += "..."
			}
		case lisp.LInt:
			value = strconv.Itoa(v.Int)
		case lisp.LFloat:
			value = strconv.FormatFloat(v.Float, 'f', -1, 64)
		case lisp.LFun:
			value = "function"
		case lisp.LNative:
			value = fmt.Sprintf("native %T", v.Native)
		default:
			value = v.Type.String()
		}
	}
	return canonizeFailure(caseName, w.resolvePath(path), reason+": "+value, nil)
}

func (w *canonWalker) count(path string) error {
	if w.values >= w.cfg.maxValues {
		return canonizeFailure("limit", w.resolvePath(path), fmt.Sprintf("value-count limit exceeded: %d values", w.cfg.maxValues), ErrTypedLimit)
	}
	w.values++
	return nil
}

// add counts the exact output bytes without building them. It bounds tree
// expansion as well as allocation and charges as work progresses.
func (w *canonWalker) add(n int, path string) error {
	if n > w.cfg.maxBytes-w.bytes {
		return canonizeFailure("limit", w.resolvePath(path), fmt.Sprintf("output exceeds %d bytes: %d bytes counted, %d more requested", w.cfg.maxBytes, w.bytes, n), ErrTypedLimit)
	}
	w.bytes += n
	if w.cfg.charge != nil {
		want := startedKiB(w.bytes)
		if want > w.charged {
			n := want - w.charged
			w.charged = want
			if err := w.cfg.charge(n); err != nil {
				return err
			}
		}
	}
	return nil
}

func (w *canonWalker) text(s, path string, v *lisp.LVal) (*lisp.LVal, error) {
	if needsTilde(s) {
		return nil, w.fail(path, "leading-tilde", "leading ~ string or key", v)
	}
	if err := w.add(2, path); err != nil {
		return nil, err
	}
	for i, r := range s {
		// A one-byte RuneError is invalid UTF-8, including surrogate
		// encodings. Validate during the metered scan, not before it.
		if r == utf8.RuneError {
			if _, n := utf8.DecodeRuneInString(s[i:]); n == 1 {
				return nil, w.fail(path, "invalid-utf8", "invalid UTF-8 string or key", v)
			}
		}
		n := utf8.RuneLen(r)
		switch r {
		case '"', '\\', '\b', '\f', '\n', '\r', '\t':
			n = 2
		case '<', '>', '&', '\u2028', '\u2029':
			n = 6
		default:
			if r < 0x20 {
				n = 6
			}
		}
		if err := w.add(n, path); err != nil {
			return nil, err
		}
	}
	return lisp.String(strings.Clone(s)), nil
}

func (w *canonWalker) scalar(v *lisp.LVal, path string) (*lisp.LVal, error) {
	switch v.Type {
	case lisp.LInt:
		if !exactInt(int64(v.Int)) {
			return nil, w.fail(path, "int-range", "int magnitude exceeds 2^53", v)
		}
		if err := w.add(len(strconv.Itoa(v.Int)), path); err != nil {
			return nil, err
		}
		return lisp.Int(v.Int), nil
	case lisp.LFloat:
		x := v.Float
		if math.IsNaN(x) || math.IsInf(x, 0) {
			return nil, w.fail(path, "non-finite", "nonfinite float", v)
		}
		if x == 0 && math.Signbit(x) {
			return nil, w.fail(path, "negative-zero", "negative zero", v)
		}
		if math.Trunc(x) == x {
			if math.Abs(x) > maxExactInt {
				return nil, w.fail(path, "float-range", "whole-number float magnitude exceeds 2^53", v)
			}
			if strconv.IntSize == 32 && (x < math.MinInt32 || x > math.MaxInt32) {
				return nil, w.fail(path, "float-range", "whole-number float does not fit platform int", v)
			}
			if err := w.add(len(strconv.Itoa(int(x))), path); err != nil {
				return nil, err
			}
			return lisp.Int(int(x)), nil
		}
		var scratch [64]byte
		if err := w.add(len(appendJSONFloat(scratch[:0], x)), path); err != nil {
			return nil, err
		}
		return lisp.Float(x), nil
	case lisp.LString:
		return w.text(v.Str, path, v)
	case lisp.LSymbol:
		switch v.Str {
		case lisp.TrueSymbol, lisp.FalseSymbol:
			if err := w.add(len(v.Str), path); err != nil {
				return nil, err
			}
			return lisp.Symbol(v.Str), nil
		case "json:null":
			return w.null(path)
		default:
			return w.text(v.Str, path, v)
		}
	case lisp.LBytes:
		return w.byteString(v.Bytes(), path)
	default:
		return nil, w.fail(path, "unsupported", "unsupported value", v)
	}
}

type canonFrame struct {
	pairs               []lisp.MapKeyPair
	keys                []*lisp.LVal
	object, transparent bool
}

func canonIndexEdge(_ *lisp.LVal, i int) string { return "[" + strconv.Itoa(i) + "]" }

func (w *canonWalker) resolvePath(path string) string {
	if path != "" {
		return path
	}
	path = "$" + w.walk.Path()
	if w.child {
		f := w.frames[len(w.frames)-1]
		path += "[" + strconv.Quote(f.pairs[w.childIndex].Key) + "]"
	}
	return path
}

func (w *canonWalker) Visit(walk *valwalk.Walker[*lisp.LVal], v *lisp.LVal) (valwalk.Step, *lisp.LVal, error) {
	w.walk, w.child = walk, false
	if err := w.count(""); err != nil {
		return valwalk.Step{}, nil, err
	}
	if v == nil {
		return valwalk.Step{}, nil, w.fail("", "unsupported", "unsupported Go nil value", v)
	}
	if walk.OnPath(v) {
		return valwalk.Step{}, nil, w.fail("", "cycle", "cycle in value", v)
	}
	shape := lisp.ShapeOf(v.Type)
	switch shape {
	case lisp.ShapeLeaf:
		out, err := w.scalar(v, "")
		return valwalk.Step{Done: true}, out, err
	case lisp.ShapeNative:
		out, err := w.native(reflect.ValueOf(v.Native), walk.Depth(), w.resolvePath(""))
		return valwalk.Step{Done: true}, out, err
	case lisp.ShapeList, lisp.ShapeArray, lisp.ShapeTagged, lisp.ShapeMap:
		if v.IsNil() {
			out, err := w.null("")
			return valwalk.Step{Done: true}, out, err
		}
		if walk.Depth() >= w.cfg.maxDepth {
			return valwalk.Step{}, nil, w.fail("", "depth", "nesting depth limit exceeded", v)
		}
	case lisp.ShapeError, lisp.ShapeFun, lisp.ShapeMark, lisp.ShapeInvalid:
		return valwalk.Step{}, nil, w.fail("", "unsupported", "unsupported value", v)
	}
	switch shape {
	case lisp.ShapeList:
		if v.Type == lisp.LQuote {
			return w.wrapper(v)
		}
		return w.cells(v.Cells)
	case lisp.ShapeTagged:
		return w.wrapper(v)
	case lisp.ShapeArray:
		if len(v.Cells) != 2 || v.Cells[0] == nil || v.Cells[1] == nil || v.Cells[0].Type != lisp.LSExpr || v.Cells[1].Type != lisp.LSExpr {
			return valwalk.Step{}, nil, w.fail("", "unsupported", "malformed array", v)
		}
		dims, cells := v.Cells[0].Cells, v.Cells[1].Cells
		if len(dims) == 0 && len(cells) == 1 {
			w.frames = append(w.frames, canonFrame{transparent: true})
			return valwalk.Step{Children: cells}, nil, nil
		}
		if len(dims) != 1 || dims[0] == nil || dims[0].Type != lisp.LInt || dims[0].Int != len(cells) {
			return valwalk.Step{}, nil, w.fail("", "unsupported", "unsupported array dimensions", v)
		}
		return w.cells(cells)
	case lisp.ShapeMap:
		return w.sortedMap(v)
	case lisp.ShapeLeaf, lisp.ShapeNative, lisp.ShapeError, lisp.ShapeFun, lisp.ShapeMark, lisp.ShapeInvalid:
	}
	return valwalk.Step{}, nil, w.fail("", "unsupported", "unsupported container", v)
}

func (w *canonWalker) wrapper(v *lisp.LVal) (valwalk.Step, *lisp.LVal, error) {
	if len(v.Cells) != 1 {
		return valwalk.Step{}, nil, w.fail("", "unsupported", "malformed wrapper", v)
	}
	w.frames = append(w.frames, canonFrame{transparent: true})
	return valwalk.Step{Children: v.Cells}, nil, nil
}

func (w *canonWalker) Child(walk *valwalk.Walker[*lisp.LVal], _ *lisp.LVal, i int) error {
	w.walk, w.child, w.childIndex = walk, false, i
	f := &w.frames[len(w.frames)-1]
	if !f.object {
		return nil
	}
	p := f.pairs[i]
	switch p.Kind {
	case lisp.LInt:
		return w.fail(w.resolvePath("")+"[key "+strconv.Itoa(p.Int)+"]", "key-type", "int map key", lisp.Int(p.Int))
	case lisp.LString, lisp.LSymbol:
	default:
		w.child = true
		return w.fail("", "key-type", "unsupported map key", lisp.Symbol(p.Kind.String()+" "+p.Key))
	}
	w.child = true
	k := lisp.String(p.Key)
	if i > 0 && f.pairs[i-1].Key == p.Key {
		return w.fail("", "key-collision", "map key collision after string conversion", k)
	}
	if i > 0 && f.pairs[i-1].Key > p.Key {
		return w.fail("", "key-order", "map key order would change dump bytes", k)
	}
	if err := w.count(""); err != nil {
		return err
	}
	key, err := w.text(p.Key, "", k)
	if err != nil {
		return err
	}
	f.keys[i] = key
	return nil
}

func (w *canonWalker) Leave(walk *valwalk.Walker[*lisp.LVal], _ *lisp.LVal, children []*lisp.LVal) (*lisp.LVal, error) {
	w.walk, w.child = walk, false
	f := w.frames[len(w.frames)-1]
	w.frames = w.frames[:len(w.frames)-1]
	if f.transparent {
		return children[0], nil
	}
	if !f.object {
		return lisp.Vector(slices.Clone(children)), nil
	}
	out := lisp.SortedMap()
	for i, k := range f.keys {
		if err := lisp.GoError(out.MapSetLVal(k, children[i])); err != nil {
			return nil, w.fail(w.resolvePath("")+"["+strconv.Quote(f.pairs[i].Key)+"]", "unsupported", "cannot construct map", lisp.String(f.pairs[i].Key))
		}
	}
	return out, nil
}

func (w *canonWalker) null(path string) (*lisp.LVal, error) {
	if err := w.add(4, path); err != nil {
		return nil, err
	}
	return lisp.SExpr(nil), nil
}

func (w *canonWalker) byteString(b []byte, path string) (*lisp.LVal, error) {
	if b == nil {
		return w.null(path)
	}
	// Bound before base64's length calculation or allocation.
	if len(b) > (w.cfg.maxBytes-w.bytes)/4*3 {
		return nil, canonizeFailure("limit", w.resolvePath(path), fmt.Sprintf("bytes value exceeds output limit: %d bytes", len(b)), ErrTypedLimit)
	}
	if err := w.add(base64.StdEncoding.EncodedLen(len(b))+2, path); err != nil {
		return nil, err
	}
	return lisp.String(base64.StdEncoding.EncodeToString(b)), nil
}

func (w *canonWalker) width(n int, path string) error {
	if n > w.cfg.maxValues-w.values || n > w.cfg.maxBytes-w.bytes {
		return canonizeFailure("limit", w.resolvePath(path), fmt.Sprintf("container width exceeds value/byte limit: %d entries", n), ErrTypedLimit)
	}
	return nil
}

func (w *canonWalker) cells(cells []*lisp.LVal) (valwalk.Step, *lisp.LVal, error) {
	if err := w.width(len(cells), ""); err != nil {
		return valwalk.Step{}, nil, err
	}
	if err := w.add(2+max(0, len(cells)-1), ""); err != nil {
		return valwalk.Step{}, nil, err
	}
	w.frames = append(w.frames, canonFrame{})
	return valwalk.Step{Children: cells, Edge: canonIndexEdge}, nil, nil
}

func (w *canonWalker) sortedMap(v *lisp.LVal) (valwalk.Step, *lisp.LVal, error) {
	if err := w.width(v.Len(), ""); err != nil {
		return valwalk.Step{}, nil, err
	}
	pairs, stock := v.AppendMapKeyPairs(nil)
	if stock {
		slices.SortFunc(pairs, func(a, b lisp.MapKeyPair) int {
			if a.Kind == lisp.LInt || b.Kind == lisp.LInt { // All int keys are rejected below.
				if a.Kind == b.Kind {
					if a.Int < b.Int {
						return -1
					}
					if a.Int > b.Int {
						return 1
					}
					return 0
				}
				if a.Kind == lisp.LInt {
					return -1
				}
				return 1
			}
			return strings.Compare(a.Key, b.Key)
		})
	} else {
		entries := v.MapEntries()
		if err := lisp.GoError(entries); err != nil {
			return valwalk.Step{}, nil, w.fail("", "unsupported", "cannot read map entries", v)
		}
		for _, p := range entries.Cells {
			if p == nil || len(p.Cells) != 2 || p.Cells[0] == nil {
				return valwalk.Step{}, nil, w.fail("", "unsupported", "malformed map entry", v)
			}
			k := p.Cells[0]
			pairs = append(pairs, lisp.MapKeyPair{Key: k.Str, Int: k.Int, Kind: k.Type, Val: p.Cells[1]})
		}
	}
	if err := w.add(2+max(0, len(pairs)-1)+len(pairs), ""); err != nil {
		return valwalk.Step{}, nil, err
	}
	children := make([]*lisp.LVal, len(pairs))
	for i, p := range pairs {
		children[i] = p.Val
	}
	w.frames = append(w.frames, canonFrame{object: true, pairs: pairs, keys: make([]*lisp.LVal, len(pairs))})
	return valwalk.Step{Children: children, Edge: func(_ *lisp.LVal, i int) string { return "[" + strconv.Quote(pairs[i].Key) + "]" }}, nil, nil
}

// Native leaves and ordinary Go containers are walked without invoking host
// marshal hooks. A hook may print arbitrary number text or member order and
// cannot provide the canonical guarantees from its Go representation alone.
func (w *canonWalker) native(v reflect.Value, depth int, path string) (*lisp.LVal, error) {
	if !v.IsValid() {
		if err := w.count(path); err != nil {
			return nil, err
		}
		return w.null(path)
	}
	if depth >= w.cfg.maxDepth {
		return nil, canonizeFailure("depth", path, fmt.Sprintf("native nesting depth limit exceeded: %s", v.Type()), ErrTypedLimit)
	}
	if v.Kind() == reflect.Interface {
		if v.IsNil() {
			if err := w.count(path); err != nil {
				return nil, err
			}
			return w.null(path)
		}
		return w.native(v.Elem(), depth, path)
	}
	// encoding/json bypasses marshal hooks only for nil pointers. Named nil
	// maps/slices may still implement hooks and must be checked first.
	if v.Kind() == reflect.Pointer && v.IsNil() {
		if err := w.count(path); err != nil {
			return nil, err
		}
		return w.null(path)
	}
	if v.CanInterface() {
		if _, ok := v.Interface().(json.Marshaler); ok {
			return nil, canonizeFailure("unsupported", path, fmt.Sprintf("opaque native JSON marshaler %s", v.Type()), nil)
		}
		if _, ok := v.Interface().(encoding.TextMarshaler); ok {
			return nil, canonizeFailure("unsupported", path, fmt.Sprintf("opaque native text marshaler %s", v.Type()), nil)
		}
	}
	if v.CanAddr() && v.Addr().CanInterface() {
		if _, ok := v.Addr().Interface().(json.Marshaler); ok {
			return nil, canonizeFailure("unsupported", path, fmt.Sprintf("opaque native JSON marshaler %s", v.Type()), nil)
		}
		if _, ok := v.Addr().Interface().(encoding.TextMarshaler); ok {
			return nil, canonizeFailure("unsupported", path, fmt.Sprintf("opaque native text marshaler %s", v.Type()), nil)
		}
	}
	if (v.Kind() == reflect.Map || v.Kind() == reflect.Slice) && v.IsNil() {
		if v.Kind() == reflect.Map && !nativeJSONMapKey(v.Type().Key()) {
			return nil, canonizeFailure("key-type", path, fmt.Sprintf("unsupported native nil map key %s", v.Type().Key()), nil)
		}
		if err := w.count(path); err != nil {
			return nil, err
		}
		return w.null(path)
	}
	if err := w.count(path); err != nil {
		return nil, err
	}
	switch v.Kind() {
	case reflect.String:
		if v.Type() == reflect.TypeFor[json.Number]() {
			return nil, canonizeFailure("unsupported", path, fmt.Sprintf("native number %q ignores string-numbers", v.String()), nil)
		}
		return w.text(v.String(), path, lisp.String(v.String()))
	case reflect.Bool:
		text := strconv.FormatBool(v.Bool())
		if err := w.add(len(text), path); err != nil {
			return nil, err
		}
		return lisp.Symbol(text), nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64, reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr, reflect.Float32, reflect.Float64:
		return nil, canonizeFailure("unsupported", path, fmt.Sprintf("native number %v ignores string-numbers", v.Interface()), nil)
	case reflect.Struct:
		if v.NumField() == 0 {
			if err := w.add(2, path); err != nil {
				return nil, err
			}
			return lisp.SortedMap(), nil
		}
		return nil, canonizeFailure("unsupported", path, fmt.Sprintf("opaque native struct %s", v.Type()), nil)
	case reflect.Pointer, reflect.Map, reflect.Slice, reflect.Array:
		if v.Kind() != reflect.Array {
			if w.nativePath == nil {
				w.nativePath = make(map[reflect.Value]bool)
			}
			if w.nativePath[v] {
				return nil, canonizeFailure("cycle", path, fmt.Sprintf("native cycle in %s", v.Type()), nil)
			}
			w.nativePath[v] = true
			defer delete(w.nativePath, v)
		}
		switch v.Kind() {
		case reflect.Pointer:
			return w.native(v.Elem(), depth+1, path)
		case reflect.Map:
			return w.nativeMap(v, depth, path)
		default:
			// encoding/json treats a uint8 slice as bytes only when its
			// elements have no marshal methods. Otherwise inspect elements,
			// which rejects hooks rather than silently base64-encoding them.
			elemPointer := reflect.PointerTo(v.Type().Elem())
			if v.Kind() == reflect.Slice && v.Type().Elem().Kind() == reflect.Uint8 &&
				!elemPointer.Implements(reflect.TypeFor[json.Marshaler]()) &&
				!elemPointer.Implements(reflect.TypeFor[encoding.TextMarshaler]()) {
				return w.byteString(v.Bytes(), path)
			}
			if err := w.width(v.Len(), path); err != nil {
				return nil, err
			}
			if err := w.add(2+max(0, v.Len()-1), path); err != nil {
				return nil, err
			}
			out := make([]*lisp.LVal, v.Len())
			for i := range out {
				c, err := w.native(v.Index(i), depth+1, fmt.Sprintf("%s[%d]", path, i))
				if err != nil {
					return nil, err
				}
				out[i] = c
			}
			return lisp.Vector(out), nil
		}
	default:
		return nil, canonizeFailure("unsupported", path, fmt.Sprintf("unsupported native %s", v.Type()), nil)
	}
}

// encoding/json validates the map key type before its nil-map shortcut.
func nativeJSONMapKey(t reflect.Type) bool {
	switch t.Kind() {
	case reflect.String, reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return true
	default:
		return t.Implements(reflect.TypeFor[encoding.TextMarshaler]())
	}
}

func (w *canonWalker) nativeMap(v reflect.Value, depth int, path string) (*lisp.LVal, error) {
	if v.Type().Key().Kind() != reflect.String {
		iter := v.MapRange()
		if iter.Next() {
			k := iter.Key()
			var text string
			switch k.Kind() {
			case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
				text = strconv.FormatInt(k.Int(), 10)
			case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
				text = strconv.FormatUint(k.Uint(), 10)
			default:
				return nil, canonizeFailure("key-type", path, fmt.Sprintf("unsupported native map key %v (%s)", k, k.Type()), nil)
			}
			return nil, canonizeFailure("key-type", path+"[key "+text+"]", "int map key "+text, nil)
		}
		return nil, canonizeFailure("key-type", path, fmt.Sprintf("unsupported native map key %s", v.Type().Key()), nil)
	}
	if err := w.width(v.Len(), path); err != nil {
		return nil, err
	}
	keys := v.MapKeys()
	slices.SortFunc(keys, func(a, b reflect.Value) int { return strings.Compare(a.String(), b.String()) })
	if err := w.add(2+max(0, len(keys)-1)+len(keys), path); err != nil {
		return nil, err
	}
	out := lisp.SortedMap()
	for _, k := range keys {
		kp := path + "[" + strconv.Quote(k.String()) + "]"
		if err := w.count(kp); err != nil {
			return nil, err
		}
		key, err := w.text(k.String(), kp, lisp.String(k.String()))
		if err != nil {
			return nil, err
		}
		c, err := w.native(v.MapIndex(k), depth+1, kp)
		if err != nil {
			return nil, err
		}
		if err := lisp.GoError(out.MapSetLVal(key, c)); err != nil {
			return nil, canonizeFailure("unsupported", kp, "invalid native map key "+strconv.Quote(k.String()), nil)
		}
	}
	return out, nil
}

// CanonizeBuiltin implements json:canonize and propagates runtime budget
// conditions unchanged, while charging each started KiB during the walk.
func CanonizeBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v := args.ReqArg(env, 0)
	if v.Type == lisp.LError {
		return v
	}
	var lerr *lisp.LVal
	charge := WithTypedCharge(func(kib int) error {
		if r := env.ChargeSteps(int64(kib)); r.Type == lisp.LError {
			lerr = r
			return errTypedCharge
		}
		return nil
	})
	c, err := Canonize(v, append(typedOptions(env), charge)...)
	if lerr != nil {
		return lerr
	}
	if err != nil {
		var failure *canonizeError
		if !errors.As(err, &failure) {
			// Custom charging errors are host failures rather than data
			// rejections; the builtin's runtime charge was handled above.
			return env.Error(err)
		}
		return env.ErrorCondition("json:canonize-error", lisp.String(failure.message()),
			lisp.Symbol(":"+failure.caseName), lisp.String(failure.path))
	}
	return c
}

// dumpModeBuiltin handles the opt-in paths of the dump family. Calls without
// flags keep the plain serializer's argument handling.
func (s *Serializer) dumpModeBuiltin(env *lisp.LEnv, args *lisp.LVal, asString bool) *lisp.LVal {
	v, sn := args.ReqArg(env, 0), args.KeyArg(1)
	if v.Type == lisp.LError {
		return v
	}
	typed := lisp.True(args.KeyArg(3))
	if typed && !sn.IsNil() {
		return env.Errorf("string-numbers is incompatible with typed")
	}
	if lisp.True(args.KeyArg(2)) {
		v = CanonizeBuiltin(env, lisp.SExpr([]*lisp.LVal{v}))
		if v.Type == lisp.LError {
			return v
		}
	}
	if typed {
		b := DumpTypedBuiltin(env, lisp.SExpr([]*lisp.LVal{v}))
		if b.Type == lisp.LError {
			return b
		}
		if asString {
			return lisp.String(string(b.Bytes()))
		}
		return b
	}
	plainArgs := lisp.SExpr([]*lisp.LVal{v, lisp.Bool(lisp.True(sn))})
	if asString {
		return s.DumpStringBuiltin(env, plainArgs)
	}
	return s.DumpBytesBuiltin(env, plainArgs)
}
