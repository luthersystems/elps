// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"fmt"
	"math"
	"slices"
	"strconv"
	"strings"
	"sync"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// Tag transforms v into plain JSON values without losing its data types.
// Whole floats use ~d followed by appendJSONFloat text, including ~d-0.
// Limits count logical values and containers, without counting tag wrappers.
func Tag(v *lisp.LVal, opts ...TypedOption) (*lisp.LVal, error) {
	w := tagWalker{cfg: newTypedConfig(opts)}
	out, err := w.value(v, 0)
	if err != nil {
		return nil, err
	}
	return out, nil
}

type tagWalker struct {
	path                  []*lisp.LVal
	cfg                   typedConfig
	values, size, charged int
}

func (w *tagWalker) count() error {
	w.values++
	if w.values > w.cfg.maxValues {
		return fmt.Errorf("%w: more than %d values", ErrTypedLimit, w.cfg.maxValues)
	}
	return nil
}

func (w *tagWalker) write(s string) error {
	return w.grow(len(s))
}

func (w *tagWalker) grow(n int) error {
	w.size += n
	if w.size > w.cfg.maxBytes {
		return fmt.Errorf("%w: encoding exceeds %d bytes", ErrTypedLimit, w.cfg.maxBytes)
	}
	if w.cfg.charge != nil {
		want := startedKiB(w.size)
		if want > w.charged {
			delta := want - w.charged
			w.charged = want
			if err := w.cfg.charge(delta); err != nil {
				return fmt.Errorf("typed json: %w", err)
			}
		}
	}
	return nil
}

func (w *tagWalker) leaf(v *lisp.LVal) (*lisp.LVal, error) {
	var n int
	switch v.Type {
	case lisp.LString:
		n = len(appendJSONString(nil, v.Str))
	case lisp.LInt:
		n = len(strconv.Itoa(v.Int))
	case lisp.LFloat:
		n = len(appendJSONFloat(nil, v.Float))
	case lisp.LSymbol:
		n = len(v.Str)
	default:
		n = 4
	}
	if err := w.grow(n); err != nil {
		return nil, err
	}
	return v, nil
}

func appendTaggedFloat(b []byte, f float64) []byte {
	switch {
	case math.IsNaN(f):
		return append(b, `"~zNaN"`...)
	case math.IsInf(f, 1):
		return append(b, `"~zINF"`...)
	case math.IsInf(f, -1):
		return append(b, `"~z-INF"`...)
	case math.Trunc(f) == f:
		b = append(b, '"', '~', 'd')
		return append(appendJSONFloat(b, f), '"')
	default:
		return appendJSONFloat(b, f)
	}
}

func tagScalar(v *lisp.LVal) (*lisp.LVal, error) {
	switch v.Type {
	case lisp.LInt:
		if !exactInt(int64(v.Int)) {
			return lisp.String("~n" + strconv.Itoa(v.Int)), nil
		}
	case lisp.LFloat:
		switch {
		case math.IsNaN(v.Float):
			return lisp.String("~zNaN"), nil
		case math.IsInf(v.Float, 1):
			return lisp.String("~zINF"), nil
		case math.IsInf(v.Float, -1):
			return lisp.String("~z-INF"), nil
		case math.Trunc(v.Float) == v.Float:
			return lisp.String("~d" + string(appendJSONFloat(nil, v.Float))), nil
		}
	case lisp.LString:
		if !utf8.ValidString(v.Str) {
			return nil, errors.New("typed json: cannot encode a string that is not valid UTF-8")
		}
		if needsTilde(v.Str) {
			return lisp.String("~" + v.Str), nil
		}
	case lisp.LSymbol:
		if v.Str == "" {
			return nil, errors.New("typed json: cannot encode an empty symbol")
		}
		if !utf8.ValidString(v.Str) {
			return nil, errors.New("typed json: cannot encode a symbol that is not valid UTF-8")
		}
		if v.Str == lisp.TrueSymbol || v.Str == lisp.FalseSymbol {
			return v, nil
		}
		if v.Str[0] == ':' {
			return lisp.String("~" + v.Str), nil
		}
		return lisp.String("~$" + v.Str), nil
	case lisp.LBytes:
		return lisp.String("~b" + enc64.EncodeToString(v.Bytes())), nil
	case lisp.LSExpr:
		if v.IsNil() {
			return v, nil
		}
	default:
		return nil, fmt.Errorf("typed json: cannot encode a %v", v.Type)
	}
	return v, nil
}

func (w *tagWalker) scalar(v *lisp.LVal) (*lisp.LVal, error) {
	out, err := tagScalar(v)
	if err != nil {
		return nil, err
	}
	return w.leaf(out)
}

func (w *tagWalker) value(v *lisp.LVal, depth int) (*lisp.LVal, error) {
	if v == nil {
		return nil, errors.New("typed json: cannot encode a Go nil value")
	}
	if err := w.count(); err != nil {
		return nil, err
	}
	minimum := 0
	if v.Type == lisp.LString || v.Type == lisp.LSymbol {
		minimum = len(v.Str) + 2
	}
	if v.Type == lisp.LSymbol && (v.Str == lisp.TrueSymbol || v.Str == lisp.FalseSymbol) {
		minimum = len(v.Str)
	}
	if v.Type == lisp.LBytes {
		minimum = enc64.EncodedLen(len(v.Bytes())) + 4
	}
	if minimum > w.cfg.maxBytes-w.size {
		return nil, fmt.Errorf("%w: encoding exceeds %d bytes", ErrTypedLimit, w.cfg.maxBytes)
	}
	shape := lisp.ShapeOf(v.Type)
	switch shape {
	case lisp.ShapeList:
		if v.Type != lisp.LSExpr || v.IsNil() {
			return w.scalar(v)
		}
	case lisp.ShapeArray, lisp.ShapeMap, lisp.ShapeTagged:
	case lisp.ShapeLeaf, lisp.ShapeError, lisp.ShapeFun, lisp.ShapeNative, lisp.ShapeMark, lisp.ShapeInvalid:
		return w.scalar(v)
	}
	if depth >= w.cfg.maxDepth {
		for _, p := range w.path {
			if p == v {
				return nil, errors.New("typed json: cannot encode a value that contains itself")
			}
		}
		return nil, fmt.Errorf("%w: nesting depth exceeds %d", ErrTypedLimit, w.cfg.maxDepth)
	}
	w.path = append(w.path, v)
	defer func() { w.path = w.path[:len(w.path)-1] }()
	switch shape {
	case lisp.ShapeList:
		if err := w.write(`["~#list",`); err != nil {
			return nil, err
		}
		inner, err := w.cells(v.Cells, depth)
		if err != nil {
			return nil, err
		}
		if err = w.write("]"); err != nil {
			return nil, err
		}
		return lisp.Vector([]*lisp.LVal{lisp.String(tagList), inner}), nil
	case lisp.ShapeArray:
		array, arrayErr := typedArrayParts(v)
		dims, cells, err := array.dims, array.cells, arrayErr
		if err != nil {
			return nil, err
		}
		if len(dims) == 1 {
			return w.cells(cells, depth)
		}
		if err = w.write(`["~#array",[`); err != nil {
			return nil, err
		}
		ds, err := w.cells(dims, depth)
		if err != nil {
			return nil, err
		}
		if err = w.write(","); err != nil {
			return nil, err
		}
		cs, err := w.cells(cells, depth)
		if err != nil {
			return nil, err
		}
		if err = w.write("]]"); err != nil {
			return nil, err
		}
		return lisp.Vector([]*lisp.LVal{lisp.String(tagArray), lisp.Vector([]*lisp.LVal{ds, cs})}), nil
	case lisp.ShapeTagged:
		if len(v.Cells) != 1 || v.Str == "" || !utf8.ValidString(v.Str) {
			return nil, errors.New("typed json: malformed tagged value")
		}
		if err := w.write(`["~#tagged",[`); err != nil {
			return nil, err
		}
		if _, err := w.leaf(lisp.String(v.Str)); err != nil {
			return nil, err
		}
		if err := w.write(","); err != nil {
			return nil, err
		}
		inner, err := w.value(v.Cells[0], depth+1)
		if err != nil {
			return nil, err
		}
		if err = w.write("]]"); err != nil {
			return nil, err
		}
		return lisp.Vector([]*lisp.LVal{lisp.String(tagTagged), lisp.Vector([]*lisp.LVal{lisp.String(v.Str), inner})}), nil
	case lisp.ShapeMap:
		return w.object(v, depth)
	case lisp.ShapeLeaf, lisp.ShapeError, lisp.ShapeFun, lisp.ShapeNative, lisp.ShapeMark, lisp.ShapeInvalid:
	}
	return w.scalar(v)
}

func (w *tagWalker) cells(cells []*lisp.LVal, depth int) (*lisp.LVal, error) {
	if err := w.write("["); err != nil {
		return nil, err
	}
	if len(cells) > w.cfg.maxValues-w.values {
		return nil, fmt.Errorf("%w: more than %d values", ErrTypedLimit, w.cfg.maxValues)
	}
	out := make([]*lisp.LVal, len(cells))
	for i, v := range cells {
		if i > 0 {
			if err := w.write(","); err != nil {
				return nil, err
			}
		}
		c, err := w.value(v, depth+1)
		if err != nil {
			return nil, err
		}
		out[i] = c
	}
	if err := w.write("]"); err != nil {
		return nil, err
	}
	return lisp.Vector(out), nil
}

var tagKeyPairPool = sync.Pool{New: func() any { p := make([]lisp.MapKeyPair, 0, 16); return &p }}

func taggedKeyText(k lisp.MapKeyPair) (string, error) {
	if k.Kind == lisp.LString && !needsTilde(k.Key) {
		if !utf8.ValidString(k.Key) {
			return "", errors.New("typed json: cannot encode a map key that is not valid UTF-8")
		}
		return k.Key, nil
	}
	b, err := appendTypedKey(nil, k.Kind, k.Key, k.Int)
	return string(b), err
}

func (w *tagWalker) object(v *lisp.LVal, depth int) (*lisp.LVal, error) {
	if v.Len() > w.cfg.maxValues-w.values {
		return nil, fmt.Errorf("%w: more than %d values", ErrTypedLimit, w.cfg.maxValues)
	}
	kp, ok := tagKeyPairPool.Get().(*[]lisp.MapKeyPair)
	if !ok {
		kp = new([]lisp.MapKeyPair)
	}
	keys, ok := v.AppendMapKeyPairs((*kp)[:0])
	defer func() {
		clear(keys)
		if cap(keys) <= mapPairRetentionLimit {
			*kp = keys[:0]
			tagKeyPairPool.Put(kp)
		}
	}()
	if !ok {
		entries := v.MapEntries()
		if entries.IsError() {
			return nil, lisp.GoError(entries)
		}
		for _, p := range entries.Cells {
			if p == nil || len(p.Cells) != 2 || p.Cells[0] == nil {
				return nil, errors.New("typed json: malformed map entry")
			}
			k := p.Cells[0]
			keys = append(keys, lisp.MapKeyPair{Kind: k.Type, Key: k.Str, Int: k.Int, Val: p.Cells[1]})
		}
		if err := checkHostMapKeys(keys); err != nil {
			return nil, err
		}
	}
	sp := getMapPairs()
	pairs := (*sp)[:0]
	defer func() {
		clear(pairs)
		if cap(pairs) <= mapPairRetentionLimit {
			*sp = pairs[:0]
			mapPairPool.Put(sp)
		}
	}()
	plainKeys := true
	for _, k := range keys {
		if k.Kind != lisp.LString || needsTilde(k.Key) {
			plainKeys = false
			break
		}
		if !utf8.ValidString(k.Key) {
			return nil, errors.New("typed json: cannot encode a map key that is not valid UTF-8")
		}
	}
	if plainKeys {
		pairs, ok = v.AppendSortedPairs(pairs)
	}
	if !plainKeys || !ok {
		for _, k := range keys {
			text, err := taggedKeyText(k)
			if err != nil {
				return nil, err
			}
			pairs = append(pairs, lisp.MapPair{Key: text, Val: k.Val})
		}
		slices.SortFunc(pairs, func(a, b lisp.MapPair) int { return strings.Compare(a.Key, b.Key) })
	}
	out := lisp.SortedMap()
	if err := w.write("{"); err != nil {
		return nil, err
	}
	for i, p := range pairs {
		if i > 0 {
			if pairs[i-1].Key == p.Key {
				return nil, errors.New("typed json: map has two keys with one encoding")
			}
			if err := w.write(","); err != nil {
				return nil, err
			}
		}
		if err := w.count(); err != nil {
			return nil, err
		}
		if _, err := w.leaf(lisp.String(p.Key)); err != nil {
			return nil, err
		}
		if err := w.write(":"); err != nil {
			return nil, err
		}
		c, err := w.value(p.Val, depth+1)
		if err != nil {
			return nil, err
		}
		out.MapSet(p.Key, c)
	}
	if err := w.write("}"); err != nil {
		return nil, err
	}
	return out, nil
}

// arrayParts holds array dimensions and contents.
type arrayParts struct {
	// dims contains array dimensions.
	dims []*lisp.LVal
	// cells contains array elements.
	cells []*lisp.LVal
}

func typedArrayParts(v *lisp.LVal) (arrayParts, error) {
	var dims []*lisp.LVal
	var cells []*lisp.LVal
	if len(v.Cells) != 2 || v.Cells[0] == nil || v.Cells[1] == nil || v.Cells[0].Type != lisp.LSExpr || v.Cells[1].Type != lisp.LSExpr {
		return arrayParts{dims: nil, cells: nil}, errors.New("typed json: malformed array")
	}
	dims, cells = v.Cells[0].Cells, v.Cells[1].Cells
	zero := false
	for _, d := range dims {
		if d == nil || d.Type != lisp.LInt || d.Int < 0 {
			return arrayParts{dims: nil, cells: nil}, errors.New("typed json: malformed array dimensions")
		}
		zero = zero || d.Int == 0
	}
	total := 1
	if zero {
		total = 0
	} else {
		for _, d := range dims {
			if total > math.MaxInt/d.Int {
				return arrayParts{dims: nil, cells: nil}, errors.New("typed json: malformed array dimensions")
			}
			total *= d.Int
		}
	}
	if total != len(cells) {
		return arrayParts{dims: nil, cells: nil}, errors.New("typed json: array contents do not match its dimensions")
	}
	return arrayParts{dims: dims, cells: cells}, nil
}

// TagBuiltin returns a plain JSON value with type tags.
func TagBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return transformBuiltin(env, args, Tag)
}

// UntagBuiltin restores the types of a plain JSON value with type tags.
func UntagBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return transformBuiltin(env, args, Untag)
}

func transformBuiltin(env *lisp.LEnv, args *lisp.LVal, transform func(*lisp.LVal, ...TypedOption) (*lisp.LVal, error)) *lisp.LVal {
	in := args.ReqArg(env, 0)
	if in.IsError() {
		return in
	}
	var lerr *lisp.LVal
	charge := WithTypedCharge(func(kib int) error {
		if rc := env.ChargeSteps(int64(kib)); rc.IsError() {
			lerr = rc
			return errTypedCharge
		}
		return nil
	})
	v, err := transform(in, append(typedOptions(env), charge)...)
	if lerr != nil {
		return lerr
	}
	if err != nil {
		return env.Error(err)
	}
	return v
}
