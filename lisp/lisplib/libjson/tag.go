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

	"github.com/luthersystems/elps/internal/valwalk"
	"github.com/luthersystems/elps/lisp"
)

// Tag transforms v into plain JSON values without losing its data types.
// Whole floats use ~d followed by appendJSONFloat text, including ~d-0.
// Limits count logical values and containers, without counting tag wrappers.
func Tag(v *lisp.LVal, opts ...TypedOption) (*lisp.LVal, error) {
	w := tagWalkerPool.Get().(*tagWalker)
	w.cfg = newTypedConfig(opts)
	defer func() {
		w.release()
		clear(w.frames[:cap(w.frames)])
		w.frames = w.frames[:0]
		w.cfg = typedConfig{}
		w.values, w.size, w.charged = 0, 0, 0
		tagWalkerPool.Put(w)
	}()
	out, err := valwalk.Walk(v, w)
	if err != nil {
		return nil, err
	}
	return out, nil
}

var tagWalkerPool = sync.Pool{New: func() any { return &tagWalker{} }}

type tagWalker struct {
	frames                []tagFrame
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

type tagFrame struct {
	pool        *[]lisp.MapPair
	pairs       []lisp.MapPair
	dims, cells int
	shape       lisp.Shape
	multi       bool
}

func (w *tagWalker) release() {
	for _, f := range w.frames {
		releaseTagPairs(f)
	}
}

func releaseTagPairs(f tagFrame) {
	if f.pool != nil {
		clear(f.pairs)
		if cap(f.pairs) <= mapPairRetentionLimit {
			*f.pool = f.pairs[:0]
			mapPairPool.Put(f.pool)
		}
	}
}

func (w *tagWalker) scalar(v *lisp.LVal) (valwalk.Step, *lisp.LVal, error) {
	out, err := tagScalar(v)
	if err != nil {
		return valwalk.Step{}, nil, err
	}
	out, err = w.leaf(out)
	return valwalk.Step{Done: true}, out, err
}

func (w *tagWalker) Visit(walk *valwalk.Walker[*lisp.LVal], v *lisp.LVal) (valwalk.Step, *lisp.LVal, error) {
	if v == nil {
		return valwalk.Step{}, nil, errors.New("typed json: cannot encode a Go nil value")
	}
	if err := w.count(); err != nil {
		return valwalk.Step{}, nil, err
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
		return valwalk.Step{}, nil, fmt.Errorf("%w: encoding exceeds %d bytes", ErrTypedLimit, w.cfg.maxBytes)
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
	if walk.Depth() >= w.cfg.maxDepth {
		if walk.OnPath(v) {
			return valwalk.Step{}, nil, errors.New("typed json: cannot encode a value that contains itself")
		}
		return valwalk.Step{}, nil, fmt.Errorf("%w: nesting depth exceeds %d", ErrTypedLimit, w.cfg.maxDepth)
	}
	f := tagFrame{shape: shape}
	var children, following []*lisp.LVal
	switch shape {
	case lisp.ShapeList:
		if err := w.write(`["~#list",`); err != nil {
			return valwalk.Step{}, nil, err
		}
		if err := w.beginCells(len(v.Cells)); err != nil {
			return valwalk.Step{}, nil, err
		}
		children = v.Cells
	case lisp.ShapeArray:
		dims, cells, err := typedArrayParts(v)
		if err != nil {
			return valwalk.Step{}, nil, err
		}
		if len(dims) == 1 {
			if err := w.beginCells(len(cells)); err != nil {
				return valwalk.Step{}, nil, err
			}
			children = cells
		} else {
			f.multi, f.dims, f.cells = true, len(dims), len(cells)
			if err := w.write(`["~#array",[`); err != nil {
				return valwalk.Step{}, nil, err
			}
			if err := w.beginCells(len(dims)); err != nil {
				return valwalk.Step{}, nil, err
			}
			children, following = dims, cells
		}
	case lisp.ShapeTagged:
		if len(v.Cells) != 1 || v.Str == "" || !utf8.ValidString(v.Str) {
			return valwalk.Step{}, nil, errors.New("typed json: malformed tagged value")
		}
		if err := w.write(`["~#tagged",[`); err != nil {
			return valwalk.Step{}, nil, err
		}
		if _, err := w.leaf(lisp.String(v.Str)); err != nil {
			return valwalk.Step{}, nil, err
		}
		if err := w.write(","); err != nil {
			return valwalk.Step{}, nil, err
		}
		children = v.Cells
	case lisp.ShapeMap:
		return w.object(v)
	case lisp.ShapeLeaf, lisp.ShapeError, lisp.ShapeFun, lisp.ShapeNative, lisp.ShapeMark, lisp.ShapeInvalid:
		return w.scalar(v)
	}
	w.frames = append(w.frames, f)
	return valwalk.Step{Children: children, Following: following}, nil, nil
}

func (w *tagWalker) beginCells(n int) error {
	if err := w.write("["); err != nil {
		return err
	}
	if n > w.cfg.maxValues-w.values {
		return fmt.Errorf("%w: more than %d values", ErrTypedLimit, w.cfg.maxValues)
	}
	return nil
}

func (w *tagWalker) nextCells(f tagFrame) error {
	if err := w.write("]"); err != nil {
		return err
	}
	if err := w.write(","); err != nil {
		return err
	}
	return w.beginCells(f.cells)
}

func (w *tagWalker) Child(_ *valwalk.Walker[*lisp.LVal], _ *lisp.LVal, i int) error {
	f := w.frames[len(w.frames)-1]
	if f.shape == lisp.ShapeMap {
		p := f.pairs[i]
		if i > 0 {
			if f.pairs[i-1].Key == p.Key {
				return errors.New("typed json: map has two keys with one encoding")
			}
			if err := w.write(","); err != nil {
				return err
			}
		}
		if err := w.count(); err != nil {
			return err
		}
		if _, err := w.leaf(lisp.String(p.Key)); err != nil {
			return err
		}
		return w.write(":")
	}
	if f.shape == lisp.ShapeTagged {
		return nil
	}
	if f.multi && i == f.dims {
		return w.nextCells(f)
	}
	if i > 0 {
		return w.write(",")
	}
	return nil
}

func (w *tagWalker) Leave(_ *valwalk.Walker[*lisp.LVal], v *lisp.LVal, children []*lisp.LVal) (*lisp.LVal, error) {
	f := w.frames[len(w.frames)-1]
	w.frames = w.frames[:len(w.frames)-1]
	defer releaseTagPairs(f)
	switch f.shape {
	case lisp.ShapeList:
		if err := w.write("]"); err != nil {
			return nil, err
		}
		if err := w.write("]"); err != nil {
			return nil, err
		}
		return lisp.Vector([]*lisp.LVal{lisp.String(tagList), lisp.Vector(slices.Clone(children))}), nil
	case lisp.ShapeArray:
		if !f.multi {
			if err := w.write("]"); err != nil {
				return nil, err
			}
			return lisp.Vector(slices.Clone(children)), nil
		}
		if f.cells == 0 {
			if err := w.nextCells(f); err != nil {
				return nil, err
			}
		}
		if err := w.write("]"); err != nil {
			return nil, err
		}
		if err := w.write("]]"); err != nil {
			return nil, err
		}
		ds, cs := lisp.Vector(slices.Clone(children[:f.dims])), lisp.Vector(slices.Clone(children[f.dims:]))
		return lisp.Vector([]*lisp.LVal{lisp.String(tagArray), lisp.Vector([]*lisp.LVal{ds, cs})}), nil
	case lisp.ShapeTagged:
		if err := w.write("]]"); err != nil {
			return nil, err
		}
		return lisp.Vector([]*lisp.LVal{lisp.String(tagTagged), lisp.Vector([]*lisp.LVal{lisp.String(v.Str), children[0]})}), nil
	case lisp.ShapeMap:
		out := lisp.SortedMap()
		for i, p := range f.pairs {
			out.MapSet(p.Key, children[i])
		}
		return out, w.write("}")
	case lisp.ShapeLeaf, lisp.ShapeError, lisp.ShapeFun, lisp.ShapeNative, lisp.ShapeMark, lisp.ShapeInvalid:
	}
	return nil, errors.New("typed json: unsupported shape")
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

func (w *tagWalker) object(v *lisp.LVal) (valwalk.Step, *lisp.LVal, error) {
	if v.Len() > w.cfg.maxValues-w.values {
		return valwalk.Step{}, nil, fmt.Errorf("%w: more than %d values", ErrTypedLimit, w.cfg.maxValues)
	}
	kp := tagKeyPairPool.Get().(*[]lisp.MapKeyPair)
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
		if err := lisp.GoError(entries); err != nil {
			return valwalk.Step{}, nil, err
		}
		for _, p := range entries.Cells {
			if p == nil || len(p.Cells) != 2 || p.Cells[0] == nil {
				return valwalk.Step{}, nil, errors.New("typed json: malformed map entry")
			}
			k := p.Cells[0]
			keys = append(keys, lisp.MapKeyPair{Kind: k.Type, Key: k.Str, Int: k.Int, Val: p.Cells[1]})
		}
		if err := checkHostMapKeys(keys); err != nil {
			return valwalk.Step{}, nil, err
		}
	}
	sp := mapPairPool.Get().(*[]lisp.MapPair)
	pairs := (*sp)[:0]
	retained := false
	defer func() {
		if !retained {
			releaseTagPairs(tagFrame{pairs: pairs, pool: sp})
		}
	}()
	plainKeys := true
	for _, k := range keys {
		if k.Kind != lisp.LString || needsTilde(k.Key) {
			plainKeys = false
			break
		}
		if !utf8.ValidString(k.Key) {
			return valwalk.Step{}, nil, errors.New("typed json: cannot encode a map key that is not valid UTF-8")
		}
	}
	if plainKeys {
		pairs, ok = v.AppendSortedPairs(pairs)
	}
	if !plainKeys || !ok {
		for _, k := range keys {
			text, err := taggedKeyText(k)
			if err != nil {
				return valwalk.Step{}, nil, err
			}
			pairs = append(pairs, lisp.MapPair{Key: text, Val: k.Val})
		}
		slices.SortFunc(pairs, func(a, b lisp.MapPair) int { return strings.Compare(a.Key, b.Key) })
	}
	if err := w.write("{"); err != nil {
		return valwalk.Step{}, nil, err
	}
	children := make([]*lisp.LVal, len(pairs))
	for i, p := range pairs {
		children[i] = p.Val
	}
	w.frames = append(w.frames, tagFrame{shape: lisp.ShapeMap, pairs: pairs, pool: sp})
	retained = true
	return valwalk.Step{Children: children}, nil, nil
}

func typedArrayParts(v *lisp.LVal) (dims, cells []*lisp.LVal, err error) {
	if len(v.Cells) != 2 || v.Cells[0] == nil || v.Cells[1] == nil || v.Cells[0].Type != lisp.LSExpr || v.Cells[1].Type != lisp.LSExpr {
		return nil, nil, errors.New("typed json: malformed array")
	}
	dims, cells = v.Cells[0].Cells, v.Cells[1].Cells
	zero := false
	for _, d := range dims {
		if d == nil || d.Type != lisp.LInt || d.Int < 0 {
			return nil, nil, errors.New("typed json: malformed array dimensions")
		}
		zero = zero || d.Int == 0
	}
	total := 1
	if zero {
		total = 0
	} else {
		for _, d := range dims {
			if total > math.MaxInt/d.Int {
				return nil, nil, errors.New("typed json: malformed array dimensions")
			}
			total *= d.Int
		}
	}
	if total != len(cells) {
		return nil, nil, errors.New("typed json: array contents do not match its dimensions")
	}
	return dims, cells, nil
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
	if in.Type == lisp.LError {
		return in
	}
	var lerr *lisp.LVal
	charge := WithTypedCharge(func(kib int) error {
		if rc := env.ChargeSteps(int64(kib)); rc.Type == lisp.LError {
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
