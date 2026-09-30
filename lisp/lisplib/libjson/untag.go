// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// Untag restores the data types represented by Tag. Unknown tags and
// malformed forms return an error. Input must contain plain JSON values.
func Untag(v *lisp.LVal, opts ...TypedOption) (*lisp.LVal, error) {
	w := untagWalker{cfg: newTypedConfig(opts)}
	return w.value(v, 0)
}

type untagWalker struct {
	cfg           typedConfig
	values        int
	size, charged int
}

func (w *untagWalker) add(n int) error {
	w.size += n
	if w.size > w.cfg.maxBytes {
		return fmt.Errorf("%w: input exceeds %d bytes", ErrTypedLimit, w.cfg.maxBytes)
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

func (w *untagWalker) stringSize(s string) error {
	if !utf8.ValidString(s) {
		return errors.New("typed json: string is not valid UTF-8")
	}
	if len(s)+2 > w.cfg.maxBytes-w.size {
		return fmt.Errorf("%w: input exceeds %d bytes", ErrTypedLimit, w.cfg.maxBytes)
	}
	return w.add(len(appendJSONString(nil, s)))
}

func (w *untagWalker) count() error {
	w.values++
	if w.values > w.cfg.maxValues {
		return fmt.Errorf("%w: more than %d values", ErrTypedLimit, w.cfg.maxValues)
	}
	return nil
}

func (w *untagWalker) value(v *lisp.LVal, depth int) (*lisp.LVal, error) {
	if v == nil {
		return nil, errors.New("typed json: Go nil value")
	}
	if err := w.count(); err != nil {
		return nil, err
	}
	switch v.Type {
	case lisp.LString:
		if !utf8.ValidString(v.Str) {
			return nil, errors.New("typed json: string is not valid UTF-8")
		}
		if err := w.stringSize(v.Str); err != nil {
			return nil, err
		}
		d := typedDecoder{}
		return d.stringValue([]byte(v.Str))
	case lisp.LInt:
		if !exactInt(int64(v.Int)) {
			return nil, errors.New("typed json: non-canonical int")
		}
		if err := w.add(len(strconv.Itoa(v.Int))); err != nil {
			return nil, err
		}
		return lisp.Int(v.Int), nil
	case lisp.LFloat:
		if math.IsNaN(v.Float) || math.IsInf(v.Float, 0) || math.Trunc(v.Float) == v.Float {
			return nil, errors.New("typed json: whole float requires ~d")
		}
		if err := w.add(len(appendJSONFloat(nil, v.Float))); err != nil {
			return nil, err
		}
		return lisp.Float(v.Float), nil
	case lisp.LSymbol:
		if v.Str == lisp.TrueSymbol || v.Str == lisp.FalseSymbol {
			if err := w.add(len(v.Str)); err != nil {
				return nil, err
			}
			return lisp.Symbol(v.Str), nil
		}
	case lisp.LSExpr:
		if v.IsNil() {
			if err := w.add(4); err != nil {
				return nil, err
			}
			return lisp.SExpr(nil), nil
		}
	case lisp.LArray, lisp.LSortMap:
		if depth >= w.cfg.maxDepth {
			return nil, fmt.Errorf("%w: nesting depth exceeds %d", ErrTypedLimit, w.cfg.maxDepth)
		}
		if v.Type == lisp.LSortMap {
			return w.object(v, depth)
		}
		dims, cells, err := typedArrayParts(v)
		if err != nil || len(dims) != 1 {
			return nil, errors.New("typed json: input is not a vector")
		}
		if len(cells) > 0 && cells[0] != nil && cells[0].Type == lisp.LString && strings.HasPrefix(cells[0].Str, "~#") {
			if len(cells) != 2 {
				return nil, errors.New("typed json: malformed composite tag")
			}
			if err := w.add(3); err != nil {
				return nil, err
			}
			if err := w.stringSize(cells[0].Str); err != nil {
				return nil, err
			}
			return w.tagged(cells[0].Str, cells[1], depth)
		}
		out, err := w.cells(cells, depth)
		if err != nil {
			return nil, err
		}
		return lisp.Vector(out), nil
	default:
		break
	}
	return nil, fmt.Errorf("typed json: input is not a plain JSON value: %v", v.Type)
}

func vectorCells(v *lisp.LVal) ([]*lisp.LVal, error) {
	if v == nil || v.Type != lisp.LArray {
		return nil, errors.New("typed json: tag payload is not a vector")
	}
	dims, cells, err := typedArrayParts(v)
	if err != nil || len(dims) != 1 {
		return nil, errors.New("typed json: tag payload is not a vector")
	}
	return cells, nil
}

func (w *untagWalker) cells(cells []*lisp.LVal, depth int) ([]*lisp.LVal, error) {
	if err := w.add(2 + max(0, len(cells)-1)); err != nil {
		return nil, err
	}
	out := make([]*lisp.LVal, len(cells))
	for i, c := range cells {
		v, err := w.value(c, depth+1)
		if err != nil {
			return nil, err
		}
		out[i] = v
	}
	return out, nil
}

func (w *untagWalker) tagged(tag string, payload *lisp.LVal, depth int) (*lisp.LVal, error) {
	cells, err := vectorCells(payload)
	if err != nil {
		return nil, err
	}
	switch tag {
	case tagList:
		if len(cells) == 0 {
			return nil, errors.New("typed json: empty list must be null")
		}
		out, err := w.cells(cells, depth)
		if err != nil {
			return nil, err
		}
		return lisp.QExpr(out), nil
	case tagTagged:
		if err := w.add(2 + max(0, len(cells)-1)); err != nil {
			return nil, err
		}
		if len(cells) != 2 || cells[0] == nil || cells[0].Type != lisp.LString || cells[0].Str == "" || !utf8.ValidString(cells[0].Str) {
			return nil, errors.New("typed json: malformed tagged value")
		}
		if err := w.stringSize(cells[0].Str); err != nil {
			return nil, err
		}
		inner, err := w.value(cells[1], depth+1)
		if err != nil {
			return nil, err
		}
		return &lisp.LVal{Type: lisp.LTaggedVal, Str: cells[0].Str, Cells: []*lisp.LVal{inner}}, nil
	case tagArray:
		if err := w.add(2 + max(0, len(cells)-1)); err != nil {
			return nil, err
		}
		if len(cells) != 2 {
			return nil, errors.New("typed json: malformed array tag")
		}
		ds, err := vectorCells(cells[0])
		if err != nil {
			return nil, err
		}
		cs, err := vectorCells(cells[1])
		if err != nil {
			return nil, err
		}
		dims, err := w.cells(ds, depth)
		if err != nil {
			return nil, err
		}
		contents, err := w.cells(cs, depth)
		if err != nil {
			return nil, err
		}
		return restoreArray(dims, contents)
	default:
		return nil, errors.New("typed json: unknown tag")
	}
}

func restoreArray(dims, cells []*lisp.LVal) (*lisp.LVal, error) {
	if len(dims) == 1 {
		return nil, errors.New("typed json: vector written as a tagged array")
	}
	shape := &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr(dims), lisp.QExpr(cells)}}
	if _, _, err := typedArrayParts(shape); err != nil {
		return nil, err
	}
	v := lisp.Array(lisp.QExpr(dims), cells)
	if v.Type == lisp.LError {
		return nil, errors.New("typed json: invalid array dimensions")
	}
	return v, nil
}

func (w *untagWalker) object(v *lisp.LVal, depth int) (*lisp.LVal, error) {
	entries := v.MapEntries()
	if entries.Type == lisp.LError {
		return nil, lisp.GoError(entries)
	}
	if err := w.add(2 + max(0, len(entries.Cells)-1)); err != nil {
		return nil, err
	}
	out := lisp.SortedMap()
	d := typedDecoder{}
	for _, p := range entries.Cells {
		if p == nil || len(p.Cells) != 2 || p.Cells[0] == nil || p.Cells[0].Type != lisp.LString {
			return nil, errors.New("typed json: input map key is not a string")
		}
		if err := w.count(); err != nil {
			return nil, err
		}
		if !utf8.ValidString(p.Cells[0].Str) {
			return nil, errors.New("typed json: key is not valid UTF-8")
		}
		if err := w.stringSize(p.Cells[0].Str); err != nil {
			return nil, err
		}
		if err := w.add(1); err != nil {
			return nil, err
		}
		k, err := d.key([]byte(p.Cells[0].Str))
		if err != nil {
			return nil, err
		}
		inner, err := w.value(p.Cells[1], depth+1)
		if err != nil {
			return nil, err
		}
		if rc := out.MapSetLVal(k, inner); rc.Type == lisp.LError {
			return nil, lisp.GoError(rc)
		}
	}
	if out.Len() != len(entries.Cells) {
		return nil, errors.New("typed json: two members name one key")
	}
	return out, nil
}
