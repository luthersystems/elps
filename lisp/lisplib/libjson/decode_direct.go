// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"math"
	"strconv"
	"unicode/utf8"

	"github.com/luthersystems/elps/internal/jsonraw"
	"github.com/luthersystems/elps/lisp"
)

// loadDirect decodes b straight into LVals, without the intermediate
// map[string]any / []any tree that json.Unmarshal builds (elps#689).
//
// It is a FAST PATH, not a second definition of JSON. It runs only on a
// document json.Valid has accepted, and json.Valid uses the same scanner as
// json.Unmarshal and json.Decoder, so acceptance is unchanged by
// construction. Whenever the fast path meets anything it would have to
// report -- a number out of range, an allocation over MaxAlloc, an exact
// integer that does not fit -- it returns ok=false and LoadWith falls back to
// the original two-step decode, so every error value and message is the one
// the original path produces.
func (s *Serializer) loadDirect(b []byte, opts LoadOpts) (*lisp.LVal, bool) {
	if !json.Valid(b) {
		return nil, false
	}
	d := directDecoder{b: b, opts: opts}
	v := d.value()
	if d.fail {
		return nil, false
	}
	return v, true
}

type directDecoder struct {
	b []byte
	// stack is scratch space shared by every array being decoded, so each
	// array's final cell slice is allocated once at its exact length.
	stack    []*lisp.LVal
	err      error
	semantic typedDecoder
	opts     LoadOpts
	i        int
	depth    int
	fail     bool
}

func (d *directDecoder) skipSpace() {
	for d.i < len(d.b) {
		switch d.b[d.i] {
		case ' ', '\t', '\n', '\r':
			if d.opts.Strict {
				d.reject(errors.New("json: non-canonical whitespace"))
			}
			d.i++
		default:
			return
		}
	}
}

// value decodes one value. The document is known to be valid JSON, so the
// decoder never checks grammar, only the semantic conditions listed on
// loadDirect.
func (d *directDecoder) value() *lisp.LVal {
	d.skipSpace()
	if d.opts.Typed {
		if err := d.semantic.count(); err != nil {
			d.reject(err)
			return nil
		}
	}
	if d.opts.Typed && (d.b[d.i] == '[' || d.b[d.i] == '{') {
		if d.opts.Typed && d.depth >= d.semantic.cfg.maxDepth {
			d.reject(fmt.Errorf("%w: nesting depth exceeds %d", ErrTypedLimit, d.semantic.cfg.maxDepth))
			return nil
		}
		d.depth++
		defer func() { d.depth-- }()
	}
	switch c := d.b[d.i]; c {
	case '{':
		return d.object()
	case '[':
		return d.array()
	case '"':
		if d.opts.Typed {
			s := d.strictString()
			if d.fail {
				return nil
			}
			v, err := d.semantic.stringValue(s)
			if err != nil {
				d.reject(err)
			}
			return v
		}
		return lisp.String(d.str())
	case 't':
		d.i += 4
		return lisp.Bool(true)
	case 'f':
		d.i += 5
		return lisp.Bool(false)
	case 'n':
		d.i += 4
		if d.opts.Typed {
			return lisp.SExpr(nil)
		}
		return lisp.Nil()
	default:
		return d.number()
	}
}

func (d *directDecoder) object() *lisp.LVal {
	d.i++
	m := make(map[string]any)
	var typed *lisp.LVal
	d.skipSpace()
	if d.b[d.i] == '}' {
		d.i++
		return jsonraw.Wrap(m)
	}
	prev, size := "", 0
	for {
		d.skipSpace()
		var k string
		var key *lisp.LVal
		if d.opts.Typed {
			raw := d.strictString()
			if d.fail {
				return nil
			}
			k = string(raw)
			if err := d.semantic.count(); err != nil {
				d.reject(err)
				return nil
			}
			if needsTilde(k) {
				var err error
				key, err = d.semantic.key(raw)
				if err != nil {
					d.reject(err)
					return nil
				}
			}
		} else {
			k = d.str()
		}
		if d.fail {
			return nil
		}
		if d.opts.Strict && size > 0 && prev >= k {
			d.reject(errors.New("json: members out of order or duplicated"))
			return nil
		}
		prev = k
		d.skipSpace()
		d.i++
		v := d.value()
		if d.fail {
			return nil
		}
		size++
		if key != nil && key.Type != lisp.LString && typed == nil {
			typed = lisp.SortedMap()
			for name, value := range m {
				typed.MapSet(name, value.(*lisp.LVal))
			}
		}
		if typed != nil {
			if key == nil {
				key = lisp.String(k)
			}
			if r := typed.MapSetLVal(key, v); r.Type == lisp.LError {
				d.reject(lisp.GoError(r))
				return nil
			}
			if typed.Len() != size {
				d.reject(errors.New("typed json: two members name one key"))
				return nil
			}
		} else {
			name := k
			if key != nil {
				name = key.Str
			}
			m[name] = v
			if d.opts.Typed && len(m) != size {
				d.reject(errors.New("typed json: two members name one key"))
				return nil
			}
		}
		d.skipSpace()
		c := d.b[d.i]
		d.i++
		if c == '}' {
			break
		}
	}
	count := len(m)
	if typed != nil {
		count = size
	}
	if d.opts.MaxAlloc > 0 && count > d.opts.MaxAlloc {
		d.reject(errors.New("json: allocation limit"))
		return nil
	}
	if typed != nil {
		return typed
	}
	return jsonraw.Wrap(m)
}

func (d *directDecoder) array() *lisp.LVal {
	if d.opts.Typed && bytes.HasPrefix(d.b[d.i:], []byte(`["~#`)) {
		return d.composite()
	}
	cells := d.elements()
	if d.fail {
		return nil
	}
	return lisp.Vector(cells)
}

func (d *directDecoder) elements() []*lisp.LVal {
	if !d.expect('[') {
		return nil
	}
	base := len(d.stack)
	d.skipSpace()
	if d.b[d.i] == ']' {
		d.i++
		return []*lisp.LVal{}
	}
	for {
		v := d.value()
		if d.fail {
			return nil
		}
		d.stack = append(d.stack, v)
		d.skipSpace()
		c := d.b[d.i]
		d.i++
		if c == ']' {
			break
		}
	}
	n := len(d.stack) - base
	if d.opts.MaxAlloc > 0 && n > d.opts.MaxAlloc {
		d.reject(errors.New("json: allocation limit"))
		return nil
	}
	cells := make([]*lisp.LVal, n)
	copy(cells, d.stack[base:])
	clear(d.stack[base:])
	d.stack = d.stack[:base]
	return cells
}

func (d *directDecoder) composite() *lisp.LVal {
	d.i++
	tag := d.str()
	if !d.expect(',') {
		return nil
	}
	var v *lisp.LVal
	switch tag {
	case tagList:
		cells := d.elements()
		if d.fail {
			return nil
		}
		if len(cells) == 0 {
			d.reject(errors.New("typed json: empty list must be null"))
			return nil
		}
		v = lisp.QExpr(cells)
	case tagTagged:
		if !d.expect('[') || d.i >= len(d.b) || d.b[d.i] != '"' {
			d.reject(errors.New("typed json: malformed tagged value"))
			return nil
		}
		name := d.str()
		if name == "" {
			d.reject(errors.New("typed json: empty type name"))
			return nil
		}
		if !d.expect(',') {
			return nil
		}
		inner := d.value()
		if d.fail || !d.expect(']') {
			return nil
		}
		v = &lisp.LVal{Type: lisp.LTaggedVal, Str: name, Cells: []*lisp.LVal{inner}}
	case tagArray:
		if !d.expect('[') {
			return nil
		}
		dims := d.elements()
		if d.fail || !d.expect(',') {
			return nil
		}
		cells := d.elements()
		if d.fail || !d.expect(']') {
			return nil
		}
		var err error
		v, err = restoreArray(dims, cells)
		if err != nil {
			d.reject(err)
			return nil
		}
	default:
		d.reject(errors.New("typed json: unknown tag"))
		return nil
	}
	if !d.expect(']') {
		return nil
	}
	return v
}

func (d *directDecoder) expect(c byte) bool {
	if d.i >= len(d.b) || d.b[d.i] != c {
		d.reject(fmt.Errorf("json: expected %q", c))
		return false
	}
	d.i++
	return true
}

func (d *directDecoder) reject(err error) {
	d.fail = true
	if d.err == nil {
		d.err = err
	}
}

// str decodes the string starting at d.i. A string with no escapes and valid
// UTF-8 is taken verbatim; anything else is handed to encoding/json, which
// owns the escape, surrogate and invalid-UTF-8 replacement rules.
func (d *directDecoder) str() string {
	if d.opts.Strict {
		return string(d.strictString())
	}
	start := d.i
	d.i++ // opening quote
	simple := true
	for {
		c := d.b[d.i]
		if c == '"' {
			break
		}
		if c == '\\' {
			simple = false
			d.i += 2
			continue
		}
		if c >= utf8.RuneSelf {
			simple = false
		}
		d.i++
	}
	d.i++ // closing quote
	raw := d.b[start:d.i]
	if simple {
		return string(raw[1 : len(raw)-1])
	}
	body := raw[1 : len(raw)-1]
	if utf8.Valid(body) && !containsByte(body, '\\') {
		return string(body)
	}
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		d.fail = true
	}
	return s
}

func (d *directDecoder) strictString() []byte {
	d.semantic.b, d.semantic.i = d.b, d.i
	s, err := d.semantic.rawString()
	d.i = d.semantic.i
	if err != nil {
		d.reject(err)
		return nil
	}
	return s
}

func containsByte(b []byte, c byte) bool {
	for _, x := range b {
		if x == c {
			return true
		}
	}
	return false
}

func (d *directDecoder) number() *lisp.LVal {
	start := d.i
	for d.i < len(d.b) {
		switch d.b[d.i] {
		case '-', '+', '.', 'e', 'E', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			d.i++
			continue
		}
		break
	}
	text := string(d.b[start:d.i])
	if d.opts.Strict {
		if _, ok := smallCanonicalInt(d.b[start:d.i]); !ok && !isJSONInteger(text) {
			f, err := strconv.ParseFloat(text, 64)
			if err != nil || math.IsInf(f, 0) || string(appendJSONFloat(nil, f)) != text {
				d.reject(errors.New("json: non-canonical number"))
				return nil
			}
		}
	}
	if d.opts.Typed {
		if n, ok := smallCanonicalInt(d.b[start:d.i]); ok {
			return lisp.Int(n)
		}
		if isJSONInteger(text) {
			n, err := d.semantic.canonicalInt(d.b[start:d.i])
			if err != nil {
				d.reject(err)
				return nil
			}
			if !exactInt(int64(n)) {
				d.reject(errors.New("typed json: non-canonical number"))
				return nil
			}
			return lisp.Int(n)
		}
		f, err := strconv.ParseFloat(text, 64)
		if err != nil || math.Trunc(f) == f {
			d.reject(errors.New("typed json: whole float requires ~d"))
			return nil
		}
		return lisp.Float(f)
	}
	if d.opts.StringNumbers {
		return lisp.String(text)
	}
	if d.opts.ExactIntegers {
		v := loadNumber(text)
		if v.Type == lisp.LError {
			d.reject(lisp.GoError(v))
			return nil
		}
		return v
	}
	f, err := strconv.ParseFloat(text, 64)
	if err != nil {
		d.reject(err)
		return nil
	}
	return lisp.Float(f)
}

// loadStrict checks canonical spelling while decoding each token.
func loadStrict(b []byte, opts LoadOpts, cfg typedConfig) (*lisp.LVal, error) {
	if opts.Typed && len(b) > cfg.maxBytes {
		return nil, fmt.Errorf("%w: input exceeds %d bytes", ErrTypedLimit, cfg.maxBytes)
	}
	if !json.Valid(b) {
		return nil, errors.New("json: invalid JSON")
	}
	opts.Strict = true
	d := directDecoder{b: b, opts: opts, semantic: typedDecoder{cfg: cfg}}
	v := d.value()
	d.skipSpace()
	if d.fail {
		return nil, d.err
	}
	if d.i != len(b) {
		return nil, errors.New("json: trailing bytes")
	}
	return v, nil
}

// LoadTyped applies strict plain decoding and restores tags during decoding.
func LoadTyped(b []byte, opts ...TypedOption) (*lisp.LVal, error) {
	return loadStrict(b, LoadOpts{Typed: true, ExactIntegers: true}, newTypedConfig(opts))
}
