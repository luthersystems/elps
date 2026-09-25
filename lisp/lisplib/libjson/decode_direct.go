// Copyright © 2026 The ELPS authors

package libjson

import (
	"encoding/json"
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
	stack []*lisp.LVal
	opts  LoadOpts
	i     int
	fail  bool
}

func (d *directDecoder) skipSpace() {
	for d.i < len(d.b) {
		switch d.b[d.i] {
		case ' ', '\t', '\n', '\r':
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
	switch c := d.b[d.i]; c {
	case '{':
		return d.object()
	case '[':
		return d.array()
	case '"':
		return lisp.String(d.str())
	case 't':
		d.i += 4
		return lisp.Bool(true)
	case 'f':
		d.i += 5
		return lisp.Bool(false)
	case 'n':
		d.i += 4
		return lisp.Nil()
	default:
		return d.number()
	}
}

func (d *directDecoder) object() *lisp.LVal {
	d.i++ // '{'
	m := make(map[string]any)
	d.skipSpace()
	if d.b[d.i] == '}' {
		d.i++
		return jsonraw.Wrap(m)
	}
	for {
		d.skipSpace()
		k := d.str()
		d.skipSpace()
		d.i++ // ':'
		v := d.value()
		if d.fail {
			return nil
		}
		m[k] = v
		d.skipSpace()
		c := d.b[d.i]
		d.i++
		if c == '}' {
			break
		}
	}
	if d.opts.MaxAlloc > 0 && len(m) > d.opts.MaxAlloc {
		d.fail = true
		return nil
	}
	return jsonraw.Wrap(m)
}

func (d *directDecoder) array() *lisp.LVal {
	d.i++ // '['
	base := len(d.stack)
	d.skipSpace()
	if d.b[d.i] == ']' {
		d.i++
		return lisp.Array(nil, []*lisp.LVal{})
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
		d.fail = true
		return nil
	}
	cells := make([]*lisp.LVal, n)
	copy(cells, d.stack[base:])
	clear(d.stack[base:])
	d.stack = d.stack[:base]
	return lisp.Array(nil, cells)
}

// str decodes the string starting at d.i. A string with no escapes and valid
// UTF-8 is taken verbatim; anything else is handed to encoding/json, which
// owns the escape, surrogate and invalid-UTF-8 replacement rules.
func (d *directDecoder) str() string {
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
	if d.opts.StringNumbers {
		return lisp.String(text)
	}
	if d.opts.ExactIntegers {
		v := loadNumber(text)
		if v.Type == lisp.LError {
			d.fail = true
			return nil
		}
		return v
	}
	f, err := strconv.ParseFloat(text, 64)
	if err != nil {
		d.fail = true
		return nil
	}
	return lisp.Float(f)
}
