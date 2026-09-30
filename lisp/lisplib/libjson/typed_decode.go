// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"encoding/base64"
	"errors"
	"fmt"
	"math"
	"slices"
	"strconv"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// LoadTyped decodes b, which must be one complete document exactly as
// DumpTyped writes it.  Anything DumpTyped could not have produced is
// rejected with an error: whitespace, a member out of JCS order or
// duplicated, number text other than the canonical text of its value (1.50,
// 1E5, -0, 01, an int written as a float or past 2^53 as a number), an
// escape other than the minimal one, a string beginning with '^' or '`', an
// unknown or misused tag, null, non-canonical base64, trailing bytes, and
// input over any configured limit.  It never panics on malformed input.
//
// Every value it returns is freshly allocated and shares no storage with b
// or with any other value, so the caller owns it outright.  Lists come back
// as data lists, as list builds them.  A tagged value comes back with its
// type name and data, and is not checked against any type defined with
// deftype.
//
// Memory is bounded by the input: every value costs at least one byte of
// it, and nothing is reserved from a count the input declares.
func LoadTyped(b []byte, opts ...TypedOption) (*lisp.LVal, error) {
	d := typedDecoder{cfg: newTypedConfig(opts), b: b}
	if len(b) > d.cfg.maxBytes {
		return nil, fmt.Errorf("%w: input exceeds %d bytes", ErrTypedLimit, d.cfg.maxBytes)
	}
	v, err := d.value(0)
	if err != nil {
		return nil, err
	}
	if d.i != len(b) {
		return nil, d.errorf("trailing bytes")
	}
	return v, nil
}

type typedDecoder struct {
	b []byte
	// stack is scratch shared by every array being decoded: elements are
	// pushed here and each array's cells are copied out at their exact
	// length, so no slice is sized from anything but values already read.
	stack  []*lisp.LVal
	str    []byte // scratch for strings with escapes
	b64    []byte // scratch for the base64 canonicality check
	cfg    typedConfig
	i      int
	values int
}

func (d *typedDecoder) errorf(format string, args ...any) error {
	return fmt.Errorf("typed json: offset %d: "+format, append([]any{d.i}, args...)...)
}

func (d *typedDecoder) peek() byte {
	if d.i < len(d.b) {
		return d.b[d.i]
	}
	return 0
}

func (d *typedDecoder) expect(c byte) error {
	if d.peek() != c {
		return d.errorf("expected %q", c)
	}
	d.i++
	return nil
}

func (d *typedDecoder) count() error {
	d.values++
	if d.values > d.cfg.maxValues {
		return fmt.Errorf("%w: more than %d values", ErrTypedLimit, d.cfg.maxValues)
	}
	return nil
}

func (d *typedDecoder) value(depth int) (*lisp.LVal, error) {
	if err := d.count(); err != nil {
		return nil, err
	}
	switch c := d.peek(); {
	case c == '"':
		s, err := d.rawString()
		if err != nil {
			return nil, err
		}
		return d.stringValue(s)
	case c == '-' || c >= '0' && c <= '9':
		return d.number()
	case c == 't':
		if bytes.HasPrefix(d.b[d.i:], []byte("true")) {
			d.i += 4
			return lisp.Symbol(lisp.TrueSymbol), nil
		}
	case c == 'f':
		if bytes.HasPrefix(d.b[d.i:], []byte("false")) {
			d.i += 5
			return lisp.Symbol(lisp.FalseSymbol), nil
		}
	case c == '[':
		if depth >= d.cfg.maxDepth {
			return nil, fmt.Errorf("%w: nesting depth exceeds %d", ErrTypedLimit, d.cfg.maxDepth)
		}
		return d.array(depth)
	case c == '{':
		if depth >= d.cfg.maxDepth {
			return nil, fmt.Errorf("%w: nesting depth exceeds %d", ErrTypedLimit, d.cfg.maxDepth)
		}
		return d.object(depth)
	}
	return nil, d.errorf("invalid value")
}

// rawString reads a JSON string written with canonical escapes and returns
// its content.  The result aliases d.b or d.str and is valid only until the
// next call.
func (d *typedDecoder) rawString() ([]byte, error) {
	if err := d.expect('"'); err != nil {
		return nil, err
	}
	start := d.i
	for d.i < len(d.b) {
		c := d.b[d.i]
		if c == '"' {
			s := d.b[start:d.i]
			d.i++
			if !utf8.Valid(s) {
				return nil, d.errorf("string is not valid UTF-8")
			}
			return s, nil
		}
		if c == '\\' || c < 0x20 {
			break
		}
		d.i++
	}
	out := append(d.str[:0], d.b[start:d.i]...)
	for d.i < len(d.b) {
		c := d.b[d.i]
		switch {
		case c == '"':
			d.i++
			d.str = out
			if !utf8.Valid(out) {
				return nil, d.errorf("string is not valid UTF-8")
			}
			return out, nil
		case c < 0x20:
			return nil, d.errorf("unescaped control character")
		case c != '\\':
			// Copy the run up to the next quote, escape or control
			// character in one append.
			j := d.i + 1
			for j < len(d.b) && d.b[j] != '"' && d.b[j] != '\\' && d.b[j] >= 0x20 {
				j++
			}
			out = append(out, d.b[d.i:j]...)
			d.i = j
			continue
		}
		if d.i+1 >= len(d.b) {
			break
		}
		esc := d.b[d.i+1]
		d.i += 2
		switch esc {
		case '"', '\\':
			out = append(out, esc)
		case 'b':
			out = append(out, '\b')
		case 'f':
			out = append(out, '\f')
		case 'n':
			out = append(out, '\n')
		case 'r':
			out = append(out, '\r')
		case 't':
			out = append(out, '\t')
		case 'u':
			// Only a control character without a short form, as
			// 00 and two lowercase hex digits.
			x, ok := canonicalControlEscape(d.b[d.i:])
			if !ok {
				return nil, d.errorf("non-canonical escape")
			}
			out = append(out, x)
			d.i += 4
		default:
			return nil, d.errorf("non-canonical escape")
		}
	}
	return nil, d.errorf("unterminated string")
}

// canonicalControlEscape reads the four hex digits of an escape that must
// be one appendJSONString writes: a control character without a short
// form, as 00 and two lowercase hex digits.
func canonicalControlEscape(b []byte) (byte, bool) {
	if len(b) < 4 || b[0] != '0' || b[1] != '0' || (b[2] != '0' && b[2] != '1') {
		return 0, false
	}
	var lo byte
	switch c := b[3]; {
	case c >= '0' && c <= '9':
		lo = c - '0'
	case c >= 'a' && c <= 'f':
		lo = c - 'a' + 10
	default:
		return 0, false
	}
	x := (b[2]-'0')<<4 | lo
	switch x {
	case '\b', '\f', '\n', '\r', '\t':
		return 0, false
	}
	return x, true
}

// stringValue interprets a string in value position.
func (d *typedDecoder) stringValue(s []byte) (*lisp.LVal, error) {
	if len(s) == 0 {
		return lisp.String(""), nil
	}
	switch s[0] {
	case '^', '`':
		return nil, d.errorf("unescaped reserved string")
	case '~':
	default:
		return lisp.String(string(s)), nil
	}
	if len(s) < 2 {
		return nil, d.errorf("invalid tagged string")
	}
	body := s[2:]
	switch s[1] {
	case '~', '^', '`':
		return lisp.String(string(s[1:])), nil
	case ':':
		return lisp.Symbol(":" + string(body)), nil
	case '$':
		if err := checkSymbolName(body); err != nil {
			return nil, d.errorf("%v", err)
		}
		return lisp.Symbol(string(body)), nil
	case 'b':
		out := make([]byte, base64.StdEncoding.DecodedLen(len(body)))
		n, err := base64.StdEncoding.Decode(out, body)
		if err != nil {
			return nil, d.errorf("invalid base64")
		}
		out = out[:n:n]
		d.b64 = base64.StdEncoding.AppendEncode(d.b64[:0], out)
		if !bytes.Equal(d.b64, body) {
			return nil, d.errorf("non-canonical base64")
		}
		return lisp.Bytes(out), nil
	case 'i':
		n, err := d.canonicalInt(body)
		if err != nil {
			return nil, err
		}
		if exactInt(int64(n)) {
			return nil, d.errorf("non-canonical int")
		}
		return lisp.Int(n), nil
	case 'z':
		switch string(body) {
		case "NaN":
			return lisp.Float(math.NaN()), nil
		case "INF":
			return lisp.Float(math.Inf(1)), nil
		case "-INF":
			return lisp.Float(math.Inf(-1)), nil
		}
	}
	return nil, d.errorf("invalid tagged string")
}

// checkSymbolName rejects a "~$" name DumpTyped writes another way.
func checkSymbolName(name []byte) error {
	switch {
	case len(name) == 0:
		return errors.New("empty symbol")
	case name[0] == ':':
		return errors.New("keyword written as a symbol")
	case string(name) == lisp.TrueSymbol || string(name) == lisp.FalseSymbol:
		return errors.New("boolean written as a symbol")
	}
	return nil
}

// parseCanonicalInt parses decimal text that is exactly strconv.Itoa of an
// int.
func parseCanonicalInt(b []byte) (int, bool) {
	n, err := strconv.ParseInt(string(b), 10, strconv.IntSize)
	if err != nil {
		return 0, false
	}
	var tmp [24]byte
	return int(n), bytes.Equal(strconv.AppendInt(tmp[:0], n, 10), b)
}

// maxFastDigits is the most digits smallCanonicalInt accumulates without
// overflow: 15 digits stay below 2^53 in a 64-bit int, 9 below 2^31 in a
// 32-bit one.
const maxFastDigits = 9 + 6*(strconv.IntSize/64)

// canonicalInt parses decimal text that must be exactly strconv.Itoa of an
// int.  Where int is 32 bits, a canonical int that needs 64 is rejected with
// an error, never truncated.
func (d *typedDecoder) canonicalInt(b []byte) (int, error) {
	if n, ok := parseCanonicalInt(b); ok {
		return n, nil
	}
	if n, err := strconv.ParseInt(string(b), 10, 64); err == nil && strconv.FormatInt(n, 10) == string(b) {
		return 0, d.errorf("integer %s does not fit in a %d-bit int", b, strconv.IntSize)
	}
	return 0, d.errorf("non-canonical number")
}

// smallCanonicalInt is the fast path of number for canonical int text of
// at most maxFastDigits digits, which always fits and is below 2^53: an optional '-', then "0"
// or a digit string without a leading zero, never "-0".  ok is false for
// anything else, which the slow path then judges.
func smallCanonicalInt(b []byte) (int, bool) {
	neg := len(b) > 0 && b[0] == '-'
	if neg {
		b = b[1:]
	}
	if len(b) == 0 || len(b) > maxFastDigits || (b[0] == '0' && (len(b) > 1 || neg)) {
		return 0, false
	}
	n := 0
	for _, c := range b {
		if c < '0' || c > '9' {
			return 0, false
		}
		n = n*10 + int(c-'0')
	}
	if neg {
		n = -n
	}
	return n, true
}

// number reads a JSON number.  Text without '.' or an exponent is an int;
// anything else a float.  Either way the text must be exactly what
// DumpTyped writes for the value it denotes.
func (d *typedDecoder) number() (*lisp.LVal, error) {
	start := d.i
	isFloat := false
	for d.i < len(d.b) {
		c := d.b[d.i]
		if c >= '0' && c <= '9' || c == '-' {
			d.i++
		} else if c == '.' || c == 'e' || c == '+' {
			isFloat = true
			d.i++
		} else {
			break
		}
	}
	text := d.b[start:d.i]
	if !isFloat {
		if n, ok := smallCanonicalInt(text); ok {
			return lisp.Int(n), nil
		}
		n, err := d.canonicalInt(text)
		if err != nil {
			return nil, err
		}
		if !exactInt(int64(n)) {
			return nil, d.errorf("non-canonical number")
		}
		return lisp.Int(n), nil
	}
	f, err := strconv.ParseFloat(string(text), 64)
	if err != nil || math.IsInf(f, 0) {
		return nil, d.errorf("non-canonical number")
	}
	var tmp [40]byte
	if !bytes.Equal(appendTypedFloat(tmp[:0], f), text) {
		return nil, d.errorf("non-canonical number")
	}
	return lisp.Float(f), nil
}

// array reads a JSON array: a list, or a tagged composite whose first
// element is a "~#" tag.
func (d *typedDecoder) array(depth int) (*lisp.LVal, error) {
	d.i++
	if bytes.HasPrefix(d.b[d.i:], []byte(`"~#`)) {
		return d.tagged(depth)
	}
	cells, err := d.elements(depth)
	if err != nil {
		return nil, err
	}
	return lisp.QExpr(cells), nil
}

// elements reads values up to and including the closing ']' (the '[' has
// been read) and returns them in a fresh slice of exact length.
func (d *typedDecoder) elements(depth int) ([]*lisp.LVal, error) {
	base := len(d.stack)
	defer func() {
		clear(d.stack[base:])
		d.stack = d.stack[:base]
	}()
	if d.peek() == ']' {
		d.i++
		return []*lisp.LVal{}, nil
	}
	for {
		v, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		d.stack = append(d.stack, v)
		switch d.peek() {
		case ',':
			d.i++
		case ']':
			d.i++
			return slices.Clone(d.stack[base:]), nil
		default:
			return nil, d.errorf("expected ',' or ']'")
		}
	}
}

func (d *typedDecoder) tagged(depth int) (*lisp.LVal, error) {
	var tag string
	for _, t := range [...]string{tagVector, tagArray, tagTagged} {
		if bytes.HasPrefix(d.b[d.i:], []byte(`"`+t+`",`)) {
			tag = t
			break
		}
	}
	if tag == "" {
		return nil, d.errorf("unknown tag")
	}
	d.i += len(tag) + 3
	if err := d.expect('['); err != nil {
		return nil, err
	}
	var v *lisp.LVal
	switch tag {
	case tagVector:
		cells, err := d.elements(depth)
		if err != nil {
			return nil, err
		}
		v = lisp.Vector(cells)
	case tagTagged:
		s, err := d.rawString()
		if err != nil {
			return nil, err
		}
		if len(s) == 0 {
			return nil, d.errorf("tagged value with an empty type")
		}
		name := string(s)
		if err := d.expect(','); err != nil {
			return nil, err
		}
		inner, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		if err := d.expect(']'); err != nil {
			return nil, err
		}
		v = &lisp.LVal{Type: lisp.LTaggedVal, Str: name, Cells: []*lisp.LVal{inner}}
	case tagArray:
		var err error
		if v, err = d.multiArray(depth); err != nil {
			return nil, err
		}
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	return v, nil
}

// multiArray reads [[dims...],[cells...]] after its '['; rank is not 1.
func (d *typedDecoder) multiArray(depth int) (*lisp.LVal, error) {
	if err := d.expect('['); err != nil {
		return nil, err
	}
	dims, err := d.elements(depth)
	if err != nil {
		return nil, err
	}
	if len(dims) == 1 {
		return nil, d.errorf("vector written as a tagged array")
	}
	total, zero := 1, false
	for _, n := range dims {
		if n.Type != lisp.LInt || n.Int < 0 {
			return nil, d.errorf("invalid array dimension")
		}
		switch {
		case n.Int == 0:
			zero = true
		case total > len(d.b)/n.Int:
			total = len(d.b) + 1 // more cells than the input can hold
		default:
			total *= n.Int
		}
	}
	if zero {
		total = 0
	}
	if err := d.expect(','); err != nil {
		return nil, err
	}
	if err := d.expect('['); err != nil {
		return nil, err
	}
	cells, err := d.elements(depth)
	if err != nil {
		return nil, err
	}
	if len(cells) != total {
		return nil, d.errorf("array contents do not match its dimensions")
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	return lisp.Array(lisp.QExpr(dims), cells), nil
}

// object reads a JSON object into a sorted map.
func (d *typedDecoder) object(depth int) (*lisp.LVal, error) {
	d.i++
	m := lisp.SortedMap()
	if d.peek() == '}' {
		d.i++
		return m, nil
	}
	var prev []byte
	size := 0
	for {
		if err := d.count(); err != nil {
			return nil, err
		}
		s, err := d.rawString()
		if err != nil {
			return nil, err
		}
		if prev != nil && compareJCS(prev, s) >= 0 {
			return nil, d.errorf("members out of order or duplicated")
		}
		// s aliases scratch the value below may reuse.
		prev = append(prev[:0], s...)
		k, err := d.key(prev)
		if err != nil {
			return nil, err
		}
		if err := d.expect(':'); err != nil {
			return nil, err
		}
		v, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		if r := m.MapSetLVal(k, v); r.Type == lisp.LError {
			return nil, d.errorf("%s", r.Str)
		}
		size++
		if m.Len() != size {
			// A string and a symbol of one spelling are one key.
			return nil, d.errorf("two members name one key")
		}
		switch d.peek() {
		case ',':
			d.i++
		case '}':
			d.i++
			return m, nil
		default:
			return nil, d.errorf("expected ',' or '}'")
		}
	}
}

// key interprets a member name.
func (d *typedDecoder) key(s []byte) (*lisp.LVal, error) {
	if len(s) == 0 {
		return lisp.String(""), nil
	}
	switch s[0] {
	case '^', '`':
		return nil, d.errorf("unescaped reserved key")
	case '~':
	default:
		return lisp.String(string(s)), nil
	}
	if len(s) < 2 {
		return nil, d.errorf("invalid tagged key")
	}
	body := s[2:]
	switch s[1] {
	case '~', '^', '`':
		return lisp.String(string(s[1:])), nil
	case ':':
		return lisp.Symbol(":" + string(body)), nil
	case '$':
		if err := checkSymbolName(body); err != nil {
			return nil, d.errorf("%v", err)
		}
		return lisp.Symbol(string(body)), nil
	case '?':
		switch string(body) {
		case "t":
			return lisp.Symbol(lisp.TrueSymbol), nil
		case "f":
			return lisp.Symbol(lisp.FalseSymbol), nil
		}
	case 'i':
		n, err := d.canonicalInt(body)
		if err != nil {
			return nil, err
		}
		return lisp.Int(n), nil
	}
	return nil, d.errorf("invalid tagged key")
}
