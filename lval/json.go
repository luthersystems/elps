// Copyright © 2026 The ELPS authors

package lval

import (
	"encoding/base64"
	"errors"
	"math"
	"strconv"
	"unicode/utf16"
	"unicode/utf8"
)

// maxExactInt is the largest int typed JSON writes as a JSON number.
const maxExactInt = 1 << 53

// maxDepth bounds nesting on both sides of the codec.
const maxDepth = 1024

// Marshal writes v as elps typed JSON: the bytes libjson.DumpTyped writes
// for the same value, map members in typed JSON's order.
func Marshal(v Value) ([]byte, error) {
	return appendValue(nil, v, 0)
}

func appendValue(b []byte, v Value, depth int) ([]byte, error) {
	// As libjson counts it: a container nested maxDepth deep is too deep.
	switch v.kind {
	case List, Vector, Map, Tagged, Array:
		if depth >= maxDepth && (v.kind != List || len(v.items) > 0) {
			return nil, errors.New("lval: value nests too deeply")
		}
	default:
	}
	switch v.kind {
	case Nil:
		return append(b, "null"...), nil
	case Int:
		if v.i >= -maxExactInt && v.i <= maxExactInt {
			return strconv.AppendInt(b, v.i, 10), nil
		}
		b = append(b, `"~n`...)
		b = strconv.AppendInt(b, v.i, 10)
		return append(b, '"'), nil
	case Float:
		return appendFloat(b, v.f), nil
	case String:
		if len(v.str) > 0 && v.str[0] == '~' {
			return appendString(b, "~"+v.str)
		}
		return appendString(b, v.str)
	case Symbol:
		if v.str == "" {
			return nil, errors.New("lval: a symbol needs a name")
		}
		return appendString(b, "~$"+v.str)
	case Keyword:
		return appendString(b, "~:"+v.str)
	case Bool:
		if v.i == 1 {
			return append(b, "true"...), nil
		}
		return append(b, "false"...), nil
	case Bytes:
		b = append(b, `"~b`...)
		b = base64.StdEncoding.AppendEncode(b, v.bytes)
		return append(b, '"'), nil
	case List:
		if len(v.items) == 0 {
			return append(b, "null"...), nil
		}
		b = append(b, `["~#list",`...)
		var err error
		b, err = appendItems(b, v.items, depth)
		if err != nil {
			return nil, err
		}
		return append(b, ']'), nil
	case Vector:
		return appendItems(b, v.items, depth)
	case Map:
		b = append(b, '{')
		for i, k := range v.keys {
			if i > 0 {
				b = append(b, ',')
			}
			kt, ok := keyText(k)
			if !ok {
				return nil, errors.New("lval: a map key must be a string, symbol, keyword, int or boolean")
			}
			if k.kind == Symbol && k.str == "" {
				return nil, errors.New("lval: a symbol needs a name")
			}
			var err error
			if b, err = appendString(b, kt); err != nil {
				return nil, err
			}
			b = append(b, ':')
			if b, err = appendValue(b, v.items[i], depth+1); err != nil {
				return nil, err
			}
		}
		return append(b, '}'), nil
	case Tagged:
		if len(v.items) != 1 || v.str == "" {
			return nil, errors.New("lval: a tagged value needs a type name and data")
		}
		return appendTagged(b, v.str, v.items[0], depth)
	case Array:
		n := 1
		for _, d := range v.dims {
			if d < 0 {
				return nil, errors.New("lval: an array dimension is negative")
			}
			n *= d
		}
		if n != len(v.items) || len(v.dims) == 1 {
			return nil, errors.New("lval: an array's cells must match its dimensions, and a rank-1 array is a vector")
		}
		b = append(b, `["~#array",[[`...)
		for i, d := range v.dims {
			if i > 0 {
				b = append(b, ',')
			}
			b = strconv.AppendInt(b, int64(d), 10)
		}
		b = append(b, "],"...)
		var err error
		b, err = appendItems(b, v.items, depth)
		if err != nil {
			return nil, err
		}
		return append(b, "]]"...), nil
	}
	return nil, errors.New("lval: unknown kind " + v.kind.String())
}

func appendTagged(b []byte, typ string, data Value, depth int) ([]byte, error) {
	b = append(b, `["~#tagged",[`...)
	b, err := appendString(b, typ)
	if err != nil {
		return nil, err
	}
	b = append(b, ',')
	if b, err = appendValue(b, data, depth+1); err != nil {
		return nil, err
	}
	return append(b, "]]"...), nil
}

func appendItems(b []byte, items []Value, depth int) ([]byte, error) {
	b = append(b, '[')
	for i, it := range items {
		if i > 0 {
			b = append(b, ',')
		}
		var err error
		if b, err = appendValue(b, it, depth+1); err != nil {
			return nil, err
		}
	}
	return append(b, ']'), nil
}

// appendFloat writes f as typed JSON does: a whole float as "~d" and its
// shortest text, any other finite float as a JSON number.
func appendFloat(b []byte, f float64) []byte {
	switch {
	case math.IsNaN(f):
		return append(b, `"~zNaN"`...)
	case math.IsInf(f, 1):
		return append(b, `"~zINF"`...)
	case math.IsInf(f, -1):
		return append(b, `"~z-INF"`...)
	case f == math.Trunc(f):
		b = append(b, `"~d`...)
		b = appendJSONFloat(b, f)
		return append(b, '"')
	}
	return appendJSONFloat(b, f)
}

// appendJSONFloat is encoding/json's float text (ES6 number formatting).
func appendJSONFloat(b []byte, f float64) []byte {
	if f == 0 {
		if math.Signbit(f) {
			return append(b, "-0"...)
		}
		return append(b, '0')
	}
	abs := math.Abs(f)
	fmtc := byte('f')
	if abs < 1e-6 || abs >= 1e21 {
		fmtc = 'e'
	}
	b = strconv.AppendFloat(b, f, fmtc, -1, 64)
	if fmtc == 'e' {
		// clean up e-09 to e-9
		n := len(b)
		if n >= 4 && b[n-4] == 'e' && b[n-3] == '-' && b[n-2] == '0' {
			b[n-2] = b[n-1]
			b = b[:n-1]
		}
	}
	return b
}

// appendString writes s as a JSON string with the plain encoder's escapes.
func appendString(b []byte, s string) ([]byte, error) {
	if !utf8.ValidString(s) {
		return nil, errors.New("lval: string is not valid UTF-8")
	}
	const hex = "0123456789abcdef"
	b = append(b, '"')
	for _, r := range s {
		switch {
		case r == '"':
			b = append(b, `\"`...)
		case r == '\\':
			b = append(b, `\\`...)
		case r == '\n':
			b = append(b, `\n`...)
		case r == '\r':
			b = append(b, `\r`...)
		case r == '\t':
			b = append(b, `\t`...)
		case r == '\b':
			b = append(b, `\b`...)
		case r == '\f':
			b = append(b, `\f`...)
		case r < 0x20, r == '<', r == '>', r == '&', r == 0x2028, r == 0x2029:
			b = append(b, `\u`...)
			b = append(b, hex[r>>12&0xF], hex[r>>8&0xF], hex[r>>4&0xF], hex[r&0xF])
		default:
			b = utf8.AppendRune(b, r)
		}
	}
	return append(b, '"'), nil
}

// Unmarshal reads typed JSON into a Value. It accepts every input
// libjson.LoadTyped accepts, and reads it as the same value. It does not
// check that the input is canonical, so it also accepts some inputs
// LoadTyped refuses, such as a number with a leading zero.
func Unmarshal(data []byte) (Value, error) {
	d := decoder{b: data}
	d.ws()
	v, err := d.value(0)
	if err != nil {
		return Value{}, err
	}
	d.ws()
	if d.i != len(d.b) {
		return Value{}, d.errorf("trailing bytes")
	}
	return v, nil
}

type decoder struct {
	b []byte
	i int
}

func (d *decoder) errorf(msg string) error {
	return errors.New("lval: typed JSON at byte " + strconv.Itoa(d.i) + ": " + msg)
}

func (d *decoder) ws() {
	for d.i < len(d.b) {
		switch d.b[d.i] {
		case ' ', '\t', '\n', '\r':
			d.i++
		default:
			return
		}
	}
}

func (d *decoder) lit(s string) bool {
	if len(d.b)-d.i >= len(s) && string(d.b[d.i:d.i+len(s)]) == s {
		d.i += len(s)
		return true
	}
	return false
}

func (d *decoder) value(depth int) (Value, error) {
	if d.i >= len(d.b) {
		return Value{}, d.errorf("unexpected end")
	}
	switch c := d.b[d.i]; {
	case c == 'n' && d.lit("null"):
		return Value{}, nil
	case c == 't' && d.lit("true"):
		return MakeBool(true), nil
	case c == 'f' && d.lit("false"):
		return MakeBool(false), nil
	case c == '"':
		s, err := d.str()
		if err != nil {
			return Value{}, err
		}
		return tilde(s, d)
	case (c == '[' || c == '{') && depth >= maxDepth:
		// As libjson counts it: a container nested maxDepth deep.
		return Value{}, d.errorf("nesting too deep")
	case c == '[':
		return d.array(depth)
	case c == '{':
		return d.object(depth)
	case c == '-' || (c >= '0' && c <= '9'):
		return d.number()
	}
	return Value{}, d.errorf("unexpected character")
}

// tilde reads a string value's Transit prefix.
func tilde(s string, d *decoder) (Value, error) {
	if len(s) < 2 || s[0] != '~' {
		return MakeString(s), nil
	}
	rest := s[2:]
	switch s[1] {
	case '~':
		return MakeString(s[1:]), nil
	case '$':
		return MakeSymbol(rest), nil
	case ':':
		// Not MakeKeyword, which would drop a colon the name starts with.
		return Value{kind: Keyword, str: rest}, nil
	case 'b':
		b, err := base64.StdEncoding.DecodeString(rest)
		if err != nil {
			return Value{}, d.errorf("bad base64")
		}
		return MakeBytes(b), nil
	case 'n':
		n, err := strconv.ParseInt(rest, 10, 64)
		if err != nil {
			return Value{}, d.errorf("bad integer")
		}
		return MakeInt(n), nil
	case 'd':
		f, err := strconv.ParseFloat(rest, 64)
		if err != nil {
			return Value{}, d.errorf("bad float")
		}
		return MakeFloat(f), nil
	case 'z':
		switch rest {
		case "NaN":
			return MakeFloat(math.NaN()), nil
		case "INF":
			return MakeFloat(math.Inf(1)), nil
		case "-INF":
			return MakeFloat(math.Inf(-1)), nil
		}
	}
	return Value{}, d.errorf("unknown tag in " + strconv.Quote(s))
}

func (d *decoder) number() (Value, error) {
	start := d.i
	isFloat := false
	for d.i < len(d.b) {
		c := d.b[d.i]
		if c == '.' || c == 'e' || c == 'E' {
			isFloat = true
		} else if (c < '0' || c > '9') && c != '-' && c != '+' {
			break
		}
		d.i++
	}
	text := string(d.b[start:d.i])
	if !isFloat {
		n, err := strconv.ParseInt(text, 10, 64)
		if err != nil {
			return Value{}, d.errorf("bad number")
		}
		return MakeInt(n), nil
	}
	f, err := strconv.ParseFloat(text, 64)
	if err != nil {
		return Value{}, d.errorf("bad number")
	}
	return MakeFloat(f), nil
}

func (d *decoder) elems(depth int) ([]Value, error) {
	d.i++ // [
	items := []Value{}
	d.ws()
	if d.lit("]") {
		return items, nil
	}
	for {
		d.ws()
		v, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		items = append(items, v)
		d.ws()
		if d.lit("]") {
			return items, nil
		}
		if !d.lit(",") {
			return nil, d.errorf("expected , or ]")
		}
	}
}

func (d *decoder) array(depth int) (Value, error) {
	save := d.i
	d.i++
	d.ws()
	if !d.lit(`"~#`) {
		d.i = save
		items, err := d.elems(depth)
		if err != nil {
			return Value{}, err
		}
		return MakeVector(items...), nil
	}
	d.i -= 3
	tag, err := d.str()
	if err != nil {
		return Value{}, err
	}
	d.ws()
	if !d.lit(",") {
		return Value{}, d.errorf("expected , after a tag")
	}
	d.ws()
	if d.i >= len(d.b) || d.b[d.i] != '[' {
		return Value{}, d.errorf("expected [ after a tag")
	}
	switch tag {
	case "~#tagged":
		return d.tagged(depth)
	case "~#array":
		return d.array2(depth)
	default:
	}
	body, err := d.elems(depth)
	if err != nil {
		return Value{}, err
	}
	d.ws()
	if !d.lit("]") {
		return Value{}, d.errorf("expected ] after a tagged value")
	}
	switch tag {
	case "~#list":
		return MakeList(body...), nil
	default:
	}
	return Value{}, d.errorf("unknown tag " + strconv.Quote(tag))
}

// array2 reads the body of an array of rank other than 1,
// [[dims...],[cells...]], and the ] that closes the value. As in libjson,
// the dims and cells are not levels of nesting of their own.
func (d *decoder) array2(depth int) (Value, error) {
	d.i++ // [
	var parts [2][]Value
	for i := range parts {
		d.ws()
		if i > 0 && !d.lit(",") {
			return Value{}, d.errorf("malformed array")
		}
		d.ws()
		if d.i >= len(d.b) || d.b[d.i] != '[' {
			return Value{}, d.errorf("malformed array")
		}
		var err error
		if parts[i], err = d.elems(depth); err != nil {
			return Value{}, err
		}
	}
	for range 2 {
		d.ws()
		if !d.lit("]") {
			return Value{}, d.errorf("malformed array")
		}
	}
	dimv, cellv := parts[0], parts[1]
	if len(dimv) == 1 {
		return Value{}, d.errorf("a vector written as an array")
	}
	dims := make([]int, 0, len(dimv))
	cells := int64(1)
	for _, x := range dimv {
		n, ok := x.Int()
		if !ok || n < 0 {
			return Value{}, d.errorf("bad array dimension")
		}
		if n > 0 && cells > int64(len(cellv))/n+1 {
			return Value{}, d.errorf("array cells do not match its dimensions")
		}
		cells *= n
		dims = append(dims, int(n))
	}
	if cells != int64(len(cellv)) {
		return Value{}, d.errorf("array cells do not match its dimensions")
	}
	return MakeArray(dims, cellv...), nil
}

// tagged reads the body of a tagged value, [type, data], and the ] that
// closes the value. The type is a plain string: its text is the type name,
// with no Transit prefix read.
func (d *decoder) tagged(depth int) (Value, error) {
	d.i++ // [
	d.ws()
	if d.i >= len(d.b) || d.b[d.i] != '"' {
		return Value{}, d.errorf("expected a tagged value's type")
	}
	typ, err := d.str()
	if err != nil {
		return Value{}, err
	}
	if typ == "" {
		return Value{}, d.errorf("tagged value with an empty type")
	}
	d.ws()
	if !d.lit(",") {
		return Value{}, d.errorf("expected , after a tagged value's type")
	}
	d.ws()
	data, err := d.value(depth + 1)
	if err != nil {
		return Value{}, err
	}
	for _, c := range []string{"]", "]"} {
		d.ws()
		if !d.lit(c) {
			return Value{}, d.errorf("expected ] after a tagged value")
		}
	}
	return MakeTagged(typ, data), nil
}

func (d *decoder) object(depth int) (Value, error) {
	d.i++ // {
	var keys, vals []Value
	d.ws()
	if d.lit("}") {
		return Value{kind: Map}, nil
	}
	for {
		d.ws()
		if d.i >= len(d.b) || d.b[d.i] != '"' {
			return Value{}, d.errorf("expected a member name")
		}
		ks, err := d.str()
		if err != nil {
			return Value{}, err
		}
		key, err := mapKey(ks, d)
		if err != nil {
			return Value{}, err
		}
		d.ws()
		if !d.lit(":") {
			return Value{}, d.errorf("expected :")
		}
		d.ws()
		v, err := d.value(depth + 1)
		if err != nil {
			return Value{}, err
		}
		keys = append(keys, key)
		vals = append(vals, v)
		d.ws()
		if d.lit("}") {
			return buildMap(keys, vals), nil
		}
		if !d.lit(",") {
			return Value{}, d.errorf("expected , or }")
		}
	}
}

func mapKey(s string, d *decoder) (Value, error) {
	if len(s) < 2 || s[0] != '~' {
		return MakeString(s), nil
	}
	switch s[1] {
	case 'i':
		n, err := strconv.ParseInt(s[2:], 10, 64)
		if err != nil {
			return Value{}, d.errorf("bad int key")
		}
		return MakeInt(n), nil
	case '?':
		return MakeBool(s == "~?t"), nil
	}
	return tilde(s, d)
}

// str reads a JSON string.
func (d *decoder) str() (string, error) {
	d.i++ // "
	var out []byte
	for {
		if d.i >= len(d.b) {
			return "", d.errorf("unterminated string")
		}
		c := d.b[d.i]
		switch c {
		case '"':
			d.i++
			return string(out), nil
		case '\\':
			if d.i+1 >= len(d.b) {
				return "", d.errorf("bad escape")
			}
			e := d.b[d.i+1]
			d.i += 2
			switch e {
			case '"', '\\', '/':
				out = append(out, e)
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
				r, ok := d.hex4()
				if !ok {
					return "", d.errorf("bad \\u escape")
				}
				if utf16.IsSurrogate(r) {
					if !d.lit(`\u`) {
						return "", d.errorf("lone surrogate")
					}
					r2, ok := d.hex4()
					if !ok {
						return "", d.errorf("bad \\u escape")
					}
					r = utf16.DecodeRune(r, r2)
				}
				out = utf8.AppendRune(out, r)
			default:
				return "", d.errorf("bad escape")
			}
		default:
			out = append(out, c)
			d.i++
		}
	}
}

func (d *decoder) hex4() (rune, bool) {
	if d.i+4 > len(d.b) {
		return 0, false
	}
	n, err := strconv.ParseUint(string(d.b[d.i:d.i+4]), 16, 32)
	if err != nil {
		return 0, false
	}
	d.i += 4
	return rune(n), true //nolint:gosec // ParseUint with bitSize 32 of 4 hex digits is at most 0xFFFF
}
