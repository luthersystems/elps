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
		return appendInt(b, v.i), nil
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
		// A zero dimension makes the array empty however large the others
		// are, so the product is checked for overflow only when none is
		// zero.  libjson's checkArrayShape has the same rule.  A product
		// that wrapped could equal the cell count, and libjson refuses the
		// bytes written for such an array.
		n, zero := 1, false
		for _, d := range v.dims {
			if d < 0 {
				return nil, errors.New("lval: an array dimension is negative")
			}
			zero = zero || d == 0
		}
		if zero {
			n = 0
		} else {
			for _, d := range v.dims {
				if n > math.MaxInt/d {
					return nil, errors.New("lval: an array's dimensions are too large")
				}
				n *= d
			}
		}
		if n != len(v.items) || len(v.dims) == 1 {
			return nil, errors.New("lval: an array's cells must match its dimensions, and a rank-1 array is a vector")
		}
		b = append(b, `["~#array",[[`...)
		for i, d := range v.dims {
			if i > 0 {
				b = append(b, ',')
			}
			// A dimension is an int value in typed JSON, so a large one
			// needs the "~n" form.
			b = appendInt(b, int64(d))
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

// appendInt writes i as typed JSON writes every int: a JSON number up to
// 2^53 in size, where a float64 reader stays exact, and a "~n" string past
// it.
func appendInt(b []byte, i int64) []byte {
	if i >= -maxExactInt && i <= maxExactInt {
		return strconv.AppendInt(b, i, 10)
	}
	b = append(b, `"~n`...)
	b = strconv.AppendInt(b, i, 10)
	return append(b, '"')
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
// libjson.LoadTyped accepts, and reads it as the same value. Like
// LoadTyped, it refuses a number, a string or a tag that is not the text
// Marshal writes for its value, such as a number with a leading zero, an
// escape Marshal does not write, or a "~n" int that fits a JSON number. It
// does not check the rest of the layout, such as whitespace, so it still
// accepts some inputs LoadTyped refuses.
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
	if s == "" || s[0] != '~' {
		return MakeString(s), nil
	}
	if len(s) < 2 {
		return Value{}, d.errorf("invalid tagged string")
	}
	rest := s[2:]
	switch s[1] {
	case '~':
		return MakeString(s[1:]), nil
	case '$':
		// As libjson's checkSymbolName: DumpTyped writes these names
		// another way, so a second spelling would not round trip.
		switch {
		case rest == "":
			return Value{}, d.errorf("empty symbol")
		case rest[0] == ':':
			return Value{}, d.errorf("keyword written as a symbol")
		case rest == "true" || rest == "false":
			return Value{}, d.errorf("boolean written as a symbol")
		}
		return MakeSymbol(rest), nil
	case ':':
		// Not MakeKeyword, which would drop a colon the name starts with.
		return Value{kind: Keyword, str: rest}, nil
	case 'b':
		b, err := base64.StdEncoding.DecodeString(rest)
		if err != nil {
			return Value{}, d.errorf("bad base64")
		}
		// As libjson: only the text Marshal writes, so the bytes in are
		// the bytes out (luthersystems/elps#983).
		if base64.StdEncoding.EncodeToString(b) != rest {
			return Value{}, d.errorf("non-canonical base64")
		}
		return MakeBytes(b), nil
	case 'n':
		n, form := parseInt(rest)
		if form == notInt {
			return Value{}, d.errorf("bad integer")
		}
		if form != canonicalInt {
			return Value{}, d.errorf("non-canonical int")
		}
		// An int up to 2^53 in size is a JSON number, never "~n".
		if n >= -maxExactInt && n <= maxExactInt {
			return Value{}, d.errorf("non-canonical int")
		}
		return MakeInt(n), nil
	case 'd':
		f, err := strconv.ParseFloat(rest, 64)
		if err != nil || math.IsInf(f, 0) || math.IsNaN(f) || math.Trunc(f) != f ||
			string(appendJSONFloat(nil, f)) != rest {
			return Value{}, d.errorf("non-canonical whole float")
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
	// As libjson: only the text Marshal writes for the value, so one value
	// has one spelling (luthersystems/elps#983).
	if !isFloat {
		n, form := parseInt(text)
		if form == notInt {
			return Value{}, d.errorf("bad number")
		}
		if form != canonicalInt || n < -maxExactInt || n > maxExactInt {
			return Value{}, d.errorf("non-canonical number")
		}
		return MakeInt(n), nil
	}
	f, err := strconv.ParseFloat(text, 64)
	if err != nil {
		return Value{}, d.errorf("bad number")
	}
	if math.IsInf(f, 0) {
		return Value{}, d.errorf("non-canonical number")
	}
	if math.Trunc(f) == f {
		return Value{}, d.errorf("whole float requires ~d")
	}
	if string(appendJSONFloat(nil, f)) != text {
		return Value{}, d.errorf("non-canonical number")
	}
	return MakeFloat(f), nil
}

// intText is what parseInt found in decimal text.
type intText int

const (
	// notInt: the text is not an int64.
	notInt intText = iota
	// nonCanonicalInt: an int64, but not strconv.FormatInt's text for it.
	nonCanonicalInt
	// canonicalInt: exactly strconv.FormatInt of the int: no sign but '-',
	// no leading zero, no "-0".
	canonicalInt
)

// parseInt parses decimal text as an int64.  Int values, "~n" values and
// "~i" keys all use it, so they keep one rule.
func parseInt(text string) (int64, intText) {
	// Fast path, one pass: up to 18 digits in canonical form cannot
	// overflow an int64, so they need neither strconv.ParseInt nor a
	// strconv.FormatInt round trip to compare (luthersystems/elps#987).
	if n, ok := parseShortCanonicalInt(text); ok {
		return n, canonicalInt
	}
	n, err := strconv.ParseInt(text, 10, 64)
	if err != nil {
		return 0, notInt
	}
	if strconv.FormatInt(n, 10) != text {
		return n, nonCanonicalInt
	}
	return n, canonicalInt
}

// parseShortCanonicalInt parses text if it is strconv.FormatInt's text
// for an int of at most 18 digits: an optional '-', then "0" or digits
// without a leading zero, and not "-0".  Otherwise it reports false and
// parseInt decides.
func parseShortCanonicalInt(text string) (int64, bool) {
	digits := text
	if digits != "" && digits[0] == '-' {
		digits = digits[1:]
	}
	if digits == "" || len(digits) > 18 || (digits[0] == '0' && len(text) > 1) {
		return 0, false
	}
	var n int64
	for i := 0; i < len(digits); i++ {
		c := digits[i]
		if c < '0' || c > '9' {
			return 0, false
		}
		n = n*10 + int64(c-'0')
	}
	if len(digits) < len(text) {
		n = -n
	}
	return n, true
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
	// The rule is libjson's: a zero dimension makes the array empty however
	// large the others are.  Without one, a product larger than the cell
	// count cannot match, which also keeps the product from overflowing.
	dims := make([]int, 0, len(dimv))
	cells, zero := int64(1), false
	for _, x := range dimv {
		n, ok := x.Int()
		if !ok || n < 0 || n > math.MaxInt {
			return Value{}, d.errorf("bad array dimension")
		}
		switch {
		case n == 0:
			zero = true
		case cells > int64(len(cellv))/n:
			cells = int64(len(cellv)) + 1 // more cells than the array has
		default:
			cells *= n
		}
		dims = append(dims, int(n))
	}
	if zero {
		cells = 0
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
			if sortedStringKeys(keys) {
				// Marshal writes a map's members in key order, so the
				// keys of a struct or a string-keyed map usually arrive
				// sorted and distinct: they are the map as it is, and
				// buildMap's sort and duplicate check would find nothing
				// to do (luthersystems/elps#987).  Clip the slices as
				// buildMap's are, so that Set on a copy of the map grows
				// a new array instead of writing into a shared one.
				return Value{kind: Map, keys: keys[:len(keys):len(keys)], items: vals[:len(vals):len(vals)]}, nil
			}
			return buildMap(keys, vals), nil
		}
		if !d.lit(",") {
			return Value{}, d.errorf("expected , or }")
		}
	}
}

// sortedStringKeys reports whether keys are all strings in strictly
// increasing order.  Then they are what buildMap would make of them: two
// distinct strings are two ELPS keys, and strings sort by keyText as they
// sort by their own text, since keyText only puts one more '~' before a
// string that starts with '~', and no other string starts with '~'.
func sortedStringKeys(keys []Value) bool {
	for i, k := range keys {
		if k.kind != String || (i > 0 && keys[i-1].str >= k.str) {
			return false
		}
	}
	return true
}

// mapKey reads a typed JSON member name.  It accepts the key tags that
// libjson's typedDecoder.key accepts and no others: a key must be a
// string, symbol, keyword, int or boolean, so a bytes or float tag here
// would build a map that cannot find or marshal its own key
// (luthersystems/elps#978).
func mapKey(s string, d *decoder) (Value, error) {
	if s == "" || s[0] != '~' {
		return MakeString(s), nil
	}
	if len(s) >= 2 {
		switch s[1] {
		case 'i':
			// Only canonical digits, as libjson: "~i007" and "~i+5"
			// would otherwise be a second spelling of one key.
			n, form := parseInt(s[2:])
			if form != canonicalInt {
				return Value{}, d.errorf("bad int key")
			}
			return MakeInt(n), nil
		case '?':
			switch s {
			case "~?t":
				return MakeBool(true), nil
			case "~?f":
				return MakeBool(false), nil
			}
		case '~', '$', ':':
			return tilde(s, d)
		}
	}
	return Value{}, d.errorf("invalid tagged key " + strconv.Quote(s))
}

// str reads a JSON string.
//
// As libjson, it accepts only the text Marshal writes for the string, so
// one string has one spelling, with no other escape and no raw control or
// HTML character (luthersystems/elps#983).  It checks that in the one pass
// that reads the string, instead of writing the string again and
// comparing: a WebAssembly guest pays for every instruction, and the
// second pass made each typed call cost about 13% more
// (luthersystems/elps#987).  A string without an escape, as most are, is
// one copy of the input.
func (d *decoder) str() (string, error) {
	d.i++ // "
	// out holds the decoded text before seg once a string has an escape;
	// until then the text is d.b[start:d.i].
	start, seg := d.i, d.i
	var out []byte
	badUTF8, nonCanonical := false, false
	for {
		if d.i >= len(d.b) {
			return "", d.errorf("unterminated string")
		}
		c := d.b[d.i]
		switch {
		case c == '"':
			text := d.b[seg:d.i]
			d.i++
			// The UTF-8 error first, as when the check wrote the string
			// again: that write failed before any comparison.
			if badUTF8 {
				return "", d.errorf("string is not valid UTF-8")
			}
			if nonCanonical {
				return "", d.errorf("non-canonical string")
			}
			if out == nil {
				return string(d.b[start : d.i-1]), nil
			}
			return string(append(out, text...)), nil
		case c == '\\':
			if d.i+1 >= len(d.b) {
				return "", d.errorf("bad escape")
			}
			out = append(out, d.b[seg:d.i]...)
			e := d.b[d.i+1]
			d.i += 2
			switch e {
			case '"', '\\':
				out = append(out, e)
			case '/':
				// Marshal writes '/' as it is.
				nonCanonical = true
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
				hex := d.i
				r, ok := d.hex4()
				if !ok {
					return "", d.errorf("bad \\u escape")
				}
				if utf16.IsSurrogate(r) {
					// Marshal writes a rune past the BMP as UTF-8, so a
					// surrogate escape is never its text.
					nonCanonical = true
					if !d.lit(`\u`) {
						return "", d.errorf("lone surrogate")
					}
					r2, ok := d.hex4()
					if !ok {
						return "", d.errorf("bad \\u escape")
					}
					r = utf16.DecodeRune(r, r2)
				} else if !isCanonicalUEscape(r, d.b[hex:hex+4]) {
					nonCanonical = true
				}
				out = utf8.AppendRune(out, r)
			default:
				return "", d.errorf("bad escape")
			}
			seg = d.i
		case c < 0x20 || c == '<' || c == '>' || c == '&':
			// Marshal escapes these.
			nonCanonical = true
			d.i++
		case c < utf8.RuneSelf:
			d.i++
		default:
			r, size := utf8.DecodeRune(d.b[d.i:])
			switch {
			case r == utf8.RuneError && size == 1:
				badUTF8 = true
			case r == 0x2028 || r == 0x2029:
				// Marshal escapes these.
				nonCanonical = true
			}
			d.i += size
		}
	}
}

// isCanonicalUEscape reports whether hex, the four digits of a \u escape
// of r (not a surrogate), is the escape appendString writes for r.
func isCanonicalUEscape(r rune, hex []byte) bool {
	switch {
	case r == '\b', r == '\f', r == '\n', r == '\r', r == '\t':
		// appendString writes the short escape.
		return false
	case r < 0x20, r == '<', r == '>', r == '&', r == 0x2028, r == 0x2029:
	default:
		// appendString writes the rune as it is.
		return false
	}
	const digits = "0123456789abcdef"
	return hex[0] == digits[r>>12&0xF] && hex[1] == digits[r>>8&0xF] &&
		hex[2] == digits[r>>4&0xF] && hex[3] == digits[r&0xF]
}

func (d *decoder) hex4() (rune, bool) {
	if d.i+4 > len(d.b) {
		return 0, false
	}
	var r rune
	for _, c := range d.b[d.i : d.i+4] {
		switch {
		case c >= '0' && c <= '9':
			r = r<<4 | rune(c-'0')
		case c >= 'a' && c <= 'f':
			r = r<<4 | rune(c-'a'+10)
		case c >= 'A' && c <= 'F':
			r = r<<4 | rune(c-'A'+10)
		default:
			return 0, false
		}
	}
	d.i += 4
	return r, true
}
