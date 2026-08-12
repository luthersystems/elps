package libjson

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"math"
	"strconv"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

func init() {
	encoderFuncs[lisp.LBytes] = (*encoder).encodeLBytes
	encoderFuncs[lisp.LSymbol] = (*encoder).encodeLSymbol
	encoderFuncs[lisp.LString] = (*encoder).encodeLString
	encoderFuncs[lisp.LInt] = (*encoder).encodeLInt
	encoderFuncs[lisp.LFloat] = (*encoder).encodeLFloat
	encoderFuncs[lisp.LNative] = (*encoder).encodeLNative
	encoderFuncs[lisp.LQuote] = (*encoder).encodeLQuote
	encoderFuncs[lisp.LSExpr] = (*encoder).encodeLSExpr
	encoderFuncs[lisp.LArray] = (*encoder).encodeArray
	encoderFuncs[lisp.LSortMap] = (*encoder).encodeSortMap
	encoderFuncs[lisp.LTaggedVal] = (*encoder).encodeTaggedVal
}

var encoderFuncs [lisp.LTypeMax]func(enc *encoder, v *lisp.LVal) error

type encodeInvalidNumberError float64

func (e encodeInvalidNumberError) Error() string {
	return fmt.Sprintf("unable to encode number %g", float64(e))
}

type encoder struct {
	// path holds the values between the root of the encoding and the value
	// being encoded now.  It stays nil until the encoder nests deeper than
	// encodeGuardDepth, so an ordinary document allocates nothing for it.
	//
	// It leads the struct for the fieldalignment gate: its pointer sits next
	// to buf's, which keeps the run of pointer bytes the GC must scan short.
	path map[*lisp.LVal]struct{}

	buf bytes.Buffer

	scratch    [64]byte
	depth      int
	stringNums bool
}

// encodeGuardDepth is the nesting depth past which the encoder stops assuming
// the value it is serializing is a tree and starts tracking the path it is on.
//
// assoc! and append! mutate a container in place, so a program can store a
// container inside itself, and an unguarded encoder walks such a value until
// the goroutine stack overflows and the Go runtime kills the process -- which
// recover() cannot catch, so it is not something the evaluator can turn into a
// condition.  See lisp/cycle.go and issue #390.
const encodeGuardDepth = 64

// errCyclicValue reports a value that contains itself.  JSON has no
// representation for one, so the encoder refuses rather than emitting a
// truncated document: the builtins turn this into an ordinary elps error that
// handler-bind can catch.
var errCyclicValue = errors.New("cannot serialize a value that contains itself")

// enter descends into v, and reports an error if v is already on the path
// being encoded.  A caller that gets nil back must pair it with leave(v).
//
// The path set is scoped to the current path rather than the whole document
// so that a value which merely shares a substructure with itself -- a DAG,
// (list x x) -- still serializes as the two copies it has always been.
func (enc *encoder) enter(v *lisp.LVal) error {
	enc.depth++
	if enc.depth < encodeGuardDepth {
		return nil
	}
	if enc.path == nil {
		enc.path = make(map[*lisp.LVal]struct{}, encodeGuardDepth)
	} else if _, ok := enc.path[v]; ok {
		return errCyclicValue
	}
	enc.path[v] = struct{}{}
	return nil
}

// leave ascends out of v.
func (enc *encoder) leave(v *lisp.LVal) {
	enc.depth--
	if enc.path != nil {
		delete(enc.path, v)
	}
}

func newEncoder(stringNums bool) *encoder {
	return &encoder{stringNums: stringNums}
}

func (enc *encoder) bytes() []byte {
	return enc.buf.Bytes()
}

func (enc *encoder) encode(v *lisp.LVal) error {
	if v.IsNil() {
		enc.buf.WriteString("null")
		return nil
	}
	if fn := encoderFuncs[v.Type]; fn != nil {
		if err := enc.enter(v); err != nil {
			return err
		}
		defer enc.leave(v)
		return fn(enc, v)
	}
	return fmt.Errorf("invalid type encountered: %v", lisp.GetType(v))
}

func (enc *encoder) encodeLQuote(v *lisp.LVal) error {
	return enc.encode(v.Cells[0])
}

func (enc *encoder) encodeArray(v *lisp.LVal) (err error) {
	switch v.Cells[0].Len() {
	case 0:
		return enc.encode(v.Cells[1].Cells[0])
	case 1:
		return enc.encodeSExpr(v.Cells[1].Cells)
	default:
		return fmt.Errorf("cannot serialize array with dimensions: %v", v.Cells[0])
	}
}

func (enc *encoder) encodeSortMap(v *lisp.LVal) (err error) {
	// TODO:  Cache map entries slices to help with "widely nested" objects
	enc.buf.WriteByte('{')
	ents := v.MapEntries()
	for i := range ents.Cells {
		if i > 0 {
			enc.buf.WriteByte(',')
		}
		err = enc.encodeMapKey(ents.Cells[i].Cells[0])
		if err != nil {
			return err
		}
		enc.buf.WriteByte(':')
		err = enc.encode(ents.Cells[i].Cells[1])
		if err != nil {
			return err
		}
	}
	enc.buf.WriteByte('}')
	return nil
}

func (enc *encoder) encodeMapKey(v *lisp.LVal) error {
	if v.Type != lisp.LString && v.Type != lisp.LSymbol {
		return invalidKeyTypeError(v.Type)
	}
	return enc.encodeString(v.Str)
}

type invalidKeyTypeError lisp.LType

func (e invalidKeyTypeError) Error() string {
	return fmt.Sprintf("invalid map key type: %v", lisp.LType(e))
}

func (enc *encoder) encodeTaggedVal(v *lisp.LVal) error {
	// Eventually there may be a way for lisp objects to implement custom
	// serialization but for now tagged values just have the user-data
	// serialized directly.
	return enc.encode(v.Cells[0])
}

func (enc *encoder) encodeLSExpr(v *lisp.LVal) error {
	return enc.encodeSExpr(v.Cells)
}

func (enc *encoder) encodeSExpr(cells []*lisp.LVal) (err error) {
	enc.buf.WriteByte('[')
	for i, v := range cells {
		if i > 0 {
			enc.buf.WriteByte(',')
		}
		err = enc.encode(v)
		if err != nil {
			return err
		}
	}
	enc.buf.WriteByte(']')
	return nil
}

func (enc *encoder) encodeLNative(v *lisp.LVal) error {
	return enc.encodeNative(v.Native)
}

func (enc *encoder) encodeNative(v interface{}) error {
	b, err := json.Marshal(v)
	enc.buf.Write(b)
	return err
}

func (enc *encoder) encodeLInt(v *lisp.LVal) error {
	return enc.encodeInt(v.Int)
}

func (enc *encoder) encodeInt(x int) (err error) {
	b := strconv.AppendInt(enc.scratch[:0], int64(x), 10)
	if enc.stringNums {
		enc.buf.WriteByte('"')
		enc.buf.Write(b)
		enc.buf.WriteByte('"')
	} else {
		enc.buf.Write(b)
	}
	return err
}

func (enc *encoder) encodeLFloat(v *lisp.LVal) error {
	return enc.encodeFloat(v.Float)
}

func (enc *encoder) encodeFloat(x float64) error {
	if math.IsInf(x, 0) || math.IsNaN(x) {
		return encodeInvalidNumberError(x)
	}
	b := enc.scratchFloat(x)
	if enc.stringNums {
		enc.buf.WriteByte('"')
		enc.buf.Write(b)
		enc.buf.WriteByte('"')
	} else {
		enc.buf.Write(b)
	}
	return nil
}

// scratchFloat encodes x to enc.scratch and returns a slice of that array.
//
// NOTE:  scratchFloat is adapted from floatEncoder.encode in encoding/json,
// simplified to only work with native float64 values.
// https://cs.opensource.google/go/go/+/refs/tags/go1.16.4:src/encoding/json/encode.go;l=575
func (enc *encoder) scratchFloat(x float64) []byte {
	// Convert as if by ES6 number to string conversion.
	// This matches most other JSON generators.
	// See golang.org/issue/6384 and golang.org/issue/14135.
	// Like fmt %g, but the exponent cutoffs are different
	// and exponents themselves are not padded to two digits.
	b := enc.scratch[:0]
	abs := math.Abs(x)
	fmt := byte('f')
	// NOTE:   Because ELPS only natively supports float64 values the exponent
	// check is simpler than in encoding/json
	if abs != 0 && (abs < 1e-6 || abs >= 1e21) {
		fmt = 'e'
	}
	b = strconv.AppendFloat(b, x, fmt, -1, 64)
	if fmt == 'e' {
		// clean up e-09 to e-9
		n := len(b)
		if n >= 4 && b[n-4] == 'e' && b[n-3] == '-' && b[n-2] == '0' {
			b[n-2] = b[n-1]
			b = b[:n-1]
		}
	}
	return b
}

func (enc *encoder) encodeLBytes(v *lisp.LVal) (err error) {
	return enc.encodeBytes(v.Bytes())
}

var enc64 = base64.StdEncoding

func (enc *encoder) encodeBytes(b []byte) (err error) {
	if b == nil {
		// This is needed for backwards compatability with v1.13.0 which would
		// use encoding/json to marshal all []byte values.
		enc.buf.WriteString("null")
		return nil
	}
	n := enc64.EncodedLen(len(b))
	enc.buf.WriteByte('"')
	if n < len(enc.scratch) {
		dst := enc.scratch[:n]
		enc64.Encode(dst, b)
		enc.buf.Write(dst)
	} else if n < 1024 {
		// 1024 is the size of the internal buffer used by base64.NewEncoder so
		// we allocate just that buffer size and avoid the extra overhead.
		dst := make([]byte, n)
		enc64.Encode(dst, b)
		enc.buf.Write(dst)
	} else {
		w := base64.NewEncoder(enc64, &enc.buf)
		for len(b) > 0 {
			// This clobbers the variable we were using for encoded-len to save
			// stack space but we don't need that anymore.
			n, _ = w.Write(b)
			b = b[n:]
		}
		_ = w.Close()
	}
	enc.buf.WriteByte('"')
	return nil
}

func (enc *encoder) encodeLSymbol(v *lisp.LVal) (err error) {
	if v.Str == lisp.TrueSymbol || v.Str == lisp.FalseSymbol {
		enc.buf.WriteString(v.Str)
		return nil
	}
	return enc.encodeString(v.Str)
}

func (enc *encoder) encodeLString(v *lisp.LVal) error {
	return enc.encodeString(v.Str)
}

// NOTE:  encodeString adapted from the json package.
// https://cs.opensource.google/go/go/+/refs/tags/go1.22.1:src/encoding/json/encode.go;l=956
func (enc *encoder) encodeString(s string) error {
	const hex = "0123456789abcdef"
	enc.buf.WriteByte('"')
	start := 0
	for i := 0; i < len(s); {
		if b := s[i]; b < utf8.RuneSelf {
			if htmlSafeSet[b] {
				i++
				continue
			}
			if start < i {
				enc.buf.WriteString(s[start:i])
			}
			enc.buf.WriteByte('\\')
			switch b {
			case '\\', '"':
				enc.buf.WriteByte(b)
			case '\b':
				enc.buf.WriteByte('b')
			case '\f':
				enc.buf.WriteByte('f')
			case '\n':
				enc.buf.WriteByte('n')
			case '\r':
				enc.buf.WriteByte('r')
			case '\t':
				enc.buf.WriteByte('t')
			default:
				// This encodes bytes < 0x20 except for \t, \n and \r.
				// If escapeHTML is set, it also escapes <, >, and &
				// because they can lead to security holes when
				// user-controlled strings are rendered into JSON
				// and served to some browsers.
				enc.buf.WriteString(`u00`)
				enc.buf.WriteByte(hex[b>>4])
				enc.buf.WriteByte(hex[b&0xF])
			}
			i++
			start = i
			continue
		}
		c, size := utf8.DecodeRuneInString(s[i:])
		if c == utf8.RuneError && size == 1 {
			if start < i {
				enc.buf.WriteString(s[start:i])
			}
			enc.buf.WriteString(`\ufffd`)
			i += size
			start = i
			continue
		}
		// U+2028 is LINE SEPARATOR.
		// U+2029 is PARAGRAPH SEPARATOR.
		// They are both technically valid characters in JSON strings,
		// but don't work in JSONP, which has to be evaluated as JavaScript,
		// and can lead to security holes there. It is valid JSON to
		// escape them, so we do so unconditionally.
		// See http://timelessrepo.com/json-isnt-a-javascript-subset for discussion.
		if c == '\u2028' || c == '\u2029' {
			if start < i {
				enc.buf.WriteString(s[start:i])
			}
			enc.buf.WriteString(`\u202`)
			enc.buf.WriteByte(hex[c&0xF])
			i += size
			start = i
			continue
		}
		i += size
	}
	if start < len(s) {
		enc.buf.WriteString(s[start:])
	}
	enc.buf.WriteByte('"')
	return nil
}

// NOTE:  htmlSafeSet is from the json package
// htmlSafeSet holds the value true if the ASCII character with the given
// array position can be safely represented inside a JSON string, embedded
// inside of HTML <script> tags, without any additional escaping.
//
// All values are true except for the ASCII control characters (0-31), the
// double quote ("), the backslash character ("\"), HTML opening and closing
// tags ("<" and ">"), and the ampersand ("&").
var htmlSafeSet = [utf8.RuneSelf]bool{
	' ':      true,
	'!':      true,
	'"':      false,
	'#':      true,
	'$':      true,
	'%':      true,
	'&':      false,
	'\'':     true,
	'(':      true,
	')':      true,
	'*':      true,
	'+':      true,
	',':      true,
	'-':      true,
	'.':      true,
	'/':      true,
	'0':      true,
	'1':      true,
	'2':      true,
	'3':      true,
	'4':      true,
	'5':      true,
	'6':      true,
	'7':      true,
	'8':      true,
	'9':      true,
	':':      true,
	';':      true,
	'<':      false,
	'=':      true,
	'>':      false,
	'?':      true,
	'@':      true,
	'A':      true,
	'B':      true,
	'C':      true,
	'D':      true,
	'E':      true,
	'F':      true,
	'G':      true,
	'H':      true,
	'I':      true,
	'J':      true,
	'K':      true,
	'L':      true,
	'M':      true,
	'N':      true,
	'O':      true,
	'P':      true,
	'Q':      true,
	'R':      true,
	'S':      true,
	'T':      true,
	'U':      true,
	'V':      true,
	'W':      true,
	'X':      true,
	'Y':      true,
	'Z':      true,
	'[':      true,
	'\\':     false,
	']':      true,
	'^':      true,
	'_':      true,
	'`':      true,
	'a':      true,
	'b':      true,
	'c':      true,
	'd':      true,
	'e':      true,
	'f':      true,
	'g':      true,
	'h':      true,
	'i':      true,
	'j':      true,
	'k':      true,
	'l':      true,
	'm':      true,
	'n':      true,
	'o':      true,
	'p':      true,
	'q':      true,
	'r':      true,
	's':      true,
	't':      true,
	'u':      true,
	'v':      true,
	'w':      true,
	'x':      true,
	'y':      true,
	'z':      true,
	'{':      true,
	'|':      true,
	'}':      true,
	'~':      true,
	'\u007f': true,
}
