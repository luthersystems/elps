package libjson

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"fmt"
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

type encoder struct {
	stringNums bool
	buf        bytes.Buffer
}

func newEncoder(stringNums bool) *encoder {
	return &encoder{stringNums: stringNums}
}

func (enc *encoder) bytes() []byte {
	return enc.buf.Bytes()
}

func (enc *encoder) reset() {
	enc.buf.Reset()
}

func (enc *encoder) encode(v *lisp.LVal) error {
	if v.IsNil() {
		enc.buf.WriteString("null")
		return nil
	}
	if fn := encoderFuncs[v.Type]; fn != nil {
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
	if enc.stringNums {
		return enc.encodeString(strconv.Itoa(x))
	}
	_, err = fmt.Fprint(&enc.buf, x)
	return err
}

func (enc *encoder) encodeLFloat(v *lisp.LVal) error {
	return enc.encodeFloat(v.Float)
}

func (enc *encoder) encodeFloat(x float64) error {
	if enc.stringNums {
		return enc.encodeString(strconv.FormatFloat(x, 'g', -1, 64))
	}
	b, err := json.Marshal(x)
	enc.buf.Write(b)
	return err
}

func (enc *encoder) encodeLBytes(v *lisp.LVal) (err error) {
	// FIXME:  Reduce allocations here or use a fixed size buffer.
	return enc.encodeString(base64.StdEncoding.EncodeToString(v.Bytes()))
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
// https://cs.opensource.google/go/go/+/refs/tags/go1.16.4:src/encoding/json/encode.go;l=1029
func (enc *encoder) encodeString(s string) error {
	const hex = "0123456789abcdef"
	enc.buf.WriteByte('"')
	start := 0
	for i := 0; i < len(s); {
		if b := s[i]; b < utf8.RuneSelf {
			if safeSet[b] {
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

// NOTE:  safeSet is from the json package
// safeSet holds the value true if the ASCII character with the given array
// position can be represented inside a JSON string without any further
// escaping.
//
// All values are true except for the ASCII control characters (0-31), the
// double quote ("), and the backslash character ("\").
var safeSet = [utf8.RuneSelf]bool{
	' ':      true,
	'!':      true,
	'"':      false,
	'#':      true,
	'$':      true,
	'%':      true,
	'&':      true,
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
	'<':      true,
	'=':      true,
	'>':      true,
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
