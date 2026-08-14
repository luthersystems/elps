// Copyright © 2018 The ELPS authors

package libjson

import (
	"fmt"
	"strconv"
)

// maxLoadDepth is the deepest document Load accepts.
//
// It restates encoding/json's maxNestingDepth, which is unexported and has no
// accessor, so the value has to be copied rather than asked for.
// TestLoadNestingLimitIsWhereWeThinkItIs finds the boundary empirically and
// asserts it is exactly here, so a standard library that moves its limit fails
// next to the constant that has to move with it.
const maxLoadDepth = 10000

// maxSafeIntDigits is the largest number of integer digits a literal can carry
// and still be certainly representable: 10**308 is below MaxFloat64
// (~1.7976931348623157e308), so any literal with at most this many digits
// before the decimal point and no exponent is in range without looking
// further.
const maxSafeIntDigits = 308

// errNativeTooDeep reports a native value nested deeper than Load will parse.
var errNativeTooDeep = fmt.Errorf("value nests more than %d levels deep", maxLoadDepth)

// numberOutOfRangeError reports a JSON number literal that float64 cannot
// hold.  It carries the literal so the message names the offending value, the
// way the decoder's own error did.
type numberOutOfRangeError string

func (e numberOutOfRangeError) Error() string {
	return "number " + string(e) + " is out of range for float64"
}

// Byte classes for the scan below.  A table keeps the hot loop to one indexed
// load and a jump, rather than a chain of comparisons per byte.
const (
	jsonOther = iota
	jsonString
	jsonOpen
	jsonClose
	jsonNumber
)

var jsonByteClass = func() (t [256]uint8) {
	t['"'] = jsonString
	t['['] = jsonOpen
	t['{'] = jsonOpen
	t[']'] = jsonClose
	t['}'] = jsonClose
	t['-'] = jsonNumber
	for c := byte('0'); c <= '9'; c++ {
		t[c] = jsonNumber
	}
	return t
}()

// checkNativeLoadable reports whether b -- bytes json.Marshal has just
// produced -- is a document Load will accept.  See encoder.checkLoadable for
// why syntax is not among the things it checks.
func checkNativeLoadable(b []byte, stringNums bool) error {
	depth := 0
	for i := 0; i < len(b); {
		switch jsonByteClass[b[i]] {
		case jsonOther:
			i++
		case jsonString:
			// Skipping strings is the whole reason this is a scan and not a
			// regexp over the bytes: digits, '-' and 'e' are ordinary text
			// inside a string literal and inside an object KEY, and a check
			// that read them as numbers would refuse `{"1E1000":1}` and
			// `"a 1E1000 b"`, both of which load perfectly well.
			i = skipJSONString(b, i)
		case jsonOpen:
			depth++
			if depth > maxLoadDepth {
				return errNativeTooDeep
			}
			i++
		case jsonClose:
			depth--
			i++
		default:
			if stringNums {
				// Load uses UseNumber in this mode, which keeps a number as
				// the text it was written as and never converts it, so no
				// literal is out of range and there is nothing to check.
				i++
				continue
			}
			n, ok := numberInFloat64Range(b[i:])
			if !ok {
				return numberOutOfRangeError(b[i : i+n])
			}
			i += n
		}
	}
	return nil
}

// skipJSONString returns the index just past the string literal whose opening
// quote is at b[i].  Backslash escapes are honoured, so an escaped quote does
// not end the string and an escaped backslash does not escape the quote after
// it -- get that wrong and the scan reads string text as JSON and vice versa.
//
// A string that is never closed cannot occur here: json.Marshal has already
// refused those.  If one somehow did, the scan stops at the end of the input
// rather than running off it.
func skipJSONString(b []byte, i int) int {
	for i++; i < len(b); i++ {
		switch b[i] {
		case '\\':
			i++
		case '"':
			return i + 1
		}
	}
	return i
}

// numberInFloat64Range reports whether the JSON number literal at the start of
// s is one float64 can represent, along with the literal's length in bytes.
// The length is returned even when the answer is false, so the caller can name
// the offending literal.
//
// "Can represent" means precisely what Load means by it: Load decodes into an
// interface{}, so encoding/json converts every number with
// strconv.ParseFloat(s, 64) and fails the whole document if that errors.
// Since json.Marshal has already established that the literal is well formed,
// the only error ParseFloat can return here is ErrRange -- overflow. Underflow
// is NOT an error in Go: 1E-1000000000000 parses to 0 and loads fine, so this
// must accept it.
//
// The decision is made from the literal's shape wherever the shape settles it,
// which is every case that occurs in practice. Writing the value as 0.d1d2...
// x 10**dp, it is strictly less than 10**dp and at least 10**(dp-1), so
// dp <= 308 is certainly in range and dp >= 310 is certainly out of it. Only
// the single decade in between straddles MaxFloat64, and only there does this
// convert -- which is why the scan does not allocate on any document that does
// not hold a number within a factor of ten of the largest float64.
func numberInFloat64Range(s []byte) (int, bool) {
	i := 0
	if s[i] == '-' {
		i++
	}
	intStart := i
	for i < len(s) && isJSONDigit(s[i]) {
		i++
	}
	intEnd := i

	fracStart, fracEnd := i, i
	if i < len(s) && s[i] == '.' {
		i++
		fracStart = i
		for i < len(s) && isJSONDigit(s[i]) {
			i++
		}
		fracEnd = i
	}

	// The common case, and the only one an ordinary document reaches: no
	// exponent, and too few integer digits to climb to 10**308. A fraction
	// cannot make a number bigger, so it needs no examination at all.
	if intEnd-intStart <= maxSafeIntDigits && (i >= len(s) || (s[i] != 'e' && s[i] != 'E')) {
		return i, true
	}

	exp, i := scanJSONExponent(s, i)

	// dp: the position of the decimal point relative to the first significant
	// digit. JSON forbids a leading zero on a non-zero integer part, so the
	// loops below strip at most the single zero of "0" or "-0" -- they are
	// there so a literal whose magnitude lives entirely in the fraction
	// ("0.00001e314") is measured from its first significant digit rather than
	// from the decimal point.
	var dp int64
	k := intStart
	for k < intEnd && s[k] == '0' {
		k++
	}
	if k < intEnd {
		dp = int64(intEnd-k) + exp
	} else {
		f := fracStart
		for f < fracEnd && s[f] == '0' {
			f++
		}
		if f == fracEnd {
			// Every digit is a zero, so the literal is zero however large its
			// exponent: 0e999999 loads, and must not be refused.
			return i, true
		}
		dp = exp - int64(f-fracStart)
	}

	switch {
	case dp <= 308:
		// Below 10**308, which is below MaxFloat64.
		return i, true
	case dp >= 310:
		// At or above 10**309, which is above MaxFloat64.
		return i, false
	}
	// 10**308 <= value < 10**309 straddles the ceiling. Only an exact
	// conversion separates 1.7976931348623157e308 from 1.7976931348623159e308,
	// so this decade -- and nothing else -- pays for one.
	_, err := strconv.ParseFloat(string(s[:i]), 64)
	return i, err == nil
}

// scanJSONExponent reads the exponent that may follow the mantissa ending at
// s[i], returning its value and the index just past the whole literal.
//
// The value saturates rather than overflowing an int64: an exponent can be
// written with any number of digits (1e-999999999999999999999999 is valid
// JSON) and the arithmetic above must stay meaningful. The saturation point is
// derived from the input rather than fixed, because a fixed cap can invert the
// answer -- clamping a huge NEGATIVE exponent to something smaller than the
// mantissa's digit count would turn an underflow into an apparent overflow.
// The literal has at most len(s) integer digits, so once the exponent's
// magnitude passes len(s)+1024 it already decides the comparison against
// 10**309 in whichever direction it points, and clamping there cannot change
// which side of the ceiling the value falls on.
func scanJSONExponent(s []byte, i int) (int64, int) {
	if i >= len(s) || (s[i] != 'e' && s[i] != 'E') {
		return 0, i
	}
	j := i + 1
	neg := false
	if j < len(s) && (s[j] == '+' || s[j] == '-') {
		neg = s[j] == '-'
		j++
	}
	start := j
	limit := int64(len(s)) + 1024
	var exp int64
	for ; j < len(s) && isJSONDigit(s[j]); j++ {
		if exp <= limit {
			exp = exp*10 + int64(s[j]-'0')
		}
	}
	if j == start {
		// An 'e' with no digits after it is not an exponent. json.Marshal has
		// already ruled this out; treating the 'e' as the end of the literal
		// keeps the scan total if it ever were not.
		return 0, i
	}
	if exp > limit {
		exp = limit
	}
	if neg {
		exp = -exp
	}
	return exp, j
}

func isJSONDigit(c byte) bool { return c >= '0' && c <= '9' }
