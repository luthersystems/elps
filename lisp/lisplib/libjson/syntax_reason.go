// Copyright © 2026 The ELPS authors

package libjson

import "strconv"

// invalidJSONError is the json:syntax-error of the load functions for a
// document that is not JSON.
//
// encoding/json decides that the document is malformed.  This package then
// finds the first fault again, with jsonFault, and words it here.  The two
// JSON engines of Go 1.27 describe one fault in different words
// (luthersystems/elps#928), and a program can read the text of a condition,
// so the text must not move with the engine.  The cost is one more scan of
// a document that is refused anyway; a document that loads never comes here.
type invalidJSONError struct {
	reason string
	offset int
}

func (e invalidJSONError) Error() string {
	if e.reason == "" {
		// jsonFault found nothing wrong in a document the engine refused.
		// FuzzSyntaxReasonMatchesEngine fails on that, so no document is
		// known to come here.
		return "json: invalid JSON"
	}
	return "json: invalid JSON at offset " + strconv.Itoa(e.offset) + ": " + e.reason
}

// newInvalidJSONError describes the first fault in b, a document that
// encoding/json refused.
func newInvalidJSONError(b []byte) invalidJSONError {
	offset, reason := jsonFault(b)
	return invalidJSONError{offset: offset, reason: reason}
}

// The reasons jsonFault gives.  They are part of the error text.
const (
	faultEnd      = "unexpected end of input"
	faultValue    = "expected a value"
	faultLiteral  = "invalid literal"
	faultNumber   = "invalid number"
	faultControl  = "control character in string"
	faultEscape   = "invalid escape in string"
	faultKey      = "expected a string key"
	faultColon    = "expected ':' after the key"
	faultInObject = "expected ',' or '}' in object"
	faultInArray  = "expected ',' or ']' in array"
	faultTrailing = "unexpected data after the top-level value"
	faultTooDeep  = "nests more than 10000 levels deep"
)

// jsonFault returns the byte offset of the first fault in b and what is
// wrong there.  The reason is empty when b is a JSON document.
//
// It accepts what encoding/json accepts: the grammar of RFC 8259, with
// containers nested at most jsonNestingLimit deep, and with any bytes of
// 0x20 and above inside a string, valid UTF-8 or not.  A fault at the end
// of b is always faultEnd, whatever was being read.
func jsonFault(b []byte) (int, string) {
	// stack holds '[' or '{' for each open container.
	var stack []byte
	i := skipJSONSpace(b, 0)
	for {
		// A value starts at i.
		if i == len(b) {
			return i, faultEnd
		}
		var reason string
		switch c := b[i]; {
		case c == '[' || c == '{':
			if len(stack) == jsonNestingLimit {
				return i, faultTooDeep
			}
			stack = append(stack, c)
			i = skipJSONSpace(b, i+1)
			if i == len(b) {
				return i, faultEnd
			}
			if c == '[' && b[i] != ']' {
				continue
			}
			if c == '{' && b[i] != '}' {
				if i, reason = jsonFaultKey(b, i); reason != "" {
					return i, reason
				}
				continue
			}
			// An empty container: fall through to close it below.
		case c == '"':
			i, reason = jsonFaultString(b, i)
		case c == '-' || (c >= '0' && c <= '9'):
			i, reason = jsonFaultNumber(b, i)
		case c == 't':
			i, reason = jsonFaultLiteral(b, i, "true")
		case c == 'f':
			i, reason = jsonFaultLiteral(b, i, "false")
		case c == 'n':
			i, reason = jsonFaultLiteral(b, i, "null")
		default:
			return i, faultValue
		}
		if reason != "" {
			return i, reason
		}
		// A value has ended at i.  Close every container that ends here,
		// then step to the next value.
		for {
			i = skipJSONSpace(b, i)
			if len(stack) == 0 {
				if i < len(b) {
					return i, faultTrailing
				}
				return 0, ""
			}
			if i == len(b) {
				return i, faultEnd
			}
			open := stack[len(stack)-1]
			if (open == '[' && b[i] == ']') || (open == '{' && b[i] == '}') {
				stack = stack[:len(stack)-1]
				i++
				continue
			}
			if b[i] != ',' {
				if open == '[' {
					return i, faultInArray
				}
				return i, faultInObject
			}
			i = skipJSONSpace(b, i+1)
			if open == '{' {
				if i, reason = jsonFaultKey(b, i); reason != "" {
					return i, reason
				}
			}
			break
		}
	}
}

func skipJSONSpace(b []byte, i int) int {
	for i < len(b) && (b[i] == ' ' || b[i] == '\t' || b[i] == '\n' || b[i] == '\r') {
		i++
	}
	return i
}

// jsonFaultKey reads an object key and its colon at i, and returns where the
// member's value starts.
func jsonFaultKey(b []byte, i int) (int, string) {
	if i == len(b) {
		return i, faultEnd
	}
	if b[i] != '"' {
		return i, faultKey
	}
	i, reason := jsonFaultString(b, i)
	if reason != "" {
		return i, reason
	}
	i = skipJSONSpace(b, i)
	if i == len(b) {
		return i, faultEnd
	}
	if b[i] != ':' {
		return i, faultColon
	}
	return skipJSONSpace(b, i+1), ""
}

// jsonFaultString reads the string that opens at i, and returns where it
// ends.
func jsonFaultString(b []byte, i int) (int, string) {
	for i++; i < len(b); i++ {
		switch c := b[i]; {
		case c == '"':
			return i + 1, ""
		case c < 0x20:
			return i, faultControl
		case c == '\\':
			i++
			if i == len(b) {
				return i, faultEnd
			}
			switch b[i] {
			case '"', '\\', '/', 'b', 'f', 'n', 'r', 't':
			case 'u':
				for range 4 {
					i++
					if i == len(b) {
						return i, faultEnd
					}
					if !isJSONHex(b[i]) {
						return i, faultEscape
					}
				}
			default:
				return i, faultEscape
			}
		}
	}
	return i, faultEnd
}

func isJSONHex(c byte) bool {
	return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
}

// jsonFaultNumber reads the number that starts at i, and returns where it
// ends.  A fault is at the byte that cannot continue the number.
func jsonFaultNumber(b []byte, i int) (int, string) {
	digits := func() bool {
		start := i
		for i < len(b) && b[i] >= '0' && b[i] <= '9' {
			i++
		}
		return i > start
	}
	fault := func() (int, string) {
		if i == len(b) {
			return i, faultEnd
		}
		return i, faultNumber
	}
	if b[i] == '-' {
		i++
	}
	if i < len(b) && b[i] == '0' {
		i++
	} else if !digits() {
		return fault()
	}
	if i < len(b) && b[i] == '.' {
		i++
		if !digits() {
			return fault()
		}
	}
	if i < len(b) && (b[i] == 'e' || b[i] == 'E') {
		i++
		if i < len(b) && (b[i] == '+' || b[i] == '-') {
			i++
		}
		if !digits() {
			return fault()
		}
	}
	return i, ""
}

// jsonFaultLiteral reads word at i, and returns where it ends.
func jsonFaultLiteral(b []byte, i int, word string) (int, string) {
	for j := range len(word) {
		if i+j == len(b) {
			return i + j, faultEnd
		}
		if b[i+j] != word[j] {
			return i + j, faultLiteral
		}
	}
	return i + len(word), ""
}
