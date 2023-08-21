package lisp

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFormatStringSimple(t *testing.T) {
	env := NewEnv(nil)
	format := String("Swap a symbol here {}")
	arg1 := String("yeah")
	args := &LVal{Cells: []*LVal{format, arg1}}
	formatted := builtinFormatString(env, args)
	assert.Equal(t, "Swap a symbol here yeah", formatted.Str)
}

func TestFormatStringComplex(t *testing.T) {
	env := NewEnv(nil)
	format := String("Swap a symbol here ")
	built := format.Str
	args := []*LVal{format}
	for x := 0; x < 100; x++ {
		args[0] = String(fmt.Sprintf("%s {}, blah blah,", args[0].Str))
		built = fmt.Sprintf("%s %d, blah blah,", built, x)
		args = append(args, String(strconv.Itoa(x)))
		formatted := builtinFormatString(env, &LVal{Cells: args})
		assert.Equal(t, built, formatted.Str)
	}
}

func TestFormatStringUnmatched(t *testing.T) {
	env := NewEnv(nil)
	format := String("Swap a symbol here }")
	arg1 := String("yeah")
	args := &LVal{Cells: []*LVal{format, arg1}}
	formatted := builtinFormatString(env, args)
	assert.Equal(t, LError, formatted.Type)
}

func TestTokenizeFormatString(t *testing.T) {
	x := tokenizeFormatString("This is a formatted {} string yeah {}.")
	assert.Equal(t, []formatToken{
		{typ: formatText, text: "This is a formatted "},
		{typ: formatOpen, text: "{"},
		{typ: formatClose, text: "}"},
		{typ: formatText, text: " string yeah "},
		{typ: formatOpen, text: "{"},
		{typ: formatClose, text: "}"},
		{typ: formatText, text: "."},
	}, x)
}

var formatTokenRegexp = regexp.MustCompile(`\{(\d+)\}`)

// var formatBadTokenPattern = regexp.MustCompile(`(?:\{[^\d\}]|[^\{\d]\})`) // This is slooooow...
// var formatBadTokenPattern = regexp.MustCompile(`\{[^\d\}]`) // This is also slow enough that I'd ditch it

func quickformatBuiltin(env *LEnv, args *LVal) *LVal {
	format := args.Cells[0]
	//if formatBadTokenPattern.MatchString(format.Str) {
	//return env.Errorf("unmatched token")
	//}
	replacements := make([]interface{}, len(args.Cells)-1)
	for k, v := range args.Cells[1:] {
		if v.Type == LString && !v.Quoted {
			replacements[k] = v.Str
		} else {
			replacements[k] = v.String()
		}
	}
	if format.Type != LString {
		return env.Errorf("first argument is not a string")
	}
	pattern := formatTokenRegexp.ReplaceAllString(strings.ReplaceAll(strings.ReplaceAll(format.Str, "%", "%%"), "{}", "%s"), "%[$1]s")
	return String(fmt.Sprintf(pattern, replacements...))
}

func TestQuickFormatStringSimple(t *testing.T) {
	env := NewEnv(nil)
	format := String("Swap a symbol here {}")
	arg1 := String("yeah")
	args := &LVal{Cells: []*LVal{format, arg1}}
	formatted := quickformatBuiltin(env, args)
	assert.Equal(t, "Swap a symbol here yeah", formatted.Str)
}

func TestQuickFormatStringComplex(t *testing.T) {
	env := NewEnv(nil)
	format := String("Swap a symbol here ")
	built := format.Str
	args := []*LVal{format}
	for x := 0; x < 100; x++ {
		args[0] = String(fmt.Sprintf("%s {}, blah blah,", args[0].Str))
		built = fmt.Sprintf("%s %d, blah blah,", built, x)
		args = append(args, String(strconv.Itoa(x)))
		formatted := quickformatBuiltin(env, &LVal{Cells: args})
		assert.Equal(t, built, formatted.Str)
	}
}

func TestQuickFormatStringUnmatched(t *testing.T) {
	env := NewEnv(nil)
	format := String("Swap a symbol here }")
	arg1 := String("yeah")
	args := &LVal{Cells: []*LVal{format, arg1}}
	formatted := quickformatBuiltin(env, args)
	assert.Equal(t, LError, formatted.Type)
}

func TestQuickFormatStringPositional(t *testing.T) {
	env := NewEnv(nil)
	format := String("Swap a symbol here {2} and here {1}")
	arg1 := String("yeah")
	arg2 := String("oh")
	args := &LVal{Cells: []*LVal{format, arg1, arg2}}
	formatted := quickformatBuiltin(env, args)
	assert.Equal(t, "Swap a symbol here oh and here yeah", formatted.Str)
}

func BenchmarkQuickFormatString(t *testing.B) {
	env := NewEnv(nil)
	format := String("Swap a symbol here {} and {} and {}")
	for x := 0; x < t.N; x++ {
		args := []*LVal{format, String("A"), String("B"), String("C")}
		formatted := quickformatBuiltin(env, &LVal{Cells: args})
		assert.Equal(t, "Swap a symbol here A and B and C", formatted.Str)
	}
}

func BenchmarkQuickFormatStringWithPositionalToken(t *testing.B) {
	env := NewEnv(nil)
	format := String("Swap a symbol here {} and {} and {1}")
	for x := 0; x < t.N; x++ {
		args := []*LVal{format, String("A"), String("B")}
		formatted := quickformatBuiltin(env, &LVal{Cells: args})
		assert.Equal(t, "Swap a symbol here A and B and A", formatted.Str)
	}
}

func BenchmarkQuickFormatStringWithATonOfTokens(t *testing.B) {
	env := NewEnv(nil)
	format := String("Swap a symbol here ")
	built := format.Str
	args := []*LVal{format}
	for x := 0; x < 100; x++ {
		args[0] = String(fmt.Sprintf("%s {}, blah blah,", args[0].Str))
		built = fmt.Sprintf("%s %d, blah blah,", built, x)
		args = append(args, String(strconv.Itoa(x)))
	}
	for x := 0; x < t.N; x++ {
		formatted := quickformatBuiltin(env, &LVal{Cells: args})
		assert.Equal(t, built, formatted.Str)
	}
}

func BenchmarkFormatString(t *testing.B) {
	env := NewEnv(nil)
	format := String("Swap a symbol here {} and {} and {}")
	for x := 0; x < t.N; x++ {
		args := []*LVal{format, String("A"), String("B"), String("C")}
		formatted := builtinFormatString(env, &LVal{Cells: args})
		assert.Equal(t, "Swap a symbol here A and B and C", formatted.Str)
	}
}

func BenchmarkFormatStringWithATonOfTokens(t *testing.B) {
	env := NewEnv(nil)
	format := String("Swap a symbol here ")
	built := format.Str
	args := []*LVal{format}
	for x := 0; x < 100; x++ {
		args[0] = String(fmt.Sprintf("%s {}, blah blah,", args[0].Str))
		built = fmt.Sprintf("%s %d, blah blah,", built, x)
		args = append(args, String(strconv.Itoa(x)))
	}
	for x := 0; x < t.N; x++ {
		formatted := builtinFormatString(env, &LVal{Cells: args})
		assert.Equal(t, built, formatted.Str)
	}
}
