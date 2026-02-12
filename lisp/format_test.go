// Copyright © 2025 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestFormatString(t *testing.T) {
	tests := elpstest.TestSuite{
		{"single sequential placeholder", elpstest.TestSequence{
			{`(format-string "hello {}" "world")`, `"hello world"`, ""},
		}},
		{"multiple substitutions", elpstest.TestSequence{
			{`(format-string "{} and {}" "one" "two")`, `"one and two"`, ""},
			{`(format-string "{}, {}, {}" "a" "b" "c")`, `"a, b, c"`, ""},
		}},
		{"no placeholders", elpstest.TestSequence{
			{`(format-string "hello world")`, `"hello world"`, ""},
		}},
		{"no placeholders with extra args", elpstest.TestSequence{
			// Fast path still ignores extra arguments.
			{`(format-string "no braces" "extra")`, `"no braces"`, ""},
		}},
		{"empty format string", elpstest.TestSequence{
			{`(format-string "")`, `""`, ""},
		}},
		{"placeholder at start", elpstest.TestSequence{
			{`(format-string "{} world" "hello")`, `"hello world"`, ""},
		}},
		{"adjacent placeholders", elpstest.TestSequence{
			{`(format-string "{}{}" "hello" "world")`, `"helloworld"`, ""},
		}},
		{"only placeholder", elpstest.TestSequence{
			{`(format-string "{}" "value")`, `"value"`, ""},
		}},
		{"empty string value", elpstest.TestSequence{
			{`(format-string "a{}b" "")`, `"ab"`, ""},
		}},
		{"integer value", elpstest.TestSequence{
			{`(format-string "count: {}" 42)`, `"count: 42"`, ""},
		}},
		{"float value", elpstest.TestSequence{
			{`(format-string "pi: {}" 3.14)`, `"pi: 3.14"`, ""},
		}},
		{"boolean values", elpstest.TestSequence{
			{`(format-string "{} and {}" true false)`, `"true and false"`, ""},
		}},
		{"nil value", elpstest.TestSequence{
			{`(format-string "value: {}" ())`, `"value: ()"`, ""},
		}},
		{"list value", elpstest.TestSequence{
			{`(format-string "list: {}" '(1 2 3))`, `"list: '(1 2 3)"`, ""},
		}},
		{"quoted string shows quotes", elpstest.TestSequence{
			{`(format-string "value: {}" (quote "hello"))`, `"value: \"hello\""`, ""},
		}},
		{"extra args ignored", elpstest.TestSequence{
			{`(format-string "hello {}" "world" "extra")`, `"hello world"`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func TestFormatStringBraceEscaping(t *testing.T) {
	tests := elpstest.TestSuite{
		{"escaped open brace", elpstest.TestSequence{
			{`(format-string "use {{ for open brace")`, `"use { for open brace"`, ""},
		}},
		{"escaped close brace", elpstest.TestSequence{
			{`(format-string "use }} for close brace")`, `"use } for close brace"`, ""},
		}},
		{"escaped open and close braces", elpstest.TestSequence{
			{`(format-string "use {{}} for braces")`, `"use {} for braces"`, ""},
		}},
		{"escaped braces around placeholder", elpstest.TestSequence{
			{`(format-string "{{{}}} works" "hello")`, `"{hello} works"`, ""},
		}},
		{"double escaped close", elpstest.TestSequence{
			{`(format-string "}}}}")`, `"}}"`, ""},
		}},
		{"double escaped open", elpstest.TestSequence{
			{`(format-string "{{{{")`, `"{{"`, ""},
		}},
		{"close then open escaped", elpstest.TestSequence{
			{`(format-string "}}{{")`, `"}{"`, ""},
		}},
		{"only escaped braces no args", elpstest.TestSequence{
			{`(format-string "{{}}")`, `"{}"`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func TestFormatStringPositional(t *testing.T) {
	tests := elpstest.TestSuite{
		{"simple positional", elpstest.TestSequence{
			{`(format-string "{0}" "world")`, `"world"`, ""},
		}},
		{"reversed order", elpstest.TestSequence{
			{`(format-string "{1} {0}" "world" "hello")`, `"hello world"`, ""},
		}},
		{"repeated arg", elpstest.TestSequence{
			{`(format-string "{0} {0}" "echo")`, `"echo echo"`, ""},
		}},
		{"arbitrary order", elpstest.TestSequence{
			{`(format-string "{2} {0} {1}" "a" "b" "c")`, `"c a b"`, ""},
		}},
		{"positional with literal text", elpstest.TestSequence{
			{`(format-string "name={0}, age={1}" "Alice" 30)`, `"name=Alice, age=30"`, ""},
		}},
		{"positional with escaped braces", elpstest.TestSequence{
			{`(format-string "{{{0}}}" "hello")`, `"{hello}"`, ""},
		}},
		{"positional with whitespace", elpstest.TestSequence{
			{`(format-string "{ 0 }" "world")`, `"world"`, ""},
		}},
		{"positional extra args ignored", elpstest.TestSequence{
			{`(format-string "{0}" "used" "unused")`, `"used"`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// errCapture wraps expr so that handler-bind catches the error and
// returns the error message string. This lets tests assert on the
// specific error message rather than just detecting that an error
// occurred.
func errCapture(expr string) string {
	return `(handler-bind ((condition (lambda (c &rest args) (car args)))) ` + expr + `)`
}

func TestFormatStringErrors(t *testing.T) {
	tests := elpstest.TestSuite{
		{"too many placeholders", elpstest.TestSequence{
			{errCapture(`(format-string "{} and {}" "only-one")`),
				`"too many formatting directives for supplied values"`, ""},
		}},
		{"unclosed brace", elpstest.TestSequence{
			{errCapture(`(format-string "hello {")`),
				`"unclosed '{' in format string"`, ""},
		}},
		{"lone closing brace", elpstest.TestSequence{
			{errCapture(`(format-string "hello }")`),
				`"unexpected closing brace '}' outside of formatting directive"`, ""},
		}},
		{"non-string first arg", elpstest.TestSequence{
			{errCapture(`(format-string 42 "world")`),
				`"first argument is not a string"`, ""},
		}},
		{"invalid directive content", elpstest.TestSequence{
			{errCapture(`(format-string "{abc}" "world")`),
				`"invalid format directive: {abc}"`, ""},
		}},
		{"positional index out of range", elpstest.TestSequence{
			{errCapture(`(format-string "{5}" "only-one")`),
				`"positional index 5 out of range for 1 supplied values"`, ""},
		}},
		{"positional zero values", elpstest.TestSequence{
			{errCapture(`(format-string "{0}")`),
				`"positional index 0 out of range for 0 supplied values"`, ""},
		}},
		{"sequential zero values", elpstest.TestSequence{
			{errCapture(`(format-string "{}")`),
				`"too many formatting directives for supplied values"`, ""},
		}},
		{"mix sequential then positional", elpstest.TestSequence{
			{errCapture(`(format-string "{} {0}" "a" "b")`),
				`"cannot mix positional {N} and sequential {} placeholders"`, ""},
		}},
		{"mix positional then sequential", elpstest.TestSequence{
			{errCapture(`(format-string "{0} {}" "a" "b")`),
				`"cannot mix positional {N} and sequential {} placeholders"`, ""},
		}},
		{"negative positional index", elpstest.TestSequence{
			{errCapture(`(format-string "{-1}" "a")`),
				`"negative positional index: {-1}"`, ""},
		}},
		{"brace at end of string", elpstest.TestSequence{
			{errCapture(`(format-string "}")`),
				`"unexpected closing brace '}' outside of formatting directive"`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func BenchmarkFormatStringSimple(b *testing.B) {
	elpstest.RunBenchmark(b, `
		(dotimes (n 1000)
			(format-string "hello {}" "world"))
	`)
}

func BenchmarkFormatStringMultiple(b *testing.B) {
	elpstest.RunBenchmark(b, `
		(dotimes (n 1000)
			(format-string "{}, {}, {}" "one" "two" "three"))
	`)
}

func BenchmarkFormatStringNoPlaceholders(b *testing.B) {
	elpstest.RunBenchmark(b, `
		(dotimes (n 1000)
			(format-string "hello world, no placeholders here"))
	`)
}

func BenchmarkFormatStringManyTokens(b *testing.B) {
	elpstest.RunBenchmark(b, `
		(dotimes (n 1000)
			(format-string "{} {} {} {} {} {} {} {} {} {}"
				"a" "b" "c" "d" "e" "f" "g" "h" "i" "j"))
	`)
}

func BenchmarkFormatStringMixed(b *testing.B) {
	elpstest.RunBenchmark(b, `
		(dotimes (n 1000)
			(format-string "name: {}, age: {}, score: {}" "Alice" 30 99.5))
	`)
}

func BenchmarkFormatStringPositional(b *testing.B) {
	elpstest.RunBenchmark(b, `
		(dotimes (n 1000)
			(format-string "{1} {0} {2}" "a" "b" "c"))
	`)
}

func BenchmarkFormatStringConcat(b *testing.B) {
	// Baseline: concat is the natural alternative to format-string for
	// simple cases. This benchmark exists to show format-string overhead
	// relative to raw string concatenation.
	elpstest.RunBenchmark(b, `
		(dotimes (n 1000)
			(concat 'string "hello " "world"))
	`)
}
