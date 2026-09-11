// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func conditionDataEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	return env
}

// These integration cases exercise callers of ErrorCondition, including the
// testing macros whose formatted assertion failures previously carried Go
// errors. Check both the original payload and conversion inside the handler:
// an ordinary error must never turn into a second, unhandled conversion error.
func TestHandlerConditionDataStrings(t *testing.T) {
	cases := []struct {
		name    string
		expr    string
		message string
	}{
		{"json-load-string", `(json:load-string "{")`, "unexpected end of JSON input"},
		{"json-load-bytes", `(json:load-bytes (to-bytes "{"))`, "unexpected end of JSON input"},
		{"json-load-message", `(json:load-message malformed-json)`, "unexpected end of JSON input"},
		{"json-dump-string-infinity", `(json:dump-string (/ 1 0))`, "+Inf"},
		{"json-dump-bytes-infinity", `(json:dump-bytes (/ 1 0))`, "+Inf"},
		{"json-dump-message-infinity", `(json:dump-message (/ 1 0))`, "+Inf"},
		{"base64-string", `(base64:decode "!")`, "illegal base64 data"},
		{"base64-bytes", `(base64:decode (to-bytes "!"))`, "illegal base64 data"},
		{"time-rfc3339", `(time:parse-rfc3339 "invalid")`, "cannot parse"},
		{"time-duration", `(time:parse-duration "invalid")`, "invalid duration"},
		{"to-int", `(to-int "abc")`, "invalid syntax"},
		{"to-float", `(to-float "1e999")`, "value out of range"},
		{"to-string-list", `(to-string '(1))`, "cannot convert type to string: list"},
		{"concat-bytes-overflow", `(concat 'bytes '(256))`, "value overflows byte: 256"},
		{"concat-string-overflow", `(concat 'string '(256))`, "value overflows byte: 256"},
		{"append-bytes-overflow", `(append-bytes (to-bytes "") '(256))`, "value overflows byte: 256"},
		{"append-bytes-mutate-overflow", `(append-bytes! (to-bytes "") '(256))`, "value overflows byte: 256"},
		{"assert", `(assert false)`, "assertion failure: false"},
		{"assert-message", `(assert false "boom {}" 1)`, "boom 1"},
		{"unquote-zero", `(quasiquote (unquote))`, "unquote: one argument expected (got 0)"},
		{"unquote-two", `(quasiquote (unquote 1 2))`, "unquote: one argument expected (got 2)"},
		{"unquote-splicing-zero", `(quasiquote ((unquote-splicing)))`, "unquote-splicing: one argument expected (got 0)"},
		{"unquote-splicing-two", `(quasiquote ((unquote-splicing 1 2)))`, "unquote-splicing: one argument expected (got 2)"},
		{"testing-assert-number", `(testing:assert= 1 2)`, "numeric expressions are not equal"},
		{"testing-assert-number-expected-type", `(testing:assert= "one" 2)`, "did not evaluate to a number"},
		{"testing-assert-number-actual-type", `(testing:assert= 1 "two")`, "did not evaluate to a number"},
		{"testing-assert-string", `(testing:assert-string= "one" "two")`, "string expressions are not equal"},
		{"testing-assert-string-expected-type", `(testing:assert-string= 1 "two")`, "did not evaluate to a string"},
		{"testing-assert-string-actual-type", `(testing:assert-string= "one" 2)`, "did not evaluate to a string"},
		{"testing-assert-equal", `(testing:assert-equal '(1) '(2))`, "expressions are not"},
		{"testing-assert-nil", `(testing:assert-nil 1)`, "is not nil"},
		{"testing-assert-not-nil", `(testing:assert-not-nil ())`, "is nil"},
		{"testing-assert-not", `(testing:assert-not true)`, "is not falsey"},
		{"unbound-symbol", `unbound-condition-data-symbol`, "unbound symbol"},
		{"builtin-arity", `(to-int)`, "argument"},
		{"builtin-type", `(car 1)`, "not a list"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			env := conditionDataEnv(t)
			msg := json.RawMessage(`{`)
			require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("malformed-json"), lisp.Native(&msg))))
			for _, payload := range []string{"(car data)", "(to-string (car data))"} {
				source := fmt.Sprintf(`(handler-bind ((condition (lambda (c &rest data) (list c (type (car data)) %s)))) %s)`, payload, tc.expr)
				v := env.LoadString("condition-data.lisp", source)
				if !assert.Equal(t, lisp.LSExpr, v.Type, "%s: %v", payload, v) || !assert.Len(t, v.Cells, 3) {
					continue
				}
				condition := "error"
				if strings.HasPrefix(tc.name, "json-load-") {
					condition = "json:syntax-error"
				}
				assert.Equal(t, condition, v.Cells[0].Str)
				assert.Equal(t, "string", v.Cells[1].Str)
				assert.Equal(t, lisp.LString, v.Cells[2].Type)
				assert.Contains(t, v.Cells[2].Str, tc.message)
			}
		})
	}
}

// A host callback can read source while a Lisp handler is active. Cover the
// location-aware and context-aware APIs too: all must preserve parser condition
// names, rather than wrapping ErrorVal in a generic error condition.
func TestHandlerParserConditionSourceAPIs(t *testing.T) {
	for _, tc := range []struct{ name, source, message string }{
		{"unmatched-syntax", "(", "unclosed ("},
		{"mismatched-syntax", "(]", "expected )"},
		{"invalid-symbol", "a:b:c", "invalid symbol"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "invalid.lisp")
			require.NoError(t, os.WriteFile(path, []byte(tc.source), 0600))
			for _, loader := range []struct {
				name string
				load func(*lisp.LEnv) *lisp.LVal
			}{
				{"Load", func(env *lisp.LEnv) *lisp.LVal { return env.Load("source.lisp", strings.NewReader(tc.source)) }},
				{"LoadString", func(env *lisp.LEnv) *lisp.LVal { return env.LoadString("source.lisp", tc.source) }},
				{"LoadFile", func(env *lisp.LEnv) *lisp.LVal { return env.LoadFile(path) }},
				{"LoadLocation", func(env *lisp.LEnv) *lisp.LVal {
					return env.LoadLocation("source.lisp", path, strings.NewReader(tc.source))
				}},
				{"LoadContext", func(env *lisp.LEnv) *lisp.LVal {
					return env.LoadContext(context.Background(), "source.lisp", strings.NewReader(tc.source))
				}},
				{"LoadStringContext", func(env *lisp.LEnv) *lisp.LVal {
					return env.LoadStringContext(context.Background(), "source.lisp", tc.source)
				}},
				{"LoadFileContext", func(env *lisp.LEnv) *lisp.LVal { return env.LoadFileContext(context.Background(), path) }},
				{"LoadLocationContext", func(env *lisp.LEnv) *lisp.LVal {
					return env.LoadLocationContext(context.Background(), "source.lisp", path, strings.NewReader(tc.source))
				}},
			} {
				t.Run(loader.name, func(t *testing.T) {
					env := conditionDataEnv(t)
					fun := lisp.FunInPackage(lisp.DefaultUserPackage, "read-invalid-source", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
						return loader.load(env)
					})
					require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("read-invalid-source"), fun)))
					source := fmt.Sprintf(`(handler-bind ((%s (lambda (c &rest data) (list c (type (car data)) (to-string (car data)))))) (read-invalid-source))`, tc.name)
					v := env.LoadString("parser-source-api.lisp", source)
					require.Equal(t, lisp.LSExpr, v.Type, "%v", v)
					require.Len(t, v.Cells, 3)
					assert.Equal(t, tc.name, v.Cells[0].Str)
					assert.Equal(t, "string", v.Cells[1].Str)
					assert.Equal(t, lisp.LString, v.Cells[2].Type)
					assert.Contains(t, v.Cells[2].Str, tc.message)
				})
			}
		})
	}
}

func TestHandlerParserConditionNames(t *testing.T) {
	for _, tc := range []struct{ name, source, message string }{
		{"unmatched-syntax", "(", "unclosed ("},
		{"mismatched-syntax", "(]", "expected )"},
		{"invalid-symbol", "a:b:c", "invalid symbol"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "invalid.lisp")
			require.NoError(t, os.WriteFile(path, []byte(tc.source), 0600))
			for _, expr := range []string{
				fmt.Sprintf("(load-string %q)", tc.source),
				fmt.Sprintf("(load-file %q)", path),
				fmt.Sprintf("(eval '(load-string %q))", tc.source),
			} {
				t.Run(expr, func(t *testing.T) {
					env := conditionDataEnv(t)
					source := fmt.Sprintf(`(handler-bind ((%s (lambda (c &rest data) (list c (type (car data)) (to-string (car data)))))) %s)`, tc.name, expr)
					v := env.LoadString("parser-condition.lisp", source)
					require.Equal(t, lisp.LSExpr, v.Type, "%v", v)
					require.Len(t, v.Cells, 3)
					assert.Equal(t, tc.name, v.Cells[0].Str)
					assert.Equal(t, "string", v.Cells[1].Str)
					assert.Equal(t, lisp.LString, v.Cells[2].Type)
					assert.Contains(t, v.Cells[2].Str, tc.message)
				})
			}
		})
	}
}
