// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestMath(t *testing.T) {
	tests := elpstest.TestSuite{
		{"arithmetic", elpstest.TestSequence{
			// arithmetic functions w/o args
			{"(+)", "0", ""},
			{"(-)", "0", ""},
			{"(*)", "1", ""},
			{"(/)", "1", ""},
			// arithmetic functions w/ one arg
			{"(+ 2)", "2", ""},
			{"(+ 2.0)", "2", ""},
			{"(- 2)", "-2", ""},
			{"(- 2.0)", "-2", ""},
			{"(* 2)", "2", ""},
			{"(* 2.0)", "2", ""},
			{"(/ 2)", "0.5", ""},
			{"(/ 2.0)", "0.5", ""},
			// arithmetic functions w/ two args
			{"(* 2 0.75)", "1.5", ""},
			{"(+ 1 2 3)", "6", ""},
			{"(+ 1 (* 2 3))", "7", ""},
			{"(+ 1 1.5)", "2.5", ""},
			{"(- 0.5 1)", "-0.5", ""},
			{"(- 1 0.5)", "0.5", ""},
			{"(* 2 0.75)", "1.5", ""},
			{"(pow 2 2)", "4", ""},
			{"(pow 2 3)", "8", ""},
			{"(pow 2.0 3.0)", "8", ""},
			{"(mod 2 2)", "0", ""},
			{"(mod 2 3)", "2", ""},
			{"(mod 4 3)", "1", ""},
			{"(mod -1 3)", "-1", ""},
			{"(mod -5 3)", "-2", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func TestNumbers(t *testing.T) {
	tests := elpstest.TestSuite{
		{"numeric types", elpstest.TestSequence{
			// arithmetic functions w/o args
			{"(int? 1)", "true", ""},
			{"(int? 0)", "true", ""},
			{"(int? -1)", "true", ""},
			{"(int? 1.0)", "false", ""},
			{"(int? 0.0)", "false", ""},
			{"(int? -1.0)", "false", ""},
			{"(number? 1)", "true", ""},
			{"(number? 0)", "true", ""},
			{"(number? -1)", "true", ""},
			{"(number? 1.0)", "true", ""},
			{"(number? 0.0)", "true", ""},
			{"(number? -1.0)", "true", ""},
			{"(int? (/ 1))", "true", ""},
			{"(int? (/ 10 5 2))", "true", ""},
			{"(int? (* 2))", "true", ""},
			{"(int? (* 2.0))", "false", ""},
			{"(int? (/ 10 2.0))", "false", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func TestLogic(t *testing.T) {
	tests := elpstest.TestSuite{
		{"logic", elpstest.TestSequence{
			{"(not true)", "false", ""},
			{"(not ())", "true", ""},
			{"(not false)", "true", ""},
			{"(not (not ()))", "false", ""},
			{"(not (not false))", "false", ""},
			{"(not (not true))", "true", ""},
			{"(or)", "false", ""},
			{"(or false false)", "false", ""},
			{"(or false ())", "()", ""},
			{"(or false true)", "true", ""},
			{"(or true false)", "true", ""},
			{"(and)", "true", ""},
			{"(and false true)", "false", ""},
			{"(and () true)", "()", ""},
			{"(and true true)", "true", ""},
			{"(set 'x 0)", "0", ""},
			{"(and (> x 0) (set 'x (+ x 1)))", "false", ""},
			{"(or (> x 0) (set 'x (+ x 1)))", "1", ""},
			{"(= 1 1.0)", "true", ""},
			{"(= 1 1)", "true", ""},
			{"(= 5 1)", "false", ""},
			{"(= 5.0 (+ 1 4.0))", "true", ""},
			{"(< 0 1)", "true", ""},
			{"(< 0 1.0)", "true", ""},
			{"(< 1 1)", "false", ""},
			{"(< 1.0 1)", "false", ""},
			{"(< 2 1)", "false", ""},
			{"(< 2.0 1)", "false", ""},
			{"(<= 0 1)", "true", ""},
			{"(<= 0 1.0)", "true", ""},
			{"(<= 1 1)", "true", ""},
			{"(<= 1.0 1)", "true", ""},
			{"(<= 2 1)", "false", ""},
			{"(<= 2.0 1)", "false", ""},
			{"(> 0.0 1)", "false", ""},
			{"(> 0 1.0)", "false", ""},
			{"(> 1 1)", "false", ""},
			{"(> 1.0 1)", "false", ""},
			{"(> 2 1)", "true", ""},
			{"(> 2.0 1)", "true", ""},
			{"(>= 0.0 1)", "false", ""},
			{"(>= 0 1.0)", "false", ""},
			{"(>= 1 1)", "true", ""},
			{"(>= 1.0 1)", "true", ""},
			{"(>= 2 1)", "true", ""},
			{"(>= 2.0 1)", "true", ""},
		}},
		{"strings", elpstest.TestSequence{
			{`(string= "" "")`, "true", ""},
			{`(string= "abc" "abc")`, "true", ""},
			{`(string= "abc" "ABC")`, `false`, ""},
			{`(string= "abc" (format-string "ab{}" "c"))`, "true", ""},
			{`(string< "" "abc")`, "true", ""},
			{`(string< "ABC" "abc")`, "true", ""},
			{`(string< "a" "z")`, "true", ""},
			{`(string< "abc" "AABC")`, `false`, ""},
			{`(string< "abc" "abc")`, `false`, ""},
			{`(string<= "" "")`, "true", ""},
			{`(string<= "abc" "abc")`, "true", ""},
			{`(string<= "ABC" "abc")`, "true", ""},
			{`(string<= "abc" "abcdef")`, "true", ""},
			{`(string<= "abc" "")`, `false`, ""},
			{`(string<= "abc" "ABC")`, `false`, ""},
			{`(string> "abc" "ABC")`, "true", ""},
			{`(string> "abc" "")`, "true", ""},
			{`(string> "" "abc")`, `false`, ""},
			{`(string> "" "")`, `false`, ""},
			{`(string> "abc" "abc")`, `false`, ""},
			{`(string>= "" "")`, "true", ""},
			{`(string>= "abc" "abc")`, "true", ""},
			{`(string>= "abc" "")`, "true", ""},
			{`(string>= "abc" "ABC")`, "true", ""},
			{`(string>= "ABC" "abc")`, `false`, ""},
			{`(string>= "abc" "abcdef")`, `false`, ""},
		}},
		{"equal?", elpstest.TestSequence{
			{`(equal? () ())`, "true", ""},
			{`(equal? 2 2)`, "true", ""},
			{`(equal? 2 3)`, `false`, ""},
			{`(equal? 2 (+ 1 1))`, "true", ""},
			{`(equal? 2 2.5)`, `false`, ""},
			{`(equal? 2.1 2.5)`, `false`, ""},
			{`(equal? 'a 'a)`, "true", ""},
			{`(equal? 'a 'b)`, `false`, ""},
			{`(equal? "a" 'a)`, `false`, ""},
			{`(equal? "a" "a")`, "true", ""},
			{`(equal? "a" "A")`, `false`, ""},
		}},
		{"aggregation", elpstest.TestSequence{
			{"(max 1)", "1", ""},
			{"(max 1 -1)", "1", ""},
			{"(max 1 -1 2 3 -1 0)", "3", ""},
			{"(min 1)", "1", ""},
			{"(min 1 -1)", "-1", ""},
			{"(min -3 -1 2 3 -1 0)", "-3", ""},
			{"(all? (expr (< % 3)) '(0 -1 2))", "true", ""},
			{"(all? (expr (< % 3)) '(0 -1 3 2))", "false", ""},
			{"(any? (expr (> % 3)) '(0 -1 2))", "false", ""},
			{"(any? (expr (> % 0)) '(0 -1 3 2))", "true", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func TestPowOverflow(t *testing.T) {
	for _, expr := range []string{"(pow 2 64)", "(pow 10 19)", "(pow 10 20)", "(pow 3 40)"} {
		elpstest.RunTestSuite(t, elpstest.TestSuite{
			{expr, elpstest.TestSequence{
				{`(handler-bind ((error (lambda (c message) (list c message)))) ` + expr + `)`,
					`'('error "integer overflow: power overflows int")`, ""},
			}},
		})
	}
}

func TestPowNumericControls(t *testing.T) {
	elpstest.RunTestSuite(t, elpstest.TestSuite{
		{"integer boundaries", elpstest.TestSequence{
			{`(list (pow 2 62) (pow -2 63) (pow 10 18) (pow 2 0) (pow 0 0))`,
				`'(4611686018427387904 -9223372036854775808 1000000000000000000 1 1)`, ""},
			{`(all? int? (list (pow 2 62) (pow -2 63) (pow 10 18) (pow 2 0) (pow 0 0)))`, `true`, ""},
		}},
		{"float paths", elpstest.TestSequence{
			{`(list (pow 2 -2) (pow -2 -3) (pow 2.0 3) (pow 2 3.0) (pow 2.0 3.0))`, `'(0.25 -0.125 8 8 8)`, ""},
			{`(all? float? (list (pow 2 -2) (pow -2 -3) (pow 2.0 3) (pow 2 3.0) (pow 2.0 3.0)))`, `true`, ""},
			{`(= (pow 2.0 64) 18446744073709551616.0)`, `true`, ""},
			{`(= (pow 2 64.0) 18446744073709551616.0)`, `true`, ""},
		}},
	})
}

func TestMaxMinNaN(t *testing.T) {
	for _, name := range []string{"max", "min"} {
		for _, args := range []string{"n", "n 1", "1 n", "n 1 2", "1 n 2", "1 2 n", "1 n 2 n", "n (/ 1 0)", "(/ 1 0) n", "n (/ -1 0)", "(/ -1 0) n"} {
			expr := `(let ((n (/ 0.0 0.0))) (` + name + ` ` + args + `))`
			elpstest.RunTestSuite(t, elpstest.TestSuite{
				{name + " " + args, elpstest.TestSequence{{expr, `NaN`, ""}}},
			})
		}
		elpstest.RunTestSuite(t, elpstest.TestSuite{
			{name + " controls", elpstest.TestSequence{
				{`(int? (` + name + ` 1 2 3))`, `true`, ""},
				{`(handler-bind ((error (lambda (&rest _) 'invalid))) (` + name + ` (/ 0.0 0.0) "bad"))`, `'invalid`, ""},
				{`(handler-bind ((error (lambda (&rest _) 'invalid))) (` + name + ` "bad" (/ 0.0 0.0)))`, `'invalid`, ""},
			}},
		})
	}
}
