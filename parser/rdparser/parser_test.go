// Copyright © 2018 The ELPS authors

package rdparser

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
)

func TestComments(t *testing.T) {
	tests := []struct {
		source string
		output string
	}{
		{`(1 2 3) ; A comment`, `(1 2 3)`},
		{`	; A comment
			(1 "abc" '(x y z))`, `(1 "abc" '(x y z))`},
		{`(1 "abc" ; A comment
			'(x y z))`, `(1 "abc" '(x y z))`},
		{`(1 "abc" ; A comment
			)`, `(1 "abc")`},
	}

	for i, test := range tests {
		name := fmt.Sprintf("test%d", i)
		p := New(token.NewScanner(name, strings.NewReader(test.source)))
		exprs, err := p.ParseProgram()
		if err != nil {
			t.Errorf("test %d: parse error: %v", i, err)
			continue
		}
		for _, expr := range exprs {
			t.Log(expr)
		}
		if len(exprs) != 1 {
			t.Errorf("test %d: parsed %d expressions", i, len(exprs))
			continue
		}
		if exprs[0].String() != test.output {
			t.Errorf("test %d: expected output: %s", i, test.output)
		}
	}
}

func TestParser(t *testing.T) {
	tests := []struct {
		source string
		output string
	}{
		{`0`, `0`},
		{`12`, `12`},
		{`0.3`, `0.3`},
		{`-1`, `-1`},
		{`abc`, `abc`},
		{`abc?`, `abc?`},
		{`xyz:abc?`, `xyz:abc?`},
		{`x`, `x`},
		{`'xyz`, `'xyz`},
		{`"xyz"`, `"xyz"`},
		{`"x\nyz"`, `"x\nyz"`},
		{`"x\tyz"`, `"x\tyz"`},
		{`"x	yz"`, `"x\tyz"`},
		{`""`, `""`},
		{`""""""`, `""`},
		{`"""\n"""`, `"\\n"`},
		{`()`, `()`},
		{`'()`, `'()`},
		{`(1 2 3)`, `(1 2 3)`},
		{`(1 "abc" '(x y z))`, `(1 "abc" '(x y z))`},
		{`(1 "abc" [x y z])`, `(1 "abc" '(x y z))`},
		{`(abc :def)`, `(abc :def)`},
		{`#^"abc"`, `(lisp:expr "abc")`},
		{`#^'%`, `(lisp:expr '%)`},
		{`#^%1`, `(lisp:expr %1)`},
		{`#^"abc"`, `(lisp:expr "abc")`},
		{`#^12.25`, `(lisp:expr 12.25)`},
		{`#^()`, `(lisp:expr ())`},
		{`#^(cons %1 %&rest)`, `(lisp:expr (cons %1 %&rest))`},
		{`#'myfun`, `(lisp:function myfun)`},
		{`#'mypkg:myfun`, `(lisp:function mypkg:myfun)`},
	}

	for i, test := range tests {
		name := fmt.Sprintf("test%d", i)
		t.Log(name)
		s := token.NewScanner(name, strings.NewReader(test.source))
		p := New(s)
		exprs, err := p.ParseProgram()
		if err != nil {
			t.Errorf("test %d: parse error: %v", i, err)
			continue
		}
		for _, expr := range exprs {
			t.Log(expr)
		}
		if len(exprs) != 1 {
			t.Errorf("test %d: parsed %d expressions", i, len(exprs))
			continue
		}
		testLValLocation(t, exprs[0])
		if exprs[0].String() != test.output {
			t.Errorf("test %d: expected output: %s", i, test.output)
		}
	}
}

func testLValLocation(t *testing.T, v *lisp.LVal) {
	if v.Source == nil {
		t.Errorf("value missing source location: %v", v)
	}
	for _, v := range v.Cells {
		testLValLocation(t, v)
	}
}

func TestErrors(t *testing.T) {
	tests := []struct {
		source string
		errmsg string
	}{
		{`(1 2 3`, `test0:1:1: unmatched-syntax: unclosed ( opened at test0:1:1`},
		{`(1 2 3)
		0
		#xABC
		#xabc
		#o123
		#o9
`, `test1:6:5: invalid-octal-literal: invalid octal literal character: '9'`},
		{`(1 2 3)
		0
		#xABC
		#xDEADBEEG
		#o123
		#o9
`, `test2:4:5: invalid-hex-literal: invalid hexidecimal literal character: 'G'`},
		{`(1 2 3)
		134.
		"abc"`, `test3:2:3: scan-error: invalid floating point literal starting: 134.`},
		{`#!/usr/bin/env elps
		(1 2 3)
		0
		#xABC
		#!/usr/bin/env foo
		#o123
		#o9
`, `test4:5:3: parse-error: unexpected token: #!`},
		// Bracket mismatch errors
		{`(1 2 3]`, `test5:1:7: mismatched-syntax: expected ) to close ( opened at test5:1:1, but found ]`},
		{`[1 2 3)`, `test6:1:7: mismatched-syntax: expected ] to close [ opened at test6:1:1, but found )`},
		{`[1 2 3`, `test7:1:1: unmatched-syntax: unclosed [ opened at test7:1:1`},
		{`(let ([x 1) y)`, `test8:1:11: mismatched-syntax: expected ] to close [ opened at test8:1:7, but found )`},
		// Empty bracket mismatch
		{`(]`, `test9:1:2: mismatched-syntax: expected ) to close ( opened at test9:1:1, but found ]`},
		// Multi-line unclosed bracket points to opening location
		{`(foo
bar
baz`, `test10:1:1: unmatched-syntax: unclosed ( opened at test10:1:1`},
		// Multi-line mismatch reports both locations across lines
		{`(foo
bar
baz]`, `test11:3:4: mismatched-syntax: expected ) to close ( opened at test11:1:1, but found ]`},
		// Nested: innermost unclosed bracket is reported
		{`(((`, `test12:1:3: unmatched-syntax: unclosed ( opened at test12:1:3`},
		// Second expression has the mismatch
		{`(foo) (bar]`, `test13:1:11: mismatched-syntax: expected ) to close ( opened at test13:1:7, but found ]`},
	}

	for i, test := range tests {
		name := fmt.Sprintf("test%d", i)
		p := New(token.NewScanner(name, strings.NewReader(test.source)))
		_, err := p.ParseProgram()
		if err == nil {
			t.Errorf("test %d: did not produce an error", i)
			continue
		}
		msg := err.Error()
		assert.Equal(t, test.errmsg, msg)
	}
}
