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
		assert.Equal(t, test.output, exprs[0].String(), "test %d", i)
	}
}

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
		{`(1 2 3`, `test0:1: unmatched-syntax: unmatched (`},
		{`(1 2 3)
		0
		#xABC
		#xabc
		#o123
		#o9
`, `test1:6: invalid-o-literal: invalid octal literal character: '9'`},
		{`(1 2 3)
		0
		#xABC
		#xDEADBEEG
		#o123
		#o9
`, `test2:4: invalid-x-literal: invalid hexidecimal literal character: 'G'`},
		{`(1 2 3)
		134.
		"abc"`, `test3:2: scan-error: invalid floating point literal starting: 134.`},
		{`#!/usr/bin/env elps
		(1 2 3)
		0
		#xABC
		#!/usr/bin/env foo
		#o123
		#o9
`, `test4:5: parse-error: unexpected token: #!`},
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
