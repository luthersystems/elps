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

// --- End positions ---

func parseOne(t *testing.T, source string) *lisp.LVal {
	t.Helper()
	p := New(token.NewScanner("test", strings.NewReader(source)))
	exprs, err := p.ParseProgram()
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(exprs) != 1 {
		t.Fatalf("expected 1 expression, got %d", len(exprs))
	}
	return exprs[0]
}

func TestEndPos_Symbol(t *testing.T) {
	v := parseOne(t, "foo")
	assert.Equal(t, 1, v.Source.Line)
	assert.Equal(t, 1, v.Source.Col)
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 4, v.Source.EndCol)
}

func TestEndPos_Int(t *testing.T) {
	v := parseOne(t, "42")
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 3, v.Source.EndCol) // "42" is 2 chars, end col is 3
}

func TestEndPos_String(t *testing.T) {
	v := parseOne(t, `"hello"`)
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 8, v.Source.EndCol) // 7 chars including quotes, end col is 8
}

func TestEndPos_SExpr(t *testing.T) {
	v := parseOne(t, "(+ 1 2)")
	assert.Equal(t, 1, v.Source.Line)
	assert.Equal(t, 1, v.Source.Col)
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 8, v.Source.EndCol) // 7 chars, end col is 8
}

func TestEndPos_SExpr_MultiLine(t *testing.T) {
	v := parseOne(t, "(foo\n  bar)")
	assert.Equal(t, 1, v.Source.Line)
	assert.Equal(t, 1, v.Source.Col)
	assert.Equal(t, 2, v.Source.EndLine)
	assert.Equal(t, 7, v.Source.EndCol) // ")" is at col 6, EndCol is 7
}

func TestEndPos_BracketList(t *testing.T) {
	v := parseOne(t, "[a b c]")
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 8, v.Source.EndCol)
}

func TestEndPos_Nested(t *testing.T) {
	// ((a b) c)
	v := parseOne(t, "((a b) c)")
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 10, v.Source.EndCol) // 9 chars, end col is 10
	// Inner s-expr (a b)
	inner := v.Cells[0]
	assert.Equal(t, 1, inner.Source.EndLine)
	assert.Equal(t, 7, inner.Source.EndCol) // "(a b)" is 5 chars starting at col 2, end col is 7
}

func TestEndPos_Quote(t *testing.T) {
	// 'foo — the quote wrapper gets its Source from the inner expression,
	// so Source.Col is 2 (start of "foo"), and EndCol is from inner too.
	v := parseOne(t, "'foo")
	assert.Equal(t, 1, v.Source.Line)
	assert.Equal(t, 2, v.Source.Col)     // from inner "foo" token
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 5, v.Source.EndCol)  // "foo" ends at col 4, EndCol is 5
}

func TestEndPos_FunRef(t *testing.T) {
	// #'myfun — the fun-ref wrapper gets Source from inner "myfun" token.
	v := parseOne(t, "#'myfun")
	assert.Equal(t, 1, v.Source.Line)
	assert.Equal(t, 3, v.Source.Col)     // "myfun" starts at col 3
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 8, v.Source.EndCol)  // "myfun" ends at col 7, EndCol is 8
}

func TestEndPos_Empty(t *testing.T) {
	v := parseOne(t, "()")
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 3, v.Source.EndCol)
}

func TestEndPos_TokenEnd(t *testing.T) {
	// Test the TokenEnd helper directly
	tok := &token.Token{
		Text:   "hello",
		Source: &token.Location{Line: 1, Col: 1, Pos: 0},
	}
	endLine, endCol, endPos := token.TokenEnd(tok)
	assert.Equal(t, 1, endLine)
	assert.Equal(t, 6, endCol) // 5 chars, end col is 6
	assert.Equal(t, 5, endPos)
}

func TestEndPos_TokenEnd_MultiLine(t *testing.T) {
	tok := &token.Token{
		Text:   "ab\ncd",
		Source: &token.Location{Line: 1, Col: 1, Pos: 0},
	}
	endLine, endCol, endPos := token.TokenEnd(tok)
	assert.Equal(t, 2, endLine)
	assert.Equal(t, 3, endCol)
	assert.Equal(t, 5, endPos)
}

func TestEndPos_TokenEnd_NilToken(t *testing.T) {
	endLine, endCol, endPos := token.TokenEnd(nil)
	assert.Equal(t, 0, endLine)
	assert.Equal(t, 0, endCol)
	assert.Equal(t, 0, endPos)
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
