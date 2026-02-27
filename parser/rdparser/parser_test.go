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

func TestEndPos_Float(t *testing.T) {
	v := parseOne(t, "3.14")
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 5, v.Source.EndCol) // "3.14" is 4 chars, end col is 5
}

func TestEndPos_ExprPrefix(t *testing.T) {
	// #^(+ 1 2) — the expr prefix wrapper gets Source from tokenLVal after
	// inner expression is parsed, so Col reflects the closing bracket position.
	v := parseOne(t, "#^(+ 1 2)")
	assert.Equal(t, 1, v.Source.Line)
	assert.Equal(t, 9, v.Source.Col)     // from ")" at col 9 (last consumed token)
	assert.Equal(t, 1, v.Source.EndLine)
	assert.Equal(t, 10, v.Source.EndCol) // inherited from inner expr: ")" at col 9, EndCol is 10
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

// --- Parse error conditions ---

func parseErrorCondition(t *testing.T, source string) string {
	t.Helper()
	p := New(token.NewScanner("test", strings.NewReader(source)))
	_, err := p.ParseProgram()
	if err == nil {
		t.Fatal("expected parse error")
	}
	ev, ok := err.(*lisp.ErrorVal)
	if !ok {
		t.Fatalf("expected *ErrorVal, got %T: %v", err, err)
	}
	return ev.Condition()
}

func TestCondition_UnmatchedSyntax(t *testing.T) {
	assert.Equal(t, lisp.CondUnmatchedSyntax, parseErrorCondition(t, "(foo"))
}

func TestCondition_MismatchedSyntax(t *testing.T) {
	assert.Equal(t, lisp.CondMismatchedSyntax, parseErrorCondition(t, "(foo]"))
}

func TestCondition_MismatchedSyntax_Bracket(t *testing.T) {
	assert.Equal(t, lisp.CondMismatchedSyntax, parseErrorCondition(t, "[foo)"))
}

func TestCondition_ScanError(t *testing.T) {
	assert.Equal(t, lisp.CondScanError, parseErrorCondition(t, "134."))
}

func TestCondition_InvalidOctalLiteral(t *testing.T) {
	assert.Equal(t, lisp.CondInvalidOctalLiteral, parseErrorCondition(t, "#o9"))
}

func TestCondition_InvalidHexLiteral(t *testing.T) {
	assert.Equal(t, lisp.CondInvalidHexLiteral, parseErrorCondition(t, "#xG"))
}

func TestCondition_ParseError(t *testing.T) {
	assert.Equal(t, lisp.CondParseError, parseErrorCondition(t, "#!/usr/bin/env elps\n#!/usr/bin/env foo\n"))
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

// --- Fault-tolerant parsing ---

func TestFaultTolerant_NoErrors(t *testing.T) {
	t.Parallel()
	source := "(a 1)\n(b 2)\n(c 3)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Empty(t, result.Errors)
	assert.Len(t, result.Exprs, 3)
	// Should produce identical output to ParseProgram.
	p2 := New(token.NewScanner("test", strings.NewReader(source)))
	exprs, err := p2.ParseProgram()
	assert.NoError(t, err)
	assert.Len(t, exprs, len(result.Exprs))
	for i := range exprs {
		assert.Equal(t, exprs[i].String(), result.Exprs[i].String())
	}
}

func TestFaultTolerant_ErrorInMiddle(t *testing.T) {
	t.Parallel()
	// Mismatched bracket terminates the broken expr, allowing recovery.
	source := "(a 1)\n(broken]\n(b 2)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1, "should have one error for the mismatched bracket")
	ev, ok := result.Errors[0].(*lisp.ErrorVal)
	assert.True(t, ok, "error should be *lisp.ErrorVal")
	assert.Equal(t, lisp.CondMismatchedSyntax, ev.Condition())
	assert.Len(t, result.Exprs, 2, "should recover (a 1) and (b 2)")
	assert.Equal(t, "(a 1)", result.Exprs[0].String())
	assert.Equal(t, "(b 2)", result.Exprs[1].String())
}

func TestFaultTolerant_ErrorAtStart(t *testing.T) {
	t.Parallel()
	// Mismatched bracket at start, valid expression after.
	source := "(broken]\n(a 1)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1)
	assert.Len(t, result.Exprs, 1, "should recover (a 1)")
	assert.Equal(t, "(a 1)", result.Exprs[0].String())
}

func TestFaultTolerant_ErrorAtEnd(t *testing.T) {
	t.Parallel()
	// Unclosed bracket at end — only the prior valid expression is recovered.
	source := "(a 1)\n(broken"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1)
	ev, ok := result.Errors[0].(*lisp.ErrorVal)
	assert.True(t, ok, "error should be *lisp.ErrorVal")
	assert.Equal(t, lisp.CondUnmatchedSyntax, ev.Condition())
	assert.Len(t, result.Exprs, 1, "should recover (a 1)")
	assert.Equal(t, "(a 1)", result.Exprs[0].String())
}

func TestFaultTolerant_StrayCloser(t *testing.T) {
	t.Parallel()
	source := ") (a 1)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1, "stray ) should produce exactly one error")
	assert.Len(t, result.Exprs, 1, "should recover (a 1)")
	assert.Equal(t, "(a 1)", result.Exprs[0].String())
}

func TestFaultTolerant_MismatchedBrackets(t *testing.T) {
	t.Parallel()
	source := "(foo] (bar)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1)
	assert.Len(t, result.Exprs, 1, "should recover (bar)")
	assert.Equal(t, "(bar)", result.Exprs[0].String())
}

func TestFaultTolerant_MultipleErrors(t *testing.T) {
	t.Parallel()
	// Two mismatched brackets with valid expressions between and after.
	source := "(a 1)\n(broken1]\n(b 2)\n(broken2]\n(c 3)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 2, "should collect both errors")
	assert.Len(t, result.Exprs, 3, "should recover (a 1), (b 2), and (c 3)")
	assert.Equal(t, "(a 1)", result.Exprs[0].String())
	assert.Equal(t, "(b 2)", result.Exprs[1].String())
	assert.Equal(t, "(c 3)", result.Exprs[2].String())
}

func TestFaultTolerant_OnlyErrors(t *testing.T) {
	t.Parallel()
	// Two stray closers with nothing valid.
	source := ") ]"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1, "stray ) is consumed by parser, ] is consumed by skip")
	assert.Empty(t, result.Exprs, "no valid expressions to recover")
}

func TestFaultTolerant_EmptyInput(t *testing.T) {
	t.Parallel()
	p := New(token.NewScanner("test", strings.NewReader("")))
	result := p.ParseProgramFaultTolerant()
	assert.Empty(t, result.Errors)
	assert.Empty(t, result.Exprs)
}

func TestFaultTolerant_ErrorPositions(t *testing.T) {
	t.Parallel()
	// Mismatched bracket on line 2: "(broken]" at col 8.
	source := "(ok 1)\n(broken]\n(ok 2)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1)
	ev, ok := result.Errors[0].(*lisp.ErrorVal)
	assert.True(t, ok, "error should be *lisp.ErrorVal")
	assert.NotNil(t, ev.Source, "error should have source location")
	assert.Equal(t, 2, ev.Source.Line, "error should be on line 2")
	assert.Equal(t, lisp.CondMismatchedSyntax, ev.Condition())
}

func TestFaultTolerant_ErrorLimit(t *testing.T) {
	t.Parallel()
	// Generate more than maxRecoveryErrors mismatched bracket expressions.
	// Each (x] produces one error; skip finds the next ( to continue.
	var parts []string
	for i := range maxRecoveryErrors + 10 {
		parts = append(parts, fmt.Sprintf("(err%d]", i))
	}
	parts = append(parts, "(ok 1)")
	source := strings.Join(parts, "\n")
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, maxRecoveryErrors, "errors should be capped at maxRecoveryErrors")
	assert.Empty(t, result.Exprs, "no valid expressions when limit is hit before (ok 1)")
}

func TestFaultTolerant_UnclosedBracketConsumesFollowing(t *testing.T) {
	t.Parallel()
	// An unclosed bracket at EOF consumes subsequent expressions because the
	// parser greedily reads children. This is expected behavior — only the
	// expressions before the unclosed bracket are recovered.
	source := "(a 1)\n(broken\n(b 2)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1)
	assert.Len(t, result.Exprs, 1, "(a 1) is recovered; (b 2) was consumed by (broken")
	assert.Equal(t, "(a 1)", result.Exprs[0].String())
}

func TestFaultTolerant_ScanError(t *testing.T) {
	t.Parallel()
	// A scan error (invalid float) doesn't consume following expressions.
	source := "(a 1)\n134.\n(b 2)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1)
	assert.Len(t, result.Exprs, 2, "should recover (a 1) and (b 2)")
	assert.Equal(t, "(a 1)", result.Exprs[0].String())
	assert.Equal(t, "(b 2)", result.Exprs[1].String())
}

func TestFaultTolerant_BareTokenRecovery(t *testing.T) {
	t.Parallel()
	// After a bracket error, recovery finds a bare symbol at top level.
	source := "(err] my-symbol (ok 1)"
	p := New(token.NewScanner("test", strings.NewReader(source)))
	result := p.ParseProgramFaultTolerant()
	assert.Len(t, result.Errors, 1)
	assert.Len(t, result.Exprs, 2, "should recover my-symbol and (ok 1)")
	assert.Equal(t, "my-symbol", result.Exprs[0].String())
	assert.Equal(t, "(ok 1)", result.Exprs[1].String())
}

func TestParseDepthLimit(t *testing.T) {
	t.Parallel()

	t.Run("exceeds limit by one", func(t *testing.T) {
		t.Parallel()
		// N parens = N calls to ParseExpression.  The check triggers when
		// depth > maxDepth, so maxDepth+1 parens is the first to fail.
		depth := DefaultMaxParseDepth + 1
		input := strings.Repeat("(", depth) + strings.Repeat(")", depth)
		p := New(token.NewScanner("deep", strings.NewReader(input)))
		_, err := p.ParseProgram()
		if err == nil {
			t.Fatal("expected parse error for deeply nested input")
		}
		assert.Contains(t, err.Error(), fmt.Sprintf("expression nesting exceeds maximum depth (%d)", DefaultMaxParseDepth))
	})

	t.Run("at exact limit succeeds", func(t *testing.T) {
		t.Parallel()
		// N parens = depth N.  The check is depth > maxDepth, so exactly
		// maxDepth parens is the deepest that succeeds.
		depth := DefaultMaxParseDepth
		input := strings.Repeat("(", depth) + strings.Repeat(")", depth)
		p := New(token.NewScanner("ok", strings.NewReader(input)))
		exprs, err := p.ParseProgram()
		if err != nil {
			t.Fatalf("unexpected error at depth %d parens: %v", depth, err)
		}
		assert.Len(t, exprs, 1)
	})

	t.Run("bracket list nesting exceeds limit", func(t *testing.T) {
		t.Parallel()
		depth := DefaultMaxParseDepth + 1
		input := strings.Repeat("[", depth) + strings.Repeat("]", depth)
		p := New(token.NewScanner("deep-bracket", strings.NewReader(input)))
		_, err := p.ParseProgram()
		if err == nil {
			t.Fatal("expected parse error for deeply nested bracket lists")
		}
		assert.Contains(t, err.Error(), fmt.Sprintf("expression nesting exceeds maximum depth (%d)", DefaultMaxParseDepth))
	})
}
