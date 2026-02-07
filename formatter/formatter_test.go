// Copyright © 2024 The ELPS authors

package formatter

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type formatTest struct {
	name     string
	input    string
	expected string
	config   *Config
}

func runFormatTests(t *testing.T, tests []formatTest) {
	t.Helper()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cfg := tt.config
			got, err := Format([]byte(tt.input), cfg)
			require.NoError(t, err, "Format failed")
			assert.Equal(t, tt.expected, string(got), "formatted output mismatch")

			// Idempotency: formatting the output again should produce identical output
			got2, err := Format(got, cfg)
			require.NoError(t, err, "Format (idempotency) failed")
			assert.Equal(t, string(got), string(got2), "not idempotent")
		})
	}
}

// roundTripEqual parses two sources with the standard parser (non-formatting)
// and compares the resulting ASTs structurally.
func roundTripEqual(t *testing.T, original, formatted string) {
	t.Helper()
	parse := func(src string) string {
		s := token.NewScanner("test", bytes.NewReader([]byte(src)))
		p := rdparser.New(s)
		var parts []string
		for {
			expr, err := p.Parse()
			if err == io.EOF {
				break
			}
			require.NoError(t, err)
			parts = append(parts, expr.String())
		}
		return joinParts(parts)
	}
	assert.Equal(t, parse(original), parse(formatted), "AST mismatch after round-trip")
}

func joinParts(parts []string) string {
	result := ""
	for i, p := range parts {
		if i > 0 {
			result += " "
		}
		result += p
	}
	return result
}

// --- Basic indentation tests ---

func TestIndentDefaultAlign(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "simple function call",
			input:    "(foo bar baz)",
			expected: "(foo bar baz)\n",
		},
		{
			name:     "already formatted",
			input:    "(foo bar baz)\n",
			expected: "(foo bar baz)\n",
		},
		{
			name:  "multiline first-arg align",
			input: "(foo bar\nbaz)",
			expected: "(foo bar\n" +
				"     baz)\n",
		},
		{
			name:  "extra spaces preserved for column alignment",
			input: "(foo   bar   baz)",
			expected: "(foo   bar   baz)\n",
		},
	})
}

func TestIndentDefun(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "defun body indent",
			input: "(defun square (x)\n(* x x))",
			expected: "(defun square (x)\n" +
				"  (* x x))\n",
		},
		{
			name: "defun single line stays single",
			input: "(defun id (x) x)",
			expected: "(defun id (x) x)\n",
		},
		{
			name: "defmacro body indent",
			input: "(defmacro my-macro (x)\n(list x))",
			expected: "(defmacro my-macro (x)\n" +
				"  (list x))\n",
		},
	})
}

func TestIndentLambda(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "lambda body indent",
			input: "(lambda (x)\n(+ x 1))",
			expected: "(lambda (x)\n" +
				"  (+ x 1))\n",
		},
	})
}

func TestIndentLet(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "let body indent",
			input: "(let ((x 1))\n(+ x 2))",
			expected: "(let ((x 1))\n" +
				"  (+ x 2))\n",
		},
		{
			name: "let with brackets",
			input: "(let ([x 1] [y 2])\n(+ x y))",
			expected: "(let ([x 1] [y 2])\n" +
				"  (+ x y))\n",
		},
	})
}

func TestIndentIf(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "if then else",
			input: "(if (> x 0)\ntrue\nfalse)",
			expected: "(if (> x 0)\n" +
				"  true\n" +
				"  false)\n",
		},
	})
}

func TestIndentProgn(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "progn body",
			input: "(progn\n(foo)\n(bar))",
			expected: "(progn\n" +
				"  (foo)\n" +
				"  (bar))\n",
		},
	})
}

func TestIndentCond(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "cond args on next line use body indent",
			input: "(cond\n((= x 1) \"one\")\n(:else \"other\"))",
			expected: "(cond\n" +
				"  ((= x 1) \"one\")\n" +
				"  (:else \"other\"))\n",
		},
		{
			name: "cond first-arg on same line uses alignment",
			input: "(cond ((= x 1) \"one\")\n((= x 2) \"two\"))",
			expected: "(cond ((= x 1) \"one\")\n" +
				"      ((= x 2) \"two\"))\n",
		},
	})
}

func TestIndentOrAnd(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:  "or args on next line use body indent",
			input: "(or\na\nb)",
			expected: "(or\n" +
				"  a\n" +
				"  b)\n",
		},
		{
			name:  "and args on next line use body indent",
			input: "(and\na\nb)",
			expected: "(and\n" +
				"  a\n" +
				"  b)\n",
		},
		{
			name:  "or first-arg on same line uses alignment",
			input: "(or (not a)\n(not b))",
			expected: "(or (not a)\n" +
				"    (not b))\n",
		},
		{
			name:  "and first-arg on same line uses alignment",
			input: "(and (not a)\n(not b))",
			expected: "(and (not a)\n" +
				"     (not b))\n",
		},
	})
}

func TestIndentTest(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "test special",
			input: "(test \"my test\"\n(assert= 1 1))",
			expected: "(test \"my test\"\n" +
				"  (assert= 1 1))\n",
		},
		{
			name: "test-let special 2 header args",
			input: "(test-let \"my test\" ((x 1))\n(assert= 1 x))",
			expected: "(test-let \"my test\" ((x 1))\n" +
				"  (assert= 1 x))\n",
		},
	})
}

func TestIndentThreading(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "thread-first aligns with first arg",
			input: "(thread-first val\n(foo)\n(bar))",
			expected: "(thread-first val\n" +
				"              (foo)\n" +
				"              (bar))\n",
		},
		{
			name: "thread-first wraps to body indent",
			input: "(thread-first\nval\n(foo)\n(bar))",
			expected: "(thread-first\n" +
				"  val\n" +
				"  (foo)\n" +
				"  (bar))\n",
		},
	})
}

func TestIndentDo(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "do special",
			input: "(do ((i 0 (+ i 1)))\n(body))",
			expected: "(do ((i 0 (+ i 1)))\n" +
				"  (body))\n",
		},
	})
}

func TestIndentIgnoreErrors(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "ignore-errors body",
			input: "(ignore-errors\n(risky-op))",
			expected: "(ignore-errors\n" +
				"  (risky-op))\n",
		},
	})
}

// --- Comment tests ---

func TestCommentLeadingTopLevel(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "comment before top-level form",
			input: "; hello\n(foo)",
			expected: "; hello\n(foo)\n",
		},
	})
}

func TestCommentBetweenForms(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "comment between two forms",
			input: "(foo)\n\n; separator\n(bar)",
			expected: "(foo)\n\n; separator\n(bar)\n",
		},
	})
}

func TestCommentInsideSExpr(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "comment inside s-expr",
			input: "(defun foo (x)\n  ; compute\n  (+ x 1))",
			expected: "(defun foo (x)\n" +
				"  ; compute\n" +
				"  (+ x 1))\n",
		},
	})
}

func TestCommentInsideList(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "comment inside brackets",
			input: "[a\n; comment\nb]",
			expected: "[a\n" +
				" ; comment\n" +
				" b]\n",
		},
	})
}

func TestCommentMultipleConsecutive(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "multiple consecutive comments",
			input: "; line 1\n; line 2\n(foo)",
			expected: "; line 1\n; line 2\n(foo)\n",
		},
	})
}

func TestCommentTrailing(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "trailing comment",
			input:    "(foo)\n; end",
			expected: "(foo)\n; end\n",
		},
	})
}

// Issue #44: comments detached (blank line) vs attached (no blank line)
// must be preserved as distinct.
func TestCommentAttachedVsDetached(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "detached comment has blank line preserved",
			input:    ";; hello\n\n(world)\n",
			expected: ";; hello\n\n(world)\n",
		},
		{
			name:     "attached comment has no blank line",
			input:    ";; hello\n(world)\n",
			expected: ";; hello\n(world)\n",
		},
		{
			name: "section header detached then attached comment",
			input: ";; section header\n\n;; attached comment\n(defun foo () 1)\n",
			expected: ";; section header\n\n;; attached comment\n(defun foo () 1)\n",
		},
		{
			name: "two comment blocks separated by blank line",
			input: ";; block 1\n;; block 1 cont\n\n;; block 2\n(foo)\n",
			expected: ";; block 1\n;; block 1 cont\n\n;; block 2\n(foo)\n",
		},
		{
			name: "detached comments inside function body",
			input: "(defun setup ()\n  ;; section one\n\n  (init)\n\n  ;; section two\n\n  (run))",
			expected: "(defun setup ()\n" +
				"  ;; section one\n\n" +
				"  (init)\n\n" +
				"  ;; section two\n\n" +
				"  (run))\n",
		},
	})
}

func TestCommentOnlyFile(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "comment only file",
			input:    "; just a comment\n",
			expected: "; just a comment\n",
		},
	})
}

func TestCommentPreservesText(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "comment text preserved",
			input:    ";; double semicolon comment\n(foo)",
			expected: ";; double semicolon comment\n(foo)\n",
		},
	})
}

// --- Bracket type tests ---

func TestBracketParens(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "parens preserved",
			input:    "(foo bar)",
			expected: "(foo bar)\n",
		},
	})
}

func TestBracketBraces(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "brackets preserved",
			input:    "[a b c]",
			expected: "[a b c]\n",
		},
	})
}

func TestBracketNested(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "parens inside brackets",
			input:    "[(foo) (bar)]",
			expected: "[(foo) (bar)]\n",
		},
		{
			name:     "brackets inside parens",
			input:    "(let ([x 1]) x)",
			expected: "(let ([x 1]) x)\n",
		},
	})
}

func TestBracketEmpty(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "empty parens",
			input:    "()",
			expected: "()\n",
		},
		{
			name:     "empty brackets",
			input:    "[]",
			expected: "[]\n",
		},
	})
}

// --- Data list indent tests ---

func TestDataListIndent(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "let* binding pairs align inside bracket",
			input: "(let* ([x 1]\n[y 2]\n[z 3])\n(+ x y z))",
			expected: "(let* ([x 1]\n" +
				"       [y 2]\n" +
				"       [z 3])\n" +
				"  (+ x y z))\n",
		},
		{
			name: "cond with first-arg on same line aligns",
			input: "(cond ((= x 1) \"one\")\n((= x 2) \"two\"))",
			expected: "(cond ((= x 1) \"one\")\n" +
				"      ((= x 2) \"two\"))\n",
		},
		{
			name: "nested data list in let",
			input: "(let (([a 1] [b 2]))\n(+ a b))",
			expected: "(let (([a 1] [b 2]))\n" +
				"  (+ a b))\n",
		},
	})
}

// --- Prefix operator tests ---

func TestPrefixQuote(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "quoted symbol",
			input:    "'foo",
			expected: "'foo\n",
		},
		{
			name:     "quoted list",
			input:    "'(a b c)",
			expected: "'(a b c)\n",
		},
	})
}

func TestPrefixFunRef(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "function reference",
			input:    "#'my-func",
			expected: "#'my-func\n",
		},
	})
}

func TestPrefixUnbound(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "unbound expression",
			input:    "#^(x y)",
			expected: "#^(x y)\n",
		},
	})
}

// --- String literal tests ---

func TestStringSimple(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "simple string",
			input:    `"hello"`,
			expected: "\"hello\"\n",
		},
	})
}

func TestStringWithEscapes(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "string with escapes preserved",
			input:    `"hello\nworld"`,
			expected: "\"hello\\nworld\"\n",
		},
	})
}

func TestStringRaw(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "raw string preserved",
			input:    `"""raw string"""`,
			expected: "\"\"\"raw string\"\"\"\n",
		},
	})
}

func TestStringEmpty(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "empty string",
			input:    `""`,
			expected: "\"\"\n",
		},
	})
}

// --- Numeric literal tests ---

func TestNumericInt(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "integer",
			input:    "42",
			expected: "42\n",
		},
	})
}

func TestNumericHex(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "hex literal preserved",
			input:    "#xFF",
			expected: "#xFF\n",
		},
	})
}

func TestNumericOctal(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "octal literal preserved",
			input:    "#o77",
			expected: "#o77\n",
		},
	})
}

func TestNumericFloat(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "float",
			input:    "3.14",
			expected: "3.14\n",
		},
	})
}

func TestNumericFloatExp(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "float with exponent",
			input:    "1.0e10",
			expected: "1.0e10\n",
		},
	})
}

func TestNumericNegative(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "negative integer",
			input:    "-42",
			expected: "-42\n",
		},
	})
}

// --- Whitespace normalization tests ---

func TestWhitespaceSpacingPreservation(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "multiple spaces preserved for column alignment",
			input:    "(foo   bar   baz)",
			expected: "(foo   bar   baz)\n",
		},
		{
			name: "column-aligned sorted-map keys preserved",
			input: "(sorted-map\n" +
				"  \"short\"         \"val1\"\n" +
				"  \"longer-key\"    \"val2\"\n" +
				"  \"very-long-key\" \"val3\")",
			expected: "(sorted-map\n" +
				"  \"short\"         \"val1\"\n" +
				"  \"longer-key\"    \"val2\"\n" +
				"  \"very-long-key\" \"val3\")\n",
		},
		{
			name: "column-aligned sorted-map with quoted list values",
			input: "(sorted-map\n" +
				"  \"short\"      '(\"a\" \"b\")\n" +
				"  \"longer-key\" '(\"c\" \"d\"))",
			expected: "(sorted-map\n" +
				"  \"short\"      '(\"a\" \"b\")\n" +
				"  \"longer-key\" '(\"c\" \"d\"))\n",
		},
		{
			name: "column-aligned data table preserved",
			input: "(vector role \"permission/name\"   \"uuid-here\"  true  false)",
			expected: "(vector role \"permission/name\"   \"uuid-here\"  true  false)\n",
		},
		{
			name:     "single space between tokens is default",
			input:    "(foo bar baz)",
			expected: "(foo bar baz)\n",
		},
	})
}

func TestListFirstChildOnNewLine(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "bracket on its own line with children below",
			input: "(foo\n" +
				"  [\n" +
				"   [a \"1\"]\n" +
				"   [b \"2\"]])",
			expected: "(foo\n" +
				"  [\n" +
				"   [a \"1\"]\n" +
				"   [b \"2\"]])\n",
		},
		{
			name: "bracket on same line as first child",
			input: "(foo\n" +
				"  [[a \"1\"]\n" +
				"   [b \"2\"]])",
			expected: "(foo\n" +
				"  [[a \"1\"]\n" +
				"   [b \"2\"]])\n",
		},
		{
			name: "empty list with bracket style preserved",
			input: "(foo\n  [])",
			expected: "(foo\n  [])\n",
		},
	})
}

func TestDataListHeadOnNewLine(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "data list with head on new line",
			input: "(\n" +
				" [\n" +
				"  [a \"1\"]\n" +
				"  [b \"2\"]]\n" +
				" [\n" +
				"  [c \"3\"]]\n" +
				" )",
			expected: "(\n" +
				" [\n" +
				"  [a \"1\"]\n" +
				"  [b \"2\"]]\n" +
				" [\n" +
				"  [c \"3\"]]\n" +
				" )\n",
		},
		{
			name: "data list head on same line stays on same line",
			input: "([a 1]\n [b 2])",
			expected: "([a 1]\n [b 2])\n",
		},
	})
}

func TestClosingBracketOnNewLine(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "closing paren on its own line preserved",
			input: "(foo\n  bar\n  baz\n  )",
			expected: "(foo\n  bar\n  baz\n  )\n",
		},
		{
			name: "closing bracket on its own line preserved",
			input: "[a\n b\n c\n ]",
			expected: "[a\n b\n c\n ]\n",
		},
		{
			name: "closing paren on same line stays on same line",
			input: "(foo\n  bar\n  baz)",
			expected: "(foo\n  bar\n  baz)\n",
		},
	})
}

func TestWhitespaceAfterOpen(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "space after open paren removed",
			input:    "( foo bar)",
			expected: "(foo bar)\n",
		},
	})
}

func TestWhitespaceBeforeClose(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "space before close paren removed",
			input:    "(foo bar )",
			expected: "(foo bar)\n",
		},
	})
}

func TestWhitespaceTrailing(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "trailing whitespace removed",
			input: "(foo)   \n",
			expected: "(foo)\n",
		},
	})
}

// --- Blank line tests ---

func TestBlankLineSinglePreserved(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:  "single blank line preserved",
			input: "(foo)\n\n(bar)\n",
			expected: "(foo)\n\n(bar)\n",
		},
	})
}

func TestBlankLineMultipleCollapsed(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:  "multiple blank lines collapsed",
			input: "(foo)\n\n\n\n(bar)\n",
			expected: "(foo)\n\n(bar)\n",
		},
	})
}

func TestBlankLineNoLeading(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "no blank line at start of file",
			input:    "\n\n(foo)\n",
			expected: "(foo)\n",
		},
	})
}

func TestTrailingNewline(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "single trailing newline",
			input:    "(foo)\n\n\n",
			expected: "(foo)\n",
		},
	})
}

// --- Blank lines inside bodies ---

func TestBlankLinesInsideBody(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "blank line between forms in defun body",
			input: "(defun foo ()\n(bar)\n\n(baz))",
			expected: "(defun foo ()\n" +
				"  (bar)\n\n" +
				"  (baz))\n",
		},
		{
			name: "blank line between forms in progn",
			input: "(progn\n(a)\n\n(b))",
			expected: "(progn\n" +
				"  (a)\n\n" +
				"  (b))\n",
		},
	})
}

// --- IndentSpecial newline fallback ---

func TestIndentSpecialNewlineFallback(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "unless condition on same line keeps special",
			input: "(unless (> x 0)\n(do-something))",
			expected: "(unless (> x 0)\n" +
				"  (do-something))\n",
		},
		{
			name: "unless condition on next line uses body indent",
			input: "(unless\n(> x 0)\n(do-something))",
			expected: "(unless\n" +
				"  (> x 0)\n" +
				"  (do-something))\n",
		},
		{
			name: "if condition on next line uses body indent",
			input: "(if\ncondition\nthen\nelse)",
			expected: "(if\n" +
				"  condition\n" +
				"  then\n" +
				"  else)\n",
		},
	})
}

// --- Nested expression tests ---

func TestNestedDeep(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "deeply nested",
			input:    "(a (b (c (d (e f)))))",
			expected: "(a (b (c (d (e f)))))\n",
		},
	})
}

func TestNestedMixedRules(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "defun with let inside",
			input: "(defun foo (x)\n(let ((y 1))\n(+ x y)))",
			expected: "(defun foo (x)\n" +
				"  (let ((y 1))\n" +
				"    (+ x y)))\n",
		},
	})
}

// --- Edge case tests ---

func TestEdgeEmptyInput(t *testing.T) {
	got, err := Format([]byte(""), nil)
	require.NoError(t, err)
	assert.Equal(t, "", string(got))
}

func TestEdgeSingleAtom(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "single atom",
			input:    "foo",
			expected: "foo\n",
		},
	})
}

func TestEdgeSingleEmptyList(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "single empty list",
			input:    "()",
			expected: "()\n",
		},
	})
}

func TestEdgeLongSingleLine(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "long line stays on one line",
			input:    "(foo bar baz quux corge grault garply waldo fred plugh xyzzy thud)",
			expected: "(foo bar baz quux corge grault garply waldo fred plugh xyzzy thud)\n",
		},
	})
}

func TestEdgeKeywordArgs(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "keyword arguments",
			input:    `(load-string code :name "x")`,
			expected: "(load-string code :name \"x\")\n",
		},
	})
}

func TestEdgeSpecialSymbols(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "rest optional key in formals",
			input:    "(defun foo (a &optional b &rest c) a)",
			expected: "(defun foo (a &optional b &rest c) a)\n",
		},
	})
}

func TestEdgePackageQualified(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "package qualified symbols",
			input:    "(lisp:set x 1)",
			expected: "(lisp:set x 1)\n",
		},
	})
}

func TestEdgeIdempotency(t *testing.T) {
	// Already formatted code should be unchanged
	input := "(defun square (x)\n  (* x x))\n"
	got, err := Format([]byte(input), nil)
	require.NoError(t, err)
	assert.Equal(t, input, string(got))
}

// --- Config tests ---

func TestConfigCustomIndentSize(t *testing.T) {
	cfg := DefaultConfig()
	cfg.IndentSize = 4
	runFormatTests(t, []formatTest{
		{
			name: "4-space indent",
			input: "(defun foo (x)\n(+ x 1))",
			expected: "(defun foo (x)\n" +
				"    (+ x 1))\n",
			config: cfg,
		},
	})
}

func TestConfigCustomRule(t *testing.T) {
	cfg := DefaultConfig()
	cfg.Rules["my-form"] = &IndentRule{Style: IndentSpecial, HeaderArgs: 1}
	runFormatTests(t, []formatTest{
		{
			name: "custom rule",
			input: "(my-form header\nbody)",
			expected: "(my-form header\n" +
				"  body)\n",
			config: cfg,
		},
	})
}

// --- Round-trip tests ---

func TestRoundTrip(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{"simple call", "(foo bar baz)"},
		{"defun", "(defun square (x) (* x x))"},
		{"let", "(let ((x 1) (y 2)) (+ x y))"},
		{"quote", "'(a b c)"},
		{"nested", "(a (b (c d)))"},
		{"funref", "#'my-func"},
		{"negative", "-42"},
		{"float", "3.14"},
		{"string", `"hello world"`},
		{"empty list", "()"},
		{"brackets", "[a b c]"},
		{"package-qualified", "(lisp:set x 1)"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			formatted, err := Format([]byte(tt.input), nil)
			require.NoError(t, err)
			roundTripEqual(t, tt.input, string(formatted))
		})
	}
}

// --- Hashbang tests ---

func TestHashbang(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:     "hashbang preserved",
			input:    "#!/usr/bin/env elps\n(foo)",
			expected: "#!/usr/bin/env elps\n(foo)\n",
		},
	})
}

// --- Deftype test ---

func TestIndentDeftype(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "deftype body indent",
			input: "(deftype my-type (x)\n(validate x))",
			expected: "(deftype my-type (x)\n" +
				"  (validate x))\n",
		},
	})
}

// --- Flet/labels/macrolet tests ---

func TestIndentFlet(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "flet body indent",
			input: "(flet ((helper (x) (+ x 1)))\n(helper 5))",
			expected: "(flet ((helper (x) (+ x 1)))\n" +
				"  (helper 5))\n",
		},
	})
}

// --- Handler-bind test ---

func TestIndentHandlerBind(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "handler-bind body indent",
			input: "(handler-bind ((error (lambda (c) (handle c))))\n(risky-op))",
			expected: "(handler-bind ((error (lambda (c) (handle c))))\n" +
				"  (risky-op))\n",
		},
	})
}

// --- Dotimes test ---

func TestIndentDotimes(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "dotimes body indent",
			input: "(dotimes (i 10)\n(print i))",
			expected: "(dotimes (i 10)\n" +
				"  (print i))\n",
		},
	})
}

// --- When/unless tests ---

func TestIndentWhen(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "when body indent",
			input: "(when (> x 0)\n(do-something))",
			expected: "(when (> x 0)\n" +
				"  (do-something))\n",
		},
	})
}

func TestIndentUnless(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "unless body indent",
			input: "(unless (> x 0)\n(do-something))",
			expected: "(unless (> x 0)\n" +
				"  (do-something))\n",
		},
	})
}

// --- Customer-pattern tests (non-sensitive, derived from real-world code) ---

func TestPatternLetStarMultilineBindings(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "let* with multiline bracket bindings",
			input: `(let* ([name (get req "name")]
       [status (get req "status")]
       [id (get req "id")])
  (process name status id))`,
			expected: "(let* ([name (get req \"name\")]\n" +
				"       [status (get req \"status\")]\n" +
				"       [id (get req \"id\")])\n" +
				"  (process name status id))\n",
		},
	})
}

func TestPatternCondWithClauses(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "cond with first clause on same line",
			input: `(cond ((equal? op 'load) result)
      ((equal? op 'save) (save-data))
      (:else (error 'unknown-op)))`,
			expected: "(cond ((equal? op 'load) result)\n" +
				"      ((equal? op 'save) (save-data))\n" +
				"      (:else (error 'unknown-op)))\n",
		},
	})
}

func TestPatternSortedMapWrapped(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "sorted-map with args on next line uses body indent",
			input: "(set 'lookup (sorted-map\n\"key1\" 1\n\"key2\" 2\n\"key3\" 3))",
			expected: "(set 'lookup (sorted-map\n" +
				"               \"key1\" 1\n" +
				"               \"key2\" 2\n" +
				"               \"key3\" 3))\n",
		},
	})
}

func TestPatternHandlerBind(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "handler-bind with handler on same line",
			input: "(handler-bind ([err (lambda (c) (handle c))])\n(risky-op)\n(more-ops))",
			expected: "(handler-bind ([err (lambda (c) (handle c))])\n" +
				"  (risky-op)\n" +
				"  (more-ops))\n",
		},
	})
}

func TestPatternNestedLetLambda(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "nested let* with lambda and foldl",
			input: `(let* ([items (get data "items")]
       [result (foldl (lambda (acc item)
                        (let* ([id (get item "id")])
                          (assoc! acc id true)
                          acc))
                      (sorted-map)
                      items)])
  result)`,
			expected: "(let* ([items (get data \"items\")]\n" +
				"       [result (foldl (lambda (acc item)\n" +
				"                        (let* ([id (get item \"id\")])\n" +
				"                          (assoc! acc id true)\n" +
				"                          acc))\n" +
				"                      (sorted-map)\n" +
				"                      items)])\n" +
				"  result)\n",
		},
	})
}

func TestPatternThreadFirst(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "thread-first with multiple transformations",
			input: "(thread-first message\n(to-bytes)\n(compress)\n(encode)\n(to-string))",
			expected: "(thread-first message\n" +
				"              (to-bytes)\n" +
				"              (compress)\n" +
				"              (encode)\n" +
				"              (to-string))\n",
		},
	})
}

func TestPatternBlankLinesInBody(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "blank lines between body forms in defun",
			input: "(defun setup ()\n(init-db)\n\n(init-cache)\n\n(start-server))",
			expected: "(defun setup ()\n" +
				"  (init-db)\n\n" +
				"  (init-cache)\n\n" +
				"  (start-server))\n",
		},
	})
}

func TestPatternBlankLineAfterComment(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "blank line between comment and form preserved at top level",
			input: "; storage key format\n; prefix:id\n\n(set 'storage-prefix \"data\")",
			expected: "; storage key format\n; prefix:id\n\n(set 'storage-prefix \"data\")\n",
		},
		{
			name: "blank line after comment inside body",
			input: "(defun foo ()\n; setup\n\n(init)\n(run))",
			expected: "(defun foo ()\n" +
				"  ; setup\n\n" +
				"  (init)\n" +
				"  (run))\n",
		},
	})
}

func TestPatternAndOrAlignment(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "and with conditions on same line aligns",
			input: "(and (not passed)\n(not optional)\n(key? lookup id))",
			expected: "(and (not passed)\n" +
				"     (not optional)\n" +
				"     (key? lookup id))\n",
		},
		{
			name: "or with conditions on same line aligns",
			input: "(or (empty? status)\n(equal? status \"DISABLED\"))",
			expected: "(or (empty? status)\n" +
				"    (equal? status \"DISABLED\"))\n",
		},
	})
}

func TestPatternFirstArgWrapped(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "long function name with first arg wrapped uses body indent",
			input: "(assert-error\n'lisp:assert\nexamples)",
			expected: "(assert-error\n" +
				"  'lisp:assert\n" +
				"  examples)\n",
		},
		{
			name: "long function name with first arg on same line aligns",
			input: "(assert-error 'lisp:assert\nexamples)",
			expected: "(assert-error 'lisp:assert\n" +
				"              examples)\n",
		},
		{
			name: "user can avoid rightward drift by wrapping first arg",
			input: "(some-very-long-function-name\nfirst-arg\nsecond-arg\nthird-arg)",
			expected: "(some-very-long-function-name\n" +
				"  first-arg\n" +
				"  second-arg\n" +
				"  third-arg)\n",
		},
	})
}

func TestPatternDefunWithDocstring(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "defun with docstring preserves formatting",
			input: `(defun validate (data)
  "Validate the given data structure and return errors."
  (let* ([errors (check-rules data)])
    (when (not (empty? errors))
      (report-errors errors))))`,
			expected: "(defun validate (data)\n" +
				"  \"Validate the given data structure and return errors.\"\n" +
				"  (let* ([errors (check-rules data)])\n" +
				"    (when (not (empty? errors))\n" +
				"      (report-errors errors))))\n",
		},
		{
			name: "defun docstring on same line stays",
			input: "(defun id (x) \"Returns its argument unchanged.\" x)",
			expected: "(defun id (x) \"Returns its argument unchanged.\" x)\n",
		},
	})
}

func TestPatternUnlessWrapped(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "unless with long condition on same line",
			input: "(unless (nil? owner-id)\n(validate-owner owner-id))",
			expected: "(unless (nil? owner-id)\n" +
				"  (validate-owner owner-id))\n",
		},
		{
			name: "unless with condition wrapped to next line",
			input: "(unless\n(and (empty? required-by) (empty? required-from))\n(validate-requirement))",
			expected: "(unless\n" +
				"  (and (empty? required-by) (empty? required-from))\n" +
				"  (validate-requirement))\n",
		},
	})
}

// --- AST comparison tests ---
func TestInlineTrailingComment(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:  "trailing comment on binding",
			input: "(let* ([aud (first items)] ; grab first\n       [x 1])\n  body)",
			expected: "(let* ([aud (first items)] ; grab first\n       [x 1])\n  body)\n",
		},
		{
			name:  "trailing comment on last child",
			input: "(foo bar ; comment\n  baz)",
			expected: "(foo bar ; comment\n     baz)\n",
		},
		{
			name:     "trailing comment after closing brackets",
			input:    "(and (check-a)\n     (check-b)) ; always both\n\n(next-form)",
			expected: "(and (check-a)\n     (check-b)) ; always both\n\n(next-form)\n",
		},
		{
			name:     "double-semicolon trailing comment after ))",
			input:    "(set 'x (list \"abc\")) ;; High Commission\n\n(next)",
			expected: "(set 'x (list \"abc\")) ;; High Commission\n\n(next)\n",
		},
	})
}

func TestInnerTrailingComment(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name: "comment before closing bracket",
			input: "(let* ([x 1]\n       [y 2]\n       ; end bindings\n       )\n  body)",
			expected: "(let* ([x 1]\n       [y 2]\n       ; end bindings\n       )\n  body)\n",
		},
		{
			name: "comment before closing bracket in list",
			input: "[a\n b\n ; last item\n ]",
			expected: "[a\n b\n ; last item\n ]\n",
		},
		{
			name: "multiple inner trailing comments",
			input: "(progn\n  (do-a)\n  ; note 1\n  ; note 2\n  )",
			expected: "(progn\n  (do-a)\n  ; note 1\n  ; note 2\n  )\n",
		},
	})
}

func TestDefPrefixHeuristic(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:  "def-acre-route like defun",
			input: "(def-acre-route \"create_thing\" (obj)\n  (let* ([id (mk-uuid)])\n    (create! id)))",
			expected: "(def-acre-route \"create_thing\" (obj)\n  (let* ([id (mk-uuid)])\n    (create! id)))\n",
		},
		{
			name:  "def-case-verification with 2 header args",
			input: "(def-case-verification config bindings\n  body)",
			expected: "(def-case-verification config bindings\n  body)\n",
		},
		{
			name:  "defmacro header arg wraps to new line",
			input: "(defmacro assert-error\n  (err msg &rest expr)\n  (progn body))",
			expected: "(defmacro assert-error\n  (err msg &rest expr)\n  (progn body))\n",
		},
	})
}

func TestIndentSpecialHeaderArgWrap(t *testing.T) {
	runFormatTests(t, []formatTest{
		{
			name:  "unless condition on new line",
			input: "(unless\n  cond\n  body1\n  body2)",
			expected: "(unless\n  cond\n  body1\n  body2)\n",
		},
		{
			name:  "if condition on new line",
			input: "(if\n  cond\n  then\n  else)",
			expected: "(if\n  cond\n  then\n  else)\n",
		},
	})
}

// Verify that formatting preserves the semantic AST by parsing both the
// original and formatted code with the standard (non-formatting) parser
// and comparing the resulting AST serializations.

func TestASTPreservation(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{"let* with bracket bindings",
			"(let* ([x 1] [y 2] [z 3]) (+ x y z))"},
		{"nested cond",
			"(cond ((= x 1) \"one\") ((= x 2) \"two\") (:else \"other\"))"},
		{"defun with docstring",
			"(defun foo (x) \"docstring\" (+ x 1))"},
		{"handler-bind",
			"(handler-bind ([err (lambda (c) c)]) (risky))"},
		{"thread-first",
			"(thread-first val (foo) (bar) (baz))"},
		{"sorted-map literal",
			"(sorted-map \"a\" 1 \"b\" 2 \"c\" 3)"},
		{"quoted list",
			"'(a b c d)"},
		{"function ref",
			"#'my-func"},
		{"anon function",
			"#^(+ % 1)"},
		{"nested let/lambda/foldl",
			"(let* ([result (foldl (lambda (acc x) (+ acc x)) 0 items)]) result)"},
		{"mixed bracket types",
			"(let ([x 1]) (when (> x 0) (list [x (+ x 1)])))"},
		{"deeply nested with comments",
			"(defun foo (x)\n; doc\n(let ((y 1))\n; inner\n(+ x y)))"},
		{"multiple top-level forms",
			"(defun a () 1)\n\n(defun b () 2)\n\n(set 'c 3)"},
		{"negative numbers",
			"(list -1 -2.5 #xFF #o77)"},
		{"keyword args",
			"(load-string code :name \"test\" :verbose true)"},
		{"package qualified",
			"(lisp:set s:validate math:sqrt)"},
		{"empty forms",
			"(progn () [] (list))"},
		{"inline trailing comment",
			"(let* ([x 1] ; bind x\n       [y 2])\n  (+ x y))"},
		{"inner trailing comment",
			"(let* ([x 1]\n       ; end\n       )\n  x)"},
		{"def- prefix form",
			"(def-my-macro \"name\" (args)\n  body)"},
		{"column-aligned spacing",
			"(sorted-map\n  \"short\"         \"v1\"\n  \"longer-key\"    \"v2\")"},
		{"list with first child on new line",
			"(foo [\n  [a 1]\n  [b 2]])"},
		{"data list head on new line",
			"(\n [a 1]\n [b 2]\n )"},
		{"closing bracket on own line",
			"(foo\n  bar\n  baz\n  )"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			formatted, err := Format([]byte(tt.input), nil)
			require.NoError(t, err, "Format failed for: %s", tt.input)
			roundTripEqual(t, tt.input, string(formatted))
		})
	}
}

// --- Repository file round-trip tests ---
// Format every .lisp file in the repo and verify AST preservation.

func TestRepoFileRoundTrip(t *testing.T) {
	patterns := []string{
		"../../_examples/**/*.lisp",
		"../../lisp/lisplib/**/*.lisp",
	}
	for _, pattern := range patterns {
		matches, err := filepath.Glob(pattern)
		if err != nil {
			t.Fatalf("glob %s: %v", pattern, err)
		}
		for _, path := range matches {
			t.Run(filepath.Base(path), func(t *testing.T) {
				src, err := os.ReadFile(path) //nolint:gosec // test reads discovered files
				require.NoError(t, err)
				formatted, err := Format(src, nil)
				require.NoError(t, err, "Format failed for %s", path)
				roundTripEqual(t, string(src), string(formatted))

				// Also verify idempotency
				formatted2, err := Format(formatted, nil)
				require.NoError(t, err, "Idempotent format failed for %s", path)
				assert.Equal(t, string(formatted), string(formatted2),
					"not idempotent for %s", path)
			})
		}
	}
}
