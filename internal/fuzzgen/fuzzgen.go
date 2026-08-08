// Copyright © 2026 The ELPS authors

// Package fuzzgen turns a fuzzer-supplied byte script into syntactically valid
// ELPS source.
//
// # Why
//
// Every other fuzz target in this repository takes raw bytes.  A
// coverage-guided mutator starting from raw bytes spends most of its budget on
// input that dies at the first token, so the states it reaches are the ones
// near the entrance: the scanner, the lexer's dispatch switch, the parser's
// error paths.  The states BEHIND a successful parse -- comment attachment,
// blank-line bookkeeping, prefix-form re-sugaring, scope-aware renaming -- are
// where `elps fmt` and `elps minify` silently rewrite a customer's program,
// and raw bytes hardly ever get there.
//
// This package generates the other kind of input: programs that are
// syntactically valid but semantically hostile.  Deep nesting, every bracket
// kind, keyword and package-qualified symbols, unicode identifiers, reader
// macros with awkward operands, oversized literals, and comments in every
// position the grammar permits one.
//
// # Contract
//
// Generate's output ALWAYS parses.  That is the load-bearing property: it is
// what lets a fuzz target treat "the parser rejected this" as a defect rather
// than as the expected outcome, which is how the float-exponent and
// trailing-backslash lexer bugs were found -- both of them rejected input the
// language is supposed to accept, and no raw-byte target could tell that
// rejection apart from the millions of legitimate ones.
//
// The contract cuts both ways.  A crasher reported by a target that asserts it
// is either a parser defect or a generator defect, and triage has to consider
// both.  TestGeneratedProgramsAlwaysParse sweeps a large deterministic sample
// so that a generator defect is caught by `go test` rather than by a red fuzz
// run.
//
// # Determinism
//
// Generation is a pure function of (script, limits).  The fuzzing engine
// records the script, not the ELPS text, so a crasher reproduces from the
// committed corpus entry with no generated source to store.  Corpus files
// under testdata/fuzz are therefore scripts.
//
// Nothing consumes randomness, wall-clock time or the environment, and every
// production consumes at least one script byte, so generation terminates on a
// finite script.  Three independent bounds apply anyway -- byte budget,
// nesting cap and script length -- because a generator that can emit an
// unbounded program hands the mutator a seed that costs seconds per execution.
//
// # On dialects
//
// There is deliberately only ONE grammar here.  An earlier iteration carried a
// second, narrower dialect defined as the intersection of the two parsers this
// repository used to ship; parser/regexparser is being removed, so that
// definition no longer names anything and a subset with no defining property
// is just an untested code path.  If a future target needs a restricted
// vocabulary it should say what restricts it.
package fuzzgen

import (
	"strconv"
	"strings"
)

// Limits bounds a single generated program.  All three are enforced
// independently: the byte budget stops a wide program, the depth cap stops a
// deep one, and the script itself runs out.
type Limits struct {
	// MaxBytes is the size at which generation stops opening new constructs.
	// The result may exceed it by the tail of the construct in flight plus
	// the closing brackets, which is bounded by MaxDepth + one literal.
	MaxBytes int
	// MaxDepth is the maximum nesting depth of generated forms.  Kept far
	// below rdparser.DefaultMaxParseDepth: the depth guard is already
	// covered by fuzzseed.Pathological, and a deep seed is expensive to
	// mutate.
	MaxDepth int
	// MaxForms is the maximum number of top-level forms.
	MaxForms int
}

// DefaultLimits are the bounds the fuzz targets use.  Sized so that a
// generated program stays in the range where a coverage-guided run manages
// thousands of executions per second; the oversized shapes that must also keep
// working live in fuzzseed.Pathological, which ordinary unit tests run once
// each.
func DefaultLimits() Limits {
	return Limits{MaxBytes: 2048, MaxDepth: 10, MaxForms: 10}
}

// Generate renders script as ELPS source under DefaultLimits.
func Generate(script []byte) []byte {
	return GenerateLimited(script, DefaultLimits())
}

// GenerateLimited renders script as ELPS source under the given limits.  A nil
// or empty script yields the empty program, which is valid.
func GenerateLimited(script []byte, lim Limits) []byte {
	if lim.MaxBytes <= 0 {
		lim.MaxBytes = DefaultLimits().MaxBytes
	}
	if lim.MaxDepth <= 0 {
		lim.MaxDepth = DefaultLimits().MaxDepth
	}
	if lim.MaxForms <= 0 {
		lim.MaxForms = DefaultLimits().MaxForms
	}
	g := &generator{script: script, lim: lim}
	g.program()
	return []byte(g.buf.String())
}

type generator struct {
	script  []byte
	pos     int
	buf     strings.Builder
	lim     Limits
	needSep bool // a token was written; the next one needs whitespace first
}

// next consumes one script byte.  It returns -1 once the script is exhausted,
// at which point every choose() falls to its default arm and generation winds
// down deterministically instead of looping.
func (g *generator) next() int {
	if g.pos >= len(g.script) {
		return -1
	}
	b := g.script[g.pos]
	g.pos++
	return int(b)
}

// choose consumes one script byte and reduces it to [0,n), or returns -1 when
// the script is exhausted.
func (g *generator) choose(n int) int {
	b := g.next()
	if b < 0 {
		return -1
	}
	return b % n
}

func (g *generator) spent() bool {
	return g.pos >= len(g.script) || g.buf.Len() >= g.lim.MaxBytes
}

// raw writes text without any separator bookkeeping.
func (g *generator) raw(s string) {
	g.buf.WriteString(s)
}

// pad writes the whitespace that must precede the next token.  ELPS separates
// tokens by whitespace everywhere except immediately inside a bracket or
// immediately after a prefix macro, and those callers write through raw
// instead.
func (g *generator) pad() {
	if !g.needSep {
		return
	}
	g.needSep = false
	switch g.choose(8) {
	case 3:
		g.raw("\n")
	case 4:
		g.raw("\n\n")
	case 5:
		g.raw("  ")
	case 6:
		g.raw("\t")
	case 7:
		g.raw(" \n ")
	default: // 0,1,2 and the exhausted script
		g.raw(" ")
	}
}

// token writes one complete token, separated from whatever came before.
func (g *generator) token(s string) {
	g.pad()
	g.raw(s)
	g.needSep = true
}

// ---------------------------------------------------------------------------
// vocabularies
//
// Every entry is a token the lexer and parser accept as written.  The rules
// that constrain them are narrow enough to be worth stating:
//
//   - miscWordSymbols is "._+-*/=<>!&~%?$"; '_' is NOT in it, so an underscore
//     is not a legal symbol rune.
//   - a symbol holds at most one ':', and neither side may be empty except the
//     package side of a keyword (":kw" is legal, "kw:" is not).
//   - a leading digit routes to the number lexer, so no symbol may start with
//     one.
//   - '-' glued to a digit or a symbol lexes as NEGATIVE and is merged into
//     the following token by ParseNegative, so "-x" is the symbol "-x".

var symbols = []string{
	"a", "b", "f", "x", "y", "foo", "bar", "baz", "list", "car", "cdr",
	"+", "-", "*", "/", "=", "<", ">", "!", "&", "~", "%", "?", "$", ".",
	"a.b", "x!", "y?", "n%", "->", "<=>", "f2", "v9x",
	// unicode identifiers: isWordStart admits any unicode.IsLetter rune, so
	// these are ordinary symbols and must survive the printer unchanged.
	"λ", "Ω", "日本語", "café", "Ünïcödé", "мир", "ασδφ",
}

var keywords = []string{":a", ":key", ":opt", ":λ", ":x1", ":rest"}

var qualified = []string{
	"lisp:car", "pkg:foo", "a:b", "user:x", "λ:μ", "test:assert=",
}

// specials are real ELPS forms.  Generating them keeps the minifier's
// scope-aware renaming and the formatter's per-form indentation rules on the
// hot path instead of leaving them to be reached by accident.
var specials = []string{
	"defun", "defmacro", "lambda", "let", "let*", "labels", "flet",
	"quasiquote", "unquote", "unquote-splicing", "quote", "if", "cond",
	"handler-bind", "in-package", "use-package", "export", "set", "set!",
	"progn", "and", "or", "not", "error", "ignore-errors",
}

// stringBodies are bodies that survive strconv.Unquote, which is what
// ParseLiteralString uses.  A raw newline is rejected by the lexer and a raw
// control character by Unquote, so escapes are the only way to reach them.
var stringBodies = []string{
	``, `a`, `hello world`, `a b`,
	`\n`, `\t`, `\\`, `\"`, `\x41`, `é`, `\U0001F600`, `\000`,
	`\\\\`, `a\\`, `\\n`, `;not a comment`, `(`, `)`, `[]`, `#'`,
	`é`, `日本語`, `λ`, `  `, `-1`, `1e5`,
}

// A raw-string body may neither contain three consecutive quotes nor end in
// one: the scanner closes the literal at the first triple quote it finds, so
// either shape terminates the token early and leaves a stray quote behind.
// Radix-macro bodies.  Both must stay inside int64 and must not be followed by
// a word rune, which readHexLiteral and readOctalLiteral both reject.
var (
	hexDigits   = []string{"0", "FF", "dead", "7fffffff", "1", "aA"}
	octalDigits = []string{"0", "7", "777", "17777777777", "1"}
)

var rawStringBodies = []string{
	``, `a`, "a\nb", `; comment`, `\`, `\n`, `(a b)`, "\n\n", `a"b`, `x'y`,
}

// ---------------------------------------------------------------------------
// productions

func (g *generator) program() {
	// A hash-bang is only legal as the very first thing in a program, and it
	// is the one token whose handling differs between Parse and ParseProgram.
	if g.choose(6) == 0 {
		g.raw("#!/usr/bin/env elps\n")
	}
	for i := 0; i < g.lim.MaxForms && !g.spent(); i++ {
		g.topLevel()
	}
	// A file ending in a comment leaves it in PendingComments rather than
	// attached to any node -- a distinct printer path.
	if g.choose(10) == 0 {
		g.comment()
	}
	if g.choose(4) != 0 {
		g.raw("\n")
	}
}

func (g *generator) topLevel() {
	switch g.choose(10) {
	case 0, 1:
		g.comment()
	case 2:
		g.comment()
		g.comment()
	case 3:
		g.comment()
		g.raw("\n") // blank line between the comment and the form it precedes
	}
	g.form(0)
	if g.choose(6) == 0 {
		g.raw(" ")
		g.rawComment() // trailing, on the same line as the form
	}
	g.raw("\n")
	g.needSep = false
}

// comment writes a whole-line comment, separated from what precedes it.
func (g *generator) comment() {
	g.pad()
	if !strings.HasSuffix(g.buf.String(), "\n") {
		g.raw("\n")
	}
	g.rawComment()
}

// rawComment writes "; ..." plus the newline that terminates it.  A comment
// runs to end of line, so the newline is part of the token, not separation.
func (g *generator) rawComment() {
	switch g.choose(8) {
	case 0:
		g.raw(";")
	case 1:
		g.raw(";;; section")
	case 2:
		g.raw("; (unbalanced")
	case 3:
		g.raw(`; "unterminated`)
	case 4:
		g.raw("; λ 日本語")
	case 5:
		g.raw(";" + strings.Repeat("x", g.longRun(200)))
	case 6:
		g.raw("; -1")
	default:
		g.raw("; c")
	}
	g.raw("\n")
	g.needSep = false
}

func (g *generator) form(depth int) {
	if depth >= g.lim.MaxDepth || g.spent() {
		g.atom()
		return
	}
	switch g.choose(24) {
	case 0, 1, 2, 3:
		g.atom()
	case 4, 5, 6:
		g.sexpr(depth, '(')
	case 7, 8:
		g.sexpr(depth, '[')
	case 9:
		g.quote(depth)
	case 10:
		g.funRef()
	case 11:
		g.unbound(depth)
	case 12:
		g.quasi(depth)
	case 13:
		g.nestBurst(depth)
	case 14:
		g.special(depth)
	case 15:
		g.huge()
	case 16:
		// A form whose leading comment sits inside the enclosing bracket.
		g.comment()
		g.form(depth + 1)
	case 17:
		// Negative literal glued to a sign token: two tokens, one literal.
		g.negative()
	default:
		g.atom()
	}
}

func (g *generator) atom() {
	switch g.choose(16) {
	case 0, 1:
		g.token(symbols[g.pick(len(symbols))])
	case 2:
		g.token(keywords[g.pick(len(keywords))])
	case 3:
		g.token(qualified[g.pick(len(qualified))])
	case 4:
		g.token(specials[g.pick(len(specials))])
	case 5, 6:
		g.token(g.intLiteral())
	case 7, 8:
		g.token(g.floatLiteral())
	case 9:
		g.token("#x" + hexDigits[g.pick(len(hexDigits))])
	case 10:
		g.token("#o" + octalDigits[g.pick(len(octalDigits))])
	case 11, 12:
		g.token(`"` + stringBodies[g.pick(len(stringBodies))] + `"`)
	case 13:
		g.token(`"""` + rawStringBodies[g.pick(len(rawStringBodies))] + `"""`)
	case 14:
		g.token("()")
	default:
		g.token(symbols[g.pick(len(symbols))])
	}
}

// longRun caps the repeat count of an oversized token once the byte budget is
// spent, so a single production cannot blow past MaxBytes by an unbounded
// amount.  Overshoot stays bounded by one long token plus the closing brackets.
func (g *generator) longRun(n int) int {
	if g.buf.Len() >= g.lim.MaxBytes {
		return 1
	}
	return n
}

// pick chooses an index into a table, defaulting to 0 on an exhausted script.
func (g *generator) pick(n int) int {
	i := g.choose(n)
	if i < 0 {
		return 0
	}
	return i
}

// intLiteral stays inside int64.  Overflowing literals are a parse error, and
// they are already covered by fuzzseed.Adversarial; generating them here would
// break the "always parses" contract for no new coverage.
func (g *generator) intLiteral() string {
	switch g.choose(8) {
	case 0:
		return "0"
	case 1:
		return "1"
	case 2:
		return "9223372036854775807" // math.MaxInt64
	case 3:
		return strings.Repeat("9", 18)
	case 4:
		return "007"
	case 5:
		return strconv.Itoa(g.pick(256))
	case 6:
		return "1000000"
	default:
		return "42"
	}
}

// floatLiteral covers the exponent grammar, which is [+-]?digit+ after e/E.
// The single-digit exponents here are the shapes the lexer used to reject.
func (g *generator) floatLiteral() string {
	switch g.choose(12) {
	case 0:
		return "1.5"
	case 1:
		return "0.0"
	case 2:
		return "1e5"
	case 3:
		return "1E5"
	case 4:
		return "1.5e2"
	case 5:
		return "1e+5"
	case 6:
		return "1.5e-2"
	case 7:
		return "1e55"
	case 8:
		return "0.1e0"
	case 9:
		return "123.456"
	case 10:
		return "1e308"
	default:
		return "2.0"
	}
}

// negative emits a sign glued to what follows.  The lexer only produces a
// NEGATIVE token when something CAN be glued to it, and ParseNegative merges
// the two, so this is the one place where a single AST node spans two tokens.
func (g *generator) negative() {
	switch g.choose(4) {
	case 0:
		g.token("-" + g.intLiteral())
	case 1:
		g.token("-" + g.floatLiteral())
	case 2:
		g.token("-" + symbols[g.pick(len(symbols))])
	default:
		g.token("-1")
	}
}

// sexpr writes a bracketed form.  Both bracket kinds are generated: '(' is a
// cons expression and '[' a list, and the formatter tracks which one the
// source used.
func (g *generator) sexpr(depth int, open byte) {
	closer := byte(')')
	if open == '[' {
		closer = ']'
	}
	g.pad()
	g.raw(string(open))
	g.needSep = false
	n := g.pick(4)
	for i := 0; i < n && !g.spent(); i++ {
		g.form(depth + 1)
		// A comment on the SAME line as the child it follows is attached to
		// that child as a TrailingComment, not to whatever comes next -- a
		// different parser path from the whole-line comment g.comment writes,
		// and the one the sign-merge defect lived behind: with a trailing
		// comment above it, a "-1" on the next line has nothing but its own
		// PrecedingNewlines to say it belongs on a new line.
		if !g.spent() && g.choose(8) == 0 {
			g.raw(" ")
			g.rawComment()
		}
	}
	// A comment between the last child and the closing bracket lands in
	// InnerTrailingComments, and a closing bracket on its own line is
	// recorded separately again.
	switch g.choose(10) {
	case 0:
		g.comment()
	case 1:
		g.raw("\n")
		g.needSep = false
	}
	g.raw(string(closer))
	g.needSep = true
}

// quote writes a ' prefix.  Whitespace and even a comment may sit between the
// quote and its operand -- unlike #' and #^, which the lexer requires to be
// glued to theirs -- so all three gaps are generated.
func (g *generator) quote(depth int) {
	g.pad()
	g.raw("'")
	g.needSep = false
	switch g.choose(12) {
	case 0:
		g.raw("'") // quote chain
	case 1:
		g.raw("\n")
	case 2:
		g.raw("\n\n")
	case 3:
		g.rawComment()
	}
	g.form(depth + 1)
}

// funRef writes #'name.  ParseFunRef reads the operand with ParseSymbol and
// then re-parses it, rejecting any name that does not read back as the same
// symbol, so the operand pool is restricted to names that do.
func (g *generator) funRef() {
	g.token("#'" + funRefNames[g.pick(len(funRefNames))])
}

// funRefNames are operands ParseFunRef accepts.  It reads the operand with
// ParseSymbol and then RE-PARSES it, rejecting any name that does not read back
// as the same symbol, and the printer's canWriteFunRef mirrors that check.
var funRefNames = []string{
	"f", "foo", "car", "+", "-", "a.b", "λ", "lisp:car", "pkg:foo",
	"x!", "defun", "日本語",
}

// unbound writes #^expr.  ParseUnbound rejects an operand containing a nested
// UNQUOTED s-expression, and the printer's re-sugaring check mirrors that
// exactly, so the operand is drawn from the shapes that satisfy both.
func (g *generator) unbound(depth int) {
	g.pad()
	g.raw("#^")
	g.needSep = false
	switch g.choose(8) {
	case 0:
		g.raw("%")
	case 1:
		g.raw("(+ % 1)")
	case 2:
		g.raw("[% 1]")
	case 3:
		g.raw("'a")
	case 4:
		g.raw("()")
	case 5:
		g.raw(`"s"`)
	case 6:
		g.raw("(list % %)")
	default:
		g.atomNoPad(depth)
	}
	g.needSep = true
}

// atomNoPad writes an atom with no leading whitespace, for the operand slot of
// a prefix macro that forbids one.
func (g *generator) atomNoPad(depth int) {
	_ = depth
	switch g.choose(6) {
	case 0:
		g.raw(symbols[g.pick(len(symbols))])
	case 1:
		g.raw(keywords[g.pick(len(keywords))])
	case 2:
		g.raw(g.intLiteral())
	case 3:
		g.raw(g.floatLiteral())
	case 4:
		g.raw(`"` + stringBodies[g.pick(len(stringBodies))] + `"`)
	default:
		g.raw("a")
	}
}

// quasi writes a quasiquote template.  ELPS has no backquote reader syntax --
// quasiquote, unquote and unquote-splicing are ordinary head symbols -- so the
// interesting shape is the nesting, not the punctuation.
func (g *generator) quasi(depth int) {
	g.pad()
	g.raw("(quasiquote")
	g.needSep = true
	switch g.choose(6) {
	case 0:
		g.token("(unquote")
		g.form(depth + 2)
		g.raw(")")
	case 1:
		g.token("(list")
		g.token("(unquote-splicing")
		g.form(depth + 3)
		g.raw("))")
	case 2:
		g.token("(quasiquote (unquote (unquote-splicing")
		g.form(depth + 4)
		g.raw(")))")
	default:
		g.form(depth + 1)
	}
	g.raw(")")
	g.needSep = true
}

// special writes a form with a real head symbol, so the formatter's per-form
// indentation table and the minifier's binding-form handling are exercised
// with the shapes they actually special-case.
func (g *generator) special(depth int) {
	g.pad()
	switch g.choose(8) {
	case 0:
		g.raw("(defun ")
		g.raw(symbols[g.pick(len(symbols))])
		g.raw(" (x &optional y &rest z)")
		g.needSep = true
		g.form(depth + 1)
		g.raw(")")
	case 1:
		g.raw("(let ([a ")
		g.needSep = false
		g.form(depth + 2)
		g.raw("] [b 2])")
		g.needSep = true
		g.form(depth + 1)
		g.raw(")")
	case 2:
		g.raw("(lambda (&key k)")
		g.needSep = true
		g.form(depth + 1)
		g.raw(")")
	case 3:
		g.raw("(in-package 'pkg)")
		g.needSep = true
	case 4:
		g.raw("(handler-bind ((condition (lambda (c) c)))")
		g.needSep = true
		g.form(depth + 1)
		g.raw(")")
	case 5:
		g.raw("(defmacro m (x)")
		g.needSep = true
		g.form(depth + 1)
		g.raw(")")
	case 6:
		g.raw("(cond ([")
		g.needSep = false
		g.form(depth + 2)
		g.raw("] 1) (:else 2))")
		g.needSep = true
	default:
		g.raw("(progn")
		g.needSep = true
		g.form(depth + 1)
		g.raw(")")
	}
}

// nestBurst writes a run of nesting in one step, so the mutator can reach a
// deep tree without needing a long script to build it one level at a time.
// The run is clamped to the remaining depth budget.
func (g *generator) nestBurst(depth int) {
	n := g.pick(8) + 1
	if room := g.lim.MaxDepth - depth; n > room {
		n = room
	}
	if n <= 0 {
		g.atom()
		return
	}
	g.pad()
	open, closer := "(", ")"
	switch g.choose(4) {
	case 0:
		open, closer = "[", "]"
	case 1:
		open, closer = "'(", ")"
	case 2:
		// A run of quotes over a single atom.  Quote nesting has no closing
		// token, so it is generated on its own rather than as a bracket pair.
		g.raw(strings.Repeat("'", n))
		g.needSep = false
		g.atomNoPad(0)
		g.needSep = true
		return
	}
	g.raw(strings.Repeat(open, n))
	g.needSep = false
	if !g.spent() {
		g.form(depth + n)
	}
	g.raw(strings.Repeat(closer, n))
	g.needSep = true
}

// huge writes an oversized but legal token.  Size is where the scanner's
// buffer-extension path, the printer's line-width logic and the minifier's
// name table all change behaviour, and none of that is reachable from a
// literal the mutator has to build one byte at a time.
func (g *generator) huge() {
	switch g.choose(6) {
	case 0:
		g.token(strings.Repeat("z", g.longRun(300)))
	case 1:
		g.token(`"` + strings.Repeat("s", g.longRun(400)) + `"`)
	case 2:
		g.token(`"""` + strings.Repeat("r", g.longRun(400)) + `"""`)
	case 3:
		g.token(strings.Repeat("λ", g.longRun(100)))
	case 4:
		g.token("pkg:" + strings.Repeat("q", g.longRun(200)))
	default:
		// 100 escaped backslashes: the trailing-backslash parity path at a
		// size no byte-at-a-time mutator is going to build by accident.
		g.token(`"` + strings.Repeat(`\\`, g.longRun(100)) + `"`)
	}
}
