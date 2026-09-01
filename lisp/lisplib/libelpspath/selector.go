// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// THE JQ-STRING SELECTOR FRONT END
//
// ParseSelector translates a jq-style selector string -- ".a.b[0][\"x y\"]
// [1:3]" -- into this package's exported Path constructors. It is a Go-level
// API, plus the one builtin that exposes it: parse-path converts a selector
// string into path steps, which is a conversion rather than an operation on
// a document. No document-operating builtin parses a string.
//
// WHY THE GRAMMAR LIVES HERE (issue #564). The jq-string surface spent its
// life in luthersystems/substrate, alongside the deprecated jq-string
// builtins (get-path, set-path!, ...) that are its only lisp-visible
// consumer. Those builtins stay downstream -- they are deprecated in favour
// of the positional-step family in query.go, and taking them would
// re-introduce retired surface here. The grammar is a different thing: every
// symbol it names (Root, Chain, Dot, Index, Iter, Range) is exported from
// this package, so it is pure translation into this package's constructors
// and it constrains this package's semantics from outside where nobody can
// test the pair together.
//
// That split had already cost something. rangePath.String() ignored
// implicitTo and rendered Range(1, 0, true) as "[1:0]" -- an empty slice, a
// different path (issue #563). The only caller that ever built an implicitTo
// path was this parser, from "[1:]" and "[:]", so a printing defect in
// path.go was reachable only through a parser path.go could not see, and the
// round-trip test that catches it (TestParseSelectorRoundTrip) could not be
// written on either side of the boundary.
//
// HOW IT PARSES. One left-to-right pass, no regexps and no backtracking.
// selectorPaths trims, applies the leading-bracket rule (selectorBody), and
// then repeatedly asks scanStep for the single step at the head of what is
// left. scanStep dispatches on ONE byte -- "[" or "." -- and a bracket
// dispatches again on the first non-blank byte inside it: a double quote
// opens a quoted key, anything else is a subscript (an index, a range or the
// iterator). Every reader returns the number of BYTES it consumed, and zero
// means "not this form" rather than an error, because the caller is the one
// that knows a step was expected here. The dispatch bytes are what make a
// single pass enough: no two readers can claim the same head, which
// TestSelectorStepFormsAreDisjoint pins over the forms each one owns.
//
// TWO WHITESPACE SETS, deliberately. Inside a step -- after the leading dot,
// around the bounds of a range, before a "?" -- only [\t\n\f\r ] separates
// tokens. BETWEEN steps the scan loop trims with strings.TrimSpace, which is
// unicode-aware. A non-breaking space therefore SEPARATES two steps but
// cannot appear inside one: ".a<nbsp>.b" is two keys, ".<nbsp>a" is a parse
// error. The narrow set is the one the grammar was specified with, and
// widening it would accept selectors nothing downstream accepts.
//
// FUZZED by FuzzParseSelector, whose invariant is that anything this parser
// accepts must PRINT to something it accepts, meaning the same path.
//
// That target could not exist until issue #565 was fixed. Chain (path.go, via
// normalizePaths and its mutual recursion with Iter) was EXPONENTIAL in the
// number of iterator steps, so a 45-byte selector of "[]"s took over a second
// to construct and each further "[]" doubled it -- a watchdog target would
// have reported an engine defect as a parser hang, with no crasher to
// minimise. The fix landed alongside this file, so the target ships with it;
// selectors over 512 bytes are still skipped, and the cost is pinned by
// TestNormalizePathsIsNotExponential, which names the defect if it returns.

// scanStep reads the ONE step at the head of s, which the caller has already
// trimmed, and returns the number of bytes it consumed.
//
// A zero length with a nil error means the head of s is not a step at all.
// That is not an error here because the message belongs to the caller, which
// knows the whole remainder and can explain the stall (keySpellingHint);
// returning an error from each reader would make every non-match look like a
// diagnosis.
//
// The dispatch is on a single byte and there is no fallback: a step begins
// with "[" or ".", and nothing else can begin one.
func scanStep(s string) (int, Path, error) {
	if s == "" {
		return 0, nil, nil
	}
	switch s[0] {
	case '[':
		return scanBracketStep(s)
	case '.':
		return scanDotKey(s)
	}
	return 0, nil, nil
}

// scanBracketStep reads the step in a bracket: a quoted key or a subscript.
//
// The two are told apart by the first non-blank byte after the "[", which is
// a complete discriminator: a subscript body is digits, "-", ":" or nothing,
// and a quoted key always opens with a double quote. Neither reader can
// consume the other's form even when called directly -- the subscript needs
// its closing "]" where a key has a quote, and the key needs its opening
// quote -- so the dispatch is an optimisation over trying both, not the thing
// that keeps them apart.
func scanBracketStep(s string) (int, Path, error) {
	if i := skipBlank(s, 1); i < len(s) && s[i] == '"' {
		return scanQuotedKey(s)
	}
	return scanSubscript(s)
}

// scanSubscript reads the bracket forms that are not a quoted key: the index
// "[n]", both range spellings, and the iterator "[]".
//
//	[n]      => Index(n)          n is -?\d+, so "[-0]" is Index(0)
//	[a:b]    => Range(a, b, false)
//	[a:]     => Range(a, 0, true) the end comes from the document
//	[:b]     => Range(0, b, false) an absent start is a literal 0
//	[:]      => Range(0, 0, true)
//	[]       => Iter()
//
// WHAT SEPARATES AN INDEX FROM A RANGE is the colon, not the bounds: "[0]"
// and "[0:]" carry the same start text and mean different things, so the
// colon is tracked as its own fact rather than inferred from what was read.
//
// A bound that will not fit in an int is the one error this returns; the
// digits are read here and converted only once the whole form has been
// recognised, so "[99999999999999999999" -- unterminated -- stalls as a
// parse failure rather than reporting an overflow it never got to.
func scanSubscript(s string) (int, Path, error) {
	if s == "" || s[0] != '[' {
		return 0, nil, nil
	}
	i := skipBlank(s, 1)
	fromText, i := scanIntText(s, i)
	i = skipBlank(s, i)
	isRange := false
	toText := ""
	if i < len(s) && s[i] == ':' {
		isRange = true
		i = skipBlank(s, i+1)
		toText, i = scanIntText(s, i)
		i = skipBlank(s, i)
	}
	if i >= len(s) || s[i] != ']' {
		return 0, nil, nil
	}
	n := skipOptionMark(s, i+1)

	from := 0
	if fromText != "" {
		v, err := strconv.Atoi(fromText)
		if err != nil {
			// Reached by an index too large for an int, which the digit
			// scan takes happily: "[99999999999999999999]".
			return 0, nil, fmt.Errorf("fail to parse array index: %s", fromText)
		}
		from = v
	}
	if !isRange {
		if fromText == "" {
			return n, Iter(), nil
		}
		return n, Index(from), nil
	}
	// to is meaningless when implicitTo is set: validateRange overwrites it
	// with the document length. Zero, not the from, so that a mis-set flag
	// degrades to an empty slice rather than to a plausible-looking wrong
	// one.
	to := 0
	if toText != "" {
		v, err := strconv.Atoi(toText)
		if err != nil {
			return 0, nil, fmt.Errorf("fail to parse second array index: %s", toText)
		}
		to = v
	}
	return n, Range(from, to, toText == ""), nil
}

// scanQuotedKey reads a map key given as a quoted string inside brackets,
// which is the only spelling for a key that is not a bare identifier:
// ["$private"], ["x y"], ["\"\n"].
//
// The literal is decoded with strconv.Unquote, so the escapes are Go's --
// and, since dotPath.String() renders a key with %q, the pair round-trips.
// Unquote runs only after the closing "]" has been found, so an unterminated
// bracket stalls with the parser's own message rather than with a decoding
// error about text that was never a key.
func scanQuotedKey(s string) (int, Path, error) {
	if s == "" || s[0] != '[' {
		return 0, nil, nil
	}
	open := skipBlank(s, 1)
	end := scanStringLiteral(s, open)
	if end < 0 {
		return 0, nil, nil
	}
	i := skipBlank(s, end)
	if i >= len(s) || s[i] != ']' {
		return 0, nil, nil
	}
	key, err := strconv.Unquote(s[open:end])
	if err != nil {
		return 0, nil, err
	}
	return skipOptionMark(s, i+1), Dot(key), nil
}

// scanStringLiteral returns the index just past the closing quote of the
// string literal starting at i, or -1 when there is no literal there.
//
// The body is the ordinary string-literal grammar: an escape SEQUENCE, or any
// byte that is neither a quote nor a backslash. Reading it as "everything up
// to the last quote" instead is the shape of issue #566 -- see the note on
// ParseSelector -- and reading it as "everything up to the FIRST quote" would
// break `.["a\"b"]`, so the two-byte skip after a backslash is the whole
// point of the loop.
//
// A backslash at the very end, or one before a newline, is not an escape
// sequence and ends the search unmatched. Everything else, invalid UTF-8
// included, is carried through to strconv.Unquote, which is the one place
// that judges whether the escapes are actually well formed.
func scanStringLiteral(s string, i int) int {
	if i >= len(s) || s[i] != '"' {
		return -1
	}
	for j := i + 1; j < len(s); {
		switch s[j] {
		case '"':
			return j + 1
		case '\\':
			if j+1 >= len(s) || s[j+1] == '\n' {
				return -1
			}
			j += 2
		default:
			j++
		}
	}
	return -1
}

// scanDotKey reads a bare key: ".foo". The identifier rule is deliberately
// narrow -- ".0" and ".$private" are parse errors, not keys -- because a
// looser rule would swallow the leading dot of a following selector. Such
// keys are reachable through the quoted form, which is what keySpellingHint
// tells a caller who tried one.
func scanDotKey(s string) (int, Path, error) {
	if s == "" || s[0] != '.' {
		return 0, nil, nil
	}
	i := skipBlank(s, 1)
	if i >= len(s) || !isKeyStartByte(s[i]) {
		return 0, nil, nil
	}
	j := i + 1
	for j < len(s) && isKeyByte(s[j]) {
		j++
	}
	return skipOptionMark(s, j), Dot(s[i:j]), nil
}

// isKeyStartByte and isKeyByte spell [A-Za-z_] and [A-Za-z_0-9].
//
// They are byte tests, not rune tests, and that is the rule rather than an
// ASCII shortcut: ".café" is a parse error and `.["café"]` is the key. A rune
// test would silently widen the grammar.
func isKeyStartByte(c byte) bool {
	return c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

func isKeyByte(c byte) bool {
	return isKeyStartByte(c) || (c >= '0' && c <= '9')
}

// scanIntText returns the -?\d+ literal at i, as text, and the index past it.
// It returns "" and i unchanged when there is no literal there, which
// includes a lone "-": "[-]" is not a subscript.
//
// The text is returned rather than a number so the caller can tell an absent
// bound from a zero one -- "[:2]" and "[0:2]" are the same path, but "[]" and
// "[0]" are not -- and so that a bound too large for an int can be named in
// the error.
func scanIntText(s string, i int) (string, int) {
	j := i
	if j < len(s) && s[j] == '-' {
		j++
	}
	digits := j
	for digits < len(s) && s[digits] >= '0' && s[digits] <= '9' {
		digits++
	}
	if digits == j {
		return "", i
	}
	return s[i:digits], digits
}

// skipBlank returns the index of the first byte at or after i that is not one
// of [\t\n\f\r ].
//
// That is the set the grammar was specified with, and it is NARROWER than
// unicode.IsSpace: a vertical tab or a non-breaking space is not blank
// INSIDE a step. See the note on the two whitespace sets above.
func skipBlank(s string, i int) int {
	for i < len(s) {
		switch s[i] {
		case ' ', '\t', '\n', '\f', '\r':
			i++
		default:
			return i
		}
	}
	return i
}

// skipOptionMark consumes the jq optional-selector suffix "?", and the blanks
// before it, if one is there.
//
// It returns i untouched when there is no "?" -- trailing blanks alone need
// not be consumed, because the scan loop trims the remainder before the next
// step either way.
func skipOptionMark(s string, i int) int {
	if j := skipBlank(s, i); j < len(s) && s[j] == '?' {
		return j + 1
	}
	return i
}

// ParseSelector translates a jq-style selector string into a Path.
//
// It is named ParseSelector rather than Parse because this repository has a
// lisp reader, and an exported Parse in a lisp package reads as that one.
//
//	p, err := libelpspath.ParseSelector(`.users[0]["full name"]`)
//	v, err := p.Get(doc)
//
// The grammar, and how each form lands on a Path:
//
//	.          => Root(Chain())      the whole document
//	.foo       => Dot("foo")         bare keys only: [A-Za-z_][A-Za-z_0-9]*
//	["$foo"]   => Dot("$foo")        any key, as a Go-quoted string literal
//	[0] [-1]   => Index(n)           negative counts from the end
//	[1:3]      => Range(1, 3, false)
//	[:3]       => Range(0, 3, false) an absent "from" is 0, not implicit
//	[1:]  [:]  => Range(n, 0, true)  the end is resolved against the document
//	[]         => Iter()             every element
//
// Whitespace is permitted around and inside brackets, and a selector must
// start with "." -- ".[0]" is accepted, a bare "[0]" is not. It holds no
// state at all, so this may be called from any goroutine.
//
// The Path it returns is an ordinary one: nothing distinguishes a parsed
// path from one assembled by hand or by ArgsToPath, and the same seven
// operations apply. A caller that is about to hand a document to one of them
// should run okSimpleType over the document first, which is what the
// builtins do -- see that function for why it is not optional.
//
// ONE WART. The jq optional-selector suffix "?" is ACCEPTED AND DISCARDED.
// In jq, ".a?" suppresses the error a non-object .a would raise; here every
// reader consumes a trailing "?" and none of them records it, so ".a?" is
// exactly ".a" and the error is raised. Nothing in the engine implements
// error-suppressing steps, so honouring the suffix would be a feature, not a
// fix.
//
// A SECOND WART, and this one can lose you data. A selector that leads with a
// bracket is cut at its first NEWLINE and the rest is discarded in silence:
//
//	ParseSelector(".[0]\n.password")  =>  .[0]      -- the tail is dropped
//	ParseSelector(".items[0]\n.id")   =>  .["items"][0]["id"]
//
// The two differ because only the bracket-led form goes through
// selectorBody's leading-bracket rule; see that function for why the
// behaviour is kept. It matters because the truncated path is a PREFIX of
// what was asked for, so a write through it lands on the wrong node rather
// than failing: `.[0]\n.password` sets the whole element, not its field.
//
// This function keeps the wart for parity with the v1 jq-string builtins
// downstream. Everything else does not: the parse-path builtin REFUSES a
// selector whose tail would be discarded, and a Go caller converting selector
// text that came from outside the program should do the same -- rejecting any
// selector containing a newline is sufficient and is what parse-path amounts
// to.
//
// Two properties worth stating because they are easy to break:
//
// The identity selector "." returns Root(Chain()), which prints "." and
// matches what ArgsToPath builds for an empty step list. Chain() alone
// behaves identically -- rootPath proxies all seven operations and adds only
// a leading "." to String() -- but prints the empty string, which this
// parser cannot read back. TestParseSelectorRootSpelling pins the agreement.
//
// A quoted key ends at the first UNESCAPED quote, which is why
// scanStringLiteral skips two bytes after a backslash. Ending it at the LAST
// quote in the selector instead -- which is what the regexp this scanner
// replaced did before issue #566, its body reading as "any character at all"
// -- limits a selector to ONE bracketed key and makes String()'s own output
// unreadable, since it brackets every map key. TestParseSelectorTwoQuotedKeys
// covers the grammar and the round-trip test carries multi-key selectors.
func ParseSelector(selector string) (Path, error) {
	paths, err := selectorPaths(selector)
	if err != nil {
		return nil, err
	}
	// Root(Chain()), not Chain(): the two are the same path -- rootPath
	// proxies all seven operations -- but Chain().String() is the empty
	// string, which is not a selector this parser reads back. Printing
	// output we cannot parse is the same defect as issue #566 in a
	// different spot, and this spelling is also what ArgsToPath builds
	// for an empty step list.
	return Root(Chain(paths...)), nil
}

// selectorPaths runs the scan and returns the FLAT leaf steps a selector
// names, in order, with no Root or Chain around them.
//
// It is the whole of the parsing; ParseSelector assembles a Path from what
// it returns and SelectorSteps renders the same steps as lisp values. One
// scan behind both, so the two surfaces cannot disagree about a grammar
// they both claim to implement.
//
// The steps are flat by construction: each scan yields a single leaf --
// Dot, Index, Iter or Range -- and all nesting happens later, in Chain and
// normalizePaths.
//
// The remainder is TRIMMED between steps, with strings.TrimSpace rather than
// with the narrow blank set the readers use; see the note on the two
// whitespace sets. Trimming is also what ends the loop, so a selector whose
// tail is whitespace is complete rather than stalled.
//
// The step slice is PRESIZED, and that is not a micro-optimisation looking
// for a problem. Splitting this scan out of ParseSelector turned the slice
// into a return value, so escape analysis can no longer keep its growth off
// the heap: a three-step selector went from one allocation to three (cap
// 1, 2, 4), which the benchmark gate reported as +11.8% allocs/op and
// +6.9% B/op on BenchmarkParseSelector/practical/dot. Presizing removes
// the growth outright rather than restoring the old escape, so the cost no
// longer depends on where the slice is built. It also improves the long
// arms, which paid log(n) growth allocations before the split as well.
func selectorPaths(selector string) ([]Path, error) {
	selector = strings.TrimSpace(selector)
	if selector == "" {
		return nil, errors.New("selector missing")
	}
	if !strings.HasPrefix(selector, ".") {
		return nil, errors.New("selector must start with '.'")
	}
	if selector == "." {
		return nil, nil
	}
	rest := selectorBody(selector)
	paths := make([]Path, 0, stepCapHint(rest))

	for {
		rest = strings.TrimSpace(rest)
		if rest == "" {
			return paths, nil
		}
		n, path, err := scanStep(rest)
		if err != nil {
			return nil, err
		}
		if n == 0 {
			// Nothing at the head of rest is a step, so another round
			// would consume nothing and loop forever.
			return nil, fmt.Errorf("failed to parse: %s%s", rest, keySpellingHint(rest))
		}
		paths = append(paths, path)
		rest = rest[n:]
	}
}

// selectorBody applies the ".[x]" special case -- a selector may lead with
// a bracket, which is closer to jq -- and returns the string the step scan
// actually consumes: ".[0].a" scans as "[0].a", ".a.b" as itself.
//
// It is a function rather than four inline lines because stepCapHint has to
// size the same string the scan walks, and computing the rule twice would put
// back an allocation this presizing exists to remove. One call, one
// definition of the rule.
//
// THE NEWLINE IS LOAD-BEARING and is a wart, pinned by
// TestSelectorBodyStopsAtANewline. A bracket-led selector is cut at its first
// newline, so ".[0]\n.a" is the path ".[0]" and the rest is DISCARDED rather
// than parsed or rejected. That is what the regexp this replaced did (its
// ".*" did not match a newline) and selectors are not written across lines,
// so the behaviour is kept deliberately rather than quietly widened: a
// selector that means one thing here and another downstream would be worse
// than a wart that means the same thing in both places.
//
// It is a WART and not merely a quirk, which is why selectorBodyCut exists
// beside it: the discarded tail is silent, and silence is the dangerous part.
// parse-path -- new surface, with no downstream parity to keep -- refuses
// such a selector rather than answering with the prefix. See BuiltinParsePath.
func selectorBody(selector string) string {
	body, _ := selectorBodyCut(selector)
	return body
}

// selectorBodyCut is selectorBody plus the fact selectorBody throws away:
// the tail the leading-bracket rule DISCARDED, or "" when it discarded
// nothing.
//
// One function so the rule has one definition. A caller that wants to reject
// a cut selector rather than parse its prefix must TrimSpace first, as
// selectorPaths does -- otherwise a trailing "\n" reads as a discarded tail
// when it is only trailing whitespace.
func selectorBodyCut(selector string) (body, discarded string) {
	if selector == "" || selector[0] != '.' {
		return selector, ""
	}
	i := skipBlank(selector, 1)
	if i >= len(selector) || selector[i] != '[' {
		return selector, ""
	}
	rest := selector[i:]
	if nl := strings.IndexByte(rest, '\n'); nl >= 0 {
		return rest[:nl], rest[nl+1:]
	}
	return rest, ""
}

// stepCapHint is an UPPER BOUND on the number of steps a selectorBody can
// name, used to size the step slice so the scan never grows it.
//
// Two bounds, and it takes the smaller:
//
//   - every step begins with "." or "[", so the count of those two bytes
//     cannot be less than the number of steps. It is EXACT for the shapes
//     callers actually write (".a.b.c" -> 3, ".items[0].id" -> 3,
//     "[\"first name\"].address.city" -> 3, ".items[1:3]" -> 2) and
//     over-counts only when a quoted key CONTAINS one of the two:
//     ["a.b"] hints 2 for one step.
//   - every step spends at least two bytes -- ".a" and "[]" are the
//     shortest -- so (len+1)/2 is a bound too, and it is the one that caps
//     a pathological quoted key: ["........"] hints 5, not 9.
//
// Being an over-estimate costs a little unused capacity on one short-lived
// slice; being an UNDER-estimate would only cost a growth, so neither
// direction is a correctness question. The two tests pin the two halves
// that DO matter -- upper bound, and tight on real selectors.
func stepCapHint(body string) int {
	hint := strings.Count(body, ".") + strings.Count(body, "[")
	if maxSteps := (len(body) + 1) / 2; hint > maxSteps {
		hint = maxSteps
	}
	return hint
}

// SelectorSteps translates a jq-style selector string into the positional
// path steps the ? family takes, as ordinary lisp values.
//
//	SelectorSteps(`.users[0]["full name"]`)  =>  "users", 0, "full name"
//	SelectorSteps(".items[].id")             =>  "items", '*, "id"
//	SelectorSteps(".items[1:3]")             =>  "items", '(range 1 3)
//	SelectorSteps(".items[1:]")              =>  "items", '(range 1)
//	SelectorSteps(".")                       =>  no steps
//
// It exists so a path that ARRIVES AS A STRING -- from inside a document, a
// client request, or a persisted envelope -- can be converted once and then
// applied many times through the positional API, instead of being re-parsed
// on every operation. The identity selector yielding no steps is what makes
// that uniform: applying an empty step list is the identity, exactly as
// (? obj) is.
//
// It shares selectorPaths with ParseSelector, so the two agree on the
// grammar by construction rather than by test -- including the newline wart
// described there, which this function therefore also has: a bracket-led
// selector is CUT at its first newline and the tail is dropped in silence.
// The agreement is asserted (TestSelectorGrammarPathologies, and
// FuzzParseSelector on every input it accepts), so the strictness that wart
// needs lives one layer up, in BuiltinParsePath, rather than being bolted on
// to one of the two functions. A Go caller wanting it should reject selectors
// containing a newline before calling.
//
// What the steps mean is checked anyway: TestSelectorStepsMatchParseSelector
// applies both routes to documents and requires the same answer.
//
// The open-ended range is why this can be lossless. Before it had a step
// spelling, "[1:]" had no positional form, so a conversion would have
// silently dropped or mis-rendered exactly the selectors that persist.
func SelectorSteps(selector string) ([]*lisp.LVal, error) {
	paths, err := selectorPaths(selector)
	if err != nil {
		return nil, err
	}
	steps := make([]*lisp.LVal, 0, len(paths))
	for _, p := range paths {
		step, err := pathToStep(p)
		if err != nil {
			return nil, err
		}
		steps = append(steps, step)
	}
	return steps, nil
}

// pathToStep renders one leaf step as the lisp value argToStep parses back.
//
// It is total over what selectorPaths can produce and deliberately has no
// default that guesses: a leaf type reaching here unhandled is a grammar
// that grew a step this cannot express, and saying so beats emitting
// something that round-trips to a different path.
func pathToStep(p Path) (*lisp.LVal, error) {
	switch v := p.(type) {
	case *dotPath:
		return lisp.String(v.key), nil
	case *indexPath:
		return lisp.Int(v.index), nil
	case *iterPath:
		return lisp.Symbol("*"), nil
	case *rangePath:
		cells := []*lisp.LVal{lisp.Symbol("range"), lisp.Int(v.from)}
		if !v.implicitTo {
			cells = append(cells, lisp.Int(v.to))
		}
		return lisp.QExpr(cells), nil
	default:
		return nil, fmt.Errorf("no path step spelling for %T", p)
	}
}

// keySpellingHint explains the bare-key rule when a stalled parse looks like
// a key that needed bracketing, and says nothing otherwise.
//
// It is APPENDED to the existing "failed to parse: %s" message rather than
// replacing it, so anything matching on that text -- a handler-bind in a
// phylum, a test -- still matches. The message is the only feedback a caller
// gets, and paths often arrive as data rather than being typed by hand, so
// "failed to parse: -type" on a client-supplied .content-type otherwise
// names neither the rule nor the fix.
//
// It stays quiet when the stall is a bracket or separator problem, where a
// key-spelling explanation would be actively misleading: ".[" is a malformed
// bracket, not a badly spelled key.
//
// WHICH RUNE IT JUDGES is the whole of the function, and getting it wrong
// silences the hint on the cases that need it most. The scan stalls at the
// first rune that could not begin a step, which is the first rune of rest
// EXCEPT after a leading "." -- ".9lead" stalls whole, where ".my-key" stalls
// at the "-" alone -- and scanDotKey skips blanks after that dot, so ". -key"
// and ".\t9lead" stall on the "-" and the "9" as surely as their unspaced
// spellings do. Reading rest[1] instead of the first non-blank rune left
// exactly those cases unexplained.
func keySpellingHint(rest string) string {
	at := 0
	if len(rest) > 0 && rest[0] == '.' {
		at = skipBlank(rest, 1)
	}
	if at >= len(rest) {
		return ""
	}
	bad, _ := utf8.DecodeRuneInString(rest[at:])
	// Say nothing unless that rune is one a bare key could NOT contain.
	// A stall on a rune that CAN start a key is a different mistake --
	// `.["a"]foo` stalls at "foo", which is a perfectly good key name; what
	// is missing there is the dot. Explaining the key rule would send the
	// reader after the wrong thing.
	if bad == '_' || (bad >= 'a' && bad <= 'z') || (bad >= 'A' && bad <= 'Z') {
		return ""
	}
	// Nor when the stall is punctuation that means something else in this
	// grammar. "?" is here because it is a STEP SUFFIX: ".a??" stalls on the
	// second one, which is a stray option mark rather than a key that needed
	// bracketing, and the key rule would be a wrong answer to it.
	switch bad {
	case '[', ']', '.', '"', ':', '?':
		return ""
	}
	// Whitespace is never a key-spelling problem either. skipBlank leaves
	// only the exotic kinds here -- a vertical tab, a non-breaking space --
	// which the scan trims between steps but does not accept inside one.
	if unicode.IsSpace(bad) {
		return ""
	}
	return ` (a bare key must match [A-Za-z_][A-Za-z_0-9]*; ` +
		`bracket and quote anything else, e.g. .["my-key"])`
}
