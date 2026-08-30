// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"strings"

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
// WHERE IT CAME FROM, AND WHY IT IS HERE NOW (issue #564). This parser spent
// its life in luthersystems/substrate, alongside the deprecated jq-string
// builtins (get-path, set-path!, ...) that are its only lisp-visible
// consumer. Those builtins stay downstream -- they are deprecated in favour
// of the positional-step family in query.go, and taking them would
// re-introduce retired surface here. The parser is a different thing: every
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

var (
	preprocPath = regexp.MustCompile(`^\.\s*(\[.*)`)

	reArray    = regexp.MustCompile(`^\[\s*(-?\d+)?(\s*:\s*(-?\d+)?)?\s*]\s*(\?)?`)
	reArrayKey = regexp.MustCompile(`^\[\s*("(?:\\.|[^"\\])*")\s*\]\s*(\?)?`)
	reDotKey   = regexp.MustCompile(`^\.\s*([A-Za-z_][A-Za-z_0-9]*)\s*(\?)?`)
)

// parseArray parses the bracket forms that are not a quoted key: the index
// "[n]", the two range spellings, and the iterator "[]".
//
// It returns the unconsumed remainder of path and the step it produced, or
// (path, nil, nil) when the head of path is not one of these forms -- a
// no-match is not an error, because the caller tries the three parsers in
// turn.
//
// IMPORTANT: whether ":" is present is what separates an index from a range,
// and it is read off match[0] rather than off a capture group. "[0]" and
// "[0:]" both capture "0" as the from; only the raw match distinguishes
// them.
func parseArray(path string) (string, Path, error) {
	match := reArray.FindStringSubmatch(path)
	if match == nil {
		return path, nil, nil
	}
	isRange := strings.Contains(match[0], ":")
	fromStr := match[1]
	var err error
	var from int
	if fromStr == "" {
		from = 0
	} else {
		from, err = strconv.Atoi(fromStr)
	}
	if err != nil {
		// Reached by an index too large for an int, which the regexp
		// matches happily: "[99999999999999999999]".
		return path, nil, fmt.Errorf("fail to parse array index: %s", fromStr)
	}
	newPath := path[len(match[0]):]
	toStr := match[3]
	if !isRange {
		if fromStr == "" && toStr == "" {
			return newPath, Iter(), nil
		}
		if toStr == "" {
			return newPath, Index(from), nil
		}
	}
	implicitTo := toStr == ""
	var to int
	if implicitTo {
		// to is meaningless when implicitTo is set: validateRange
		// overwrites it with the document length. Zero, not the from, so
		// that a mis-set flag degrades to an empty slice rather than to a
		// plausible-looking wrong one.
		to = 0
	} else {
		to, err = strconv.Atoi(toStr)
	}
	if err != nil {
		return path, nil, fmt.Errorf("fail to parse second array index: %s", toStr)
	}

	return newPath, Range(from, to, implicitTo), nil
}

// parseArrayKey parses a map key given as a quoted string inside brackets,
// which is the only spelling for a key that is not a bare identifier:
// ["$private"], ["x y"], ["\"\n"].
//
// The literal is decoded with strconv.Unquote, so the escapes are Go's --
// and, since dotPath.String() renders a key with %q, the pair round-trips.
func parseArrayKey(path string) (string, Path, error) {
	match := reArrayKey.FindStringSubmatch(path)
	if match == nil {
		return path, nil, nil
	}
	key := match[1]
	newPath := path[len(match[0]):]
	key, err := strconv.Unquote(key)
	if err != nil {
		// The empty remainder is inconsistent with the other two parsers,
		// which return `path` untouched on error. It is harmless because
		// ParseSelector returns on a non-nil error before looking at the
		// remainder, and it is left as it was so that this stays a port.
		return "", nil, err
	}

	return newPath, Dot(key), nil
}

// parseDotKey parses a bare key: ".foo". The identifier rule is deliberately
// narrow -- ".0" and ".$private" are parse errors, not keys -- because a
// looser rule would swallow the leading dot of a following selector. Such
// keys are reachable through the quoted form.
func parseDotKey(path string) (string, Path, error) {
	match := reDotKey.FindStringSubmatch(path)
	if match == nil {
		return path, nil, nil
	}
	key := match[1]
	newPath := path[len(match[0]):]

	return newPath, Dot(key), nil
}

// parsers define all the individual path parsers.
//
// The order is NOT load-bearing, which is worth knowing because the first
// two both anchor on "[" and look as though it would be. reArray's groups
// are all optional but its closing "]" is not, so it cannot match the head
// of a quoted key. TestSelectorRegexpsDoNotOverlap asserts that
// non-overlap over the forms each regexp is meant to claim -- a finite
// list, not a proof over all strings -- and, measured, permuting this
// slice leaves the whole suite green. Anything added here has to keep the
// three disjoint, because the loop below appends EVERY match in a round
// rather than stopping at the first: two parsers matching one round would
// silently emit two steps for one selector.
var parsers = []func(string) (string, Path, error){parseArray, parseArrayKey, parseDotKey}

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
// state and the compiled regexps are safe for concurrent use, so this may
// be called from any goroutine.
//
// The Path it returns is an ordinary one: nothing distinguishes a parsed
// path from one assembled by hand or by ArgsToPath, and the same seven
// operations apply. A caller that is about to hand a document to one of them
// should run okSimpleType over the document first, which is what the
// builtins do -- see that function for why it is not optional.
//
// ONE WART. The jq optional-selector suffix "?" is ACCEPTED AND DISCARDED.
// In jq, ".a?" suppresses the error a non-object .a would raise; here all
// three regexps capture the "?" and no caller reads the group, so ".a?" is
// exactly ".a" and the error is raised. Nothing in the engine implements
// error-suppressing steps, so honouring the suffix would be a feature, not a
// fix.
//
// Two properties worth stating because they are easy to break:
//
// The identity selector "." returns Root(Chain()), which prints "." and
// matches what ArgsToPath builds for an empty step list. Chain() alone
// behaves identically -- rootPath proxies all seven operations and adds only
// a leading "." to String() -- but prints the empty string, which this
// parser cannot read back. TestParseSelectorRootSpelling pins the agreement.
//
// reArrayKey's quoted-key body is `(?:\\.|[^"\\])*`: an escape SEQUENCE, or
// any character that is neither a quote nor a backslash -- the ordinary
// string-literal grammar. Spelling it `(?:\"|[^"])*` instead makes the
// regexp engine read `\"` as a plain escaped quote, so the alternation
// becomes `"` OR `not "` -- every character -- and the group runs greedily
// to the last quote in the selector, which limits a selector to ONE
// bracketed key and makes String()'s own output unreadable, since it
// brackets every map key. TestParseSelectorTwoQuotedKeys covers the grammar
// and the round-trip test carries multi-key selectors.
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
// The steps are flat by construction: each parser yields a single leaf --
// Dot, Index, Iter or Range -- and all nesting happens later, in Chain and
// normalizePaths.
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
	selector = selectorBody(selector)
	paths := make([]Path, 0, stepCapHint(selector))

	var path Path
	var err error
	for len(selector) > 0 {
		origLen := len(selector)
		// Every parser runs every round, and a round that matches more
		// than one appends more than one step. There is no break: a
		// selector like `.a[0]` is consumed by parseDotKey and then, in
		// the same round, by parseArray.
		for _, parser := range parsers {
			selector, path, err = parser(selector)
			selector = strings.TrimSpace(selector)
			if err != nil {
				return nil, err
			}
			if path == nil {
				continue
			}
			paths = append(paths, path)
		}
		if len(selector) == origLen {
			// no progress was made, abort since another round
			// will result in an infinite loop
			return nil, fmt.Errorf("failed to parse: %s%s", selector, keySpellingHint(selector))
		}
	}

	return paths, nil
}

// selectorBody applies the ".[x]" special case -- a selector may lead with
// a bracket, which is closer to jq -- and returns the string the step scan
// actually consumes: ".[0].a" scans as "[0].a", ".a.b" as itself.
//
// It is a function rather than four inline lines because stepCapHint has to
// size the same string the scan walks, and running preprocPath a second
// time to find that out would put back an allocation this presizing exists
// to remove. One call, one definition of the rule.
func selectorBody(selector string) string {
	if match := preprocPath.FindStringSubmatch(selector); match != nil {
		return match[1]
	}
	return selector
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
// grammar by construction rather than by test. What the steps mean is
// checked anyway: TestSelectorStepsMatchParseSelector applies both routes to
// documents and requires the same answer.
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
func keySpellingHint(rest string) string {
	r := []rune(rest)
	if len(r) == 0 {
		return ""
	}
	// The offending rune is the first one, except after a leading "." --
	// ".9lead" stalls whole, where ".my-key" stalls at the "-" alone.
	bad := r[0]
	if bad == '.' && len(r) > 1 {
		bad = r[1]
	}
	// Say nothing unless that rune is one a bare key could NOT contain.
	// A stall on a rune that CAN start a key is a different mistake --
	// `.["a"]foo` stalls at "foo", which is a perfectly good key name; what
	// is missing there is the dot. Explaining the key rule would send the
	// reader after the wrong thing.
	if bad == '_' || (bad >= 'a' && bad <= 'z') || (bad >= 'A' && bad <= 'Z') {
		return ""
	}
	switch bad {
	case '[', ']', '.', '"', ':', ' ', '\t', '\n':
		return ""
	}
	return ` (a bare key must match [A-Za-z_][A-Za-z_0-9]*; ` +
		`bracket and quote anything else, e.g. .["my-key"])`
}
