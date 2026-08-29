// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// THE JQ-STRING SELECTOR FRONT END
//
// ParseSelector translates a jq-style selector string -- ".a.b[0][\"x y\"]
// [1:3]" -- into this package's exported Path constructors. It is a Go-level
// API only: nothing here is reachable from lisp, and no builtin calls it.
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
// NO FUZZ TARGET YET, and the reason is not the parser. The obvious target
// -- generate a selector, assert that ParseSelector terminates -- goes red on
// its first campaign against code that is already shipped: Chain (path.go,
// via normalizePaths and its mutual recursion with Iter) is EXPONENTIAL in
// the number of iterator steps, so a 49-byte selector of 24 "[]"s takes 4.8s
// to construct and each further "[]" doubles it. That is reachable from the
// positional API too -- ArgsToPath of 24 '* steps costs the same 4.7s -- so
// it is an engine defect this parser only makes easier to type, and a
// watchdog target here would report it as a parser hang. Filed as issue
// #565; the target belongs with the fix.

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
//	.          => Chain()            the whole document (see the wart below)
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
// should run OKSimpleType over the document first, which is what the
// builtins do -- see that function for why it is not optional.
//
// THREE WARTS, all preserved deliberately because this is a port of a
// parser with live downstream callers, not a rewrite.
//
// The identity selector "." returns Chain() and NOT Root(Chain()). The two
// behave identically -- rootPath proxies all seven operations to the path it
// wraps and adds nothing but a leading "." in String() -- so the only
// observable difference is that ParseSelector(".").String() is "", which is
// not a selector that parses back. ArgsToPath spells the same path
// Root(Chain()), whose String() is ".". TestParseSelectorRootSpelling pins
// this so it stays a decision rather than becoming an accident.
//
// reArrayKey's quoted-key body is `(?:\\.|[^"\\])*` -- an escape SEQUENCE,
// or any character that is neither a quote nor a backslash: the ordinary
// string-literal grammar. Written `(?:\"|[^"])*`, as it was until issue
// #566, the regexp engine reads `\"` as a plain escaped quote, so the
// alternation is `"` OR `not "`, which is every character. The group ran
// greedily to the last quote in the selector, `.["a"]["b"]` captured
// `"a"]["b"`, and strconv.Unquote rejected the interior quote -- so a
// selector could carry at most ONE bracketed key.
//
// That was not only an input restriction. String() renders every map key
// bracketed, so `.a.b` printed as `.["a"]["b"]`: this parser emitting
// output it could not read back. TestParseSelectorTwoQuotedKeys covers the
// grammar, and the round-trip test now carries the two-key selectors it
// previously had to exclude.
//
// The jq optional-selector suffix "?" is ACCEPTED AND DISCARDED. In jq,
// ".a?" suppresses the error a non-object .a would raise; here all three
// regexps capture the "?" and no caller reads the group, so ".a?" is exactly
// ".a" and the error is raised. Nothing in the engine implements
// error-suppressing steps, so honouring the suffix would be a feature, not a
// fix.
func ParseSelector(selector string) (Path, error) {
	// handle special case where first op is of form ".[x]"
	// we support this notation to be closer to jq.
	selector = strings.TrimSpace(selector)
	if selector == "" {
		return nil, errors.New("selector missing")
	}
	if !strings.HasPrefix(selector, ".") {
		return nil, errors.New("selector must start with '.'")
	}
	var paths []Path
	if selector == "." {
		return Chain(paths...), nil
	}
	match := preprocPath.FindStringSubmatch(selector)
	if match != nil {
		selector = match[1]
	}

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
			return nil, fmt.Errorf("failed to parse: %s", selector)
		}
	}

	return Root(Chain(paths...)), nil
}
