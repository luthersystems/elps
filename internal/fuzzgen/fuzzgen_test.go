// Copyright © 2026 The ELPS authors

package fuzzgen_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzgen"
)

// formProductions is the arity of the switch in generator.form.  A seed per
// production is what makes the corpus reach every construct on the very first
// pass instead of waiting for the mutator to stumble onto one; if the switch
// grows and this does not, TestSeedScriptsReachEveryProduction says so.
const formProductions = 24

// seedScripts returns the f.Add corpus.
//
// The entries are SCRIPTS, not ELPS source.  Generation is a pure function of
// the script, so the script is the reproducible artifact -- committing source
// would leave the fuzzing engine mutating text it does not actually consume.
//
// Layout of the first three bytes, from generator.program and generator.form:
// byte 0 chooses whether a hash-bang is written (choose(6)), byte 1 the
// top-level comment shape (choose(10)), byte 2 the production in form
// (choose(24)).  The systematic block below therefore lands on each production
// in turn, and the tail gives it script to expand into.
func seedScripts() [][]byte {
	scripts := [][]byte{
		nil,
		{},
		{0},
		{255},
		{0, 0, 0, 0, 0, 0, 0, 0},
		{255, 255, 255, 255, 255, 255, 255, 255},
		[]byte("\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f"),
		[]byte("elps fmt round trip"),
		[]byte(";;;;;;;;;;;;;;;;"),
		[]byte("''''''''''''''''"),
		// A shebang (byte 0 divisible by 6) followed by prefix-form material:
		// the shape whose comment handling broke the parser's own output.
		{0, 0, 10, 3, 1, 0, 10, 3, 1},
		{0, 1, 10, 7, 16, 10, 3, 2, 9, 4},
		// Long tails of one repeated byte drive a single production to its
		// depth or byte bound.
		bytes.Repeat([]byte{13}, 64),
		bytes.Repeat([]byte{9}, 64),
		bytes.Repeat([]byte{4}, 64),
		bytes.Repeat([]byte{16}, 64),
	}
	for k := range formProductions {
		s := []byte{1, 9, byte(k)}
		for i := range 24 {
			s = append(s, byte(i*7+k))
		}
		scripts = append(scripts, s)
	}
	return scripts
}

// lcgScripts returns n deterministic pseudo-random scripts.  A fixed linear
// congruential generator rather than math/rand so the sample is identical on
// every platform and every Go release: a sweep that silently changes shape is
// not a gate.
func lcgScripts(n int, seed uint64) [][]byte {
	state := seed | 1
	next := func() uint64 {
		state = state*6364136223846793005 + 1442695040888963407
		return state >> 33
	}
	out := make([][]byte, 0, n)
	for range n {
		script := make([]byte, next()%400)
		for i := range script {
			script[i] = byte(next())
		}
		out = append(out, script)
	}
	return out
}

// TestGeneratedProgramsAlwaysParse pins fuzzgen's central contract.  Every
// assertion in FuzzGeneratedPipeline rests on it: without it a rejected
// program cannot be told apart from the ordinary garbage a raw-byte fuzz
// target feeds the parser, and the two lexer defects that showed up as
// "the parser refused input the language accepts" would be invisible.
//
// It also protects the fuzz target from fuzzgen itself.  A generator bug
// reports as a crash in FuzzGeneratedPipeline; this test catches it in `go
// test` instead, where the failure names the generator.
func TestGeneratedProgramsAlwaysParse(t *testing.T) {
	scripts := append(seedScripts(), lcgScripts(6000, 1)...)
	for _, lim := range limitProfiles {
		for _, script := range scripts {
			src := fuzzgen.GenerateLimited(script, lim)
			if _, err := parseProgram(src); err != nil {
				t.Fatalf("generated program does not parse (limits %+v, script %q): %v\n--- source ---\n%s",
					lim, script, err, src)
			}
		}
	}
}

// TestGenerateIsDeterministic pins the property the corpus depends on: a
// crasher recorded as a script has to render the same source on every replay,
// on every machine.
func TestGenerateIsDeterministic(t *testing.T) {
	for _, script := range append(seedScripts(), lcgScripts(500, 7)...) {
		first := fuzzgen.Generate(script)
		for range 3 {
			if again := fuzzgen.Generate(script); !bytes.Equal(first, again) {
				t.Fatalf("Generate is not deterministic for script %q:\n%q\n%q", script, first, again)
			}
		}
	}
}

// TestGenerateRespectsLimits checks the three bounds.  A generator that can
// emit an unbounded program hands the mutator a seed costing seconds per
// execution, which is how a coverage-guided run stops making progress.
//
// The byte budget is a stopping condition rather than a hard cap: generation
// refuses to open new constructs past it, then finishes the construct in
// flight and closes its brackets.  The slack allowed here covers the largest
// single token fuzzgen writes (a 400-byte literal), one closing bracket per
// level of nesting, and the tail of one special form.
func TestGenerateRespectsLimits(t *testing.T) {
	for _, lim := range limitProfiles {
		slack := 512 + 4*lim.MaxDepth
		for _, script := range lcgScripts(3000, 3) {
			src := fuzzgen.GenerateLimited(script, lim)
			if len(src) > lim.MaxBytes+slack {
				t.Fatalf("limits %+v: generated %d bytes, budget %d + %d slack (script %q)",
					lim, len(src), lim.MaxBytes, slack, script)
			}
			if d := maxParenDepth(src); d > lim.MaxDepth+8 {
				t.Fatalf("limits %+v: nesting depth %d exceeds cap %d (script %q)\n%s",
					lim, d, lim.MaxDepth, script, src)
			}
		}
	}
}

// maxParenDepth measures bracket nesting outside string literals and comments.
// It is deliberately approximate -- the point is to catch a generator that has
// stopped bounding depth at all, not to re-implement the lexer.
func maxParenDepth(src []byte) int {
	depth, maxDepth := 0, 0
	inString, inRaw, inComment := false, false, false
	for i := 0; i < len(src); i++ {
		c := src[i]
		switch {
		case inComment:
			if c == '\n' {
				inComment = false
			}
		case inRaw:
			if c == '"' && i+2 < len(src) && src[i+1] == '"' && src[i+2] == '"' {
				inRaw = false
				i += 2
			}
		case inString:
			if c == '\\' {
				i++
			} else if c == '"' {
				inString = false
			}
		case c == ';':
			inComment = true
		case c == '"':
			if i+2 < len(src) && src[i+1] == '"' && src[i+2] == '"' {
				inRaw = true
				i += 2
			} else {
				inString = true
			}
		case c == '(' || c == '[':
			depth++
			maxDepth = max(maxDepth, depth)
		case c == ')' || c == ']':
			depth--
		}
	}
	return maxDepth
}

// TestSeedScriptsReachEveryProduction fails when a production is added to
// generator.form without a seed that lands on it.  A construct nothing seeds
// is one the mutator has to discover by chance, which for a 24-way switch
// three bytes into the script it largely will not.
func TestSeedScriptsReachEveryProduction(t *testing.T) {
	seen := make(map[int]bool)
	for _, script := range seedScripts() {
		if len(script) >= 3 && script[0]%6 != 0 && script[1]%10 >= 4 {
			seen[int(script[2])%formProductions] = true
		}
	}
	for k := range formProductions {
		if !seen[k] {
			t.Errorf("no seed script reaches generator.form production %d", k)
		}
	}
}

// TestGeneratedProgramsCoverTheGrammar guards against the generator quietly
// losing a construct -- a table renamed, a switch arm folded away -- which
// would leave the pipeline target running at full speed over a grammar with a
// hole in it.  The markers are the ones the whole exercise is about: reader
// macros, both bracket kinds, keyword and package-qualified symbols, unicode
// identifiers, exponents, comments, oversized tokens.
func TestGeneratedProgramsCoverTheGrammar(t *testing.T) {
	markers := map[string]string{
		"funref":            "#'",
		"unbound":           "#^",
		"hash-bang":         "#!",
		"quote":             "'",
		"paren":             "(",
		"brace":             "[",
		"keyword-symbol":    ":a",
		"qualified-symbol":  "lisp:car",
		"unicode-symbol":    "日本語",
		"float-exponent":    "e5",
		"negative-literal":  "-1",
		"hex-literal":       "#x",
		"octal-literal":     "#o",
		"raw-string":        `"""`,
		"escaped-backslash": `\\`,
		"comment":           ";",
		"quasiquote":        "quasiquote",
		"unquote-splicing":  "unquote-splicing",
		"huge-token":        strings.Repeat("z", 300),
		"deep-nesting":      "((((",
	}
	var all strings.Builder
	for _, script := range append(seedScripts(), lcgScripts(4000, 11)...) {
		all.Write(fuzzgen.Generate(script))
		all.WriteByte('\n')
	}
	corpus := all.String()
	for name, marker := range markers {
		if !strings.Contains(corpus, marker) {
			t.Errorf("generated corpus never contains %s (%q)", name, marker)
		}
	}
}
