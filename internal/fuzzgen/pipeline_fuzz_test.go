// Copyright © 2026 The ELPS authors

package fuzzgen_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/formatter"
	"github.com/luthersystems/elps/internal/fuzzgen"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/minifier"
	"github.com/luthersystems/elps/parser/lexer"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// FuzzGeneratedPipeline drives the whole source-rewriting pipeline --
//
//	parse -> format -> re-parse -> format again -> minify -> re-parse ->
//	format the minified output -> re-parse
//
// -- over programs that are syntactically valid by construction.  Together
// `elps fmt` and `elps minify` are the two commands that REWRITE a customer's
// source, and downstream (luthersystems/substrate) the minified output IS the
// chaincode that gets uploaded and executed.  A defect anywhere in that chain
// is a silent edit to someone's program.
//
// Every assertion below restates a property the repository already tests for
// fixed inputs, generalised to arbitrary generated input.  Nothing is asserted
// about how a program is laid out, which names the minifier picks, or what any
// of it MEANS -- those belong to the unit tests, and asserting them here would
// manufacture crashes out of legitimate formatting choices.
//
// The one genuinely new assertion is the first: the generator's own contract
// that its output parses.  Without it a rejected program is indistinguishable
// from the millions of rejections a raw-byte target sees every second, and
// neither of the two lexer defects this target found -- "1e5" and a string
// ending in a backslash, both of them VALID input the lexer refused -- would
// have been visible.  A failure of that assertion is a defect in the parser or
// in fuzzgen, and triage has to consider both; TestGeneratedProgramsAlwaysParse
// sweeps a large deterministic sample so a generator defect is normally caught
// by `go test` first.

// maxCommentTokens bounds the comment scan.  The lexer answers forever once
// wedged, so the loop needs its own bound; generated programs are capped at a
// couple of KiB, which cannot hold anywhere near this many tokens.
const maxCommentTokens = 1 << 16

// limitProfiles give the mutator a knob for the SHAPE of a program
// independently of its content: the same script can be rendered deep and
// narrow or shallow and wide.  All of them are small -- the oversized inputs
// live in fuzzseed.Pathological, which unit tests run once each.
var limitProfiles = []fuzzgen.Limits{
	{MaxBytes: 2048, MaxDepth: 10, MaxForms: 10}, // the default shape
	{MaxBytes: 256, MaxDepth: 3, MaxForms: 4},    // small and shallow
	{MaxBytes: 4096, MaxDepth: 40, MaxForms: 2},  // deep and narrow
	{MaxBytes: 4096, MaxDepth: 2, MaxForms: 40},  // flat and wide
}

// programString renders a parsed program as formatter_test.go's roundTripEqual
// does -- LVal.String() per top-level form -- so this target and the table
// tests agree on what "the same AST" means.  ParseProgram rather than a Parse
// loop, because it is what lisp.Reader.Read calls and the only entry point
// that consumes a leading hash-bang.
func programString(src []byte) (string, error) {
	p := rdparser.New(token.NewScanner("fuzzgen", bytes.NewReader(src)))
	exprs, err := p.ParseProgram()
	if err != nil {
		return "", err
	}
	parts := make([]string, 0, len(exprs))
	for _, expr := range exprs {
		parts = append(parts, expr.String())
	}
	return strings.Join(parts, "\n"), nil
}

// skeleton renders a program's structure with every symbol, string and number
// replaced by its type name, so two programs compare equal iff they have the
// same shape.  Minification renames identifiers, which is exactly what a
// skeleton ignores.
func skeleton(exprs []*lisp.LVal) string {
	var b strings.Builder
	for _, expr := range exprs {
		writeSkeleton(&b, expr)
		b.WriteByte('\n')
	}
	return b.String()
}

func writeSkeleton(b *strings.Builder, v *lisp.LVal) {
	if v == nil {
		b.WriteString("<nil>")
		return
	}
	if v.Quoted {
		b.WriteByte('\'')
	}
	b.WriteString(v.Type.String())
	if len(v.Cells) == 0 {
		return
	}
	b.WriteByte('(')
	for i, c := range v.Cells {
		if i > 0 {
			b.WriteByte(' ')
		}
		writeSkeleton(b, c)
	}
	b.WriteByte(')')
}

func parseProgram(src []byte) ([]*lisp.LVal, error) {
	p := rdparser.New(token.NewScanner("fuzzgen", bytes.NewReader(src)))
	return p.ParseProgram()
}

// commentTexts returns the text of every comment token in src, in order.  A
// hash-bang lexes as HASH_BANG plus a COMMENT holding the rest of the line, so
// both types count.
//
// This is the only way to see a dropped comment.  Comments are not part of the
// AST, so `elps fmt` deleting one leaves programString unchanged AND leaves
// Format idempotent -- the formatted output is a stable fixed point that is
// simply missing a line of the user's file.  Four separate defects presented
// exactly that way.
func commentTexts(src []byte) []string {
	lx := lexer.New(token.NewScanner("fuzzgen", bytes.NewReader(src)))
	out := []string{}
	for range maxCommentTokens {
		for _, tok := range lx.ReadToken() {
			switch tok.Type {
			case token.COMMENT, token.HASH_BANG:
				out = append(out, tok.Text)
			case token.EOF, token.ERROR, token.INVALID:
				return out
			default:
				// Every other token type is irrelevant here.
			}
		}
	}
	return out
}

// interiorBlankLines counts blank lines strictly between the first and last
// non-blank lines of src.  Leading and trailing blank runs are excluded so the
// count survives the formatter's trailing-newline normalisation, which is not
// what this is measuring.
func interiorBlankLines(src []byte) int {
	lines := strings.Split(string(src), "\n")
	first, last := -1, -1
	for i, l := range lines {
		if strings.TrimSpace(l) != "" {
			if first < 0 {
				first = i
			}
			last = i
		}
	}
	if first < 0 {
		return 0
	}
	n := 0
	for _, l := range lines[first : last+1] {
		if strings.TrimSpace(l) == "" {
			n++
		}
	}
	return n
}

func sameStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func FuzzGeneratedPipeline(f *testing.F) {
	for _, script := range seedScripts() {
		for shape := range limitProfiles {
			f.Add(script, uint8(shape)) //nolint:gosec // G115: bounded by len(limitProfiles)
		}
	}
	f.Fuzz(func(t *testing.T, script []byte, shape uint8) {
		src := fuzzgen.GenerateLimited(script, limitProfiles[int(shape)%len(limitProfiles)])

		// (1) The generator's contract.  See the package comment on fuzzgen:
		// a failure here is a parser defect or a generator defect.
		before, err := parseProgram(src)
		if err != nil {
			t.Fatalf("generated program does not parse: %v\n--- source ---\n%s", err, src)
		}
		beforeAST, err := programString(src)
		if err != nil {
			t.Fatalf("re-parse of the generated program failed: %v", err)
		}
		beforeComments := commentTexts(src)

		// (2) Format accepts exactly what the parser accepts.  FuzzFormat
		// asserts the other direction (Format succeeded, parser rejected);
		// together they say `elps fmt` can read every file elps can run.
		// Format-preserving mode only ATTACHES metadata to the same tree the
		// standard parser builds -- FuzzParseFormatting pins that -- so it has
		// no reason of its own to reject anything.
		first, err := formatter.Format(src, nil)
		if err != nil {
			t.Fatalf("Format rejected a program the parser accepted: %v\n--- source ---\n%s", err, src)
		}

		// (3) AST preservation.  formatter_test.go's roundTripEqual asserts
		// this for the repository's .lisp files.
		afterAST, err := programString(first)
		if err != nil {
			t.Fatalf("the parser rejected formatted output: %v\n--- output ---\n%s", err, first)
		}
		if beforeAST != afterAST {
			t.Fatalf("Format changed the AST\n--- before ---\n%s\n--- after ---\n%s\n--- source ---\n%s\n--- output ---\n%s",
				beforeAST, afterAST, src, first)
		}

		// (4) Comment preservation.  Not an AST property, so nothing above
		// can see it.  The default config does not strip comments, and a
		// formatter that deletes one is editing the user's file.
		if afterComments := commentTexts(first); !sameStrings(beforeComments, afterComments) {
			t.Fatalf("Format changed the comments\n--- before ---\n%q\n--- after ---\n%q\n--- source ---\n%s\n--- output ---\n%s",
				beforeComments, afterComments, src, first)
		}

		// (5) Blank lines are never INVENTED.  Every blank line the printer
		// writes comes from a count the parser measured in the source and the
		// printer then clamps to cfg.MaxBlankLines -- blankLinesBefore,
		// writeBlankLinesAfterComments, writeInnerTrailingComments -- so the
		// interior blank-line count can shrink but has no legitimate way to
		// grow.
		//
		// This is the only assertion here that catches a formatter which
		// REFLOWS rather than rewrites.  Inserting a blank line leaves the AST
		// identical, leaves every comment in place, and leaves the output a
		// perfectly stable fixed point, so (3), (4) and (6) all pass: the
		// prefix-form blank-line regression turned "(a)\n; c\n\n#'f\n" into
		// "(a)\n\n; c\n\n#'f\n" and nothing else could see it.  That matters
		// because a comment's POSITION carries meaning -- the `; <-` markers
		// in editors/vscode/test/grammar/basics.lisp are syntax-test
		// assertions about the line above them.
		if b, a := interiorBlankLines(src), interiorBlankLines(first); a > b {
			t.Fatalf("Format invented %d blank line(s) (%d -> %d)\n--- source ---\n%q\n--- output ---\n%q",
				a-b, b, a, src, first)
		}

		// (6) Idempotence.  runFormatTests and TestEdgeIdempotency assert it
		// for fixed inputs; a non-idempotent formatter makes `elps fmt`
		// produce diff churn forever.
		second, err := formatter.Format(first, nil)
		if err != nil {
			t.Fatalf("Format rejected its own output: %v\n--- output ---\n%s", err, first)
		}
		if !bytes.Equal(first, second) {
			t.Fatalf("Format is not idempotent\n--- pass 1 ---\n%q\n--- pass 2 ---\n%q\n--- source ---\n%s",
				first, second, src)
		}

		// (7) Minification.  Its only failure mode is the parse it does
		// itself (minifier.parseFile), so rejecting formatted output that
		// the parser accepts is unambiguously a defect.
		minified, symbols, err := minifier.MinifySource(first, "fuzzgen.lisp", nil)
		if err != nil {
			t.Fatalf("MinifySource rejected formatted output: %v\n--- input ---\n%s", err, first)
		}

		// Determinism, as TestMinifySource_DeterministicAndScopeAware asserts:
		// a non-deterministic minifier makes builds unreproducible and its
		// symbol map useless for decoding a trace.
		minified2, symbols2, err := minifier.MinifySource(first, "fuzzgen.lisp", nil)
		if err != nil {
			t.Fatalf("second MinifySource of the same input failed: %v", err)
		}
		if !bytes.Equal(minified, minified2) {
			t.Fatalf("MinifySource is not deterministic\n--- run 1 ---\n%s\n--- run 2 ---\n%s", minified, minified2)
		}
		if len(symbols.Entries) != len(symbols2.Entries) {
			t.Fatalf("symbol map is not deterministic: %d entries then %d",
				len(symbols.Entries), len(symbols2.Entries))
		}
		for i := range symbols.Entries {
			if symbols.Entries[i] != symbols2.Entries[i] {
				t.Fatalf("symbol map entry %d differs between runs: %+v vs %+v",
					i, symbols.Entries[i], symbols2.Entries[i])
			}
		}

		// (8) The minified program parses and has the shape it started with.
		// Names are what minification is allowed to change, so only the type
		// skeleton is compared -- the same contract FuzzMinifySource asserts.
		after, err := parseProgram(minified)
		if err != nil {
			t.Fatalf("the parser rejected minified output: %v\n--- output ---\n%s", err, minified)
		}
		if got, want := skeleton(after), skeleton(before); got != want {
			t.Fatalf("minification changed the program shape\n--- before ---\n%s\n--- after ---\n%s\n--- output ---\n%s",
				want, got, minified)
		}

		// (9) Formatting the minified output.  This is the state a build
		// pipeline reaches when a shipped bundle is read back, and it is a
		// different input class from anything a human writes: no comments, no
		// blank lines, one-letter names, everything on as few lines as
		// possible.  The same three properties apply, against the MINIFIED
		// program rather than the original.
		formattedMin, err := formatter.Format(minified, nil)
		if err != nil {
			t.Fatalf("Format rejected minified output: %v\n--- input ---\n%s", err, minified)
		}
		minAST, err := programString(minified)
		if err != nil {
			t.Fatalf("re-parse of minified output failed: %v\n--- output ---\n%s", err, minified)
		}
		formattedMinAST, err := programString(formattedMin)
		if err != nil {
			t.Fatalf("the parser rejected formatted minified output: %v\n--- output ---\n%s", err, formattedMin)
		}
		if minAST != formattedMinAST {
			t.Fatalf("formatting minified output changed the AST\n--- before ---\n%s\n--- after ---\n%s\n--- output ---\n%s",
				minAST, formattedMinAST, formattedMin)
		}
		formattedMin2, err := formatter.Format(formattedMin, nil)
		if err != nil {
			t.Fatalf("Format rejected its own output on the minified program: %v\n--- output ---\n%s",
				err, formattedMin)
		}
		if !bytes.Equal(formattedMin, formattedMin2) {
			t.Fatalf("Format is not idempotent on minified output\n--- pass 1 ---\n%q\n--- pass 2 ---\n%q",
				formattedMin, formattedMin2)
		}
	})
}
