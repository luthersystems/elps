// Copyright © 2018 The ELPS authors

// Package formsync_test guards the hand-maintained mirrors of the special
// operator table against drift.
//
// lisp.DefaultSpecialOps() is the authority, but five places restate parts of
// it by hand so that tooling outside the evaluator can recognise a form:
//
//	editors/vscode/syntaxes/elps.tmLanguage.json  TextMate keyword highlighting
//	tree-sitter-elps/queries/highlights.scm       tree-sitter keyword highlighting
//	lsp/semantic_tokens.go                        LSP semantic tokens
//	analysis/perf/local.go                        perf analyzer's non-call forms
//	internal/fuzzgen/fuzzgen.go                   generator vocabulary
//
// Nothing tied any of them to the authority, and they drifted: with-cleanup
// (#554) reached the TextMate grammar and not its tree-sitter sibling, so the
// form highlighted as a keyword in VS Code and as an ordinary function call in
// every editor built on tree-sitter -- nvim-treesitter, Helix, Zed, Emacs
// treesit.  The tree-sitter suite has a test for that file, but it only
// asserts the query COMPILES, which a stale-but-valid query does.
//
// The two grammars get the strictest treatment because they do the identical
// job and there is no reason for them ever to differ.  The Go mirrors are
// gated in their own packages, where the tables are reachable without parsing
// source: lsp/semantic_tokens_drift_test.go and analysis/perf/local_drift_test.go.
//
// internal/fuzzgen/fuzzgen.go is NOT gated, and saying otherwise would be the
// same shape of untrue-comment this package exists to prevent.  Its list is a
// deliberate SAMPLE ("keeps the minifier and formatter on the hot path"), not
// a mirror, so it needs no gate at all.
//
// One correction worth keeping, since this file is where the claim was made:
// perf's isCallable was reported as drifted, missing lambda, quote and
// quasiquote and so recording a call edge for (lambda ...).  That was wrong.
// scanExpr resolves all three in its own switch and returns ~20 lines before
// isCallable is consulted, so their isCallable value is unreachable -- and
// each case does strictly MORE than suppress an edge (quote and quasiquote
// stop the descent so quoted data is never costed as code; lambda skips the
// formals and scans the body at the caller's loop depth), none of which
// isCallable can express, since it only chooses whether to emit an edge and
// cannot stop the walk.  Adding them there would restate the fact in a second
// place while the switch stayed load-bearing.  The gate pins the switch with
// probes instead of restating it.
//
// Two limits worth knowing rather than discovering: a keyword shared by both
// grammars but belonging to neither the op table nor a documented exemption
// passes, because coverage is a subset check plus set equality; and a form
// handled by some rule OTHER than the ones an exemption names is not
// detected.
//
// A NOTE ON THE FAILURE MODE THIS FILE IS BUILT TO AVOID.  formatter's
// TestRepoFileRoundTrip used to pass while testing nothing: its globs
// resolved outside the repository, and filepath.Glob reports no error for a
// pattern that matches nothing, so it ran zero subtests and reported success.
// (Fixed since -- it now locates the repo the same way this file does and
// covers 21 files.)  Every read below therefore locates the repository from
// this file's own path, never a relative guess, and fails loudly on an empty
// or unrecognised extraction, so a restructured grammar breaks this test
// instead of quietly emptying it.
package formsync_test

import (
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"sort"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// grammarExempt lists special operators that deliberately do not appear in
// either grammar's keyword list, with the rule that handles them instead.
// Both grammars agree on every one of these, which is why the exemption is
// shared rather than per-grammar.
//
// A NEW special operator is not exempt.  Adding one without touching the
// grammars fails TestGrammarsCoverSpecialOps until someone either adds it or
// records here why it does not belong -- which is the point.
var grammarExempt = map[string]exemption{
	"quote": {"reader syntax: highlighted as the ' operator, not as a head symbol",
		"quote.elps", "(quote \"'\")"},
	"lambda": {"its own rule, which also scopes its formals",
		"lambda-form", "lambda_form"},
	"let":      {"let family rule, which also highlights the binding list", "let-form", "let_form"},
	"let*":     {"let family rule", "let-form", "let_form"},
	"flet":     {"let family rule", "let-form", "let_form"},
	"labels":   {"let family rule", "let-form", "let_form"},
	"macrolet": {"let family rule", "let-form", "let_form"},
}

// exemption records WHY a special operator is absent from the keyword lists
// and, crucially, the rule that is supposed to handle it instead.  Asserting
// only the absence made the reason a promise nothing checked: deleting
// let-form and lambda_form outright left both gates green while nothing
// highlighted let, flet, labels, macrolet or lambda in either editor.
type exemption struct {
	reason       string
	textmateRule string
	treeSitter   string
}

// repoRoot returns the repository root, located from this source file's own
// path rather than from the working directory, so the test cannot end up
// reading a directory that does not exist and silently finding nothing.
func repoRoot(t *testing.T) string {
	t.Helper()
	_, self, _, ok := runtime.Caller(0)
	require.True(t, ok, "runtime.Caller could not locate this test file")
	dir := filepath.Dir(self)
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		require.NotEqual(t, parent, dir, "walked to the filesystem root without finding go.mod")
		dir = parent
	}
}

// textmateKeywords extracts the keyword alternation from the TextMate
// grammar.  It walks the decoded JSON rather than matching a line, so moving
// the rule inside the file does not break the test -- but an alternation that
// can no longer be found does.
func textmateKeywords(t *testing.T) map[string]bool {
	t.Helper()
	path := filepath.Join(repoRoot(t), "editors/vscode/syntaxes/elps.tmLanguage.json")
	// The path is built from repoRoot, which is derived from this file's own
	// compiled-in location -- it is not input.
	raw, err := os.ReadFile(path) //nolint:gosec // path derived from runtime.Caller, not input
	require.NoError(t, err, "the TextMate grammar must be readable at %s", path)

	var doc any
	require.NoError(t, json.Unmarshal(raw, &doc), "the TextMate grammar must be valid JSON")

	// The rule is the one whose begin pattern matches a head symbol against a
	// long alternation; find it by a sentinel every version of it contains.
	var patterns []string
	var walk func(any)
	walk = func(v any) {
		switch n := v.(type) {
		case map[string]any:
			for _, child := range n {
				walk(child)
			}
		case []any:
			for _, child := range n {
				walk(child)
			}
		case string:
			if strings.Contains(n, "handler-bind|ignore-errors") {
				patterns = append(patterns, n)
			}
		}
	}
	walk(doc)
	// Exactly one, not at-least-one.  Keeping the last match over Go's
	// randomised map iteration made a duplicated or legacy rule a coin
	// flip: half the runs extracted the stale copy and reported success.
	require.Len(t, patterns, 1,
		"expected exactly one keyword alternation in %s, found %d;"+
			" a duplicated or legacy rule makes the extraction"+
			" nondeterministic, so fix the grammar or teach this extractor"+
			" which rule is authoritative", path, len(patterns))
	pattern := patterns[0]

	group := regexp.MustCompile(`\\s\*\(([^)]*)\)`).FindStringSubmatch(pattern)
	require.Len(t, group, 2, "could not isolate the alternation group in %q", pattern)

	out := map[string]bool{}
	for _, alt := range strings.Split(group[1], "|") {
		// The grammar spells the set/set! pair as the regex "set!?".
		if strings.HasSuffix(alt, "!?") {
			stem := strings.TrimSuffix(alt, "!?")
			out[stem] = true
			out[stem+"!"] = true
			continue
		}
		require.NotContains(t, alt, "?",
			"unhandled regex metacharacter in alternative %q; teach this"+
				" extractor about it rather than letting it mis-read the grammar", alt)
		out[alt] = true
	}
	return out
}

// treeSitterKeywords extracts the #any-of? keyword list from the tree-sitter
// highlight query.  tree-sitter-elps is a separate Go module, so the query is
// read as text rather than imported.
func treeSitterKeywords(t *testing.T) map[string]bool {
	t.Helper()
	path := filepath.Join(repoRoot(t), "tree-sitter-elps/queries/highlights.scm")
	// The path is built from repoRoot, which is derived from this file's own
	// compiled-in location -- it is not input.
	raw, err := os.ReadFile(path) //nolint:gosec // path derived from runtime.Caller, not input
	require.NoError(t, err, "the tree-sitter highlight query must be readable at %s", path)

	src := string(raw)
	start := strings.Index(src, "#any-of? @keyword")
	require.NotEqual(t, -1, start,
		"could not find the #any-of? keyword predicate in %s; if the query was"+
			" restructured, update this extractor rather than deleting the test", path)
	end := strings.Index(src[start:], "))")
	require.NotEqual(t, -1, end, "unterminated #any-of? predicate in %s", path)

	out := map[string]bool{}
	for _, m := range regexp.MustCompile(`"([^"]+)"`).FindAllStringSubmatch(src[start:start+end], -1) {
		out[m[1]] = true
	}
	return out
}

func sortedKeys(m map[string]bool) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// TestGrammarsAgree pins the two grammars against each other.
//
// They highlight the same language for different editors, so a symbol that is
// a keyword in one and not the other is always a bug -- and it is invisible
// to anyone developing in the editor that happens to be right.
func TestGrammarsAgree(t *testing.T) {
	t.Parallel()
	textmate := textmateKeywords(t)
	treesitter := treeSitterKeywords(t)

	// Guard against a silently-empty extraction: a test that compares two
	// empty sets passes and proves nothing.
	require.NotEmpty(t, textmate, "extracted no keywords from the TextMate grammar")
	require.NotEmpty(t, treesitter, "extracted no keywords from the tree-sitter query")
	require.Contains(t, textmate, "handler-bind", "TextMate extraction looks wrong")
	require.Contains(t, treesitter, "handler-bind", "tree-sitter extraction looks wrong")

	require.Equal(t, sortedKeys(textmate), sortedKeys(treesitter),
		"the TextMate grammar (editors/vscode/syntaxes/elps.tmLanguage.json) and the"+
			" tree-sitter query (tree-sitter-elps/queries/highlights.scm) highlight"+
			" different keyword sets; a form highlighted in one editor and not the"+
			" other is a bug in whichever is stale")
}

// TestGrammarsCoverSpecialOps pins both grammars against the authority.
//
// TestGrammarsAgree alone would not catch this: two grammars can agree with
// each other and both be stale, which is exactly what happens when a special
// operator is added and neither file is touched.
func TestGrammarsCoverSpecialOps(t *testing.T) {
	t.Parallel()
	textmate := textmateKeywords(t)
	treesitter := treeSitterKeywords(t)

	ops := lisp.DefaultSpecialOps()
	require.NotEmpty(t, ops, "lisp.DefaultSpecialOps() returned nothing")
	textmateSrc, treeSitterSrc := grammarSources(t)

	for _, def := range ops {
		name := def.Name()
		if ex, ok := grammarExempt[name]; ok {
			require.NotEmpty(t, ex.reason, "exemption for %q must carry a reason", name)
			require.Contains(t, textmateSrc, ex.textmateRule,
				"%q is exempt because %s, but the TextMate rule %q it names is gone",
				name, ex.reason, ex.textmateRule)
			require.Contains(t, treeSitterSrc, ex.treeSitter,
				"%q is exempt because %s, but the tree-sitter rule %q it names is gone",
				name, ex.reason, ex.treeSitter)
			// An exempt op must be absent from BOTH, or the exemption is
			// stale and one grammar is now doing something the other is not.
			require.False(t, textmate[name] || treesitter[name],
				"%q is listed in grammarExempt (%s) but appears in a grammar's"+
					" keyword list; drop the exemption", name, ex.reason)
			continue
		}
		require.True(t, textmate[name],
			"special operator %q is missing from the TextMate grammar"+
				" (editors/vscode/syntaxes/elps.tmLanguage.json); add it, or add it to"+
				" grammarExempt with the rule that handles it instead", name)
		require.True(t, treesitter[name],
			"special operator %q is missing from the tree-sitter query"+
				" (tree-sitter-elps/queries/highlights.scm); add it, or add it to"+
				" grammarExempt with the rule that handles it instead", name)
	}
}

// grammarSources returns the raw text of both grammar files, for assertions
// about rules rather than about keyword lists.
func grammarSources(t *testing.T) (textmate, treeSitter string) {
	t.Helper()
	root := repoRoot(t)
	//nolint:gosec // path derived from runtime.Caller, not input
	tm, err := os.ReadFile(filepath.Join(root, "editors/vscode/syntaxes/elps.tmLanguage.json"))
	require.NoError(t, err)
	//nolint:gosec // path derived from runtime.Caller, not input
	ts, err := os.ReadFile(filepath.Join(root, "tree-sitter-elps/queries/highlights.scm"))
	require.NoError(t, err)
	return string(tm), string(ts)
}
