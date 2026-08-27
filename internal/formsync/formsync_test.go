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
// checked in their own packages, where the tables are reachable without
// parsing source.
//
// A NOTE ON THE FAILURE MODE THIS FILE IS BUILT TO AVOID.  formatter's
// TestRepoFileRoundTrip passes today while testing nothing: its globs resolve
// outside the repository, and filepath.Glob reports no error for a pattern
// that matches nothing, so it runs zero subtests and reports success.  Every
// read below therefore locates the repository from this file's own path
// (never a relative guess) and fails loudly on an empty or unrecognised
// extraction, so a restructured grammar breaks this test instead of quietly
// emptying it.
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
var grammarExempt = map[string]string{
	"quote":    "reader syntax: highlighted as the ' operator, not as a head symbol",
	"lambda":   "has its own rule (lambda-form / lambda_form) that also scopes its formals",
	"let":      "let family rule (let-form / let_form), which also highlights the binding list",
	"let*":     "let family rule",
	"flet":     "let family rule",
	"labels":   "let family rule",
	"macrolet": "let family rule",
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
	var pattern string
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
				pattern = n
			}
		}
	}
	walk(doc)
	require.NotEmpty(t, pattern,
		"could not find the keyword alternation in %s; if the grammar was"+
			" restructured, update this extractor rather than deleting the test", path)

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

	for _, def := range ops {
		name := def.Name()
		if reason, ok := grammarExempt[name]; ok {
			require.NotEmpty(t, reason, "exemption for %q must carry a reason", name)
			// An exempt op must be absent from BOTH, or the exemption is
			// stale and one grammar is now doing something the other is not.
			require.False(t, textmate[name] || treesitter[name],
				"%q is listed in grammarExempt (%s) but appears in a grammar's"+
					" keyword list; drop the exemption", name, reason)
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
