package tree_sitter_elps_test

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_elps "github.com/luthersystems/elps/tree-sitter-elps/bindings/go"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newParser(t *testing.T) *tree_sitter.Parser {
	t.Helper()
	parser := tree_sitter.NewParser()
	lang := tree_sitter.NewLanguage(tree_sitter_elps.Language())
	require.NotNil(t, lang, "failed to load ELPS grammar")
	err := parser.SetLanguage(lang)
	require.NoError(t, err)
	t.Cleanup(func() { parser.Close() })
	return parser
}

func language(t *testing.T) *tree_sitter.Language {
	t.Helper()
	lang := tree_sitter.NewLanguage(tree_sitter_elps.Language())
	require.NotNil(t, lang, "failed to load ELPS grammar")
	return lang
}

// corpusTest represents a single test case from a tree-sitter corpus file.
type corpusTest struct {
	Name     string
	Input    string
	Expected string
}

// parseCorpusFile parses a tree-sitter test corpus file into individual test cases.
// Format:
//
//	================================================================================
//	Test Name
//	================================================================================
//
//	input code
//
//	---
//
//	(expected sexp)
func parseCorpusFile(t *testing.T, path string) []corpusTest {
	t.Helper()
	data, err := os.ReadFile(path)
	require.NoError(t, err, "reading corpus file %s", path)

	content := string(data)
	separator := "================================================================================"

	// Split on separator lines
	parts := strings.Split(content, separator)

	var tests []corpusTest
	// Parts come in groups: [preamble, name, body, name, body, ...]
	// After the first separator, we get pairs of (name section, body section)
	for i := 1; i+1 < len(parts); i += 2 {
		name := strings.TrimSpace(parts[i])
		body := parts[i+1]

		// Split body on "---" separator
		sections := strings.SplitN(body, "\n---\n", 2)
		if len(sections) != 2 {
			t.Fatalf("corpus file %s: test %q missing --- separator", path, name)
		}

		input := strings.TrimSpace(sections[0])
		expected := strings.TrimSpace(sections[1])

		tests = append(tests, corpusTest{
			Name:     name,
			Input:    input,
			Expected: expected,
		})
	}

	return tests
}

// normalizeSexp normalizes a tree-sitter S-expression for comparison.
// Collapses all whitespace runs into single spaces.
func normalizeSexp(s string) string {
	s = strings.TrimSpace(s)
	// Collapse all whitespace (including newlines) into single spaces
	re := regexp.MustCompile(`\s+`)
	return re.ReplaceAllString(s, " ")
}

func TestCorpus(t *testing.T) {
	corpusDir := "test/corpus"
	entries, err := os.ReadDir(corpusDir)
	require.NoError(t, err, "reading corpus directory")

	parser := newParser(t)

	totalTests := 0
	for _, entry := range entries {
		if !strings.HasSuffix(entry.Name(), ".txt") {
			continue
		}

		path := filepath.Join(corpusDir, entry.Name())
		suiteName := strings.TrimSuffix(entry.Name(), ".txt")
		tests := parseCorpusFile(t, path)

		t.Run(suiteName, func(t *testing.T) {
			for _, tc := range tests {
				tc := tc
				t.Run(tc.Name, func(t *testing.T) {
					tree := parser.Parse([]byte(tc.Input), nil)
					require.NotNil(t, tree, "parse returned nil tree")
					defer tree.Close()

					root := tree.RootNode()
					got := normalizeSexp(root.ToSexp())
					want := normalizeSexp(tc.Expected)

					assert.Equal(t, want, got,
						"input: %s\nexpected sexp:\n%s\ngot sexp:\n%s",
						tc.Input, tc.Expected, root.ToSexp())
				})
				totalTests++
			}
		})
	}

	t.Logf("Ran %d corpus tests across %d files", totalTests, len(entries))
}

// hasErrorNode walks the tree and returns true if any ERROR node is found.
func hasErrorNode(node *tree_sitter.Node) bool {
	if node.IsError() || node.IsMissing() {
		return true
	}
	for i := uint(0); i < node.ChildCount(); i++ {
		child := node.Child(i)
		if hasErrorNode(child) {
			return true
		}
	}
	return false
}

// collectErrorNodes walks the tree and collects descriptions of all ERROR nodes.
func collectErrorNodes(node *tree_sitter.Node, source []byte) []string {
	var errors []string
	if node.IsError() {
		start := node.StartPosition()
		end := node.EndPosition()
		text := node.Utf8Text(source)
		if len(text) > 60 {
			text = text[:60] + "..."
		}
		errors = append(errors, fmt.Sprintf("ERROR at %d:%d-%d:%d: %s",
			start.Row, start.Column, end.Row, end.Column, text))
	}
	if node.IsMissing() {
		start := node.StartPosition()
		errors = append(errors, fmt.Sprintf("MISSING %s at %d:%d",
			node.Kind(), start.Row, start.Column))
	}
	for i := uint(0); i < node.ChildCount(); i++ {
		errors = append(errors, collectErrorNodes(node.Child(i), source)...)
	}
	return errors
}

func TestParseAllLispFiles(t *testing.T) {
	parser := newParser(t)

	// Find all .lisp files in the parent repo
	var lispFiles []string
	err := filepath.Walk("..", func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // skip unreadable dirs
		}
		// Skip hidden dirs and node_modules (but not ".." itself)
		if info.IsDir() {
			base := filepath.Base(path)
			if (base != ".." && strings.HasPrefix(base, ".")) || base == "node_modules" || base == "vendor" {
				return filepath.SkipDir
			}
			return nil
		}
		if strings.HasSuffix(path, ".lisp") {
			lispFiles = append(lispFiles, path)
		}
		return nil
	})
	require.NoError(t, err)
	require.NotEmpty(t, lispFiles, "no .lisp files found in repo")

	t.Logf("Found %d .lisp files to parse", len(lispFiles))

	for _, path := range lispFiles {
		relPath := strings.TrimPrefix(path, "../")
		t.Run(relPath, func(t *testing.T) {
			source, err := os.ReadFile(path)
			require.NoError(t, err)

			tree := parser.Parse(source, nil)
			require.NotNil(t, tree, "parse returned nil tree")
			defer tree.Close()

			root := tree.RootNode()
			if hasErrorNode(root) {
				errors := collectErrorNodes(root, source)
				t.Errorf("parse errors in %s:\n%s",
					relPath,
					strings.Join(errors, "\n"))
			}
		})
	}
}

func TestHighlightQueryCompiles(t *testing.T) {
	lang := language(t)
	data, err := os.ReadFile("queries/highlights.scm")
	require.NoError(t, err, "reading highlights.scm")

	query, queryErr := tree_sitter.NewQuery(lang, string(data))
	if queryErr != nil {
		t.Fatalf("highlights.scm failed to compile: %v", queryErr)
	}
	defer query.Close()

	t.Logf("highlights.scm compiled successfully with %d patterns", query.PatternCount())
}

func TestLocalsQueryCompiles(t *testing.T) {
	lang := language(t)
	data, err := os.ReadFile("queries/locals.scm")
	require.NoError(t, err, "reading locals.scm")

	query, queryErr := tree_sitter.NewQuery(lang, string(data))
	if queryErr != nil {
		t.Fatalf("locals.scm failed to compile: %v", queryErr)
	}
	defer query.Close()

	t.Logf("locals.scm compiled successfully with %d patterns", query.PatternCount())
}
