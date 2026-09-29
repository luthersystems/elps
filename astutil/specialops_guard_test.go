// Copyright © 2026 The ELPS authors

package astutil

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// specialOpLiteral matches a Go string literal spelling a special form
// whose syntax a code walker must know (the distinctive names; if, and, or
// and friends are too common as plain words to police this way).
var specialOpLiteral = regexp.MustCompile(`"(let\*?|flet|labels|macrolet|lambda|handler-bind|dotimes|quasiquote|with-cleanup|ignore-errors|thread-first|thread-last|qualified-symbol|set!|cond|progn|unless)"`)

// specialOpAllowlist names the non-test files that may spell special-form
// names, and why.  lisp.CodeWalker (lisp/codewalk.go) is the one place that
// knows the syntax of special forms; code elsewhere should walk with it.
// Add a file only with a reason; the test also fails when an entry no
// longer spells any, so the list shrinks as code moves onto the walker.
var specialOpAllowlist = map[string]string{
	"lisp/codewalk.go":                      "the code walker: the one table of special-form syntax",
	"lisp/op.go":                            "defines the special operators",
	"lisp/macro.go":                         "builtin macros build code that uses special forms",
	"lisp/inspect.go":                       "runtime introspection of function values",
	"lisp/loader.go":                        "builds load forms",
	"lisp/testdefs.go":                      "builds test bodies",
	"lisp/lisplib/libtesting/libtesting.go": "builds test macros' expansions",
	"astutil/query.go":                      "reads walker events by Op name (quasiquote templates)",
	"astutil/walk.go":                       "plain syntactic helpers (UserDefined, CollectFormals); follow-up",
	"astutil/package_forms.go":              "package form helpers; follow-up",
	"analysis/analyzer.go":                  "scope resolver; moving onto the walker",
	"analysis/scope.go":                     "scope kinds named after forms",
	"analysis/perf/config.go":               "performance analysis; follow-up",
	"analysis/perf/local.go":                "performance analysis; follow-up",
	"formatter/rules.go":                    "indentation rules are per form name, not syntax",
	"internal/fuzzgen/fuzzgen.go":           "generates programs",
	"internal/fuzzseed/evalseed.go":         "seed programs",
	"lint/analyzers.go":                     "syntactic checks (arity, let-bindings, templates); follow-up",
	"lsp/hover.go":                          "recognizes the lambda word under the cursor",
	"lsp/semantic_tokens.go":                "token classes for form names",
	"minifier/minifier.go":                  "renaming needs binding forms; follow-up",
}

// TestSpecialFormNamesStayInTheWalker fails when a new file hard-codes
// special-form names instead of walking code with lisp.CodeWalker.
func TestSpecialFormNamesStayInTheWalker(t *testing.T) {
	root, err := filepath.Abs("..")
	require.NoError(t, err)
	var found []string
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(root, path)
		if d.IsDir() {
			if strings.HasPrefix(d.Name(), ".") || d.Name() == "testdata" || d.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path) //nolint:gosec // test reads the repository's own sources
		if err != nil {
			return err
		}
		if specialOpLiteral.Match(src) {
			found = append(found, filepath.ToSlash(rel))
		}
		return nil
	})
	require.NoError(t, err)
	sort.Strings(found)
	seen := map[string]bool{}
	for _, f := range found {
		seen[f] = true
		_, ok := specialOpAllowlist[f]
		assert.True(t, ok, "%s spells special-form names; walk code with lisp.CodeWalker, or allowlist it with a reason", f)
	}
	for f := range specialOpAllowlist {
		assert.True(t, seen[f], "%s no longer spells special-form names; remove it from the allowlist", f)
	}
}
