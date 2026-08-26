// Copyright © 2026 The ELPS authors

package lint

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Guards for FuzzLintSource.
//
// PR #348 found three harness bugs by measuring per-function coverage from the
// seed corpus, in each case where every operation "ran", every test passed, and
// whole functions sat at 0%. A fuzz target cannot report that it is covering
// less than it thinks, so the reachability claims in the harness comments are
// asserted here instead.

// TestLintFuzzSeedsProduceDiagnostics checks the primary seeds actually make
// the analyzers FIRE. A corpus that lints clean exercises each analyzer's walk
// and none of its reporting, which is most of the interesting code -- and it
// would pass every test while doing so.
func TestLintFuzzSeedsProduceDiagnostics(t *testing.T) {
	stats, err := runLint(t.TempDir(), []byte(lintSeedUses), []byte(lintSeedDefs), lintScripts[0])
	require.NoError(t, err)
	assert.Positive(t, stats.analyzers, "buildLinter produced a Linter with no analyzers")
	assert.Positive(t, stats.syntactic,
		"the primary seed produced no syntactic diagnostics, so the reporting half of"+
			" every analyzer is unreached")
	assert.Positive(t, stats.semantic,
		"the primary seed produced no semantic diagnostics, so the analyzers that"+
			" require an analysis.Result never report")
	assert.Positive(t, stats.workspace,
		"LintFiles over the temp workspace produced nothing, so BuildAnalysisConfig's"+
			" output is never actually used")
}

// TestLintFuzzSeedsCoverEveryAnalyzer checks the corpus is not quietly linting
// against a one-analyzer Linter.
//
// buildLinter draws a SUBSET from the script, and a script whose bytes happen
// to clear every bit yields all[:1]. That is a legitimate configuration, but if
// no seed ever ran the full set, most of analyzers.go would be unreached from
// the corpus while the target still looked healthy.
func TestLintFuzzSeedsCoverEveryAnalyzer(t *testing.T) {
	want := len(DefaultAnalyzers())
	require.Greater(t, want, 1, "DefaultAnalyzers is degenerate")

	best := 0
	for i, sc := range lintScripts {
		stats, err := runLint(t.TempDir(), []byte(lintSeedUses), []byte(lintSeedDefs), sc)
		require.NoError(t, err, "lintScripts[%d] failed", i)
		if stats.analyzers > best {
			best = stats.analyzers
		}
	}
	assert.GreaterOrEqual(t, best, want,
		"no seed script ran the full analyzer set (best was %d of %d), so most of"+
			" analyzers.go is unreachable from the seed corpus", best, want)
}

// TestLintFuzzEmbedderAnalyzerReports checks the embedder-registered analyzer
// actually reports through every Pass method.
//
// Pass.ReportNode and Pass.ReportWithNotes have NO caller anywhere in this
// repository -- they exist for embedders such as luthersystems/substrate. That
// makes them exactly the kind of exported function a target can leave at 0%
// while reporting a healthy overall number, so the harness registers an
// analyzer that uses them and this test asserts the registration works.
func TestLintFuzzEmbedderAnalyzerReports(t *testing.T) {
	a := embedderAnalyzer(&script{b: []byte{1, 1, 1, 1}})
	pass := &Pass{
		Analyzer: a,
		Filename: lintFileA,
		Exprs:    parseForGuard(t, "(defun f (x) (+ x 1))\n(f 1)"),
	}
	require.NoError(t, a.Run(pass))
	require.NotEmpty(t, pass.diagnostics,
		"the embedder analyzer reported nothing, so ReportNode and ReportWithNotes"+
			" stay at 0% however long the sweep runs")

	seen := map[string]bool{}
	for _, d := range pass.diagnostics {
		for _, m := range []string{msgReport, msgReportWithNotes, msgReportNode, msgReportf} {
			if strings.HasPrefix(d.Message, m) {
				seen[m] = true
			}
		}
		assert.Equal(t, "fuzz-embedder", d.Analyzer,
			"Report* must stamp the analyzer name onto every diagnostic")
		assert.NotEqual(t, severityUnset, d.Severity,
			"Report* must fill an unset severity from the analyzer default")
	}
	for _, m := range []string{msgReport, msgReportWithNotes, msgReportNode, msgReportf} {
		assert.True(t, seen[m],
			"no diagnostic came from %q, so that Pass method stays at 0%% however"+
				" long the sweep runs", m)
	}
}

// TestLintFuzzDeterminismCheckIsLive guards the determinism assertion itself:
// if runLint ever stopped calling LintFile twice, the check would pass
// vacuously and the property would silently stop being tested.
func TestLintFuzzDeterminismCheckIsLive(t *testing.T) {
	linter := &Linter{Analyzers: DefaultAnalyzers()}
	src := []byte(lintSeedUses)
	first, err := linter.LintFile(src, lintFileA)
	require.NoError(t, err)
	require.NotEmpty(t, first,
		"the determinism check compares two empty slices unless the seed produces"+
			" diagnostics")
	second, err := linter.LintFile(src, lintFileA)
	require.NoError(t, err)
	assert.Equal(t, first, second)
}

func parseForGuard(t *testing.T, src string) []*lisp.LVal {
	t.Helper()
	return rdparser.New(token.NewScanner(lintFileA, bytes.NewReader([]byte(src)))).
		ParseProgramFaultTolerant().Exprs
}

// TestLintFuzzSeedsReachDeprecated checks the corpus actually reaches the
// `deprecated` check's REPORTING half, in both of the ways a docstring gets to
// it: from the workspace scan of the neighbouring file, and from a definition
// in the file being linted.
//
// It is the same hazard TestLintFuzzSeedsProduceDiagnostics guards in
// aggregate, sharpened for one analyzer. `deprecated` fires only on a docstring
// carrying the marker, and a corpus of ordinary docstrings would run its whole
// reference loop, report nothing, and leave the diagnostic it builds -- the
// notice text, the kind word, the span suppression -- entirely unreached while
// the seeds still looked healthy.
func TestLintFuzzSeedsReachDeprecated(t *testing.T) {
	deprecatedDiags := func(dir string, files ...string) []Diagnostic {
		t.Helper()
		linter := &Linter{Analyzers: DefaultAnalyzers()}
		diags, err := linter.LintFiles(&LintConfig{Workspace: dir}, files)
		require.NoError(t, err)
		var found []Diagnostic
		for _, d := range diags {
			if d.Analyzer == AnalyzerDeprecated.Name {
				found = append(found, d)
			}
		}
		return found
	}
	write := func(dir, name, src string) string {
		t.Helper()
		path := filepath.Join(dir, name)
		require.NoError(t, os.WriteFile(path, []byte(src), 0o600))
		return path
	}

	t.Run("cross file", func(t *testing.T) {
		dir := t.TempDir()
		uses := write(dir, lintFileA, lintSeedUses)
		write(dir, lintFileB, lintSeedDefs)
		found := deprecatedDiags(dir, uses)
		require.NotEmpty(t, found,
			"the primary seed pair produces no deprecated diagnostic, so the check's"+
				" reporting half is unreached from the corpus")
		for _, d := range found {
			assert.True(t, d.Deprecated, "the deprecated check must set Deprecated")
		}
	})

	t.Run("same file", func(t *testing.T) {
		dir := t.TempDir()
		src := write(dir, lintFileA, "(in-package 'user)\n"+
			"(defun old-fn (x) \"Deprecated: use new-fn instead.\" x)\n"+
			"(defun also-old (y) \"Deprecated:\" (old-fn y))\n"+
			"(old-fn 1)\n")
		found := deprecatedDiags(dir, src)
		require.Len(t, found, 1,
			"expected exactly the top-level use: the declaration is not a use and the"+
				" call from also-old's body is suppressed")
		assert.Equal(t, 4, found[0].Pos.Line)
	})
}
