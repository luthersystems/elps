// Copyright © 2026 The ELPS authors

package lint

import (
	"sync"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestLintFileWithAnalysis_DoesNotMutateCallerConfig is the regression test for
// issue #444. It is a CATCH: it fails on main.
//
// LintFileWithAnalysis took a *analysis.Config the caller owns and wrote
// cfg.Filename into it before analysing. After the call the caller's config
// named a file the caller never asked about, so any later analysis through
// that config was attributed to the wrong file — and two goroutines linting
// different files through one config wrote the field unsynchronised.
func TestLintFileWithAnalysis_DoesNotMutateCallerConfig(t *testing.T) {
	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol}}

	t.Run("zero config keeps its zero Filename", func(t *testing.T) {
		cfg := &analysis.Config{}
		_, err := l.LintFileWithAnalysis([]byte(`(+ 1 2)`), "a.lisp", cfg)
		require.NoError(t, err)
		assert.Empty(t, cfg.Filename,
			"LintFileWithAnalysis stamped the linted filename into the caller's config")
	})

	t.Run("a config that already names a file keeps that name", func(t *testing.T) {
		cfg := &analysis.Config{Filename: "owned-by-caller.lisp"}
		_, err := l.LintFileWithAnalysis([]byte(`(+ 1 2)`), "a.lisp", cfg)
		require.NoError(t, err)
		assert.Equal(t, "owned-by-caller.lisp", cfg.Filename,
			"the caller's Filename was overwritten with the linted file")
	})

	t.Run("reused config is not carrying the previous file", func(t *testing.T) {
		cfg := &analysis.Config{}
		_, err := l.LintFileWithAnalysis([]byte(`(+ 1 2)`), "first.lisp", cfg)
		require.NoError(t, err)
		_, err = l.LintFileWithAnalysis([]byte(`(+ 1 2)`), "second.lisp", cfg)
		require.NoError(t, err)
		assert.Empty(t, cfg.Filename,
			"after two calls the caller's config names whichever file ran last")
	})
}

// TestLintFileWithAnalysis_ConcurrentSharedConfig is the -race half of issue
// #444 and is also a CATCH: under -race on main it reports a genuine DATA RACE
// on analysis.Config.Filename, written from two goroutines at lint.go:665.
//
// The shape is an embedder that builds one analysis.Config (as
// BuildAnalysisConfig hands back) and lints several files through it in
// parallel. Nothing in the signature or doc comment warned that the config was
// written to, so this is the natural way to use the method.
func TestLintFileWithAnalysis_ConcurrentSharedConfig(t *testing.T) {
	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol}}
	cfg := &analysis.Config{}

	files := []string{"a.lisp", "b.lisp", "c.lisp", "d.lisp"}
	var wg sync.WaitGroup
	for _, name := range files {
		wg.Add(1)
		go func() {
			defer wg.Done()
			_, err := l.LintFileWithAnalysis([]byte(`(defun foo () 1)`), name, cfg)
			assert.NoError(t, err)
		}()
	}
	wg.Wait()
}

// TestLintFileWithAnalysis_AttributesDiagnosticsToTheLintedFile is a GUARD
// (passes on main): copying the config must not cost the filename its effect.
// Diagnostics still carry the file that was linted, and the nil-config path
// still works.
func TestLintFileWithAnalysis_AttributesDiagnosticsToTheLintedFile(t *testing.T) {
	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol}}

	t.Run("with a caller config", func(t *testing.T) {
		cfg := &analysis.Config{}
		diags, err := l.LintFileWithAnalysis([]byte(`(unknown-fn 1)`), "target.lisp", cfg)
		require.NoError(t, err)
		require.NotEmpty(t, diags)
		for _, d := range diags {
			assert.Equal(t, "target.lisp", d.Pos.File)
		}
	})

	t.Run("with a nil config", func(t *testing.T) {
		diags, err := l.LintFileWithAnalysis([]byte(`(unknown-fn 1)`), "target.lisp", nil)
		require.NoError(t, err)
		require.NotEmpty(t, diags)
		for _, d := range diags {
			assert.Equal(t, "target.lisp", d.Pos.File)
		}
	})
}
