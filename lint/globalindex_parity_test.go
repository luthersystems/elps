// Copyright © 2026 The ELPS authors

package lint

import (
	"bytes"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// parityWorkspace is a workspace whose files reach the workspace globals in
// every way a lookup can: a bare name, a qualified name, a name in several
// packages, a global hidden by a definition of the file, a global with no
// package, a builtin a workspace function replaces, use-package, and
// duplicates within and across files and packages.
var parityWorkspace = map[string]string{
	"a.lisp": `(in-package 'alpha)
(export 'f 'g)
(defun f () 1)
(defun g () (f))
(set 'v 1)
(defun dup () 1)
(defun unused-alpha () 0)
`,
	"b.lisp": `(in-package 'beta)
(use-package 'alpha)
(defun f () (g))
(defun h () (alpha:f) (beta:f) (alpha:dup) (alpha:v) (gamma:car 1))
(defun dup () 2)
(set 'v 2)
`,
	"c.lisp": `(defun u () (f) (alpha:g) (undefined-thing) (dup) (h))
(defun dup () 3)
(set 'free 1)
`,
	"d.lisp": `(in-package 'alpha)
(defun f () 2)
(defun dup () (v) (g) (beta:h))
`,
	"e.lisp": `(in-package 'gamma)
(export 'car)
(defun car (x) x)
(defun g () (car 1) (h) (dup) (free) (u))
`,
	// The removed-builtin check asks whether another package supplies a
	// short name: clock supplies utc-now, and nothing supplies sleep.
	"g.lisp": `(use-package 'time)
(defun t1 () (utc-now) (sleep 1))
`,
	"h.lisp": `(in-package 'clock)
(defun utc-now () 0)
`,
	"sub/f.lisp": `(in-package 'beta)
(defun h () (f) (alpha:unused-alpha) (gamma:g))
(defun only-here (x) (only-here x))
`,
}

func writeWorkspace(t *testing.T, files map[string]string) (string, []string) {
	t.Helper()
	dir := t.TempDir()
	var paths []string
	for name, src := range files {
		path := filepath.Join(dir, name)
		require.NoError(t, os.MkdirAll(filepath.Dir(path), 0o700))
		require.NoError(t, os.WriteFile(path, []byte(src), 0o600))
		paths = append(paths, path)
	}
	sort.Strings(paths)
	return dir, paths
}

// TestGlobalIndexKeepsLintResults pins that Config.GlobalIndex changes no
// result: the workspace references, every diagnostic of every file, and
// what each reference resolves to are the same with and without it.
func TestGlobalIndexKeepsLintResults(t *testing.T) {
	scalingDir, scalingPaths := writeScalingWorkspace(t, 30)
	parityDir, parityPaths := writeWorkspace(t, parityWorkspace)
	for _, ws := range []struct {
		name  string
		dir   string
		paths []string
	}{
		{"parity", parityDir, parityPaths},
		{"scaling", scalingDir, scalingPaths},
	} {
		t.Run(ws.name, func(t *testing.T) {
			indexed, err := BuildAnalysisConfig(&LintConfig{Workspace: ws.dir})
			require.NoError(t, err)
			require.NotNil(t, indexed.GlobalIndex, "lint must build the index")
			plain := *indexed
			plain.GlobalIndex = nil
			plain.WorkspaceRefs = analysis.ScanWorkspaceRefs(ws.dir, &plain, &analysis.ScanConfig{})
			assert.Equal(t, plain.WorkspaceRefs, indexed.WorkspaceRefs, "workspace references")

			l := &Linter{Analyzers: DefaultAnalyzers()}
			var diags int
			for _, path := range ws.paths {
				src, err := os.ReadFile(path) //nolint:gosec // a file this test wrote
				require.NoError(t, err)
				want, err := l.LintFileWithAnalysis(src, path, &plain)
				require.NoError(t, err)
				got, err := l.LintFileWithAnalysis(src, path, indexed)
				require.NoError(t, err)
				assert.Equal(t, want, got, "diagnostics of %s", path)
				diags += len(want)

				assert.Equal(t, resolution(t, src, path, &plain), resolution(t, src, path, indexed),
					"resolution of %s", path)
			}
			if ws.name == "parity" {
				require.Positive(t, diags, "the parity workspace must give diagnostics, or it compares nothing")
			}
		})
	}
}

// resolution describes what the analysis of one file resolves: each
// reference and the symbol it finds, the unresolved names, and what the
// root scope's lookup methods return for every name the file mentions.
func resolution(t *testing.T, src []byte, path string, cfg *analysis.Config) []string {
	t.Helper()
	exprs, err := rdparser.New(token.NewScanner(path, bytes.NewReader(src))).ParseProgram()
	require.NoError(t, err)
	res := analysis.Analyze(exprs, analysis.ConfigForFile(cfg, path))
	describe := func(sym *analysis.Symbol) string {
		if sym == nil {
			return "<nil>"
		}
		var loc string
		if sym.Source != nil {
			loc = fmt.Sprintf("%s:%d:%d", sym.Source.File, sym.Source.Line, sym.Source.Col)
		}
		return fmt.Sprintf("%s:%s kind=%v ext=%v exp=%v refs=%d root=%v at=%s",
			sym.Package, sym.Name, sym.Kind, sym.External, sym.Exported, sym.References,
			sym.Scope == res.RootScope, loc)
	}
	var out []string
	names := map[string]bool{}
	for _, r := range res.References {
		out = append(out, fmt.Sprintf("ref %d:%d -> %s", r.Source.Line, r.Source.Col, describe(r.Symbol)))
		names[r.Symbol.Name] = true
	}
	for _, u := range res.Unresolved {
		out = append(out, fmt.Sprintf("unresolved %s %d:%d", u.Name, u.Source.Line, u.Source.Col))
		names[u.Name] = true
	}
	for _, s := range res.Symbols {
		out = append(out, "symbol "+describe(s))
		names[s.Name] = true
	}
	for _, g := range cfg.ExtraGlobals {
		names[g.Name] = true
	}
	root := res.RootScope
	for _, name := range slices.Sorted(maps.Keys(names)) {
		out = append(out, "lookup "+name+" "+describe(root.Lookup(name)), "local "+name+" "+describe(root.LookupLocal(name)))
		var all []string
		for _, sym := range root.LookupAllLocal(name) {
			all = append(all, describe(sym))
		}
		sort.Strings(all)
		out = append(out, fmt.Sprintf("all %s %v", name, all))
		for _, pkg := range []string{"", "user", "alpha", "beta", "gamma", "pkg-1"} {
			out = append(out,
				"inpkg "+pkg+" "+name+" "+describe(root.LookupInPackage(name, pkg)),
				"localinpkg "+pkg+" "+name+" "+describe(root.LookupLocalInPackage(name, pkg)),
				"visible "+pkg+" "+name+" "+describe(root.LookupLocalVisible(name, pkg)))
		}
	}
	return out
}
