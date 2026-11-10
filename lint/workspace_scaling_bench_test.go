// Copyright © 2026 The ELPS authors

package lint

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// scalingWorkspaceDefs is the number of definitions in each file of the
// synthetic workspace of BenchmarkLintWorkspaceScaling.
const scalingWorkspaceDefs = 40

// writeScalingWorkspace writes a synthetic workspace of the given number
// of files to a new directory and returns the directory and the file
// paths. Each file has scalingWorkspaceDefs functions in one of ten
// packages. Each function calls a function of its own file, one of
// another file of its package (through the workspace globals) and one of
// another package (a qualified name).
func writeScalingWorkspace(tb testing.TB, files int) (string, []string) {
	tb.Helper()
	const packages = 10
	dir := tb.TempDir()
	paths := make([]string, 0, files)
	for f := range files {
		var src strings.Builder
		fmt.Fprintf(&src, "(in-package 'pkg-%d)\n", f%packages)
		// Another file of the same package, and one of the next package.
		samePkg := (f + packages) % files
		otherPkg := (f + 1) % files
		for d := range scalingWorkspaceDefs {
			fmt.Fprintf(&src, "(export 'fn-%d-%d)\n", f, d)
			fmt.Fprintf(&src, "(defun fn-%d-%d (a b)\n  \"Adds.\"\n  (+ (fn-%d-%d a b) (fn-%d-%d a b) (pkg-%d:fn-%d-%d a b) %d))\n",
				f, d,
				f, (d+1)%scalingWorkspaceDefs,
				samePkg, d,
				otherPkg%packages, otherPkg, d,
				d)
		}
		path := filepath.Join(dir, fmt.Sprintf("mod-%d", f%7), fmt.Sprintf("file-%d.lisp", f))
		if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
			tb.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(src.String()), 0o600); err != nil {
			tb.Fatal(err)
		}
		paths = append(paths, path)
	}
	return dir, paths
}

// BenchmarkLintWorkspaceScaling measures a semantic lint run
// (LintFiles with a workspace) over synthetic workspaces of growing size
// (luthersystems/elps#944).
//
// The files are alike, so the cost of one file must not grow with the
// number of files. The ns/file and allocs/file metrics show it: they
// stay level when each file costs the same, and grow in proportion to
// the workspace when the analysis of a file does work for each symbol of
// the workspace.
func BenchmarkLintWorkspaceScaling(b *testing.B) {
	for _, files := range []int{25, 100, 400} {
		b.Run(fmt.Sprintf("files=%d", files), func(b *testing.B) {
			dir, paths := writeScalingWorkspace(b, files)
			l := &Linter{Analyzers: DefaultAnalyzers()}
			cfg := &LintConfig{Workspace: dir}
			b.ReportAllocs()
			b.ResetTimer()
			for b.Loop() {
				if _, err := l.LintFiles(cfg, paths); err != nil {
					b.Fatal(err)
				}
			}
			b.StopTimer()
			perFile := float64(b.N) * float64(files)
			b.ReportMetric(float64(b.Elapsed().Nanoseconds())/perFile, "ns/file")
		})
	}
}
