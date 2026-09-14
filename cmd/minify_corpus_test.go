// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"errors"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

// TestMinifyCorpus compares actual CLI exit codes and stdout. Each source is
// minified independently, with its load-file dependencies available unchanged.
func TestMinifyCorpus(t *testing.T) {
	bin := buildTestBinary(t)
	root, err := filepath.Abs("..")
	require.NoError(t, err)
	for _, corpus := range []string{"_examples", "parser/rdparser/testdata/bench", "lisp/x/debugger/dapserver/testdata", "lisp/x/profiler", "analysis/perf/testdata", "lisp/testfixtures"} {
		require.NoError(t, filepath.WalkDir(filepath.Join(root, corpus), func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() || !strings.HasSuffix(path, ".lisp") {
				return nil
			}
			rel, err := filepath.Rel(root, path)
			require.NoError(t, err)
			t.Run(rel, func(t *testing.T) {
				originalCode, originalOut, _ := runCorpusCLI(t, bin, root, "run", rel)
				code, output, stderr := runCorpusCLI(t, bin, root, "minify", rel)
				require.Equal(t, 0, code, "minify failed: %s", stderr)
				// Place the output beside copies of its sibling sources so relative
				// load-file paths keep the same meaning without modifying the repository.
				dir := t.TempDir()
				siblings, err := os.ReadDir(filepath.Dir(path))
				require.NoError(t, err)
				for _, sibling := range siblings {
					if sibling.IsDir() {
						continue
					}
					data, err := os.ReadFile(filepath.Join(filepath.Dir(path), sibling.Name()))
					require.NoError(t, err)
					require.NoError(t, os.WriteFile(filepath.Join(dir, sibling.Name()), data, 0o600))
				}
				require.NoError(t, os.WriteFile(filepath.Join(dir, filepath.Base(path)), []byte(output), 0o600))
				minCode, minOut, minErr := runCorpusCLI(t, bin, dir, "run", filepath.Base(path))
				require.Equal(t, originalCode, minCode, "minified failed: %s", minErr)
				require.Equal(t, originalOut, minOut, "stdout differs")
			})
			return nil
		}))
	}
}

func runCorpusCLI(t *testing.T, bin, dir string, args ...string) (int, string, string) {
	t.Helper()
	cmd := exec.CommandContext(t.Context(), bin, args...) //nolint:gosec // test executes the locally built CLI
	cmd.Dir = dir
	var stdout, stderr bytes.Buffer
	cmd.Stdout, cmd.Stderr = &stdout, &stderr
	err := cmd.Run()
	if err == nil {
		return 0, stdout.String(), stderr.String()
	}
	var exitErr *exec.ExitError
	require.True(t, errors.As(err, &exitErr), "CLI did not run: %v", err)
	return exitErr.ExitCode(), stdout.String(), stderr.String()
}
