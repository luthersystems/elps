// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/formatter"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestSymbolSizeRunAndFormat(t *testing.T) {
	for _, n := range []int{131071, 131072, 140000} {
		t.Run(fmt.Sprint(n), func(t *testing.T) {
			src := "(set '" + strings.Repeat("a", n) + " 1)\n"
			t.Run("run", func(t *testing.T) {
				resetRunFlags(t)
				runExpression, runPrint = true, true
				var out bytes.Buffer
				stderr, err := os.CreateTemp(t.TempDir(), "stderr")
				require.NoError(t, err)
				previousStderr := os.Stderr
				os.Stderr = stderr
				t.Cleanup(func() { os.Stderr = previousStderr; _ = stderr.Close() })
				err = runElps([]string{src}, &out)
				if n < token.DefaultBufSize {
					require.NoError(t, err)
					require.Equal(t, "1\n", out.String())
				} else {
					require.ErrorIs(t, err, errRendered)
					_, seekErr := stderr.Seek(0, io.SeekStart)
					require.NoError(t, seekErr)
					diagnostic, readErr := io.ReadAll(stderr)
					require.NoError(t, readErr)
					require.Contains(t, string(diagnostic), "token exceeds maximum allowable size (131072 bytes)")
					require.Empty(t, out.String())
				}
			})
			t.Run("fmt", func(t *testing.T) {
				out, err := formatter.Format([]byte(src), nil)
				if n < token.DefaultBufSize {
					require.NoError(t, err)
					require.Equal(t, src, string(out))
				} else {
					require.ErrorContains(t, err, "token exceeds maximum allowable size (131072 bytes)")
					require.Empty(t, out)
				}
			})
		})
	}
}

func TestBOMToolPipeline(t *testing.T) {
	for _, body := range []string{"(+ 1 2)\n", "#!/usr/bin/env elps\n(+ 1 2)\n"} {
		t.Run(fmt.Sprintf("shebang=%t", strings.HasPrefix(body, "#!")), func(t *testing.T) {
			src := "\ufeff" + body
			dir := t.TempDir()
			path := filepath.Join(dir, "bom.lisp")
			require.NoError(t, os.WriteFile(path, []byte(src), 0o600))
			t.Run("run", func(t *testing.T) {
				resetRunFlags(t)
				runPrint, runRootDir = true, dir
				var out bytes.Buffer
				require.NoError(t, runElps([]string{path}, &out))
				require.Equal(t, "3\n", out.String())
			})
			t.Run("fmt", func(t *testing.T) {
				out, err := formatter.Format([]byte(src), nil)
				require.NoError(t, err)
				require.Equal(t, body, string(out), "formatter drops the leading BOM")
				again, err := formatter.Format(out, nil)
				require.NoError(t, err)
				require.Equal(t, out, again)
			})
			t.Run("lint", func(t *testing.T) {
				l := &lint.Linter{Analyzers: lint.DefaultAnalyzers()}
				diags, err := l.LintFile([]byte(src), path)
				require.NoError(t, err)
				require.Empty(t, diags)
			})
			t.Run("minify", func(t *testing.T) {
				resetMinifyFlags()
				defer resetMinifyFlags()
				var out bytes.Buffer
				require.NoError(t, runMinify([]string{path}, strings.NewReader(""), &out))
				require.NotContains(t, out.String(), "\ufeff")
				_, err := parser.NewReader().Read("minified", strings.NewReader(out.String()))
				require.NoError(t, err)
			})
			t.Run("analyze", func(t *testing.T) {
				var out, stderr bytes.Buffer
				config := writeAnalyzeFixture(t, filepath.Join(dir, ".elps-analyze.yaml"), "{}\n")
				code, err := runAnalyze([]string{path}, &out, &stderr, analyzeRunConfig{configFile: config, failOn: "error"})
				require.NoError(t, err)
				require.Equal(t, 0, code, stderr.String())
			})
		})
	}
}

func TestNumericLiteralParsingAndLint(t *testing.T) {
	for _, src := range []string{"1.5", "-1", "1e5", ".5", "x10", "(1 .3)", "#x10", "#o17", "#X10", "#O17"} {
		t.Run(src, func(t *testing.T) {
			_, err := parser.NewReader().Read("valid", strings.NewReader(src))
			require.NoError(t, err)
		})
	}
	for _, literal := range []string{"0x10", "1_000", "1.2.3", "1e5x"} {
		t.Run(literal, func(t *testing.T) {
			src := "'(" + literal + ")"
			_, err := parser.NewReader().Read("invalid", strings.NewReader(src))
			require.ErrorContains(t, err, "invalid numeric literal")
			l := &lint.Linter{Analyzers: lint.DefaultAnalyzers()}
			diags, err := l.LintFile([]byte(src), "invalid.lisp")
			require.ErrorContains(t, err, "invalid numeric literal")
			require.Empty(t, diags)
		})
	}
	for _, src := range []string{"#x" + strings.Repeat("f", 40), "#o" + strings.Repeat("7", 40)} {
		_, err := parser.NewReader().Read("overflow", strings.NewReader(src))
		require.ErrorContains(t, err, "integer-overflow-error")
	}
}
