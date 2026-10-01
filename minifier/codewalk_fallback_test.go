// Copyright © 2026 The ELPS authors

package minifier

import (
	"encoding/json"
	"fmt"
	"os"
	"sort"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestGlobalFallbackGolden(t *testing.T) {
	sources := []string{
		`(in-package 'p) (export 'f) (defun f (x) x)`,
		`(progn (in-package 'p) (export computed))`,
		`(quote (export 'f)) '(export 'f) [in-package 'p]`,
		`(quasiquote (unquote (quote (export 'f))))`,
		`(defmacro m () '(export 'f)) (export 'later)`,
		`(macrolet ((m () (quasiquote (unquote (export 'f))))) (export 'g))`,
		`(progn (quasiquote safe) (export 'f))`,
		`(progn (defmacro m () safe) (export 'f))`,
		`(progn (export 'f) (use-package computed) (export computed))`,
		`(lisp:export '(a b)) (lisp:in-package 'p)`,
		`(in-package) (use-package) (export)`,
		`(export '(a (b) 1)) (export (quote f)) (export "f")`,
	}
	names := []string{"defun", "defmacro", "deftype", "test-let", "test-let*"}
	for _, op := range lisp.DefaultSpecialOps() {
		names = append(names, op.Name())
	}
	sort.Strings(names)
	for _, name := range names {
		for _, prefix := range []string{"", "lisp:"} {
			for _, args := range []string{"", " () (export 'f)", " bad (in-package 'p)", " ((m () (quasiquote (unquote (export 'f))))) (export 'g)"} {
				sources = append(sources, "("+prefix+name+args+")")
			}
		}
	}
	type snapshot struct {
		Source    string
		Fallbacks []string
		Output    string
		Symbols   SymbolMap
	}
	var snapshots []snapshot
	for _, source := range sources {
		exprs, err := rdparser.New(token.NewScanner("fallback.lisp", strings.NewReader(source))).ParseProgram()
		require.NoError(t, err)
		s := snapshot{Source: source}
		for _, expr := range exprs {
			for _, top := range []bool{false, true} {
				for _, template := range []bool{false, true} {
					found := firstGlobalFallback(expr, top, template)
					text := "-"
					if found != nil {
						loc, ok := found.Source()
						require.True(t, ok)
						text = fmt.Sprintf("%s@%d:%d:%d", found, loc.Pos, loc.Line, loc.Col)
					}
					s.Fallbacks = append(s.Fallbacks, text)
				}
			}
		}
		out, symbols, err := MinifySource([]byte(source), "fallback.lisp", nil)
		require.NoError(t, err)
		s.Output, s.Symbols = string(out), symbols
		snapshots = append(snapshots, s)
	}
	data, err := json.MarshalIndent(snapshots, "", "  ")
	require.NoError(t, err)
	data = append(data, '\n')
	path := "testdata/codewalk-fallback.golden.json"
	if os.Getenv("ELPS_UPDATE_CODEWALK_GOLDEN") == "1" {
		require.NoError(t, os.WriteFile(path, data, 0o600))
	}
	want, err := os.ReadFile(path)
	require.NoError(t, err)
	require.Equal(t, string(want), string(data))
}
