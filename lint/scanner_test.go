package lint

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestOversizedTokenMigrationDiagnostic(t *testing.T) {
	l := &Linter{Analyzers: DefaultAnalyzers()}
	_, err := l.LintFile([]byte(strings.Repeat("a", token.DefaultBufSize+1)), "large.lisp")
	require.ErrorContains(t, err, "large.lisp:1:1: scan-error: token exceeds maximum allowable size")
	for _, tt := range []struct {
		name  string
		src   string
		count int
		form  string
	}{
		{"comment", ";" + strings.Repeat("a", 300000) + "(unknown-function)\n(+ 1 2)\n", 1, "(+ 1 2)"},
		{"shebang", "#!" + strings.Repeat("a", 300000) + "(unknown-function)\n(+ 1 2)\n", 1, "(+ 1 2)"},
		{"whitespace", strings.Repeat(" ", 300000) + "(+ 1 2)\n", 1, "(+ 1 2)"},
		{"short tokens", strings.Repeat("abc ", 40000), 40000, "abc"},
	} {
		t.Run(tt.name, func(t *testing.T) {
			analyzed := false
			inspect := &Analyzer{
				Name: "inspect-forms",
				Run: func(pass *Pass) error {
					analyzed = true
					require.Len(t, pass.Exprs, tt.count, "only intended forms reach analyzers")
					for _, expr := range pass.Exprs {
						require.Equal(t, tt.form, expr.String())
					}
					return nil
				},
			}
			l := &Linter{Analyzers: append(DefaultAnalyzers(), inspect)}
			diagnostics, err := l.LintFile([]byte(tt.src), "valid.lisp")
			require.NoError(t, err, "comments, whitespace, and separate short tokens remain valid")
			require.True(t, analyzed, "the analyzer must inspect the parsed forms")
			for _, diagnostic := range diagnostics {
				require.NotContains(t, diagnostic.Message, "unknown-function", "comment text must not reach analyzers")
			}
		})
	}
}
