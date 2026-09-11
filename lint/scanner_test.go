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
	for _, src := range []string{
		";" + strings.Repeat("a", 300000) + "(unknown-function)\n(+ 1 2)\n",
		"#!" + strings.Repeat("a", 300000) + "(unknown-function)\n(+ 1 2)\n",
		strings.Repeat(" ", 300000) + "(+ 1 2)\n",
		strings.Repeat("abc ", 40000),
	} {
		_, err := l.LintFile([]byte(src), "valid.lisp")
		require.NoError(t, err, "comments, whitespace, and separate short tokens remain valid")
	}
}
