package mcpserver

import (
	"sort"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lint"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// helpAnalyzerRow is one parsed row of the help prompt's "Lint Analyzers"
// table.
type helpAnalyzerRow struct {
	Name        string
	Severity    string
	Semantic    string
	HasSemantic bool
}

// parseHelpAnalyzerRows extracts the rows of the "## Lint Analyzers" markdown
// table out of the help prompt, resolving cells by their header name so the
// test does not depend on column order.
func parseHelpAnalyzerRows(t *testing.T, content string) []helpAnalyzerRow {
	t.Helper()
	_, after, found := strings.Cut(content, "## Lint Analyzers")
	require.True(t, found, "help prompt must carry a Lint Analyzers section")
	section, _, _ := strings.Cut(after, "\n## ")

	cells := func(line string) []string {
		line = strings.TrimSpace(line)
		line = strings.TrimPrefix(line, "|")
		line = strings.TrimSuffix(line, "|")
		out := strings.Split(line, "|")
		for i := range out {
			out[i] = strings.TrimSpace(out[i])
		}
		return out
	}

	var header []string
	var rows []helpAnalyzerRow
	for _, line := range strings.Split(section, "\n") {
		if !strings.HasPrefix(strings.TrimSpace(line), "|") {
			continue
		}
		c := cells(line)
		if header == nil {
			header = c
			continue
		}
		if strings.HasPrefix(c[0], "---") {
			continue
		}
		col := func(name string) (string, bool) {
			for i, h := range header {
				if strings.EqualFold(h, name) && i < len(c) {
					return c[i], true
				}
			}
			return "", false
		}
		name, _ := col("Analyzer")
		severity, _ := col("Severity")
		semantic, hasSemantic := col("Semantic")
		rows = append(rows, helpAnalyzerRow{
			Name:        name,
			Severity:    severity,
			Semantic:    semantic,
			HasSemantic: hasSemantic,
		})
	}
	require.NotEmpty(t, rows, "Lint Analyzers table must have rows")
	return rows
}

// TestHelpAnalyzerTableMatchesRegistry pins the help prompt's analyzer table to
// lint.DefaultAnalyzers(). A hand-maintained table drifted: it advertised
// analyzers that were never registered and omitted registered ones (#646).
func TestHelpAnalyzerTableMatchesRegistry(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.helpTool(t.Context(), nil, HelpInput{})
	require.NoError(t, err)

	rows := parseHelpAnalyzerRows(t, resp.Content)

	registered := make(map[string]*lint.Analyzer)
	for _, a := range lint.DefaultAnalyzers() {
		registered[a.Name] = a
	}

	listed := make(map[string]bool)
	var fictional []string
	for _, row := range rows {
		listed[row.Name] = true
		if registered[row.Name] == nil {
			fictional = append(fictional, row.Name)
		}
	}
	var missing []string
	for _, name := range lint.AnalyzerNames() {
		if !listed[name] {
			missing = append(missing, name)
		}
	}
	sort.Strings(fictional)
	assert.Empty(t, fictional,
		"help prompt advertises analyzers that lint.DefaultAnalyzers() does not register")
	assert.Empty(t, missing,
		"help prompt omits registered analyzers")

	// Every row's severity and semantic flag must match the registered
	// analyzer, so the table cannot drift in its cells either.
	for _, row := range rows {
		a := registered[row.Name]
		if a == nil {
			continue
		}
		assert.Equal(t, a.Severity.String(), row.Severity,
			"severity for %s must match the registered analyzer", row.Name)
		require.True(t, row.HasSemantic,
			"Lint Analyzers table must carry a Semantic column (row %s)", row.Name)
		want := "no"
		if a.Semantic {
			want = "yes"
		}
		assert.Equal(t, want, row.Semantic,
			"semantic flag for %s must match the registered analyzer", row.Name)
	}

	// Rows are sorted by name.
	names := make([]string, len(rows))
	for i, row := range rows {
		names[i] = row.Name
	}
	assert.True(t, sort.StringsAreSorted(names), "analyzer rows must be sorted by name: %v", names)
}
