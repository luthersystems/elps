// Copyright © 2026 The ELPS authors

package analysis_test

import (
	"flag"
	"os"
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/resolvergolden"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

var updateResolverExpansionGolden = flag.Bool("update-resolver-expansion-golden", false, "regenerate the expander-enabled resolver golden")

// TestResolverExpansionParity pins Analyze with an EnvMacroExpander over the
// frozen fixtures in testdata/resolver-expansion-inputs: expansion, generated
// definitions and their origins, package transitions (#769) and forward
// references, independently of the no-expander golden. Each fixture gets a
// fresh env loaded with only its in-package and defmacro forms, so Analyze
// itself must discover generated bindings. After reviewing an intentional
// change, regenerate with:
//
//	go test ./analysis -run '^TestResolverExpansionParity$' -count=1 -args -update-resolver-expansion-golden
func TestResolverExpansionParity(t *testing.T) {
	inputs, err := resolvergolden.FixtureInputs("testdata/resolver-expansion-inputs")
	require.NoError(t, err)
	require.NotEmpty(t, inputs)
	snapshots := make([]string, 0, len(inputs))
	for _, input := range inputs {
		snapshots = append(snapshots, resolvergolden.SnapshotWithExpander(input, fixtureMacroExpander(t, input)))
	}
	path := "testdata/resolver-expansion.golden.txt"
	if *updateResolverExpansionGolden {
		require.NoError(t, os.WriteFile(path, []byte(strings.Join(snapshots, "")), 0o600))
	}
	want, err := os.ReadFile(path)
	require.NoError(t, err)
	chunks := strings.Split(strings.TrimSuffix(string(want), "\n"), "\ninput ")
	for i := 1; i < len(chunks); i++ {
		chunks[i] = "input " + chunks[i]
	}
	require.Len(t, inputs, len(chunks), "fixtures and golden entries must correspond one to one")
	for i, input := range inputs {
		t.Run(input.Name, func(t *testing.T) {
			require.Equal(t, chunks[i]+"\n", snapshots[i])
		})
	}
}

func fixtureMacroExpander(t *testing.T, input resolvergolden.Input) *analysis.EnvMacroExpander {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = rdparser.NewReader()
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	exprs, err := rdparser.New(token.NewScanner(input.Name, strings.NewReader(string(input.Source)))).ParseProgram()
	require.NoError(t, err)
	// Load declarations only: evaluating fixture calls would conceal whether
	// Analyze itself discovers generated bindings and package transitions.
	var preamble []*lisp.LVal
	for _, expr := range astutil.PackageForms(exprs) {
		switch astutil.HeadSymbol(expr) {
		case "in-package", "defmacro":
			preamble = append(preamble, expr)
		}
	}
	expander := &analysis.EnvMacroExpander{Env: env}
	require.Empty(t, expander.LoadWorkspaceMacros(preamble), input.Name)
	return expander
}
