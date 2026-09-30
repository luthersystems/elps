// Copyright © 2026 The ELPS authors

package analysis_test

import (
	"os"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/resolvergolden"
	"github.com/stretchr/testify/require"
)

// TestRepoResolverParity compares the resolver with main's pre-#754 resolver
// over frozen copies of the repository's Lisp files and the fuzz seeds, as
// they were when the golden was recorded (testdata/resolver-inputs). Every
// fixture must have a golden entry with the same content hash, and every
// golden entry a fixture: a missing or changed input fails.
func TestRepoResolverParity(t *testing.T) {
	inputs, err := resolvergolden.FixtureInputs("testdata/resolver-inputs")
	require.NoError(t, err)
	want, err := os.ReadFile("testdata/resolver-main.golden.txt")
	require.NoError(t, err)
	chunks := strings.Split(strings.TrimSuffix(string(want), "\n"), "\ninput ")
	for i := 1; i < len(chunks); i++ {
		chunks[i] = "input " + chunks[i]
	}
	require.Len(t, inputs, len(chunks), "fixtures and golden entries must correspond one to one")
	for i, input := range inputs {
		t.Run(input.Name, func(t *testing.T) {
			golden := chunks[i] + "\n"
			if input.Name == "editors/vscode/test/grammar/builtins.lisp" {
				// PR #754 adds this builtin. Pin that single registry delta
				// explicitly while retaining the old resolver's golden verbatim.
				unresolved := "unresolved macroexpand-all macro=false source=1706:120:2-1721:120:17 node=symbol:\"macroexpand-all\" quoted=false@1706:120:2-1721:120:17\n"
				ref := "ref :macroexpand-all scope=0 source=1706:120:2-1721:120:17 node=symbol:\"macroexpand-all\" quoted=false@1706:120:2-1721:120:17\n"
				require.Equal(t, 1, strings.Count(golden, unresolved))
				golden = strings.Replace(golden, unresolved, "", 1)
				at := strings.Index(golden, "ref :gensym ")
				require.GreaterOrEqual(t, at, 0)
				golden = golden[:at] + ref + golden[at:]
			}
			require.Equal(t, golden, resolvergolden.Snapshot(input))
		})
	}
}
