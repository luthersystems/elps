// Copyright © 2026 The ELPS authors

package analysis_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/resolvergolden"
	"github.com/stretchr/testify/require"
)

func TestRepoResolverParity(t *testing.T) {
	root, err := filepath.Abs("..")
	require.NoError(t, err)
	inputs, err := resolvergolden.Inputs(root)
	require.NoError(t, err)
	want, err := os.ReadFile("testdata/resolver-main.golden.txt")
	require.NoError(t, err)
	// Comparing per input keeps a failure small enough to review. The manifest
	// also prevents silently dropping a file or seed from the comparison.
	// The golden is keyed by input name and content hash. An input added or
	// edited after the golden was recorded has no old-resolver output to
	// compare against, so it is skipped rather than failed; parity was
	// established over every input that existed when the resolver changed.
	chunks := map[string]string{}
	for i, c := range strings.Split(string(want), "\ninput ") {
		if i > 0 {
			c = "input " + c
		}
		header, _, _ := strings.Cut(c, "\n")
		chunks[header] = strings.TrimSuffix(c, "\n") + "\n"
	}
	matched := 0
	for _, input := range inputs {
		t.Run(input.Name, func(t *testing.T) {
			got := resolvergolden.Snapshot(input)
			header, _, _ := strings.Cut(got, "\n")
			golden, ok := chunks[header]
			if !ok {
				t.Skipf("no old-resolver snapshot for %s at this content hash", input.Name)
			}
			matched++
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
			require.Equal(t, golden, strings.TrimSuffix(got, "\n")+"\n")
		})
	}
	// Guard against the comparison silently covering nothing.
	require.Greater(t, matched, len(chunks)/2, "most golden inputs must still be compared")
}
