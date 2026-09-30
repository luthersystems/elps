// Copyright © 2026 The ELPS authors

package analysis

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestReferenceLocationsRemainIndependent(t *testing.T) {
	exprs, err := rdparser.New(token.NewScanner("references.lisp", strings.NewReader("plain 'quoted"))).ParseProgram()
	require.NoError(t, err)
	missing := lisp.Symbol("missing")
	missing.SetSource(nil)
	for _, node := range append(exprs, missing) {
		t.Run(node.Str, func(t *testing.T) {
			sym := &Symbol{Name: node.Str}
			first, second := newReference(sym, node), newReference(sym, node)
			unresolved := newUnresolvedRef(node, true)
			want := astutil.SymbolLoc(node)
			require.Same(t, sym, first.Symbol)
			require.Same(t, node, first.Node)
			require.Equal(t, want, first.Source)
			require.Equal(t, want, unresolved.Source)
			require.Equal(t, node.Str, unresolved.Name)
			require.True(t, unresolved.InsideMacroCall)
			if want == nil {
				return
			}
			before, ok := node.Source()
			require.True(t, ok)
			first.Source.Col++
			unresolved.Source.Line++
			require.Equal(t, want, second.Source)
			after, ok := node.Source()
			require.True(t, ok)
			require.Equal(t, before, after)
		})
	}
}
