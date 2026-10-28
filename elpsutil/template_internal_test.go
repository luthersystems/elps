// Copyright © 2026 The ELPS authors

package elpsutil

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// TestExpandCheckedArgumentKinds: for each kind of argument, in an
// unquote, a quoted unquote and a splice, ExpandChecked returns what
// quasiquote's walk of the template returns.  Plain arguments take the
// fast path and the others the walk, so this pins fits to the walk's
// treatment of its arguments: an error is the result, a spliced-flag value
// is spliced, and a nil, a non-list splice or a quoted splice fails
// (luthersystems/substrate#973).
func TestExpandCheckedArgumentKinds(t *testing.T) {
	kinds := []struct {
		name string
		arg  func(env *lisp.LEnv) *lisp.LVal
	}{
		{"symbol", func(*lisp.LEnv) *lisp.LVal { return lisp.Symbol("v") }},
		{"int", func(*lisp.LEnv) *lisp.LVal { return lisp.Int(3) }},
		{"list", func(*lisp.LEnv) *lisp.LVal { return lisp.Cells{lisp.Symbol("p"), lisp.Symbol("q")}.SExpr() }},
		{"nil", func(*lisp.LEnv) *lisp.LVal { return lisp.Nil() }},
		{"quoted list", func(*lisp.LEnv) *lisp.LVal { return lisp.Quote(lisp.Cells{lisp.Symbol("p")}.SExpr()) }},
		{"error", func(env *lisp.LEnv) *lisp.LVal { return env.Errorf("boom") }},
		{"spliced list", func(*lisp.LEnv) *lisp.LVal {
			return lisp.Splice(lisp.Cells{lisp.Symbol("p"), lisp.Symbol("q")}.SExpr())
		}},
		{"spliced int", func(*lisp.LEnv) *lisp.LVal { return lisp.Splice(lisp.Int(3)) }},
		{"go nil", func(*lisp.LEnv) *lisp.LVal { return nil }},
	}
	for _, src := range []string{`(f (unquote x) g)`, `(f '(unquote x) g)`, `(f (unquote-splicing x) g)`} {
		tmpl := MustTemplate(src, "x")
		for _, k := range kinds {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			require.False(t, lisp.InitializeUserEnv(env).IsError())
			arg := k.arg(env)
			want := env.QuasiquoteWith(tmpl.root.source(), func(*lisp.LVal) *lisp.LVal { return arg })
			got := tmpl.ExpandChecked(env, arg)
			msg := src + " with " + k.name
			require.Equal(t, want.IsError(), got.IsError(), "%s: walk %v, ExpandChecked %v", msg, want, got)
			if want.IsError() {
				require.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage(), msg)
				continue
			}
			require.Equal(t, want.String(), got.String(), msg)
		}
	}
}
