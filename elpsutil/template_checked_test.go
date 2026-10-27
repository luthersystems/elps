// Copyright © 2026 The ELPS authors

package elpsutil_test

import (
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// checkedEnv is a user env with the allocation and value depth limits.
func checkedEnv(t *testing.T, maxAlloc, maxDepth int) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.False(t, lisp.InitializeUserEnv(env).IsError())
	env.Runtime.MaxAlloc = maxAlloc
	env.Runtime.MaxValueDepth = maxDepth
	return env
}

// quasiquoted is (quasiquote src) in env with each param bound globally
// to its arg, which is what a defmacro's quasiquote body sees.
func quasiquoted(t *testing.T, env *lisp.LEnv, src string, params []string, args []*lisp.LVal) *lisp.LVal {
	t.Helper()
	for i, p := range params {
		require.False(t, env.PutGlobal(lisp.Symbol(p), args[i]).IsError())
	}
	return env.LoadStringContext(context.Background(), "qq.lisp", "(quasiquote "+src+")")
}

// sameOutcome requires the two results to be the same value, or errors
// with the same condition and message. The source location and function
// name differ, since quasiquote runs from a file and ExpandChecked from Go.
func sameOutcome(t *testing.T, want, got *lisp.LVal, msg string) {
	t.Helper()
	require.Equal(t, want.IsError(), got.IsError(), "%s: quasiquote %v, ExpandChecked %v", msg, want, got)
	if want.IsError() {
		w, g := (*lisp.ErrorVal)(want), (*lisp.ErrorVal)(got)
		require.Equal(t, w.Condition(), g.Condition(), msg)
		require.Equal(t, w.ErrorMessage(), g.ErrorMessage(), msg)
		return
	}
	require.Equal(t, want.String(), lisp.Quote(got).String(), msg)
}

// TestExpandCheckedMatchesQuasiquote: under every allocation limit, for
// splices of each length, ExpandChecked returns what quasiquote returns,
// the value or the error and its message (luthersystems/elps#894).
func TestExpandCheckedMatchesQuasiquote(t *testing.T) {
	list := func(n int, tag string) *lisp.LVal {
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			cells[i] = lisp.Symbol(tag + strings.Repeat("i", i+1))
		}
		return lisp.SExpr(cells)
	}
	for _, tc := range []struct {
		src    string
		params []string
	}{
		{`(progn (unquote-splicing xs) (f (unquote x)))`, []string{"x", "xs"}},
		{`(a (unquote-splicing xs) b (unquote-splicing ys) (c (unquote x)))`, []string{"x", "xs", "ys"}},
		{`(with-cleanup ((g (unquote x))) (unquote-splicing xs))`, []string{"x", "xs"}},
		{`(h '(unquote x) ''(k (unquote x) l m) (unquote-splicing xs))`, []string{"x", "xs"}},
		{`(let* ([(unquote x) 1] [y (unquote-splicing ys)]) (unquote-splicing xs))`, []string{"x", "xs", "ys"}},
	} {
		tmpl := elpsutil.MustTemplate(tc.src, tc.params...)
		failed := 0
		for n := range 7 {
			for m := range 4 {
				// Each runtime gets its own arguments: an LVal belongs to
				// one runtime (the elpscheck ownership check).
				args := func() []*lisp.LVal {
					args := make([]*lisp.LVal, len(tc.params))
					for i, p := range tc.params {
						switch p {
						case "xs":
							args[i] = list(n, "x")
						case "ys":
							args[i] = list(m, "y")
						default:
							args[i] = lisp.Symbol("v")
						}
					}
					return args
				}
				for limit := 1; limit <= 12; limit++ {
					want := quasiquoted(t, checkedEnv(t, limit, 0), tc.src, tc.params, args())
					got := tmpl.ExpandChecked(checkedEnv(t, limit, 0), args()...)
					sameOutcome(t, want, got, tc.src)
					if want.IsError() {
						failed++
					}
				}
			}
		}
		require.Positive(t, failed, "%s: no limit made the quasiquote fail", tc.src)
	}
}

// TestExpandCheckedDepth: a form deeper than the value depth limit fails
// as quasiquote fails, and one within it expands.
func TestExpandCheckedDepth(t *testing.T) {
	for _, levels := range []int{10, 1100} {
		src := strings.Repeat("(a ", levels) + "'(unquote x)" + strings.Repeat(")", levels)
		tmpl := elpsutil.MustTemplate(src, "x")
		want := quasiquoted(t, checkedEnv(t, 0, 1024), src, []string{"x"}, []*lisp.LVal{lisp.Symbol("v")})
		got := tmpl.ExpandChecked(checkedEnv(t, 0, 1024), lisp.Symbol("v"))
		sameOutcome(t, want, got, src[:20])
		require.Equal(t, levels > 1024, got.IsError())
	}
}

// TestExpandCheckedErrors: a splice of a non-list is quasiquote's error,
// a cancelled context stops the expansion at its first placeholder, and a
// wrong argument count panics.
func TestExpandCheckedErrors(t *testing.T) {
	tmpl := elpsutil.MustTemplate(`(a (unquote-splicing xs))`, "xs")
	want := quasiquoted(t, checkedEnv(t, 0, 0), `(a (unquote-splicing xs))`, []string{"xs"}, []*lisp.LVal{lisp.Int(3)})
	got := tmpl.ExpandChecked(checkedEnv(t, 0, 0), lisp.Int(3))
	sameOutcome(t, want, got, "non-list splice")

	env := checkedEnv(t, 0, 0)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	require.False(t, lisp.WithContext(ctx)(env).IsError())
	got = tmpl.ExpandChecked(env, lisp.Nil())
	require.True(t, got.IsError())
	require.Equal(t, "context-cancelled", (*lisp.ErrorVal)(got).Condition())

	require.Panics(t, func() { tmpl.ExpandChecked(checkedEnv(t, 0, 0)) })
}

// TestExpandCheckedFresh: each expansion is fresh syntax that shares only
// the arguments with another.
func TestExpandCheckedFresh(t *testing.T) {
	tmpl := elpsutil.MustTemplate(`(f (g 1) (unquote x))`, "x")
	x := lisp.Symbol("v")
	env := checkedEnv(t, 0, 0)
	a, b := tmpl.ExpandChecked(env, x), tmpl.ExpandChecked(env, x)
	require.Equal(t, tmpl.Expand(x).String(), a.String())
	require.NotSame(t, a, b)
	require.NotSame(t, a.Cells[0], b.Cells[0])
	require.NotSame(t, a.Cells[1], b.Cells[1])
	require.Same(t, x, a.Cells[2])
}

// TestQuasiquoteWithNilResolve: a nil resolve is an error, not a panic.
func TestQuasiquoteWithNilResolve(t *testing.T) {
	env := checkedEnv(t, 0, 0)
	require.True(t, env.QuasiquoteWith(lisp.Symbol("a"), nil).IsError())
}

// TestExpandCheckedQuotedHead: a list with a quoted unquote head is a
// literal to Expand but an unquote to quasiquote, so ExpandChecked panics
// on it rather than substitute an argument for it.
func TestExpandCheckedQuotedHead(t *testing.T) {
	for _, src := range []string{`(f ('unquote y) (unquote x))`, `(f (g ('unquote-splicing y z)) (unquote x))`} {
		tmpl := elpsutil.MustTemplate(src, "x")
		require.NotPanics(t, func() { tmpl.Expand(lisp.Symbol("v")) })
		require.Panics(t, func() { tmpl.ExpandChecked(checkedEnv(t, 0, 0), lisp.Symbol("v")) }, src)
	}
	// A twice-quoted head is an LQuote wrapper, not an unquote, so both
	// expand it as a literal.
	src := `(f '(unquote x) 'unquote (''unquote y))`
	want := quasiquoted(t, checkedEnv(t, 0, 0), src, []string{"x"}, []*lisp.LVal{lisp.Symbol("v")})
	got := elpsutil.MustTemplate(src, "x").ExpandChecked(checkedEnv(t, 0, 0), lisp.Symbol("v"))
	sameOutcome(t, want, got, src)
}

// TestExpandCheckedNilArgument: a Go nil argument is an error, not a
// panic in the walk.
func TestExpandCheckedNilArgument(t *testing.T) {
	tmpl := elpsutil.MustTemplate(`(f (unquote x))`, "x")
	require.True(t, tmpl.ExpandChecked(checkedEnv(t, 0, 0), nil).IsError())
	env := checkedEnv(t, 0, 0)
	form := lisp.Cells{lisp.Symbol("a"), lisp.Cells{lisp.Symbol("unquote"), lisp.Symbol("b")}.SExpr()}.SExpr()
	require.True(t, env.QuasiquoteWith(form, func(*lisp.LVal) *lisp.LVal { return nil }).IsError())
}
