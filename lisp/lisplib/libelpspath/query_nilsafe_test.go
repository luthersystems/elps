// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// TestNilSafeReadMatchesRead pins the rule of luthersystems/elps#974 over
// every path of up to four steps through one document: ?? gives the result
// of ?, except where ? fails because a step met nil.  There ?? gives nil.
// Any other error of ? is the error of ??, so ?? hides no wrong type.
func TestNilSafeReadMatchesRead(t *testing.T) {
	env := testEnv(t)
	doc := func() *lisp.LVal {
		inner := lisp.SortedMap()
		inner.MapSetString("target", lisp.String("x.example.com"))
		inner.MapSetString("n", lisp.Int(1))
		inner.MapSetString("z", lisp.Nil())
		root := lisp.SortedMap()
		root.MapSetString("api", lisp.Vector([]*lisp.LVal{inner, lisp.Nil()}))
		root.MapSetString("s", lisp.String("leaf"))
		// Nested sequences with nil cells exercise the nested-iterator
		// collapse, which must tell a failed element from an empty one.
		root.MapSetString("grid", lisp.Vector([]*lisp.LVal{
			lisp.Vector([]*lisp.LVal{inner, lisp.Nil()}),
			lisp.Nil(),
			lisp.Vector(nil),
		}))
		return root
	}
	alphabet := []*lisp.LVal{
		lisp.String("api"), lisp.String("target"), lisp.String("n"), lisp.String("z"),
		lisp.String("s"), lisp.String("nope"), lisp.String("grid"), lisp.Int(0), lisp.Int(1), lisp.Int(9),
		lisp.Int(-1), lisp.Symbol("*"),
		lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0), lisp.Int(1)}),
	}
	var paths [][]*lisp.LVal
	var grow func(prefix []*lisp.LVal)
	grow = func(prefix []*lisp.LVal) {
		paths = append(paths, prefix)
		if len(prefix) == 4 {
			return
		}
		for _, s := range alphabet {
			grow(append(append([]*lisp.LVal{}, prefix...), s))
		}
	}
	grow(nil)

	var nilSafe, same int
	for _, path := range paths {
		env.Runtime.ResetSteps()
		read := BuiltinQueryGet(env, lisp.QExpr(append([]*lisp.LVal{doc()}, path...)))
		readSteps := env.Runtime.Steps()
		env.Runtime.ResetSteps()
		safe := BuiltinQueryGetNilSafe(env, lisp.QExpr(append([]*lisp.LVal{doc()}, path...)))
		safeSteps := env.Runtime.Steps()
		name := lisp.QExpr(path).String()
		// ?? walks the path once, as ? does, and stops early at nil, so
		// it never charges more.
		require.LessOrEqual(t, safeSteps, readSteps, "%s: step charge", name)
		if read.Type == lisp.LError && strings.Contains(lisp.GoError(read).Error(), "first argument is nil") {
			require.True(t, safe.IsNil(), "%s: ?? = %v, want nil (? = %v)", name, safe, read)
			nilSafe++
			continue
		}
		require.Equal(t, read.Type == lisp.LError, safe.Type == lisp.LError, "%s: ? = %v, ?? = %v", name, read, safe)
		if read.Type == lisp.LError {
			require.Equal(t, lisp.GoError(read).Error(), lisp.GoError(safe).Error(), name)
		} else {
			require.Equal(t, read.String(), safe.String(), name)
		}
		same++
	}
	// Both outcomes must occur, or the alphabet tests nothing.
	require.Positive(t, nilSafe)
	require.Positive(t, same)
	t.Logf("%d paths: %d nil-safe, %d the same as ?", len(paths), nilSafe, same)
}
