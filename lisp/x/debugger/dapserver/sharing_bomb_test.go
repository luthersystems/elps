// Copyright © 2026 The ELPS authors

package dapserver

import (
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The step-in expression walks under sharing (see exprWalk and
// lisp/sharing.go).  A paused expression can embed a program-built value,
// and one built as (set! x (list x x)) D times has 2^D paths: the walks
// must finish in time linear in its distinct expressions, and give their
// tree answer on every tree.

// sharingEnv returns a user environment where my-add is a user-defined
// function and list, length and progn are not.
func sharingEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	for _, rc := range []*lisp.LVal{
		lisp.InitializeUserEnv(env),
		env.InPackage(lisp.String("user")),
		env.LoadString("user-test", `(defun my-add (a b) (+ a b))`),
	} {
		require.NotEqual(t, lisp.LError, rc.Type, "%v", rc)
	}
	return env
}

func userCall() *lisp.LVal {
	return lisp.SExpr([]*lisp.LVal{lisp.Symbol("my-add"), lisp.Int(1), lisp.Int(2)})
}

// userCallAt is userCall with its head at source line line, so a target
// says which call it is.
func userCallAt(line int) *lisp.LVal {
	call := userCall()
	call.Cells[0].SetSource(&token.Location{File: "calls", Line: line, Col: 1})
	return call
}

// stepTarget is a step-in target as the tests compare it: the function,
// which call of it (the occurrence the engine counts), and where.
type stepTarget struct {
	name       string
	occurrence int
	line       int
}

// callBomb is (list x x) nested depth times over leaf: depth distinct
// expressions and 2^depth paths, each level's head a builtin call.
func callBomb(depth int, leaf *lisp.LVal) *lisp.LVal {
	for range depth {
		leaf = lisp.SExpr([]*lisp.LVal{lisp.Symbol("list"), leaf, leaf})
	}
	return leaf
}

// quoted is (length (quote v) rest...): what a program pauses on after
// (eval (list 'length (list 'quote v))).
func quoted(v *lisp.LVal, rest ...*lisp.LVal) *lisp.LVal {
	cells := []*lisp.LVal{lisp.Symbol("length"), lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), v})}
	return lisp.SExpr(append(cells, rest...))
}

// wideTree is (list (my-add 1 2) ...) with n distinct calls, or with
// (length 1) in place of every call when calls is false: a tree larger
// than the budget.
func wideTree(n int, calls bool) *lisp.LVal {
	cells := []*lisp.LVal{lisp.Symbol("list")}
	for range n {
		if calls {
			cells = append(cells, userCall())
		} else {
			cells = append(cells, lisp.SExpr([]*lisp.LVal{lisp.Symbol("length"), lisp.Int(1)}))
		}
	}
	return lisp.SExpr(cells)
}

func TestHasUserFunCallSharingBomb(t *testing.T) {
	t.Parallel()
	env := sharingEnv(t)
	bomb := callBomb(40, lisp.SExpr([]*lisp.LVal{lisp.Symbol("length"), lisp.Int(1)}))
	cycle := lisp.SExpr([]*lisp.LVal{lisp.Symbol("list"), lisp.Int(1)})
	cycle.Cells[1] = cycle
	for _, tt := range []struct {
		name string
		expr *lisp.LVal
		want bool
	}{
		// No user call anywhere: the whole shared value is walked.
		{"bomb", quoted(bomb), true},
		// The user call follows the shared value, so the walk must get
		// past it -- skipping repeats -- to find the call.
		{"bomb then user call", quoted(bomb, userCall()), false},
		{"user call under the bomb", quoted(callBomb(40, userCall())), false},
		// A cyclic expression used to recurse without end.
		{"cycle", quoted(cycle), true},
		{"cycle then user call", quoted(cycle, userCall()), false},
	} {
		var got bool
		testdeadline.Watch("isBuiltinCall: "+tt.name, 20*time.Second, 1<<30, func() { got = isBuiltinCall(env, tt.expr) })
		assert.Equal(t, tt.want, got, "isBuiltinCall(%s)", tt.name)
	}
}

// A tree larger than the budget gets its tree answer, wherever the call is.
func TestHasUserFunCallLargeTree(t *testing.T) {
	t.Parallel()
	env := sharingEnv(t)
	assert.True(t, isBuiltinCall(env, quoted(wideTree(3*sharedWalkBudget, false))))
	last := wideTree(3*sharedWalkBudget, false)
	last.Cells[len(last.Cells)-1] = userCall()
	assert.False(t, isBuiltinCall(env, quoted(last)))
}

// treeTargets is the oracle for collectStepInTargets: the targets of the
// unguarded tree walk, as (name, occurrence) pairs, stopping at limit.
func treeTargets(env *lisp.LEnv, expr *lisp.LVal, limit int) []stepTarget {
	var out []stepTarget
	occ := map[string]int{}
	visit := func(v *lisp.LVal) {
		head := v.Cells[0]
		if head == nil || head.Type != lisp.LSymbol {
			return
		}
		if r := env.Get(lisp.Symbol(head.Str)); r.Type == lisp.LFun && r.FunType == lisp.LFunNone && funraw.Env(r) != nil {
			name := r.Package() + ":" + head.Str
			loc, _ := head.Source()
			out = append(out, stepTarget{name: name, occurrence: occ[name], line: loc.Line})
			occ[name]++
		}
	}
	var walk func(v *lisp.LVal)
	walk = func(v *lisp.LVal) {
		if len(out) >= limit || v == nil || v.Type != lisp.LSExpr || v.IsNil() {
			return
		}
		visit(v)
		for _, c := range v.Cells[1:] {
			walk(c)
		}
	}
	visit(expr)
	for _, c := range expr.Cells[1:] {
		walk(c)
	}
	return out
}

// collected runs collectStepInTargets and returns its targets as
// (name, occurrence) pairs, in order.
func collected(env *lisp.LEnv, expr *lisp.LVal) []stepTarget {
	h := &handler{}
	var out []stepTarget
	for _, target := range h.collectStepInTargets(env, expr) {
		info := h.stepInTargets[target.Id]
		out = append(out, stepTarget{name: info.qualifiedName, occurrence: info.occurrence, line: target.Line})
	}
	return out
}

// Past the budget, the first expression reached again ends the walk: the
// targets are a prefix of the tree walk's, with its occurrence numbers.
func TestCollectStepInTargetsSharingBomb(t *testing.T) {
	t.Parallel()
	env := sharingEnv(t)
	t.Run("depth 40", func(t *testing.T) {
		var got []stepTarget
		expr := quoted(callBomb(40, userCall()))
		testdeadline.Watch("collectStepInTargets over a 40-level sharing bomb", 20*time.Second, 1<<30, func() { got = collected(env, expr) })
		require.NotEmpty(t, got)
		require.Less(t, len(got), sharedWalkBudget)
		assert.Equal(t, treeTargets(env, expr, len(got)), got)
	})
	t.Run("prefix", func(t *testing.T) {
		// Small enough for the oracle to walk every path, large enough
		// for the guard to switch on.  A call at every level makes a
		// target on each side of a repeat, so a walk that skipped the
		// repeat instead of stopping would misnumber what follows.
		leaf := userCallAt(1)
		for i := range 14 {
			leaf = lisp.SExpr([]*lisp.LVal{lisp.Symbol("list"), leaf, userCallAt(i + 2), leaf})
		}
		expr := quoted(leaf)
		got := collected(env, expr)
		tree := treeTargets(env, expr, 1<<30)
		require.NotEmpty(t, got)
		require.Less(t, len(got), len(tree), "the guard did not switch on")
		assert.Equal(t, tree[:len(got)], got)
	})
	t.Run("cycle", func(t *testing.T) {
		cycle := lisp.SExpr([]*lisp.LVal{lisp.Symbol("my-add"), lisp.Int(1)})
		cycle.Cells[1] = cycle
		var got []stepTarget
		testdeadline.Watch("collectStepInTargets over a cycle", 20*time.Second, 1<<30, func() { got = collected(env, cycle) })
		require.NotEmpty(t, got)
		assert.Equal(t, treeTargets(env, cycle, len(got)), got)
	})
}

// Below the budget, and on any tree, the targets are the tree walk's: a
// small shared call is a target at each place it appears.
func TestCollectStepInTargetsTreeUnchanged(t *testing.T) {
	t.Parallel()
	env := sharingEnv(t)
	call := userCall()
	for name, expr := range map[string]*lisp.LVal{
		"small shared": lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), call, call}),
		"large tree":   wideTree(3*sharedWalkBudget, true),
	} {
		got := collected(env, expr)
		tree := treeTargets(env, expr, 1<<30)
		assert.Equal(t, tree, got, name)
	}
	assert.Len(t, collected(env, lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), call, call})), 2)
	assert.Len(t, collected(env, wideTree(3*sharedWalkBudget, true)), 3*sharedWalkBudget)
}
