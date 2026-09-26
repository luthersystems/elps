// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/parser/token"
)

// The sharing-bomb regression net (lisp/sharing.go).  A value built as
// (set! x (list x x)) 40 times has 40 distinct containers and 2^40 paths.
// Every deep walker in this package that a program's value can reach must
// finish on it in time linear in the distinct containers: a walker that
// treats it as a tree does 2^40 work inside one step, with no step charge,
// and at this depth never comes back.  Each row runs behind a watchdog that
// ends the test binary rather than hang or exhaust the host.
//
// A new deep walker belongs in this table.  The per-walker regressions --
// sharing preserved in the output, depth limits exact, trees unchanged --
// live in sharing_bomb_test.go and sharing_bomb_eval_test.go.
func TestSharingBombEveryWalker(t *testing.T) {
	bombOf := func() *LVal {
		v := Int(1)
		for range 40 {
			v = SExpr([]*LVal{v, v})
		}
		return v
	}
	type walker struct {
		name string
		run  func(v *LVal)
		// skip, when set, names why the walker is not yet bounded.  Only
		// walkers a Lisp program cannot reach on its own -- Go APIs an
		// embedder calls -- may carry one, and each is a tracked fix.
		skip string
	}
	loc := &token.Location{File: "bomb", Pos: 1}
	walkers := []walker{
		{name: "render (String)", run: func(v *LVal) { _ = v.String() }},
		{name: "equal", run: func(v *LVal) { _ = v.Equal(bombOf()) }},
		{name: "Copy (copier)", run: func(v *LVal) { _ = v.Copy() }},
		{name: "detach", run: func(v *LVal) { _, _ = v.detach() }},
		{name: "checkValueDepth", run: func(v *LVal) { _ = checkValueDepth(v, MaxValueDepth, context.Background()) }},
		{name: "macro stamp", run: func(v *LVal) { _ = stampMacroExpansion(v, loc, nil, StandardRuntime()) }},
		{name: "macro stamp (debugger)", run: func(v *LVal) {
			_ = stampMacroExpansion(v, loc, &macroExpansionContext{Name: "m", CallSite: loc}, StandardRuntime())
		}},
		{name: "locate Go macro expansion", run: func(v *LVal) { locateExpansionTree(v, loc, nil) }},
		{name: "quasiquote", run: func(v *LVal) { _ = findAndUnquote(NewEnv(nil), v, 0) }},
		{name: "SealAST", run: func(v *LVal) { v.SealAST() }},
		{name: "template snapshot", run: func(v *LVal) {
			env := NewEnv(nil)
			env.scope = scopeOf(map[string]*LVal{"bomb": v})
			if tmpl, err := NewTemplate(env); err == nil {
				if vm, err := tmpl.NewVM(); err == nil {
					_ = vm.scope.val("bomb")
				}
			}
		}},
		{name: "GoValue", run: func(v *LVal) { _ = GoValue(v) },
			skip: "Go API: converts as a tree; bounded by claude/fix-sharing-bomb-govalue"},
		{name: "package admission (AddPackage)", run: func(v *LVal) { _ = admitSymbolValue(v, MaxValueDepth) },
			skip: "Go API: classifies as a tree; bounded by claude/fix-sharing-bomb-admit"},
	}
	for _, w := range walkers {
		t.Run(w.name, func(t *testing.T) {
			if w.skip != "" {
				t.Skip(w.skip)
			}
			v := bombOf()
			testdeadline.Watch(w.name+" over a 40-level sharing bomb", 20*time.Second, 1<<30, func() { w.run(v) })
		})
	}
}
