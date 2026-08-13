// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
)

// TestCycleContainersSkipFunctionSubtrees pins the rule that keeps
// FuzzCyclicValueWalks inside the value it built: containers() may only offer
// knot() nodes the input constructed, and everything below an LFun -- its
// formals and its body -- belongs to whoever defined the function.
//
// fuzzval hands back the environment's own global function objects (fun()
// kinds 2 and 3, reached from the seed corpus by []byte{kindFun, 2}), whose
// Cells[0] is the process-wide formals list from the builtin table.  Before
// issue #398 containers() collected that list and knot() appended to it, so
// running the seed corpus rewrote lisp:car's signature for the rest of the
// process.
//
// This is the attributable half of the guard pair.  TestMain's
// builtin-formals snapshot says the standard library was corrupted; this says
// by whom, and fails on the collection rather than on its consequence.
func TestCycleContainersSkipFunctionSubtrees(t *testing.T) {
	env := newCycleEnv(t)
	for _, name := range []string{"lisp:car", "lisp:identity", "lisp:map", "lisp:let", "lisp:defmacro"} {
		fn := env.GetGlobal(lisp.Symbol(name))
		if fn.Type != lisp.LFun {
			t.Fatalf("%s is not a function: %v", name, fn.Type)
		}
		if got := containers(fn); len(got) != 0 {
			t.Errorf("containers(%s) offered knot() %d node(s) inside a function value -- the first is a %v: %s"+
				"\nthose cells are %s's own formals and body, shared by every environment in the process,"+
				"\nnot structure this input built", name, len(got), got[0].Type, got[0], name)
		}
		// The same function nested inside a container the input DID build:
		// the list is writable, nothing under the function is.
		wrapper := lisp.QExpr([]*lisp.LVal{lisp.Int(1), fn})
		if got := containers(wrapper); len(got) != 1 || got[0] != wrapper {
			t.Errorf("containers(list holding %s) collected %d node(s), want exactly the list itself", name, len(got))
		}
	}
}

// TestCycleSeedsStillTieCycles is the other half of the #398 fix: proof that
// skipping function subtrees in containers() did not quietly turn
// FuzzCyclicValueWalks into a target that walks trees.
//
// A guard against corruption that works by collecting nothing would pass
// TestCycleContainersSkipFunctionSubtrees and TestMain's formals snapshot
// perfectly, and would be the same "green by not looking" failure in a new
// place.  So this pins the opposite direction, over the target's own seed
// corpus and with the target's own helpers:
//
//   - a substantial share of the corpus still yields a writable node;
//   - all three writable families -- list, array, sorted-map -- are still
//     reached, so knot()'s three branches are all still exercised;
//   - and the knotted values really are cyclic afterwards, which is measured
//     the only way that cannot be faked by the collector: the rendering
//     carries #<cycle>, which only the recursion guard emits, and only when
//     it meets a value that contains itself.
//
// Measured across the fix: 32 seeds yielded a container before and 23 after,
// and every one of the nine differences is a value that is NOTHING but a
// function, whose only "container" was that function's formals list.  Not one
// node the input actually built was dropped.  A 60s `-fuzz` run either side
// agreed: 511 vs 513 interesting inputs from ~161k execs.
func TestCycleSeedsStillTieCycles(t *testing.T) {
	// Floors, not exact counts: fuzzval.Seeds() grows when a value kind is
	// added, and this test should not have to be edited for that.  They sit
	// just under the measured values (23 seeds, families 24/9/6).
	const minSeedsWithContainers = 20
	const minCyclicRenders = 20

	env := newCycleEnv(t)
	seeds := append(fuzzval.Seeds(), []byte{0x00}, []byte{0xff, 0xff, 0xff, 0xff})
	families := map[lisp.LType]int{}
	var withContainers, cyclic int
	for i, data := range seeds {
		gen := fuzzval.New(data, env)
		v := gen.Value()
		nodes := containers(v)
		if len(nodes) > 0 {
			withContainers++
		}
		for _, c := range nodes {
			families[c.Type]++
		}
		knot(v, gen, i%maxKnots+1)
		if strings.Contains(v.String(), "#<cycle>") {
			cyclic++
		}
	}

	if withContainers < minSeedsWithContainers {
		t.Errorf("only %d of %d seeds yielded a writable node, want at least %d:"+
			"\ncontainers() has stopped collecting the values the input built, and the target no longer ties cycles",
			withContainers, len(seeds), minSeedsWithContainers)
	}
	if cyclic < minCyclicRenders {
		t.Errorf("only %d of %d seeds rendered #<cycle> after knotting, want at least %d:"+
			"\nthe target is walking trees, which is what it was written to stop doing",
			cyclic, len(seeds), minCyclicRenders)
	}
	for _, typ := range []lisp.LType{lisp.LSExpr, lisp.LArray, lisp.LSortMap} {
		if families[typ] == 0 {
			t.Errorf("no %v was ever collected: knot()'s %v branch is unreachable from the seed corpus", typ, typ)
		}
	}
}
