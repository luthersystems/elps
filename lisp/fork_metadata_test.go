// Copyright © 2026 The ELPS authors

// Guards for the per-environment and per-package metadata Fork used to
// rebuild wholesale (issue #440 follow-up): the evaluator's location
// register, which no longer travels at all, and the package documentation
// table, which is now allocated lazily and must still be copied rather than
// shared.
package lisp

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// forkedEnvs enumerates every environment reachable from a forked (or
// template) root: the lexical parent chain, plus the environment captured
// by every function value bound in the registry and that environment's own
// parents.  This is the same set forker.env visits.
func forkedEnvs(root *LEnv) map[*LEnv]bool {
	seen := map[*LEnv]bool{}
	walk := func(e *LEnv) {
		for ; e != nil && !seen[e]; e = e.parent {
			seen[e] = true
		}
	}
	walk(root)
	if root.Runtime != nil && root.Runtime.Registry != nil {
		for _, pkg := range root.Runtime.Registry.packages {
			for _, v := range pkg.symbols {
				if v.Type == LFun {
					if fd := v.funData(); fd != nil {
						walk(fd.env)
					}
				}
			}
		}
	}
	return seen
}

// TestForkDropsEvaluatorLocation pins the location register's fork
// treatment: it does not travel.
//
// LEnv.loc is not state an environment owns, it is where eval parks the
// source position of the node it is evaluating right now — rebound on every
// step, aliased from the parent at construction (see newEnvN's
// //elps:aliases note).  What a quiescent template holds there is the
// leftover position of the last node it evaluated, which is not the fork's
// to report, so a fork starts with an empty register exactly as it starts
// with an empty call stack and no evalCtx.  Fork used to copyLocation it
// per environment, which allocated once per environment to carry a value
// the fork's first evaluation immediately overwrites.
//
// The test fails both ways the treatment could regress: reinstating the
// copy leaves a non-nil register, and sharing the template's pointer leaves
// one that is non-nil AND pointer-equal to the template's.
func TestForkDropsEvaluatorLocation(t *testing.T) {
	env := newForkTestEnv(t)

	// A closure, so the forked tree is deeper than its root: its captured
	// environment goes through forker.env too, and it is the environment
	// whose register a template holds a *definition-site* location in.
	lam := lambdaExpr()
	templateLoc := &token.Location{File: "template.lisp", Path: "template.lisp", Line: 12, Col: 3, Pos: 40}
	lam.SetSource(templateLoc)
	fun := env.Eval(lam)
	if fun.Type != LFun {
		t.Fatalf("lambda eval: %v", fun)
	}
	if lerr := env.PutGlobal(Symbol("closure"), fun); lerr.Type == LError {
		t.Fatalf("bind closure: %v", lerr)
	}

	// Anti-vacuity: the template must actually be holding a location, or
	// "the fork holds none" is true of both sides and proves nothing.
	if env.loc == nil {
		t.Fatal("template's evaluator location register is empty; the assertions below would pass vacuously")
	}
	if got := env.Source(); got == nil || got.Line != 12 {
		t.Fatalf("template Source() = %v, want the lambda's line 12", got)
	}

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}

	envs := forkedEnvs(fork)
	if len(envs) < 2 {
		t.Fatalf("forked tree has %d environments; the walk found no closure environment and the check is near-vacuous", len(envs))
	}
	for e := range envs {
		if e.loc == nil {
			continue
		}
		if e.loc == templateLoc {
			t.Errorf("forked env %d SHARES the template's Location pointer (%v); the register must not travel", e.ID, e.loc)
			continue
		}
		t.Errorf("forked env %d carries an evaluator location %v; a fork starts with an empty register", e.ID, e.loc)
	}
	if got := fork.Source(); got != nil {
		t.Errorf("fork.Source() = %v, want nil (no evaluation has happened in the fork yet)", got)
	}

	// The register is empty, not broken: the fork's first evaluation fills
	// it, and the two runtimes' registers then move independently.
	probe := Symbol("closure")
	forkLoc := &token.Location{File: "fork.lisp", Path: "fork.lisp", Line: 99, Col: 1, Pos: 7}
	probe.SetSource(forkLoc)
	if r := fork.Eval(probe); r.Type != LFun {
		t.Fatalf("fork eval of the inherited closure: %v", r)
	}
	if got := fork.Source(); got == nil || got.Line != 99 {
		t.Errorf("fork.Source() after evaluating = %v, want the fork's own line 99", got)
	}
	if got := env.Source(); got == nil || got.Line != 12 {
		t.Errorf("template Source() = %v after the fork evaluated; want its own line 12 — the registers are not independent", got)
	}
}

// TestPackageSymbolDocsLazyTable pins the lazy allocation of a package's
// documentation table: absent until something is documented, and writable
// from absent.  A write is the one operation a nil map cannot serve, so the
// allocation guard in setSymbolDoc is what makes the whole saving legal —
// remove it and this test panics rather than merely failing.
func TestPackageSymbolDocsLazyTable(t *testing.T) {
	pkg := NewPackage("lazy-docs")
	if pkg.symbolDocs != nil {
		t.Errorf("NewPackage allocated a documentation table for a package with no documented symbols")
	}
	if got := pkg.SymbolDoc("undocumented"); got != "" {
		t.Errorf(`SymbolDoc on an empty table = %q, want ""`, got)
	}
	// The write path must allocate rather than assign into the nil map.
	pkg.setSymbolDoc("answer", "the answer")
	if got := pkg.SymbolDoc("answer"); got != "the answer" {
		t.Errorf("SymbolDoc(answer) = %q, want %q", got, "the answer")
	}
	pkg.setSymbolDoc("second", "another")
	if got := pkg.SymbolDoc("second"); got != "another" {
		t.Errorf("SymbolDoc(second) = %q, want %q", got, "another")
	}
	if got := pkg.SymbolDoc("answer"); got != "the answer" {
		t.Errorf("the second write dropped the first: SymbolDoc(answer) = %q", got)
	}
}

// TestForkSymbolDocs covers the fork side of the same table: documented
// symbols travel, undocumented packages fork without allocating a table,
// and the two sides' tables are independent — the last is what makes the
// per-package COPY (rather than a share) load-bearing, since the template
// stays writable after a fork and `(set 'x v "doc")` is a lisp-reachable
// write into it.
func TestForkSymbolDocs(t *testing.T) {
	env := newForkTestEnv(t)
	userPkg := env.Runtime.Package.Name
	if lerr := env.PutGlobal(Symbol("documented"), Int(1)); lerr.Type == LError {
		t.Fatalf("bind: %v", lerr)
	}
	env.SetSymbolDoc("documented", "template doc")

	// Anti-vacuity: at least one package must be documented and at least
	// one must not, or neither half of the assertion means anything.
	documented, undocumented := 0, 0
	for _, p := range env.Runtime.Registry.packages {
		if len(p.symbolDocs) > 0 {
			documented++
		} else {
			undocumented++
		}
	}
	if documented == 0 || undocumented == 0 {
		t.Fatalf("template has %d documented and %d undocumented packages; need both for this test", documented, undocumented)
	}

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}

	for name, p := range env.Runtime.Registry.packages {
		fp := fork.Runtime.Registry.packages[name]
		if fp == nil {
			t.Fatalf("package %q missing from the fork", name)
		}
		if len(p.symbolDocs) == 0 {
			if fp.symbolDocs != nil {
				t.Errorf("package %q has no documented symbols but forked with an allocated table", name)
			}
			continue
		}
		if fp.symbolDocs == nil {
			t.Errorf("package %q forked without its documentation table", name)
			continue
		}
		if reflect.ValueOf(fp.symbolDocs).Pointer() == reflect.ValueOf(p.symbolDocs).Pointer() {
			t.Errorf("package %q: fork SHARES the template's documentation table; the template stays "+
				"writable after a fork, so a shared table is a concurrent map write away from issue #397", name)
		}
		for sym, doc := range p.symbolDocs {
			if got := fp.SymbolDoc(sym); got != doc {
				t.Errorf("package %q symbol %q: fork doc %q, template doc %q", name, sym, got, doc)
			}
		}
	}

	// Independence in both directions.  Fork writes first, into the table
	// it inherited, then the template writes into its own.
	fork.Runtime.Package = fork.Runtime.Registry.packages[userPkg]
	if lerr := fork.PutGlobal(Symbol("fork-only"), Int(2)); lerr.Type == LError {
		t.Fatalf("fork bind: %v", lerr)
	}
	fork.SetSymbolDoc("fork-only", "fork doc")
	fork.SetSymbolDoc("documented", "fork overwrote it")

	env.SetSymbolDoc("template-only", "template doc 2")

	tpl := env.Runtime.Registry.packages[userPkg]
	if got := tpl.SymbolDoc("fork-only"); got != "" {
		t.Errorf("the fork's documentation write reached the template: SymbolDoc(fork-only) = %q", got)
	}
	if got := tpl.SymbolDoc("documented"); got != "template doc" {
		t.Errorf("the fork overwrote the template's doc: SymbolDoc(documented) = %q", got)
	}
	fpkg := fork.Runtime.Registry.packages[userPkg]
	if got := fpkg.SymbolDoc("template-only"); got != "" {
		t.Errorf("the template's documentation write reached the fork: SymbolDoc(template-only) = %q", got)
	}
	if got := fpkg.SymbolDoc("documented"); got != "fork overwrote it" {
		t.Errorf("the fork's own doc write did not stick: SymbolDoc(documented) = %q", got)
	}
}
