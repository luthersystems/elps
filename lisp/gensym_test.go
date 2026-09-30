// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newGenSymsEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithMaxSteps(1000000))))
	return env
}

func TestGetDefaultGenSymsDeterminism(t *testing.T) {
	cold, warm := newGenSymsEnv(t), newGenSymsEnv(t)
	for range 11 {
		warm.GenSym()
	}
	require.NoError(t, lisp.GoError(warm.LoadString("history.lisp", `(get-default () "old" 9)`)))
	tmpl, err := lisp.NewTemplate(warm, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)
	fork, err := tmpl.NewVM()
	require.NoError(t, err)
	for range 7 {
		fork.GenSym()
	}

	for _, program := range []string{
		`(macroexpand-1 '(get-default m k d))`,
		`(get-default (sorted-map "k" 7) "k" 42)`,
		`(get-default () "k" (get-default () "inner" 42))`,
		`(get-default 1 "k" 42)`,
	} {
		t.Run(program, func(t *testing.T) {
			var want string
			var wantSteps int64
			for i, env := range []*lisp.LEnv{cold, warm, fork} {
				before := env.Runtime.TotalSteps()
				got := env.LoadString("determinism.lisp", program)
				steps := env.Runtime.TotalSteps() - before
				if i == 0 {
					want, wantSteps = got.String(), steps
				} else {
					assert.Equal(t, want, got.String())
					assert.Equal(t, wantSteps, steps)
				}
			}
		})
	}
}

// get-default no longer draws from the runtime gensym counter, so gensym
// numbering after it changed: this program printed gen00000004 before
// get-default used NewGenSyms (it took gen00000002 and gen00000003) and
// prints gen00000002 now.  Deliberate and visible in printed output, so
// changing it again is a coordinated upgrade for embedders.
func TestGetDefaultLeavesGenSymCounter(t *testing.T) {
	env := newGenSymsEnv(t)
	got := env.LoadString("numbering.lisp", `(gensym) (get-default (sorted-map) "a" 0) (gensym)`)
	require.NoError(t, lisp.GoError(got))
	assert.Equal(t, "gen00000002", got.String())
}

func TestGetDefaultGenSymsOldUserName(t *testing.T) {
	env := newGenSymsEnv(t)
	got := env.LoadString("collision.lisp", `(let ((gen00000001 42)) (get-default () "missing" gen00000001))`)
	require.NoError(t, lisp.GoError(got))
	assert.Equal(t, "42", got.String())
}

func TestGetDefaultGenSymsNestedSameLevel(t *testing.T) {
	env := newGenSymsEnv(t)
	outer := env.LoadString("nested.lisp", `(macroexpand-1 '(get-default m k (get-default m2 k2 d)))`)
	require.NoError(t, lisp.GoError(outer))
	innerCall := outer.Cells[2].Cells[3]
	inner := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("macroexpand-1"), lisp.Quote(innerCall)}))
	require.NoError(t, lisp.GoError(inner))
	for _, expansion := range []*lisp.LVal{outer, inner} {
		bindings := expansion.Cells[1].Cells
		assert.Equal(t, "map@1@1", bindings[0].Cells[0].Str)
		assert.Equal(t, "key@1@2", bindings[1].Cells[0].Str)
	}
	// The inner args mention none of the outer temporaries. Its shadowing
	// therefore cannot affect the lookup of m2, k2 or d.
	got := env.LoadString("nested.lisp", `
		(let ((m (sorted-map)) (k "outer")
		      (m2 (sorted-map "inner" 42)) (k2 "inner") (d -1))
		  (get-default m k (get-default m2 k2 d)))`)
	require.NoError(t, lisp.GoError(got))
	assert.Equal(t, "42", got.String())
}

func TestGenSymsNames(t *testing.T) {
	g := lisp.NewGenSyms(lisp.Nil())
	first, second, third := g.Symbol("tmp"), g.Symbol("tmp"), g.Symbol("other")
	assert.Equal(t, "tmp@1@1", first.Str)
	assert.Equal(t, "tmp@1@2", second.Str)
	assert.Equal(t, "other@1@3", third.Str)
	assert.NotSame(t, first, second)
	assert.Equal(t, lisp.LSymbol, first.Type)
	assert.False(t, first.IsQuoted())
	again := lisp.NewGenSyms(nil).Symbol("tmp")
	assert.Equal(t, first.Str, again.Str)
	assert.NotSame(t, first, again)
	first.Str = "changed"
	assert.Equal(t, "tmp@1@1", again.Str)
	assert.Panics(t, func() { g.Symbol("pkg:tmp") }, "a hint must not introduce a package qualifier")
}

func TestGenSymsReservedNamespace(t *testing.T) {
	sym := lisp.NewGenSyms(nil).Symbol("tmp")
	for _, preserving := range []bool{false, true} {
		reader := parser.NewReader()
		if preserving {
			reader = parser.NewReader(parser.WithFormatPreserving())
		}
		for _, src := range []string{sym.Str, "'" + sym.Str, "(" + sym.Str + ")", "#'" + sym.Str} {
			_, err := reader.Read("reserved.lisp", strings.NewReader(src))
			require.Error(t, err, src)
		}
	}
	parts := lisp.SplitSymbol(sym)
	require.NoError(t, lisp.GoError(parts))
	require.Len(t, parts.Cells, 1)
	assert.Same(t, sym, parts.Cells[0])
	for _, src := range []string{
		`(lisp:let (((unquote s) (unquote v))) (unquote s))`,
		`((lisp:lambda ((unquote s)) (unquote s)) (unquote v))`,
		`(lisp:progn (lisp:set '(unquote s) (unquote v)) (unquote s))`,
	} {
		env := newGenSymsEnv(t)
		localSym := lisp.NewGenSyms(nil).Symbol("tmp")
		form := elpsutil.MustTemplate(src, "s", "v").Expand(localSym, lisp.Int(42))
		got := env.Eval(form)
		require.NoError(t, lisp.GoError(got))
		assert.Equal(t, "42", got.String())
	}
}

func TestGenSymsLazyLevel(t *testing.T) {
	args := lisp.SExpr([]*lisp.LVal{lisp.Nil()})
	g := lisp.NewGenSyms(args)
	// Construction must not inspect the argument graph. The first call sees
	// this change; subsequent calls keep that expansion's original level.
	args.Cells[0] = lisp.Quote(lisp.NewGenSyms(nil).Symbol("outer"))
	assert.Equal(t, "tmp@2@1", g.Symbol("tmp").Str)
	args.Cells[0] = lisp.NewGenSyms(args).Symbol("deeper")
	assert.Equal(t, "tmp@2@2", g.Symbol("tmp").Str)
}

func TestGenSymsArgumentWalk(t *testing.T) {
	level1 := lisp.NewGenSyms(nil).Symbol("tmp")
	level2 := lisp.NewGenSyms(lisp.SExpr([]*lisp.LVal{level1})).Symbol("tmp")
	for _, arg := range []*lisp.LVal{
		lisp.Quote(lisp.Quote(level2)),
		lisp.Vector([]*lisp.LVal{level1, lisp.SExpr([]*lisp.LVal{level2})}),
		newGenSymsEnv(t).TaggedValue(lisp.Symbol("user:box"), level2),
	} {
		assert.Equal(t, "tmp@3@1", lisp.NewGenSyms(lisp.SExpr([]*lisp.LVal{arg})).Symbol("tmp").Str)
	}
	// Neither text in string literals nor the old runtime gensyms is a
	// reserved symbol. Malformed foreign spellings are not toolkit names.
	for _, arg := range []*lisp.LVal{
		lisp.String("tmp@99@1"), lisp.Symbol("gen00000099"),
		lisp.Symbol("tmp@0@1"), lisp.Symbol("tmp@2@0"),
		lisp.Symbol("tmp@02@1"), lisp.Symbol("tmp@2@01"),
		lisp.Symbol("tmp@x@1"), lisp.Symbol("tmp@2@x"),
	} {
		assert.Equal(t, "tmp@1@1", lisp.NewGenSyms(lisp.SExpr([]*lisp.LVal{arg})).Symbol("tmp").Str)
	}
	// The hint may itself contain the separator: read the final two fields.
	withSeparator := lisp.NewGenSyms(nil).Symbol("tmp@extra")
	assert.Equal(t, "tmp@2@1", lisp.NewGenSyms(lisp.SExpr([]*lisp.LVal{withSeparator})).Symbol("tmp").Str)
}

func TestGenSymsCyclesAndSharing(t *testing.T) {
	sym := lisp.NewGenSyms(nil).Symbol("tmp")
	a, b := lisp.SExpr(nil), lisp.SExpr(nil)
	a.Cells = []*lisp.LVal{a, b, b}
	b.Cells = []*lisp.LVal{a, sym}
	assert.Equal(t, "tmp@2@1", lisp.NewGenSyms(a).Symbol("tmp").Str)
	deep := sym
	for range 100 {
		deep = lisp.SExpr([]*lisp.LVal{deep})
	}
	assert.Equal(t, "tmp@2@1", lisp.NewGenSyms(deep).Symbol("tmp").Str)
	shared := lisp.SExpr([]*lisp.LVal{sym})
	assert.Equal(t, "tmp@2@1", lisp.NewGenSyms(lisp.SExpr([]*lisp.LVal{shared, shared})).Symbol("tmp").Str)
}

func TestGenSymsAllocations(t *testing.T) {
	args := lisp.SExpr([]*lisp.LVal{lisp.Symbol("ordinary")})
	var got *lisp.LVal
	assert.Zero(t, testing.AllocsPerRun(100, func() {
		// Keep the generator local, as a Go macro does; it needs no heap
		// allocation when it does not escape the expansion.
		_ = lisp.NewGenSyms(args)
	}))
	assert.InDelta(t, 2, testing.AllocsPerRun(100, func() {
		got = lisp.NewGenSyms(args).Symbol("tmp")
	}), 0)
	args.Cells = []*lisp.LVal{args, lisp.NewGenSyms(nil).Symbol("tmp")}
	assert.InDelta(t, 2, testing.AllocsPerRun(100, func() {
		got = lisp.NewGenSyms(args).Symbol("tmp")
	}), 0)
	require.NotNil(t, got)
}

var genSymsOuterForm = elpsutil.MustTemplate(
	`(lisp:let (((unquote s) (unquote value))) (bind-fresh (unquote s)))`, "s", "value")
var genSymsInnerForm = elpsutil.MustTemplate(
	`(lisp:let (((unquote s) (unquote value))) (unquote arg))`, "s", "value", "arg")

func macroGenSymsOuter(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	sym := lisp.NewGenSyms(args).Symbol("tmp")
	return genSymsOuterForm.Expand(sym, lisp.Int(42))
}

func macroGenSymsInner(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	sym := lisp.NewGenSyms(args).Symbol("tmp")
	return genSymsInnerForm.Expand(sym, lisp.Int(-1), args.Cells[0])
}

func TestGenSymsGoMacroNoCapture(t *testing.T) {
	env := newGenSymsEnv(t)
	env.AddMacros(true,
		elpsutil.FunctionDoc("pass-fresh", lisp.Formals(), macroGenSymsOuter, "Passes its temporary to bind-fresh."),
		elpsutil.FunctionDoc("bind-fresh", lisp.Formals("arg"), macroGenSymsInner, "Binds a temporary around arg."))
	got := env.LoadString("capture.lisp", `(pass-fresh)`)
	require.NoError(t, lisp.GoError(got))
	assert.Equal(t, "42", got.String())
	outer := env.LoadString("capture.lisp", `(macroexpand-1 '(pass-fresh))`)
	require.NoError(t, lisp.GoError(outer))
	assert.Equal(t, "tmp@1@1", outer.Cells[1].Cells[0].Cells[0].Str)
	inner := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("macroexpand-1"), lisp.Quote(outer.Cells[2])}))
	require.NoError(t, lisp.GoError(inner))
	assert.Equal(t, "tmp@2@1", inner.Cells[1].Cells[0].Cells[0].Str)
}

// A runtime-built argument whose sharing doubles at every level has 2^40
// paths; the walk must stay linear in its distinct nodes.
func TestGenSymsSharedArgumentIsLinear(t *testing.T) {
	v := lisp.NewGenSyms(nil).Symbol("tmp")
	for range 40 {
		v = lisp.SExpr([]*lisp.LVal{v, v})
	}
	assert.Equal(t, "tmp@2@1", lisp.NewGenSyms(lisp.SExpr([]*lisp.LVal{v})).Symbol("tmp").Str)
}
