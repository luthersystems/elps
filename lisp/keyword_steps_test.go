// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// keywordStepsSetup defines every kind of callee a keyword literal can meet
// in a call form today: a Lisp function with &key, a Lisp function taking a
// keyword positionally, a macro expanding to a keyword call, and a Go builtin
// with &key registered the ordinary way (no free-keyword opt-in).
const keywordStepsSetup = `
(defun kf (&key a b) (list a b))
(defun pf (x) x)
(defun of (x &optional y &key z) (list x y z))
(defmacro km (x) (list 'kf :a x))
(set 'kv :a)
`

// keywordStepsGolden pins the step count of every keyword-bearing call shape
// as measured on origin/main before free keywords existed (luthersystems/elps
// #745).  Every keyword literal here is evaluated and charged, and must stay
// so: only a builtin registered with FreeKeywords may skip one.  A change to
// any number is a consensus change for every embedder that meters steps.
var keywordStepsGolden = []struct {
	src   string
	steps int64
}{
	{`(kf :a 1 :b 2)`, 10},
	{`(kf :b 2)`, 8},
	{`(kf)`, 6},
	{`(pf :x)`, 4},
	{`(of 1 :z 3)`, 5},
	{`(of 1 2 :z 3)`, 11},
	{`(km 5)`, 9},
	{`(funcall kf :a 1)`, 9},
	{`(apply kf '(:a 1))`, 8},
	{`(apply kf :a '(1))`, 9},
	{`(kf kv 1)`, 8},
	{`(load-string "1" :name "n")`, 6},
	{`(sorted-map :a 1 :b 2)`, 6},
	{`(list :a :b ':c)`, 5},
	{`(gk 1 :k 2)`, 5},
	{`(gk :k :k 2)`, 5},
	{`(kf :a)`, 3},
	{`(gk 1 :k)`, 4},
}

func keywordStepsEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := templateTestEnv(t)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	env.AddBuiltins(false, elpsutil.Function("gk", lisp.Formals("x", lisp.KeyArgSymbol, "k"),
		func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			return lisp.QExpr([]*lisp.LVal{args.Cells[0], args.KeyArg(1)})
		}))
	require.NotEqual(t, lisp.LError, env.LoadString("setup", keywordStepsSetup).Type)
	return env
}

func measureSteps(env *lisp.LEnv, src string) int64 {
	env.LoadStringContext(context.Background(), "test", src)
	return env.Runtime.Steps()
}

func TestKeywordStepsGolden(t *testing.T) {
	cold := keywordStepsEnv(t)
	tmpl, err := lisp.NewTemplate(keywordStepsEnv(t), templateCorePolicy())
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	for _, tc := range keywordStepsGolden {
		got := measureSteps(cold, tc.src)
		assert.Equal(t, tc.steps, got, "cold %s", tc.src)
		assert.Equal(t, got, measureSteps(vm, tc.src), "template VM %s", tc.src)
	}
}

func keyEcho(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lisp.QExpr([]*lisp.LVal{args.Cells[0], args.KeyArg(1), args.KeyArg(2)})
}

// freeKeywordEnv adds (fk x &key a b), opted in to free keywords, and (uk x
// &key a b), the same builtin without the opt-in, to keywordStepsEnv.
func freeKeywordEnv(t *testing.T, bind bool) *lisp.LEnv {
	t.Helper()
	env := keywordStepsEnv(t)
	fk := lisp.FreeKeywords(elpsutil.FunctionDoc("fk", lisp.Formals("x", lisp.KeyArgSymbol, "a", "b"), keyEcho, "free"))
	uk := elpsutil.Function("uk", lisp.Formals("x", lisp.KeyArgSymbol, "a", "b"), keyEcho)
	if bind {
		require.True(t, env.BindBuiltins(lisp.BindOpts{}, fk, uk).IsNil())
	} else {
		env.AddBuiltins(false, fk, uk)
	}
	require.NotEqual(t, lisp.LError, env.LoadString("setup", `(set 'g fk)`).Type)
	return env
}

// saved is how many keyword literals FreeKeywords skips in each call; the
// rest of the call costs what the unflagged builtin costs.
var freeKeywordCases = []struct {
	args  string
	saved int64
}{
	{`1 :a 2 :b 3`, 2},
	{`1 :b 3`, 1},
	{`1`, 0},
	{`:a :a 1`, 1}, // the first :a is the required x: charged
	{`1 :a :b`, 1}, // :b is a value: charged
	{`1 kv 2`, 0},  // a variable holding a keyword: evaluated
	{`1 ':a 2`, 0}, // a quoted keyword: evaluated
	{`1 :a`, 1},    // odd keyword list: same error, one step saved
	{`1 :c 2`, 1},  // unknown key: same error
}

func TestFreeKeywordsSavesOneStepPerKeyLiteral(t *testing.T) {
	for _, bind := range []bool{false, true} {
		env := freeKeywordEnv(t, bind)
		for _, tc := range freeKeywordCases {
			for _, head := range []string{"fk", "g"} {
				free := "(" + head + " " + tc.args + ")"
				plain := "(uk " + tc.args + ")"
				fv := env.LoadStringContext(context.Background(), "test", free)
				fs := env.Runtime.Steps()
				uv := env.LoadStringContext(context.Background(), "test", plain)
				us := env.Runtime.Steps()
				assert.Equal(t, us-tc.saved, fs, "%s vs %s", free, plain)
				require.Equal(t, uv.Type, fv.Type, free)
				if uv.Type == lisp.LError {
					assert.Equal(t, (*lisp.ErrorVal)(uv).ErrorMessage(), (*lisp.ErrorVal)(fv).ErrorMessage(), free)
				} else {
					assert.Equal(t, uv.String(), fv.String(), free)
				}
			}
			// Through funcall and apply the keywords are funcall's
			// arguments: charged exactly as for the unflagged builtin.
			f := measureSteps(env, "(funcall fk "+tc.args+")")
			u := measureSteps(env, "(funcall uk "+tc.args+")")
			assert.Equal(t, u, f, "funcall %s", tc.args)
		}
	}
}

// The discount is a property of the function value, so a cold environment
// and every kind of template VM count identically (the consensus
// precondition TestStepBudgetTemplateParity pins for ordinary programs).
func TestFreeKeywordsTemplateParity(t *testing.T) {
	cold := freeKeywordEnv(t, false)
	for _, opts := range [][]lisp.TemplateOption{
		{templateCorePolicy()},
		{templateCorePolicy(), lisp.TemplateWithEagerInstantiation()},
	} {
		tmpl, err := lisp.NewTemplate(freeKeywordEnv(t, false), opts...)
		require.NoError(t, err)
		vms := []*lisp.LEnv{}
		for _, vmOpts := range [][]lisp.VMOption{nil, {lisp.VMWithPrewarm()}} {
			vm, err := tmpl.NewVM(vmOpts...)
			require.NoError(t, err)
			vms = append(vms, vm)
		}
		for _, tc := range freeKeywordCases {
			src := "(fk " + tc.args + ")"
			want := measureSteps(cold, src)
			for i, vm := range vms {
				assert.Equal(t, want, measureSteps(vm, src), "vm %d %s", i, src)
			}
		}
		for _, tc := range keywordStepsGolden {
			for i, vm := range vms {
				assert.Equal(t, tc.steps, measureSteps(vm, tc.src), "vm %d %s", i, tc.src)
			}
		}
	}
}

func TestFreeKeywordsFormalsShape(t *testing.T) {
	for _, formals := range []*lisp.LVal{
		lisp.Formals("x"),
		lisp.Formals("x", lisp.KeyArgSymbol),
		lisp.Formals("x", lisp.OptArgSymbol, "y", lisp.KeyArgSymbol, "k"),
		lisp.Formals(lisp.VarArgSymbol, "r"),
	} {
		def := lisp.FreeKeywords(elpsutil.Function("bad", formals, keyEcho))
		env := keywordStepsEnv(t)
		lerr := env.BindBuiltins(lisp.BindOpts{}, def)
		require.Equal(t, lisp.LError, lerr.Type, "%v", formals)
		assert.Contains(t, lisp.GoError(lerr).Error(), "free-keyword builtin bad")
		assert.Panics(t, func() { env.AddBuiltins(false, def) }, "%v", formals)
	}
	// The wrapper keeps the docstring.
	env := freeKeywordEnv(t, true)
	assert.Equal(t, "free", env.Get(lisp.Symbol("fk")).Docstring())
}

// Call forms that reach a builtin by other routes: a macro expansion, a form
// built at run time with a keyword symbol made from a string, a tail call,
// and forms nested in special operators.  Each shape costs the same for the
// flagged builtin as for the unflagged one minus exactly the key-name
// literals, and cold and template VMs agree.  For the unflagged builtin (the
// only kind existing programs can have) nothing is saved.
func TestFreeKeywordsOtherRoutes(t *testing.T) {
	setup := `
(defmacro call-with-key (f v) (list f 1 :a v))
(defun tail-call (f) (if true (funcall f) ()))
(defun tail-fk () (fk 1 :a 2))
(defun tail-uk () (uk 1 :a 2))
(defun run-built (f) (eval (list f 1 (to-symbol ":a") 2)))
`
	build := func() *lisp.LEnv {
		env := freeKeywordEnv(t, false)
		require.NotEqual(t, lisp.LError, env.LoadString("setup", setup).Type)
		return env
	}
	cold := build()
	tmpl, err := lisp.NewTemplate(build(), templateCorePolicy())
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	for _, tc := range []struct {
		free, plain string
		saved       int64
	}{
		{`(call-with-key fk 2)`, `(call-with-key uk 2)`, 1},
		// A symbol made at run time is a quoted value, not a literal: charged.
		{`(run-built 'fk)`, `(run-built 'uk)`, 0},
		{`(tail-fk)`, `(tail-uk)`, 1},
		{`(tail-call tail-fk)`, `(tail-call tail-uk)`, 1},
		{`(progn (fk 1 :a 2))`, `(progn (uk 1 :a 2))`, 1},
		{`(if (fk 1 :b 2) (fk 1 :a 2) ())`, `(if (uk 1 :b 2) (uk 1 :a 2) ())`, 2},
		{`(let ([x (fk 1 :a 2)]) x)`, `(let ([x (uk 1 :a 2)]) x)`, 1},
		{`(if :a 1 2)`, `(if :a 1 2)`, 0},
		{`(apply fk 1 '(:a 2))`, `(apply uk 1 '(:a 2))`, 0},
	} {
		f, u := measureSteps(cold, tc.free), measureSteps(cold, tc.plain)
		assert.Equal(t, u-tc.saved, f, "%s vs %s", tc.free, tc.plain)
		assert.Equal(t, f, measureSteps(vm, tc.free), "vm %s", tc.free)
		assert.Equal(t, u, measureSteps(vm, tc.plain), "vm %s", tc.plain)
		fv := cold.LoadString("t", tc.free)
		uv := cold.LoadString("t", tc.plain)
		assert.Equal(t, uv.String(), fv.String(), tc.free)
	}
}
