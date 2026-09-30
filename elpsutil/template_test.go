// Copyright © 2026 The ELPS authors

package elpsutil_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestTemplateSubstitution(t *testing.T) {
	// Argument order comes from params, not the order of first occurrence.
	tmpl := elpsutil.MustTemplate(`(lisp:list (unquote a) (unquote b) (unquote a))`, "b", "a")
	a, b := lisp.Int(7), lisp.String("hello")
	got := tmpl.Expand(b, a)
	assert.False(t, got.IsQuoted())
	assert.Equal(t, `(lisp:list 7 "hello" 7)`, got.String())
	assert.Same(t, a, got.Cells[1])
	assert.Same(t, b, got.Cells[2])
	assert.Same(t, a, got.Cells[3])
	assert.Same(t, a, elpsutil.MustTemplate(`(unquote a)`, "a").Expand(a))
	assert.Same(t, lisp.Nil(), elpsutil.MustTemplate(`()`).Expand())

	// A quote adds a private header without changing the substituted argument.
	list := lisp.SExpr([]*lisp.LVal{a})
	quoted := elpsutil.MustTemplate(`'(unquote a)`, "a").Expand(list)
	assert.True(t, quoted.IsQuoted())
	assert.False(t, list.IsQuoted())
	assert.Same(t, a, quoted.Cells[0])
}

func TestTemplateSplice(t *testing.T) {
	a, b := lisp.Int(7), lisp.String("hello")
	forms := lisp.SExpr([]*lisp.LVal{a, b})
	tmpl := elpsutil.MustTemplate(`(lisp:list before (unquote-splicing forms) after ((unquote last) (unquote-splicing forms)))`, "forms", "last")
	got := tmpl.Expand(forms, a)
	assert.Equal(t, `(lisp:list before 7 "hello" after (7 7 "hello"))`, got.String())
	assert.Same(t, a, got.Cells[2])
	assert.Same(t, b, got.Cells[3])
	got.Cells[2] = lisp.Symbol("changed")
	assert.Same(t, a, forms.Cells[0], "the enclosing cells slice must be fresh")
	for _, empty := range []*lisp.LVal{lisp.Nil(), lisp.SExpr(nil)} {
		assert.Equal(t, `(lisp:list before after (7))`, tmpl.Expand(empty, a).String())
		assert.True(t, elpsutil.MustTemplate(`((unquote-splicing forms))`, "forms").Expand(empty).IsNil())
	}
}

// Templates are read by the elps reader, so literals are written in place.
func TestTemplateLiterals(t *testing.T) {
	const src = `(f "tab\there" 42 -1 +1 .5 -.5 -1e3 1.25 inf "" sym-bol 'q '(1 "two"))`
	tmpl := elpsutil.MustTemplate(src)
	parsed, err := parser.NewReader().Read("literals.lisp", strings.NewReader(src))
	require.NoError(t, err)
	require.Len(t, parsed, 1)
	got := tmpl.Expand()
	assert.Equal(t, parsed[0].String(), got.String())
	for i, c := range got.Cells {
		assert.Equal(t, parsed[0].Cells[i].Type, c.Type, "cell %d (%v)", i, c)
	}
	assert.Equal(t, lisp.LInt, got.Cells[3].Type)
	assert.Equal(t, int(-1), got.Cells[3].Int)
	assert.Equal(t, "tab\there", got.Cells[1].Str)
}

func TestTemplateQuoteMatchesReader(t *testing.T) {
	sym := func(s string) *lisp.LVal { return lisp.Symbol(s) }
	for _, tt := range []struct {
		src, parsed string
		params      []string
		args        []*lisp.LVal
	}{
		{src: `'x`, parsed: `'x`},
		{src: `'(x y)`, parsed: `'(x y)`},
		{src: `'()`, parsed: `'()`},
		{src: `''x`, parsed: `''x`},
		{src: `''(x () 'y)`, parsed: `''(x () 'y)`},
		{src: `'(unquote x)`, parsed: `'x`, params: []string{"x"}, args: []*lisp.LVal{sym("x")}},
		{src: `'(unquote x)`, parsed: `''x`, params: []string{"x"}, args: []*lisp.LVal{lisp.Quote(sym("x"))}},
		{src: `'(x (unquote-splicing rest))`, parsed: `'(x y z)`, params: []string{"rest"}, args: []*lisp.LVal{lisp.SExpr([]*lisp.LVal{sym("y"), sym("z")})}},
	} {
		t.Run(tt.src+"/"+tt.parsed, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
			parsed, err := parser.NewReader().Read("quote.lisp", strings.NewReader(tt.parsed))
			require.NoError(t, err)
			require.Len(t, parsed, 1)
			form := elpsutil.MustTemplate(tt.src, tt.params...).Expand(tt.args...)
			assert.Equal(t, parsed[0].String(), form.String())
			want, got := env.Eval(parsed[0]), env.Eval(form)
			require.NoError(t, lisp.GoError(want))
			require.NoError(t, lisp.GoError(got))
			assert.Equal(t, want.String(), got.String())
		})
	}
}

// Expansions are fresh, unsealed and unlocated: the evaluator locates them at
// the macro call site, and a caller may mutate them.
func TestTemplateFreshness(t *testing.T) {
	tmpl := elpsutil.MustTemplate(`(f (g h) 'x ''(y z) '() () "s" 1)`)
	first, second := tmpl.Expand(), tmpl.Expand()
	nodes := make(map[*lisp.LVal]bool)
	var walk func(*lisp.LVal, bool)
	walk = func(v *lisp.LVal, record bool) {
		if v == lisp.Nil() {
			return // () is the immutable Nil singleton, not template storage.
		}
		_, located := v.Source()
		assert.False(t, located, "template node %v carries a location", v)
		// '() is a quoted header over the sealed Nil singleton and keeps
		// its seal, as the reader's '() does.
		if !v.IsNil() {
			assert.False(t, v.IsSealed(), "template node %v is sealed", v)
		}
		if record {
			nodes[v] = true
		} else {
			assert.False(t, nodes[v], "shared syntax node: %v", v)
		}
		for _, child := range v.Cells {
			walk(child, record)
		}
	}
	walk(first, true)
	walk(second, false)
	first.Cells[0].Str = "changed"
	first.Cells[1].Cells[0] = lisp.Symbol("replaced")
	assert.Equal(t, `(f (g h) 'x ''(y z) '() () "s" 1)`, second.String())
	assert.Equal(t, second.String(), tmpl.Expand().String())
	for range 8 {
		t.Run("concurrent", func(t *testing.T) {
			t.Parallel()
			for range 20 {
				got := tmpl.Expand()
				require.Equal(t, "f", got.Cells[0].Str)
				got.Cells[0].Str = "private"
			}
		})
	}
}

// Every unquote form is a placeholder, even inside a nested quasiquote, so a
// literal unquote cannot be written in a template (documented on Template).
func TestTemplateNestedQuasiquoteSubstitutes(t *testing.T) {
	got := elpsutil.MustTemplate(`(quasiquote (a (unquote x)))`, "x").Expand(lisp.Symbol("y"))
	assert.Equal(t, `(quasiquote (a y))`, got.String())
}

func TestTemplateCompilePanics(t *testing.T) {
	for _, tt := range []struct {
		name, src, message string
		params             []string
	}{
		{"empty", ``, "expected one form, got 0", nil},
		{"two forms", `(x) (y)`, "expected one form, got 2", nil},
		{"parse error", `(x`, "", nil},
		{"undeclared", `((unquote x))`, `undeclared placeholder "x"`, nil},
		{"undeclared splice", `((unquote-splicing x))`, `undeclared placeholder "x"`, nil},
		{"unused", `(x)`, `unused parameter "x"`, []string{"x"}},
		{"duplicate", `(unquote x)`, `duplicate parameter "x"`, []string{"x", "x"}},
		{"unquote no name", `((unquote))`, "unquote takes one parameter name", nil},
		{"unquote two names", `((unquote x y))`, "unquote takes one parameter name", []string{"x", "y"}},
		{"unquote non-symbol", `((unquote "x"))`, "unquote takes one parameter name", nil},
		{"top-level splice", `(unquote-splicing x)`, "unquote-splicing must be directly inside a list", []string{"x"}},
		{"quoted splice", `'(unquote-splicing x)`, "unquote-splicing must be directly inside a list", []string{"x"}},
	} {
		t.Run(tt.name, func(t *testing.T) {
			defer func() {
				r := recover()
				require.NotNil(t, r, "expected a panic")
				msg, ok := r.(string)
				require.True(t, ok, "panic value %v", r)
				assert.True(t, strings.HasPrefix(msg, "elpsutil.MustTemplate: "), msg)
				assert.Contains(t, msg, tt.message)
			}()
			elpsutil.MustTemplate(tt.src, tt.params...)
		})
	}
}

func TestTemplateExpandPanics(t *testing.T) {
	tmpl := elpsutil.MustTemplate(`(f (unquote-splicing xs))`, "xs")
	assert.PanicsWithValue(t, "elpsutil.Template.Expand: expected 1 arguments, got 0", func() { tmpl.Expand() })
	assert.PanicsWithValue(t, "elpsutil.Template.Expand: splice argument 1 must be an unquoted list or nil", func() {
		tmpl.Expand(lisp.Int(1))
	})
}

// The embedding guide's unless example, registered both ways: body forms stay
// unevaluated until the expansion runs in the caller.
func TestTemplateMacroRegistration(t *testing.T) {
	const docs = "Evaluates body only when condition is falsey."
	tmpl := elpsutil.MustTemplate(`(lisp:if (unquote condition) () (lisp:progn (unquote-splicing body)))`, "condition", "body")
	for _, throughPackage := range []bool{false, true} {
		name := "AddMacros"
		if throughPackage {
			name = "PackageMacros"
		}
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			env := newTestEnv(t)
			macro := elpsutil.FunctionDoc("unless", lisp.Formals("condition", lisp.VarArgSymbol, "body"),
				func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
					return tmpl.Expand(args.Cells[0], lisp.SExpr(args.Cells[1:]))
				}, docs)
			if throughPackage {
				require.NoError(t, lisp.GoError(elpsutil.Load(env, elpsutil.PackageLoader(&testPackage{
					name: "macros", macros: []lisp.LBuiltinDef{macro},
				}))))
			} else {
				require.NoError(t, lisp.GoError(env.DefinePackage(lisp.Symbol("macros"))))
				require.NoError(t, lisp.GoError(env.InPackage(lisp.Symbol("macros"))))
				env.AddMacros(true, macro)
				require.NoError(t, lisp.GoError(env.InPackage(lisp.Symbol(lisp.DefaultUserPackage))))
			}
			registered := env.Runtime.Registry.Package("macros").Get(lisp.Symbol("unless"))
			require.True(t, registered.IsMacro())
			assert.Equal(t, docs, registered.Docstring())
			got := env.LoadString("unless.lisp", `
(let ((condition false) (value 0))
  (macros:unless condition (set! value (+ value 1)) (set! value (+ value 2)))
  (macros:unless true (error 'unexpected-body))
  (list value (macros:unless false)))`)
			require.NoError(t, lisp.GoError(got))
			assert.Equal(t, `'(3 ())`, got.String())
		})
	}
}
