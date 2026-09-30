// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// These goldens were recorded before macroGetDefault used FormTemplate. Keep
// the result and evaluation cost unchanged. The GenSyms port changes only
// temporary names in the printed expansion.
func TestGetDefaultFormTemplateParity(t *testing.T) {
	for _, tt := range []struct {
		name, program, result string
		steps                 int64
	}{
		{"hit", `(get-default (sorted-map "k" 7) "k" 42)`, `7`, 24},
		{"miss", `(get-default (sorted-map "other" 7) "k" 42)`, `42`, 21},
		{"nil map", `(get-default () "k" 42)`, `42`, 15},
		{"non-map", `(handler-bind ((condition (lambda (&rest _) 'caught))) (get-default 1 "k" 42))`, `'caught`, 23},
		{"nested default", `(get-default (sorted-map) "k" (get-default () "inner" 42))`, `42`, 33},
		{"default side effects", `(let ((calls 0)) (list (get-default (sorted-map) "k" (progn (set! calls (+ calls 1)) 42)) calls))`, `'(42 1)`, 33},
		{"unused default", `(let ((calls 0)) (list (get-default (sorted-map "k" 7) "k" (progn (set! calls (+ calls 1)) 42)) calls))`, `'(7 0)`, 30},
		{"macroexpand", `(macroexpand-1 '(get-default m k d))`, `'(lisp:let ((map@1@1 m) (key@1@2 k)) (lisp:if (lisp:if (lisp:nil? map@1@1) lisp:false (lisp:key? map@1@1 key@1@2)) (lisp:get map@1@1 key@1@2) d))`, 3},
	} {
		t.Run(tt.name, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithMaxSteps(1000000))))
			before := env.Runtime.TotalSteps()
			got := env.LoadString("get-default.lisp", tt.program)
			steps := env.Runtime.TotalSteps() - before
			require.NoError(t, lisp.GoError(got))
			assert.Equal(t, tt.result, got.String())
			assert.Equal(t, tt.steps, steps)
		})
	}
}

func TestFormTemplateSubstitution(t *testing.T) {
	// Argument order comes from params, not the order of first occurrence.
	tmpl := lisp.MustFormTemplate(`(lisp:list ,a ,b ,a)`, "b", "a")
	a, b := lisp.Int(7), lisp.String("hello")
	got := tmpl.Expand(b, a)
	assert.False(t, got.IsQuoted())
	assert.Equal(t, `(lisp:list 7 "hello" 7)`, got.String())
	assert.Same(t, a, got.Cells[1])
	assert.Same(t, b, got.Cells[2])
	assert.Same(t, a, got.Cells[3])
	assert.Same(t, a, lisp.MustFormTemplate(`,a`, "a").Expand(a))
	assert.Same(t, lisp.Nil(), lisp.MustFormTemplate(`()`).Expand())

	// Quotes add a private header, without changing the substituted argument.
	list := lisp.SExpr([]*lisp.LVal{a})
	quoted := lisp.MustFormTemplate(`',a`, "a").Expand(list)
	assert.True(t, quoted.IsQuoted())
	assert.False(t, list.IsQuoted())
	assert.Same(t, a, quoted.Cells[0])
}

func TestFormTemplateSplice(t *testing.T) {
	a, b := lisp.Int(7), lisp.String("hello")
	forms := lisp.SExpr([]*lisp.LVal{a, b})
	tmpl := lisp.MustFormTemplate(`(lisp:list before ,@forms after (,last ,@forms))`, "forms", "last")
	got := tmpl.Expand(forms, a)
	assert.Equal(t, `(lisp:list before 7 "hello" after (7 7 "hello"))`, got.String())
	assert.Same(t, a, got.Cells[2])
	assert.Same(t, b, got.Cells[3])
	assert.Same(t, a, got.Cells[5].Cells[1])
	assert.Same(t, b, got.Cells[5].Cells[2])
	got.Cells[2] = lisp.Symbol("changed")
	assert.Same(t, a, forms.Cells[0], "the enclosing cells slice must be fresh")

	for _, empty := range []*lisp.LVal{lisp.Nil(), lisp.SExpr(nil)} {
		assert.Equal(t, `(lisp:list before after (7))`, tmpl.Expand(empty, a).String())
		assert.True(t, lisp.MustFormTemplate(`(,@forms)`, "forms").Expand(empty).IsNil())
	}
}

func TestFormTemplateExpandAt(t *testing.T) {
	parsed, err := parser.NewReader().Read("template.lisp", strings.NewReader("(anchor)\n(arg child)\n(spliced (nested))"))
	require.NoError(t, err)
	require.Len(t, parsed, 3)
	at, arg, rest := parsed[0], parsed[1], parsed[2]
	bare := lisp.SExpr([]*lisp.LVal{lisp.Symbol("unlocated")})
	before := lisp.SealedASTFingerprint(parsed)
	inputs := make(map[*lisp.LVal]*token.Location)
	var record func(*lisp.LVal)
	record = func(v *lisp.LVal) {
		inputs[v] = lisp.SourceRefForTest(v)
		for _, child := range v.Cells {
			record(child)
		}
	}
	for _, v := range append(parsed, bare) {
		record(v)
	}

	tmpl := lisp.MustFormTemplate(`(wrap ,arg ,bare ,@rest (inner 'symbol ''(nested)) '() () ',arg '',arg)`, "arg", "bare", "rest")
	got := tmpl.ExpandAt(at, arg, bare, rest)
	assert.Equal(t, tmpl.Expand(arg, bare, rest).String(), got.String())
	assert.Same(t, arg, got.Cells[1])
	assert.Same(t, bare, got.Cells[2])
	assert.Same(t, rest.Cells[0], got.Cells[3])
	assert.Same(t, rest.Cells[1], got.Cells[4])
	assert.Same(t, lisp.Nil(), got.Cells[7], "empty template lists remain singletons")
	assert.Same(t, arg, lisp.MustFormTemplate(`,arg`, "arg").ExpandAt(at, arg))
	assert.Same(t, lisp.Nil(), lisp.MustFormTemplate(`()`).ExpandAt(at))
	assert.Same(t, lisp.Nil(), lisp.MustFormTemplate(`(,@rest)`, "rest").ExpandAt(at, lisp.Nil()))

	// Quoting an argument creates a private header over the same sealed
	// cells. Locate that header while keeping the argument and seal intact.
	quoted := got.Cells[8]
	assert.NotSame(t, arg, quoted)
	assert.True(t, quoted.IsQuoted())
	assert.True(t, quoted.IsSealed())
	assert.Same(t, arg.Cells[0], quoted.Cells[0])
	assert.Same(t, arg.Cells[1], quoted.Cells[1])
	assert.Equal(t, lisp.LQuote, got.Cells[9].Type)
	assert.True(t, got.Cells[9].Cells[0].IsSealed())

	want, ok := at.Source()
	require.True(t, ok)
	shared := lisp.SourceRefForTest(got)
	require.NotNil(t, shared)
	assert.NotSame(t, inputs[at], shared, "the expansion owns one location copy")
	var check func(*lisp.LVal)
	check = func(v *lisp.LVal) {
		if _, inserted := inputs[v]; inserted || v == lisp.Nil() {
			return
		}
		loc, ok := v.Source()
		assert.True(t, ok, "template node %v must be located", v)
		assert.Equal(t, want, loc)
		assert.Same(t, shared, lisp.SourceRefForTest(v), "template nodes share one location")
		for _, child := range v.Cells {
			check(child)
		}
	}
	check(got)
	for v, source := range inputs {
		assert.Same(t, source, lisp.SourceRefForTest(v), "inserted node's source changed: %v", v)
	}
	assert.False(t, arg.IsQuoted())
	assert.Equal(t, before, lisp.SealedASTFingerprint(parsed))
	second := tmpl.ExpandAt(at, arg, bare, rest)
	assert.NotSame(t, shared, lisp.SourceRefForTest(second), "expansions own independent locations")
}

func TestFormTemplateExpandAtWithoutLocation(t *testing.T) {
	arg := lisp.Symbol("arg")
	arg.SetSource(&token.Location{File: "arg.lisp", Pos: 42, Line: 3, Col: 4})
	rest := lisp.SExpr([]*lisp.LVal{arg})
	for _, src := range []string{`(f ,arg ,@rest () '() ''symbol)`, `'(,arg ,@rest)`, `(',arg ,@rest)`} {
		t.Run(src, func(t *testing.T) {
			tmpl := lisp.MustFormTemplate(src, "arg", "rest")
			want := tmpl.Expand(arg, rest)
			assert.Equal(t, want, tmpl.ExpandAt(nil, arg, rest))
			assert.Equal(t, want, tmpl.ExpandAt(lisp.Symbol("unlocated"), arg, rest))
		})
	}
}

func TestFormTemplateExpandAtAllocations(t *testing.T) {
	tmpl := lisp.MustFormTemplate(`(f (g ,arg) 'symbol ''(nested ,arg) ,@rest)`, "arg", "rest")
	arg, rest := lisp.Symbol("arg"), lisp.SExpr([]*lisp.LVal{lisp.Symbol("spliced")})
	at, unlocated := lisp.Symbol("at"), lisp.Symbol("unlocated")
	at.SetSource(&token.Location{File: "template.lisp", Pos: 1, Line: 1, Col: 2})
	var got *lisp.LVal
	base := testing.AllocsPerRun(100, func() { got = tmpl.Expand(arg, rest) })
	assert.Equal(t, base, testing.AllocsPerRun(100, func() { got = tmpl.ExpandAt(nil, arg, rest) }))
	assert.Equal(t, base, testing.AllocsPerRun(100, func() { got = tmpl.ExpandAt(unlocated, arg, rest) }))
	assert.Equal(t, base+1, testing.AllocsPerRun(100, func() { got = tmpl.ExpandAt(at, arg, rest) }),
		"locating an expansion allocates one location, with no extra nodes or walk")
	require.NotNil(t, got)
}

func TestFormTemplateQuoteMatchesReader(t *testing.T) {
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
		{src: `',x`, parsed: `'x`, params: []string{"x"}, args: []*lisp.LVal{lisp.Symbol("x")}},
		{src: `',x`, parsed: `''x`, params: []string{"x"}, args: []*lisp.LVal{lisp.Quote(lisp.Symbol("x"))}},
		{src: `',x`, parsed: `'(x y)`, params: []string{"x"}, args: []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("x"), lisp.Symbol("y")})}},
		{src: `'(x ,@rest)`, parsed: `'(x y z)`, params: []string{"rest"}, args: []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("y"), lisp.Symbol("z")})}},
	} {
		t.Run(tt.src+"/"+tt.parsed, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
			parsed, err := parser.NewReader().Read("quote.lisp", strings.NewReader(tt.parsed))
			require.NoError(t, err)
			require.Len(t, parsed, 1)
			form := lisp.MustFormTemplate(tt.src, tt.params...).Expand(tt.args...)
			assert.Equal(t, parsed[0].String(), form.String())
			want, got := env.Eval(parsed[0]), env.Eval(form)
			require.NoError(t, lisp.GoError(want))
			require.NoError(t, lisp.GoError(got))
			assert.Equal(t, want.String(), got.String())
		})
	}
}

func TestFormTemplateSymbols(t *testing.T) {
	const src = "\t\n(lisp:if\r\n ready?\u2003π+ foo#bar a`b &rest @name -name)\v\f "
	assert.Equal(t, "(lisp:if ready? π+ foo#bar a`b &rest @name -name)", lisp.MustFormTemplate(src).Expand().String())
	assert.Equal(t, `(foo 'bar baz)`, lisp.MustFormTemplate(`(foo'bar,x)`, "x").Expand(lisp.Symbol("baz")).String())
}

func TestFormTemplateFreshness(t *testing.T) {
	tmpl := lisp.MustFormTemplate(`(f (g h) 'x ''(y z) '() ())`)
	first, second := tmpl.Expand(), tmpl.Expand()
	nodes := make(map[*lisp.LVal]bool)
	var walk func(*lisp.LVal, bool)
	walk = func(v *lisp.LVal, record bool) {
		// () is explicitly the immutable Nil singleton, not template storage.
		if v == lisp.Nil() {
			return
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
	assert.Equal(t, `(f (g h) 'x ''(y z) '() ())`, second.String())
	assert.Equal(t, second.String(), tmpl.Expand().String())

	// Concurrent users may mutate the fresh syntax returned by one template.
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

func TestFormTemplateSyntaxPanics(t *testing.T) {
	for _, tt := range []struct {
		name, src, message string
		params             []string
	}{
		{"empty", ``, "offset 0: expected a form", nil},
		{"whitespace", " \t", "offset 2: expected a form", nil},
		{"unclosed list", `(x`, "offset 0: unclosed list", nil},
		{"unclosed nested list", `(x (y`, "offset 3: unclosed list", nil},
		{"unexpected close", `)`, "offset 0: unexpected ')'", nil},
		{"extra close", `(x))`, "offset 3: trailing input", nil},
		{"trailing form", `(x) y`, "offset 4: trailing input", nil},
		{"missing quoted form", `'`, "offset 1: expected a form", nil},
		{"missing nested quoted form", `(')`, "offset 2: unexpected ')'", nil},
		{"missing placeholder", `,`, "offset 1: expected a symbol", nil},
		{"missing splice name", `(,@)`, "offset 3: expected a symbol", nil},
		{"space after comma", `, x`, "offset 1: expected a symbol", []string{"x"}},
		{"list after comma", `,(x)`, "offset 1: expected a symbol", []string{"x"}},
		{"undeclared", `(,x)`, `offset 1: undeclared placeholder "x"`, nil},
		{"undeclared splice", `(,@x)`, `offset 1: undeclared placeholder "x"`, nil},
		{"unused", `(x)`, `unused parameter "x"`, []string{"x"}},
		{"partly unused", `,x`, `unused parameter "y"`, []string{"x", "y"}},
		{"duplicate", `,x`, `duplicate parameter "x"`, []string{"x", "x"}},
		{"top-level splice", `,@x`, "offset 0: splice must be directly inside a list", []string{"x"}},
		{"quoted splice", `',@x`, "offset 1: splice must be directly inside a list", []string{"x"}},
		{"quoted splice in list", `(',@x)`, "offset 2: splice must be directly inside a list", []string{"x"}},
		{"double quoted splice", `('',@x)`, "offset 3: splice must be directly inside a list", []string{"x"}},
	} {
		t.Run(tt.name, func(t *testing.T) {
			assert.PanicsWithValue(t, "lisp.MustFormTemplate: "+tt.message, func() {
				lisp.MustFormTemplate(tt.src, tt.params...)
			})
		})
	}
	for _, token := range []string{`0`, `123abc`, `1.5`, `"text"`, `;comment`, `[x]`, `]`, `#x10`, "`x"} {
		t.Run(token, func(t *testing.T) {
			// Byte offsets, including when earlier symbols are multibyte.
			defer func() {
				got := recover()
				require.NotNil(t, got)
				assert.Contains(t, got, "lisp.MustFormTemplate: offset 4:")
			}()
			lisp.MustFormTemplate("(π " + token + ")")
		})
	}
}

func TestFormTemplateExpandPanics(t *testing.T) {
	tmpl := lisp.MustFormTemplate(`(,@xs)`, "xs")
	assert.PanicsWithValue(t, "lisp.FormTemplate.Expand: expected 1 arguments, got 0", func() {
		tmpl.Expand()
	})
	assert.PanicsWithValue(t, "lisp.FormTemplate.Expand: expected 1 arguments, got 2", func() {
		tmpl.Expand(lisp.Nil(), lisp.Nil())
	})
	for _, arg := range []*lisp.LVal{
		nil, lisp.Int(1), lisp.String("x"), lisp.Symbol("x"),
		lisp.QExpr(nil), lisp.QExpr([]*lisp.LVal{lisp.Int(1)}),
		lisp.Quote(lisp.Quote(lisp.Nil())), lisp.Vector(nil),
	} {
		assert.PanicsWithValue(t, "lisp.FormTemplate.Expand: splice argument 1 must be an unquoted list or nil", func() {
			tmpl.Expand(arg)
		})
	}
}
