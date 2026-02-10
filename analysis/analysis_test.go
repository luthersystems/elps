// Copyright © 2024 The ELPS authors

package analysis

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// parseAndAnalyze is a test helper that parses source and runs analysis.
func parseAndAnalyze(t *testing.T, source string) *Result {
	t.Helper()
	s := token.NewScanner("test.lisp", bytes.NewReader([]byte(source)))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	return Analyze(exprs, &Config{Filename: "test.lisp"})
}

// --- Scope tests ---

func TestScope_Define_Lookup(t *testing.T) {
	parent := NewScope(ScopeGlobal, nil, nil)
	child := NewScope(ScopeLet, parent, nil)

	parent.Define(&Symbol{Name: "x", Kind: SymVariable})
	child.Define(&Symbol{Name: "y", Kind: SymVariable})

	// Child can see both x and y
	assert.NotNil(t, child.Lookup("x"))
	assert.NotNil(t, child.Lookup("y"))

	// Parent can only see x
	assert.NotNil(t, parent.Lookup("x"))
	assert.Nil(t, parent.Lookup("y"))
}

func TestScope_LookupLocal(t *testing.T) {
	parent := NewScope(ScopeGlobal, nil, nil)
	child := NewScope(ScopeLet, parent, nil)

	parent.Define(&Symbol{Name: "x", Kind: SymVariable})
	child.Define(&Symbol{Name: "y", Kind: SymVariable})

	assert.Nil(t, child.LookupLocal("x"))
	assert.NotNil(t, child.LookupLocal("y"))
}

func TestScope_Shadowing(t *testing.T) {
	parent := NewScope(ScopeGlobal, nil, nil)
	child := NewScope(ScopeLet, parent, nil)

	parentSym := &Symbol{Name: "x", Kind: SymVariable}
	childSym := &Symbol{Name: "x", Kind: SymVariable}
	parent.Define(parentSym)
	child.Define(childSym)

	// Child lookup finds the child's symbol
	assert.Same(t, childSym, child.Lookup("x"))
	// Parent lookup finds parent's symbol
	assert.Same(t, parentSym, parent.Lookup("x"))
}

// --- Signature tests ---

func TestSignature_MinMaxArity(t *testing.T) {
	tests := []struct {
		name    string
		sig     *Signature
		wantMin int
		wantMax int
	}{
		{
			name:    "nil signature",
			sig:     nil,
			wantMin: 0,
			wantMax: -1,
		},
		{
			name:    "empty",
			sig:     &Signature{},
			wantMin: 0,
			wantMax: 0,
		},
		{
			name: "required only",
			sig: &Signature{Params: []lisp.ParamInfo{
				{Name: "a", Kind: lisp.ParamRequired},
				{Name: "b", Kind: lisp.ParamRequired},
			}},
			wantMin: 2,
			wantMax: 2,
		},
		{
			name: "with optional",
			sig: &Signature{Params: []lisp.ParamInfo{
				{Name: "a", Kind: lisp.ParamRequired},
				{Name: "b", Kind: lisp.ParamOptional},
			}},
			wantMin: 1,
			wantMax: 2,
		},
		{
			name: "variadic",
			sig: &Signature{Params: []lisp.ParamInfo{
				{Name: "a", Kind: lisp.ParamRequired},
				{Name: "rest", Kind: lisp.ParamRest},
			}},
			wantMin: 1,
			wantMax: -1,
		},
		{
			name: "with key",
			sig: &Signature{Params: []lisp.ParamInfo{
				{Name: "a", Kind: lisp.ParamRequired},
				{Name: "k", Kind: lisp.ParamKey},
			}},
			wantMin: 1,
			wantMax: -1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assert.Equal(t, tt.wantMin, tt.sig.MinArity())
			assert.Equal(t, tt.sig.MaxArity(), tt.wantMax)
		})
	}
}


// --- Builtins ---

func TestPopulateBuiltins(t *testing.T) {
	scope := NewScope(ScopeGlobal, nil, nil)
	populateBuiltins(scope)

	// Should have common builtins
	assert.NotNil(t, scope.Lookup("car"))
	assert.NotNil(t, scope.Lookup("+"))
	assert.NotNil(t, scope.Lookup("not"))

	// Should have special ops
	assert.NotNil(t, scope.Lookup("if"))
	assert.NotNil(t, scope.Lookup("lambda"))

	// Should have macros
	assert.NotNil(t, scope.Lookup("defun"))
	assert.NotNil(t, scope.Lookup("cond"))

	// Should have true/false
	assert.NotNil(t, scope.Lookup("true"))
	assert.NotNil(t, scope.Lookup("false"))
}

// --- Analyze: basic resolution ---

func TestAnalyze_BuiltinResolved(t *testing.T) {
	result := parseAndAnalyze(t, `(+ 1 2)`)
	assert.Empty(t, result.Unresolved)
}

func TestAnalyze_UnresolvedSymbol(t *testing.T) {
	result := parseAndAnalyze(t, `(foo 1 2)`)
	require.Len(t, result.Unresolved, 1)
	assert.Equal(t, "foo", result.Unresolved[0].Name)
}

func TestAnalyze_KeywordsSkipped(t *testing.T) {
	result := parseAndAnalyze(t, `(list :key :value)`)
	// :key and :value are keywords, should not be unresolved
	for _, u := range result.Unresolved {
		assert.NotEqual(t, ":key", u.Name)
		assert.NotEqual(t, ":value", u.Name)
	}
}

func TestAnalyze_QualifiedSkipped(t *testing.T) {
	result := parseAndAnalyze(t, `(math:floor 1.5)`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "math:floor", u.Name)
	}
}

func TestAnalyze_QuotedIgnored(t *testing.T) {
	result := parseAndAnalyze(t, `'(unknown-symbol 1 2)`)
	assert.Empty(t, result.Unresolved)
}

func TestAnalyze_QuotedSymbolIgnored(t *testing.T) {
	// 'type-error is a quoted symbol used as data, not a variable reference
	result := parseAndAnalyze(t, `(error 'type-error "bad")`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "type-error", u.Name, "quoted symbol should not be resolved")
	}
}

// --- Analyze: defun ---

func TestAnalyze_Defun(t *testing.T) {
	result := parseAndAnalyze(t, `(defun foo (x) (+ x 1))`)
	assert.NotNil(t, result.RootScope.LookupLocal("foo"))
	// x should be resolved inside the function
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name)
	}
}

func TestAnalyze_DefunForwardRef(t *testing.T) {
	// bar calls foo, which is defined later — should resolve
	result := parseAndAnalyze(t, "(defun bar () (foo))\n(defun foo () 42)")
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "foo", u.Name, "foo should be resolved via prescan")
	}
}

func TestAnalyze_DefunParams(t *testing.T) {
	result := parseAndAnalyze(t, `(defun add (a b) (+ a b))`)
	// a and b should resolve
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "a", u.Name)
		assert.NotEqual(t, "b", u.Name)
	}
}

func TestAnalyze_DefunDocstring(t *testing.T) {
	result := parseAndAnalyze(t, `(defun add (a b) "Add two numbers." (+ a b))`)
	sym := result.RootScope.LookupLocal("add")
	require.NotNil(t, sym)
	assert.Equal(t, "Add two numbers.", sym.DocString)
}

// --- Analyze: lambda ---

func TestAnalyze_Lambda(t *testing.T) {
	result := parseAndAnalyze(t, `(lambda (x) (+ x 1))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name)
	}
}

// --- Analyze: let ---

func TestAnalyze_Let(t *testing.T) {
	result := parseAndAnalyze(t, `(let ((x 1) (y 2)) (+ x y))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name)
		assert.NotEqual(t, "y", u.Name)
	}
}

func TestAnalyze_LetStar_Sequential(t *testing.T) {
	// In let*, y can reference x
	result := parseAndAnalyze(t, `(let* ((x 1) (y x)) (+ x y))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name)
		assert.NotEqual(t, "y", u.Name)
	}
}

func TestAnalyze_Let_ParallelScope(t *testing.T) {
	// In let (not let*), y cannot reference x in its initializer
	result := parseAndAnalyze(t, `(let ((x 1) (y x)) (+ x y))`)
	// x in y's initializer should be unresolved (unless x is globally defined)
	hasUnresolvedX := false
	for _, u := range result.Unresolved {
		if u.Name == "x" {
			hasUnresolvedX = true
		}
	}
	assert.True(t, hasUnresolvedX, "x in let initializer should be unresolved")
}

// --- Analyze: flet and labels ---

func TestAnalyze_Flet(t *testing.T) {
	result := parseAndAnalyze(t, `(flet ((helper (x) (+ x 1))) (helper 42))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "helper", u.Name)
	}
}

func TestAnalyze_Labels_MutualRecursion(t *testing.T) {
	source := `(labels ((even? (n) (if (= n 0) true (odd? (- n 1))))
                  (odd? (n) (if (= n 0) false (even? (- n 1)))))
              (even? 4))`
	result := parseAndAnalyze(t, source)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "even?", u.Name)
		assert.NotEqual(t, "odd?", u.Name)
	}
}

// --- Analyze: dotimes ---

func TestAnalyze_Dotimes(t *testing.T) {
	result := parseAndAnalyze(t, `(dotimes (i 10) (print i))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "i", u.Name)
	}
}

// --- Analyze: set and set! ---

func TestAnalyze_Set(t *testing.T) {
	result := parseAndAnalyze(t, "(set 'x 42)\n(+ x 1)")
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name)
	}
}

func TestAnalyze_SetBang(t *testing.T) {
	result := parseAndAnalyze(t, "(set 'x 1)\n(set! x 42)")
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name)
	}
}

// --- Analyze: function (#') ---

func TestAnalyze_FunctionRef(t *testing.T) {
	result := parseAndAnalyze(t, `(defun foo () 42) (map (function foo) '(1 2 3))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "foo", u.Name)
	}
}

// --- Analyze: handler-bind ---

func TestAnalyze_HandlerBind(t *testing.T) {
	result := parseAndAnalyze(t, `(handler-bind ((condition (lambda (c &rest args) c))) (+ 1 2))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "c", u.Name)
	}
}

// --- Analyze: test-let / test-let* ---

func TestAnalyze_TestLet_BindingsResolved(t *testing.T) {
	// test-let creates let bindings in its body
	result := parseAndAnalyze(t, `(test-let "my test" ((x 1) (y 2)) (+ x y))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name)
		assert.NotEqual(t, "y", u.Name)
	}
}

func TestAnalyze_TestLetStar_Sequential(t *testing.T) {
	// test-let* uses sequential binding — y can reference x
	result := parseAndAnalyze(t, `(test-let* "my test" ((x 1) (y x)) (+ x y))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name)
		assert.NotEqual(t, "y", u.Name)
	}
}

func TestAnalyze_TestLet_OuterScopeVisible(t *testing.T) {
	result := parseAndAnalyze(t, "(set 'z 10)\n(test-let \"t\" ((x z)) (+ x z))")
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "z", u.Name)
		assert.NotEqual(t, "x", u.Name)
	}
}

// --- Analyze: prefix lambda (#^) ---

func TestAnalyze_PrefixLambda_PercentParam(t *testing.T) {
	// #^(+ % 1) parses to (lisp:expr (+ % 1))
	result := parseAndAnalyze(t, `(lisp:expr (+ % 1))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "%", u.Name, "% should be defined as implicit param")
	}
}

func TestAnalyze_PrefixLambda_NumberedParams(t *testing.T) {
	result := parseAndAnalyze(t, `(lisp:expr (+ %1 %2))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "%1", u.Name)
		assert.NotEqual(t, "%2", u.Name)
	}
}

func TestAnalyze_PrefixLambda_RestParam(t *testing.T) {
	result := parseAndAnalyze(t, `(lisp:expr (cons %1 %&rest))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "%1", u.Name)
		assert.NotEqual(t, "%&rest", u.Name)
	}
}

func TestAnalyze_PrefixLambda_OuterScopeVisible(t *testing.T) {
	// Outer scope symbols should still be visible inside prefix lambda
	result := parseAndAnalyze(t, "(set 'y 10)\n(lisp:expr (+ % y))")
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "y", u.Name)
		assert.NotEqual(t, "%", u.Name)
	}
}

// --- Analyze: quasiquote ---

func TestAnalyze_Quasiquote_TemplateIgnored(t *testing.T) {
	// Symbols inside quasiquote template are data, not code references
	result := parseAndAnalyze(t, `(quasiquote (unknown-fn x y))`)
	assert.Empty(t, result.Unresolved, "quasiquote template should not produce unresolved symbols")
}

func TestAnalyze_Quasiquote_UnquoteAnalyzed(t *testing.T) {
	// unquote inside quasiquote IS code and should be analyzed
	result := parseAndAnalyze(t, "(set 'x 1)\n(quasiquote (list (unquote x)))")
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "x", u.Name, "x in unquote should be resolved")
	}
}

func TestAnalyze_Quasiquote_UnquoteSplicingAnalyzed(t *testing.T) {
	result := parseAndAnalyze(t, "(set 'items 1)\n(quasiquote (list (unquote-splicing items)))")
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "items", u.Name, "items in unquote-splicing should be resolved")
	}
}

func TestAnalyze_Quasiquote_UnquoteUnresolved(t *testing.T) {
	// An undefined symbol inside unquote SHOULD be flagged
	result := parseAndAnalyze(t, `(quasiquote (list (unquote undefined-var)))`)
	require.Len(t, result.Unresolved, 1)
	assert.Equal(t, "undefined-var", result.Unresolved[0].Name)
}

func TestAnalyze_Quasiquote_UnquoteInBracketList(t *testing.T) {
	// Unquote inside bracket list ([...]) in quasiquote should resolve symbols.
	// This is common in macro templates like (quasiquote (let ([(unquote x) val]) ...))
	result := parseAndAnalyze(t, `
(defmacro my-macro (patt)
  (quasiquote (let ([(unquote patt) 42]) (unquote patt))))`)
	for _, sym := range result.Symbols {
		if sym.Name == "patt" && sym.Kind == SymParameter {
			assert.Greater(t, sym.References, 0, "patt should be used via unquote inside bracket list")
			return
		}
	}
	t.Fatal("parameter patt not found in symbols")
}

// --- Analyze: reference counting ---

func TestAnalyze_ReferenceCount(t *testing.T) {
	result := parseAndAnalyze(t, `(defun foo (x) (+ x x))`)
	// Find the parameter x
	var paramX *Symbol
	for _, sym := range result.Symbols {
		if sym.Name == "x" && sym.Kind == SymParameter {
			paramX = sym
			break
		}
	}
	require.NotNil(t, paramX)
	assert.Equal(t, 2, paramX.References, "x is referenced twice in (+ x x)")
}

// --- Analyze: export ---

func TestAnalyze_Export(t *testing.T) {
	result := parseAndAnalyze(t, "(defun foo () 42)\n(export 'foo)")
	sym := result.RootScope.LookupLocal("foo")
	require.NotNil(t, sym)
	assert.True(t, sym.Exported)
}

// --- Analyze: external symbols ---

func TestAnalyze_ExternalSymbols(t *testing.T) {
	result := Analyze(nil, &Config{
		ExtraGlobals: []ExternalSymbol{
			{Name: "external-fn", Kind: SymFunction},
		},
	})
	assert.NotNil(t, result.RootScope.Lookup("external-fn"))
}

// --- Analyze: use-package ---

func TestAnalyze_UsePackage_ResolvesImports(t *testing.T) {
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"testing": {
				{Name: "assert-equal", Kind: SymMacro, Package: "testing"},
				{Name: "test", Kind: SymMacro, Package: "testing"},
			},
		},
	}
	source := "(use-package 'testing)\n(assert-equal 1 1)"
	s := token.NewScanner("test.lisp", bytes.NewReader([]byte(source)))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	result := Analyze(exprs, cfg)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "assert-equal", u.Name,
			"assert-equal should be resolved from use-package 'testing")
	}
}

func TestAnalyze_UsePackage_StringArg(t *testing.T) {
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"testing": {
				{Name: "assert-equal", Kind: SymMacro, Package: "testing"},
			},
		},
	}
	source := `(use-package "testing")` + "\n" + `(assert-equal 1 1)`
	s := token.NewScanner("test.lisp", bytes.NewReader([]byte(source)))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	result := Analyze(exprs, cfg)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "assert-equal", u.Name)
	}
}

func TestAnalyze_UsePackage_UnknownPackage(t *testing.T) {
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{},
	}
	source := "(use-package 'unknown)\n(some-fn)"
	s := token.NewScanner("test.lisp", bytes.NewReader([]byte(source)))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	result := Analyze(exprs, cfg)
	require.Len(t, result.Unresolved, 1)
	assert.Equal(t, "some-fn", result.Unresolved[0].Name)
}

func TestAnalyze_InPackage_ImportsLisp(t *testing.T) {
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"lisp": {
				{Name: "lisp-builtin", Kind: SymFunction, Package: "lisp"},
			},
		},
	}
	source := "(in-package \"my-pkg\")\n(lisp-builtin)"
	s := token.NewScanner("test.lisp", bytes.NewReader([]byte(source)))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	result := Analyze(exprs, cfg)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "lisp-builtin", u.Name,
			"lisp-builtin should be auto-imported via in-package")
	}
}

func TestAnalyze_UsePackage_NoOverwriteLocal(t *testing.T) {
	// Local definitions should not be overwritten by package imports
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"testing": {
				{Name: "helper", Kind: SymFunction, Package: "testing"},
			},
		},
	}
	source := "(defun helper () 42)\n(use-package 'testing)\n(helper)"
	s := token.NewScanner("test.lisp", bytes.NewReader([]byte(source)))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	result := Analyze(exprs, cfg)
	// The local helper should be used, not the package one
	sym := result.RootScope.LookupLocal("helper")
	require.NotNil(t, sym)
	assert.Equal(t, SymFunction, sym.Kind)
	assert.NotNil(t, sym.Source, "local defun should retain its source")
}

// --- ScopeKind.String() ---

func TestScopeKind_String(t *testing.T) {
	assert.Equal(t, "global", ScopeGlobal.String())
	assert.Equal(t, "function", ScopeFunction.String())
	assert.Equal(t, "lambda", ScopeLambda.String())
	assert.Equal(t, "let", ScopeLet.String())
	assert.Equal(t, "flet", ScopeFlet.String())
	assert.Equal(t, "dotimes", ScopeDotimes.String())
}

// --- SymbolKind.String() ---

func TestSymbolKind_String(t *testing.T) {
	assert.Equal(t, "variable", SymVariable.String())
	assert.Equal(t, "function", SymFunction.String())
	assert.Equal(t, "macro", SymMacro.String())
	assert.Equal(t, "parameter", SymParameter.String())
	assert.Equal(t, "special-op", SymSpecialOp.String())
	assert.Equal(t, "builtin", SymBuiltin.String())
}
