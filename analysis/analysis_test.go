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

func TestScope_Define_PackageScopedLookup(t *testing.T) {
	scope := NewScope(ScopeGlobal, nil, nil)
	sym := &Symbol{Name: "run", Package: "foo", Kind: SymFunction}
	scope.Define(sym)

	// Package-agnostic lookups find the symbol regardless of package.
	assert.Same(t, sym, scope.LookupLocal("run"))
	assert.Same(t, sym, scope.Lookup("run"))

	// Package-qualified lookup finds it for the correct package.
	assert.Same(t, sym, scope.LookupLocalInPackage("run", "foo"))
	assert.Nil(t, scope.LookupInPackage("run", "bar"))
}

func TestScope_Lookup_NonUserPackage(t *testing.T) {
	// Regression: Lookup and LookupLocal must find symbols defined in
	// non-user packages. Without this, the minifier's macro template
	// protection and the lint user-arity check both miss symbols in
	// files that use (in-package 'some-pkg).
	parent := NewScope(ScopeGlobal, nil, nil)
	child := NewScope(ScopeLet, parent, nil)

	parentSym := &Symbol{Name: "helper", Package: "mylib", Kind: SymFunction}
	parent.Define(parentSym)
	child.Define(&Symbol{Name: "local-var", Kind: SymVariable})

	// Child walks up and finds the parent's package-scoped symbol.
	assert.Same(t, parentSym, child.Lookup("helper"))
	// Parent also finds it directly.
	assert.Same(t, parentSym, parent.Lookup("helper"))
	assert.Same(t, parentSym, parent.LookupLocal("helper"))
	// Child's LookupLocal does NOT see it (it's in the parent).
	assert.Nil(t, child.LookupLocal("helper"))
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

func TestAnalyze_Labels_SelfRecursion(t *testing.T) {
	source := `(defun countdown (n)
  (labels ([go (i) (if (<= i 0) "done" (go (- i 1)))])
    (go n)))`
	result := parseAndAnalyze(t, source)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "go", u.Name,
			"self-recursive labels call should not be flagged as undefined")
	}
}

func TestAnalyze_Labels_SelfRecursion_WithPackageExports(t *testing.T) {
	// Simulates the code path when PackageRegistry is set (issue #90).
	source := `(defun countdown (n)
  (labels ([go (i) (if (<= i 0) "done" (go (- i 1)))])
    (go n)))`
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"testing": {
				{Name: "assert-equal", Kind: SymMacro, Package: "testing"},
			},
		},
	}
	result := parseAndAnalyzeWithConfig(t, source, cfg)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "go", u.Name,
			"self-recursive labels call should not be flagged as undefined (with PackageExports)")
	}
}

func TestAnalyze_Labels_SelfRecursion_WithExtraGlobals(t *testing.T) {
	// Simulate workspace scanning where a file exports a function with
	// the same name as a labels-defined function.
	source := `(defun countdown (n)
  (labels ([go (i) (if (<= i 0) "done" (go (- i 1)))])
    (go n)))`
	cfg := &Config{
		ExtraGlobals: []ExternalSymbol{
			{Name: "go", Kind: SymFunction},
		},
		PackageExports: map[string][]ExternalSymbol{
			"testing": {
				{Name: "assert-equal", Kind: SymMacro, Package: "testing"},
			},
		},
	}
	result := parseAndAnalyzeWithConfig(t, source, cfg)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "go", u.Name,
			"labels-defined function should shadow external global")
	}
}

func TestAnalyze_Labels_WithUsePackage(t *testing.T) {
	// Regression test: labels should work correctly even when use-package
	// imports symbols into the root scope.
	source := `(use-package 'testing)
(defun countdown (n)
  (labels ([go (i) (if (<= i 0) "done" (go (- i 1)))])
    (go n)))`
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"testing": {
				{Name: "assert-equal", Kind: SymMacro, Package: "testing"},
				{Name: "test", Kind: SymMacro, Package: "testing"},
			},
		},
	}
	result := parseAndAnalyzeWithConfig(t, source, cfg)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "go", u.Name,
			"labels function should not be flagged with use-package")
	}
}

func TestAnalyze_Labels_MutualRecursion_WithPackageExports(t *testing.T) {
	source := `(labels ((even? (n) (if (= n 0) true (odd? (- n 1))))
                  (odd? (n) (if (= n 0) false (even? (- n 1)))))
              (even? 4))`
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"testing": {
				{Name: "assert-equal", Kind: SymMacro, Package: "testing"},
			},
		},
	}
	result := parseAndAnalyzeWithConfig(t, source, cfg)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "even?", u.Name,
			"even? should resolve (with PackageExports)")
		assert.NotEqual(t, "odd?", u.Name,
			"odd? should resolve (with PackageExports)")
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

func TestAnalyze_HandlerBind_ConditionTypeNotFlagged(t *testing.T) {
	// The first element of each handler-bind clause is a condition type name,
	// not a variable reference. It should not be flagged as undefined.
	result := parseAndAnalyze(t, `
(defun safe-op ()
  (handler-bind
    ((condition (lambda (&rest e) (debug-print "caught" e) ())))
    (/ 1 0)))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "condition", u.Name,
			"condition type in handler-bind clause should not be flagged as undefined")
	}
}

func TestAnalyze_HandlerBind_CustomConditionType(t *testing.T) {
	// Custom condition type names should also not be flagged.
	result := parseAndAnalyze(t, `
(handler-bind
  ((my-error-type (lambda (e) e)))
  (do-something))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "my-error-type", u.Name,
			"custom condition type in handler-bind should not be flagged")
	}
}

func TestAnalyze_HandlerBind_MultipleClauses(t *testing.T) {
	// Multiple handler-bind clauses — all condition types should be skipped.
	result := parseAndAnalyze(t, `
(handler-bind
  ((type-error (lambda (e) e))
   (condition (lambda (e) e)))
  (+ 1 2))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "type-error", u.Name,
			"type-error condition type should not be flagged")
		assert.NotEqual(t, "condition", u.Name,
			"condition type should not be flagged")
	}
}

func TestAnalyze_HandlerBind_BodyStillAnalyzed(t *testing.T) {
	// Ensure that body forms inside handler-bind ARE still analyzed.
	// An undefined symbol in the body should be flagged as unresolved.
	result := parseAndAnalyze(t, `
(handler-bind
  ((condition (lambda (e) e)))
  (undefined-body-call 1 2))`)
	found := false
	for _, u := range result.Unresolved {
		if u.Name == "undefined-body-call" {
			found = true
			break
		}
	}
	assert.True(t, found,
		"undefined symbol in handler-bind body should be flagged as unresolved")
}

func TestAnalyze_HandlerBind_HandlerBodyStillAnalyzed(t *testing.T) {
	// Ensure handler lambda bodies ARE analyzed — undefined symbols inside
	// handler lambdas should be flagged.
	result := parseAndAnalyze(t, `
(handler-bind
  ((condition (lambda (e) (undefined-handler-call e))))
  (+ 1 2))`)
	found := false
	for _, u := range result.Unresolved {
		if u.Name == "undefined-handler-call" {
			found = true
			break
		}
	}
	assert.True(t, found,
		"undefined symbol in handler-bind handler body should be flagged as unresolved")
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

func TestAnalyze_PrefixLambda_UnqualifiedExpr(t *testing.T) {
	// (expr body) is the unqualified form of (lisp:expr body) — both should
	// create an implicit % parameter scope.
	result := parseAndAnalyze(t, `
(defun remove-string (type-specifier lis s)
  (reject type-specifier (expr (string= s %)) lis))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "%", u.Name,
			"% should be defined as implicit param in unqualified (expr ...) form")
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

func TestAnalyze_ExportBeforeDefun(t *testing.T) {
	// export before defun is a common ELPS convention — prescan must handle it.
	result := parseAndAnalyze(t, "(export 'greet)\n(defun greet (name) name)")
	sym := result.RootScope.LookupLocal("greet")
	require.NotNil(t, sym)
	assert.Equal(t, SymFunction, sym.Kind, "greet should be registered as a function")
	assert.True(t, sym.Exported, "export before defun should mark greet as exported")
}

func TestAnalyze_ExportBeforeDefun_Multiple(t *testing.T) {
	source := "(export 'a)\n(export 'b)\n(defun a () 1)\n(defun b () 2)"
	result := parseAndAnalyze(t, source)
	for _, name := range []string{"a", "b"} {
		sym := result.RootScope.LookupLocal(name)
		require.NotNil(t, sym, "symbol %s should exist", name)
		assert.True(t, sym.Exported, "%s should be exported", name)
	}
}

func TestAnalyze_ExportBeforeDefmacro(t *testing.T) {
	result := parseAndAnalyze(t, "(export 'my-macro)\n(defmacro my-macro (x) x)")
	sym := result.RootScope.LookupLocal("my-macro")
	require.NotNil(t, sym)
	assert.Equal(t, SymMacro, sym.Kind)
	assert.True(t, sym.Exported)
}

func TestAnalyze_ExportNonexistent(t *testing.T) {
	result := parseAndAnalyze(t, "(export 'ghost)")
	sym := result.RootScope.LookupLocal("ghost")
	assert.Nil(t, sym, "export of undefined name should not create a symbol")
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

func TestAnalyze_UsePackage_PrefersImportedPackageOverOtherWorkspaceSymbol(t *testing.T) {
	cfg := &Config{
		ExtraGlobals: []ExternalSymbol{
			{Name: "run", Kind: SymFunction, Package: "bar"},
		},
		PackageExports: map[string][]ExternalSymbol{
			"foo": {
				{Name: "run", Kind: SymFunction, Package: "foo"},
			},
		},
	}
	result := parseAndAnalyzeWithConfig(t, "(use-package 'foo)\n(run)", cfg)
	require.Len(t, result.References, 1)
	assert.Equal(t, "foo", result.References[0].Symbol.Package)
	assert.Equal(t, "run", result.References[0].Symbol.Name)
}

func TestScope_LookupLocal_NonUserPackageDefun(t *testing.T) {
	// Regression: LookupLocal must find symbols from (in-package 'foo)
	// files. The lint user-arity analyzer uses RootScope.LookupLocal to
	// detect locally-shadowed functions; without this, arity mismatches in
	// non-user packages become invisible.
	result := parseAndAnalyze(t, `(in-package 'mylib)
(defun helper (x y) (+ x y))
(helper 1 2)`)
	sym := result.RootScope.LookupLocal("helper")
	require.NotNil(t, sym, "LookupLocal should find symbol defined in non-user package")
	assert.Equal(t, SymFunction, sym.Kind)
	assert.Equal(t, "mylib", sym.Package)
}

func TestScope_Lookup_NonUserPackageWalk(t *testing.T) {
	// Regression: Lookup must walk the parent chain and find symbols from
	// non-user packages. The minifier's preserveMacroTemplateSymbol uses
	// scope.Lookup(name) to protect quasiquoted symbols; without this,
	// globals in non-user packages are renamed but quoted template symbols
	// are not, breaking macro expansion.
	result := parseAndAnalyze(t, `(in-package 'mylib)
(defun target () 42)
(defmacro wrap (x) (quasiquote (target (unquote x))))`)
	// Lookup from a child scope should find the root-level symbol.
	for _, scope := range result.RootScope.Children {
		sym := scope.Lookup("target")
		require.NotNil(t, sym, "Lookup should find non-user package symbol from child scope")
		assert.Equal(t, "mylib", sym.Package)
	}
}

func TestAnalyze_InPackage_PrefersCurrentPackageDefinition(t *testing.T) {
	result := parseAndAnalyzeWithConfig(t, `(in-package 'foo)
(defun run () 1)
(defun use-foo () (run))
(in-package 'bar)
(defun run () 2)
(defun use-bar () (run))`, &Config{})

	var fooRef *Reference
	var barRef *Reference
	for _, ref := range result.References {
		if ref.Symbol == nil || ref.Symbol.Name != "run" || ref.Source == nil {
			continue
		}
		switch ref.Source.Line {
		case 3:
			fooRef = ref
		case 6:
			barRef = ref
		}
	}
	require.NotNil(t, fooRef, "expected run reference inside use-foo")
	require.NotNil(t, barRef, "expected run reference inside use-bar")
	assert.Equal(t, "foo", fooRef.Symbol.Package)
	assert.Equal(t, "bar", barRef.Symbol.Package)
}

func TestAnalyze_InPackage_DoesNotLeakUnqualifiedAcrossPackages(t *testing.T) {
	result := parseAndAnalyzeWithConfig(t, `(in-package 'foo)
(defun run () 1)
(in-package 'bar)
(run)`, &Config{})

	require.Len(t, result.Unresolved, 1)
	assert.Equal(t, "run", result.Unresolved[0].Name)
}

func TestAnalyze_UsePackage_DoesNotBlockSamePackageDefinition(t *testing.T) {
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"bar": {
				{Name: "run", Kind: SymFunction, Package: "bar"},
			},
		},
	}
	result := parseAndAnalyzeWithConfig(t, `(in-package 'foo)
(use-package 'bar)
(defun run () 1)
(run)`, cfg)

	require.Len(t, result.References, 1)
	assert.Equal(t, "foo", result.References[0].Symbol.Package)
	assert.Equal(t, "run", result.References[0].Symbol.Name)
}

func TestAnalyze_UsePackage_DoesNotLeakAcrossLaterInPackage(t *testing.T) {
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"bar": {
				{Name: "run", Kind: SymFunction, Package: "bar"},
			},
		},
	}
	result := parseAndAnalyzeWithConfig(t, `(in-package 'foo)
(use-package 'bar)
(run)
(in-package 'baz)
(run)`, cfg)

	require.Len(t, result.References, 1)
	assert.Equal(t, "bar", result.References[0].Symbol.Package)
	require.Len(t, result.Unresolved, 1)
	assert.Equal(t, "run", result.Unresolved[0].Name)
	assert.Equal(t, 5, result.Unresolved[0].Source.Line)
}

func TestAnalyze_UsePackage_ConflictingImportsStayPackageLocal(t *testing.T) {
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"bar": {
				{Name: "run", Kind: SymFunction, Package: "bar"},
			},
			"qux": {
				{Name: "run", Kind: SymFunction, Package: "qux"},
			},
		},
	}
	result := parseAndAnalyzeWithConfig(t, `(in-package 'foo)
(use-package 'bar)
(run)
(in-package 'baz)
(use-package 'qux)
(run)`, cfg)

	require.Len(t, result.References, 2)
	var fooRef *Reference
	var bazRef *Reference
	for _, ref := range result.References {
		if ref.Source == nil || ref.Symbol == nil || ref.Symbol.Name != "run" {
			continue
		}
		switch ref.Source.Line {
		case 3:
			fooRef = ref
		case 6:
			bazRef = ref
		}
	}
	require.NotNil(t, fooRef)
	require.NotNil(t, bazRef)
	assert.Equal(t, "bar", fooRef.Symbol.Package)
	assert.Equal(t, "qux", bazRef.Symbol.Package)
}

func TestAnalyze_InPackage_ImportsCurrentPackageExportsLocally(t *testing.T) {
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"foo": {
				{Name: "run", Kind: SymFunction, Package: "foo"},
			},
			"bar": {
				{Name: "run", Kind: SymFunction, Package: "bar"},
			},
		},
	}
	result := parseAndAnalyzeWithConfig(t, `(in-package 'foo)
(run)
(in-package 'bar)
(run)`, cfg)

	require.Len(t, result.References, 2)
	assert.Equal(t, "foo", result.References[0].Symbol.Package)
	assert.Equal(t, "bar", result.References[1].Symbol.Package)
}

// --- Analyze: def prefix heuristic ---

func TestAnalyze_DefPrefix_DefmethodFormals(t *testing.T) {
	// defmethod is a user macro: (defmethod type :name (self) body...)
	// The formals (self) should create a scope so self is resolved.
	result := parseAndAnalyze(t, `(defmethod point :move (self) (+ self 1))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "self", u.Name, "self should be resolved as a formal parameter")
	}
}

func TestAnalyze_DefPrefix_MultipleArgs(t *testing.T) {
	result := parseAndAnalyze(t, `(defmethod point :add (self other) (+ self other))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "self", u.Name)
		assert.NotEqual(t, "other", u.Name)
	}
}

func TestAnalyze_DefPrefix_BodyUndefined(t *testing.T) {
	// Undefined symbol in body should still be flagged.
	result := parseAndAnalyze(t, `(defmethod point :foo (self) (+ self undefined-var))`)
	hasUndefined := false
	for _, u := range result.Unresolved {
		if u.Name == "undefined-var" {
			hasUndefined = true
		}
	}
	assert.True(t, hasUndefined, "undefined-var in body should be flagged")
}

func TestAnalyze_DefPrefix_NonFormalsSkipped(t *testing.T) {
	// Children before formals (type-spec, method-name) are analyzed in outer scope.
	// point and :move are resolved normally (point as symbol, :move as keyword).
	result := parseAndAnalyze(t, `
(set 'point 1)
(defmethod point :move (self) self)`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "point", u.Name, "point should be resolved from outer scope")
		assert.NotEqual(t, "self", u.Name, "self should be resolved as a formal")
	}
}

func TestAnalyze_DefPrefix_NoFormals(t *testing.T) {
	// (defconst 'name value) has no formals list — falls back to analyzeCall.
	result := parseAndAnalyze(t, `(defconst 'name 42)`)
	// Should not crash; name is quoted so no resolution needed.
	assert.NotNil(t, result)
}

func TestAnalyze_DefPrefix_DefaultCallFallsBackToNormalCall(t *testing.T) {
	result := parseAndAnalyze(t, `(defun f (ctx) (default ctx (sorted-map)))`)

	for _, sym := range result.Symbols {
		assert.False(t,
			(sym.Name == "ctx" && sym.Kind == SymFunction) ||
				(sym.Name == "sorted-map" && sym.Kind == SymParameter),
			"default call should not synthesize def-like symbols: %+v", sym)
	}

	var ctxRef *Reference
	for _, ref := range result.References {
		if ref.Node != nil && ref.Node.Str == "ctx" {
			ctxRef = ref
			break
		}
	}
	require.NotNil(t, ctxRef, "ctx reference in default call should be resolved")
	require.NotNil(t, ctxRef.Symbol)
	assert.Equal(t, SymParameter, ctxRef.Symbol.Kind)
	require.NotNil(t, ctxRef.Symbol.Source)
	assert.Equal(t, 1, ctxRef.Symbol.Source.Line)
	assert.Equal(t, 11, ctxRef.Symbol.Source.Col)
}

func TestAnalyze_DefPrefix_OptionalParams(t *testing.T) {
	// Formals with &optional markers should be detected.
	result := parseAndAnalyze(t, `(defmethod point :foo (self &optional extra) (list self extra))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "self", u.Name)
		assert.NotEqual(t, "extra", u.Name)
	}
}

func TestAnalyze_DefPrefix_NameRegistered(t *testing.T) {
	// When formals are at index 2, child[1] is a definition name (like defun).
	// It should be registered in scope, not resolved as a reference.
	result := parseAndAnalyze(t, `(defendpoint my-handler (req) (+ req 1))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "my-handler", u.Name,
			"my-handler should be registered as a definition name, not flagged as undefined")
	}
	// The name should be defined in scope.
	sym := result.RootScope.LookupLocal("my-handler")
	assert.NotNil(t, sym, "my-handler should be defined in scope")
}

func TestAnalyze_DefPrefix_EmptyFormals(t *testing.T) {
	// (defendpoint healthcheck ()) — empty formals should be detected
	// when in the defun-like position (name then empty list).
	result := parseAndAnalyze(t, `(defendpoint healthcheck () (list 1))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "healthcheck", u.Name,
			"healthcheck should be registered as a definition name")
	}
	sym := result.RootScope.LookupLocal("healthcheck")
	assert.NotNil(t, sym, "healthcheck should be defined in scope")
}

func TestAnalyze_DefPrefix_DefendpointWithArgs(t *testing.T) {
	// Regression: (defendpoint subtract (minuend subtrahend) body)
	// should register 'subtract' as a definition name and bind formals.
	result := parseAndAnalyze(t, `(defendpoint subtract (minuend subtrahend) (- minuend subtrahend))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "subtract", u.Name, "subtract should be a definition name")
		assert.NotEqual(t, "minuend", u.Name, "minuend should be a formal param")
		assert.NotEqual(t, "subtrahend", u.Name, "subtrahend should be a formal param")
	}
}

func TestAnalyze_DefPrefix_NameNotRegisteredWhenFormalsLater(t *testing.T) {
	// When formals are not at index 2 (e.g. defmethod type :name (formals) body),
	// pre-formals children are analyzed normally — not treated as definitions.
	result := parseAndAnalyze(t, `
(set 'point 1)
(defmethod point :move (self) self)`)
	// point should be resolved as a reference (it's defined via set above)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "point", u.Name)
	}
}

func TestAnalyze_CustomDefForm_NonDefPrefixBindsNameAndParams(t *testing.T) {
	result := parseAndAnalyzeWithConfig(t, `(endpoint handler (req) (+ req 1))`, &Config{
		DefForms: []DefFormSpec{
			{Head: "endpoint", FormalsIndex: 2, BindsName: true, NameIndex: 1, NameKind: SymFunction},
		},
	})

	sym := result.RootScope.LookupLocal("handler")
	require.NotNil(t, sym)
	assert.Equal(t, SymFunction, sym.Kind)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "handler", u.Name)
		assert.NotEqual(t, "req", u.Name)
	}
}

func TestAnalyze_CustomDefForm_LaterFormalsWithoutBoundName(t *testing.T) {
	result := parseAndAnalyzeWithConfig(t, `
(set 'point 1)
(register-method point :move (self) self)`, &Config{
		DefForms: []DefFormSpec{
			{Head: "register-method", FormalsIndex: 3},
		},
	})

	for _, u := range result.Unresolved {
		assert.NotEqual(t, "point", u.Name)
		assert.NotEqual(t, "self", u.Name)
	}
	assert.Nil(t, result.RootScope.LookupLocal(":move"))
}

func TestAnalyze_CustomDefForm_EmptyFormals(t *testing.T) {
	result := parseAndAnalyzeWithConfig(t, `(endpoint healthcheck () (list 1))`, &Config{
		DefForms: []DefFormSpec{
			{Head: "endpoint", FormalsIndex: 2, BindsName: true, NameIndex: 1, NameKind: SymFunction},
		},
	})

	sym := result.RootScope.LookupLocal("healthcheck")
	require.NotNil(t, sym)
	assert.Equal(t, SymFunction, sym.Kind)
}

func TestAnalyze_CustomDefForm_PrescanSupportsExportBeforeDefinition(t *testing.T) {
	result := parseAndAnalyzeWithConfig(t, `
(export 'handler)
(endpoint handler (req) req)`, &Config{
		DefForms: []DefFormSpec{
			{Head: "endpoint", FormalsIndex: 2, BindsName: true, NameIndex: 1, NameKind: SymFunction},
		},
	})

	sym := result.RootScope.LookupLocal("handler")
	require.NotNil(t, sym)
	assert.True(t, sym.Exported)
}

func TestAnalyze_CustomDefForm_PrescanSupportsForwardReference(t *testing.T) {
	result := parseAndAnalyzeWithConfig(t, `
(defun caller () (handler 1))
(endpoint handler (req) req)`, &Config{
		DefForms: []DefFormSpec{
			{Head: "endpoint", FormalsIndex: 2, BindsName: true, NameIndex: 1, NameKind: SymFunction},
		},
	})

	for _, u := range result.Unresolved {
		assert.NotEqual(t, "handler", u.Name)
	}
}

func TestAnalyze_DefPrefix_DefaultInsideLabelsOptionalParam(t *testing.T) {
	result := parseAndAnalyze(t, `
(defun outer ()
  (labels ([register (id name &optional ctx)
            (let* ([body (default ctx (sorted-map))])
              (assoc! body "id" id)
              (assoc! body "name" name)
              body)])
    (register "a" "b")))`)

	for _, sym := range result.Symbols {
		assert.False(t,
			(sym.Name == "ctx" && sym.Kind == SymFunction) ||
				(sym.Name == "sorted-map" && sym.Kind == SymParameter),
			"default call in labels should not synthesize def-like symbols: %+v", sym)
	}

	var ctxRef *Reference
	for _, ref := range result.References {
		if ref.Node != nil && ref.Node.Str == "ctx" && ref.Source != nil && ref.Source.Line == 4 {
			ctxRef = ref
			break
		}
	}
	require.NotNil(t, ctxRef, "ctx reference in labels body should be resolved")
	require.NotNil(t, ctxRef.Symbol)
	assert.Equal(t, SymParameter, ctxRef.Symbol.Kind)
	require.NotNil(t, ctxRef.Symbol.Source)
	assert.Equal(t, 3, ctxRef.Symbol.Source.Line)
	assert.Equal(t, 41, ctxRef.Symbol.Source.Col)
}

// --- Analyze: nested defun/defmacro ---

func TestAnalyze_NestedDefmacro_VisibleToSiblings(t *testing.T) {
	// A defmacro inside a test body should be visible to later siblings.
	result := parseAndAnalyze(t, `
(test "my-test"
  (defmacro my-assert (x) (quasiquote (assert (unquote x))))
  (my-assert true))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "my-assert", u.Name,
			"my-assert should be resolved from nested defmacro")
	}
}

func TestAnalyze_NestedDefun_VisibleToSiblings(t *testing.T) {
	// A defun inside a call body should be visible to later siblings.
	result := parseAndAnalyze(t, `
(progn
  (defun helper (x) (+ x 1))
  (helper 42))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "helper", u.Name,
			"helper should be resolved from nested defun")
	}
}

func TestAnalyze_Test_ForwardRef(t *testing.T) {
	// Forward references within a test body should resolve via prescan.
	result := parseAndAnalyze(t, `
(test "forward-ref"
  (my-helper 1)
  (defun my-helper (x) (+ x 1)))`)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "my-helper", u.Name,
			"my-helper should be resolved via prescan in test body")
	}
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
	assert.Equal(t, "type", SymType.String())
}

// --- Analyze: deftype ---

func TestAnalyze_Deftype_NameResolved(t *testing.T) {
	result := parseAndAnalyze(t, `
(deftype point (x y) (sorted-map :x x :y y))
(new point 1 2)`)
	assert.Empty(t, result.Unresolved, "point should be resolved after deftype")
}

func TestAnalyze_Deftype_ConstructorFormals(t *testing.T) {
	// Constructor formals (x, y) should be in scope within the body.
	result := parseAndAnalyze(t, `
(deftype point (x y) (sorted-map :x x :y y))`)
	assert.Empty(t, result.Unresolved)
}

func TestAnalyze_Deftype_BodyUndefined(t *testing.T) {
	// Reference to undefined symbol in deftype body should be flagged.
	result := parseAndAnalyze(t, `(deftype point (x) (+ x z))`)
	require.Len(t, result.Unresolved, 1)
	assert.Equal(t, "z", result.Unresolved[0].Name)
}

func TestAnalyze_StringDeftype(t *testing.T) {
	// s:deftype with string literal creates a symbol binding.
	result := parseAndAnalyzeWithConfig(t, `
(s:deftype "mystring" s:string)
(s:validate mystring "hello")`,
		&Config{
			PackageExports: map[string][]ExternalSymbol{
				"s": {
					{Name: "deftype", Kind: SymFunction},
					{Name: "validate", Kind: SymFunction},
					{Name: "string", Kind: SymVariable},
				},
			},
		})
	// mystring should be resolved (created by s:deftype)
	for _, u := range result.Unresolved {
		assert.NotEqual(t, "mystring", u.Name, "mystring should be resolved by s:deftype")
	}
}

// --- LookupInPackage cross-package negative ---

func TestScope_LookupInPackage_CrossPackageNegative(t *testing.T) {
	// A symbol defined in package "math" should NOT be found when
	// looking up in package "string" — the qualified key differs.
	scope := NewScope(ScopeGlobal, nil, nil)
	scope.Define(&Symbol{Name: "add", Package: "math", Kind: SymFunction})

	// Correct package finds it.
	assert.NotNil(t, scope.LookupInPackage("add", "math"))
	// Wrong package does not.
	assert.Nil(t, scope.LookupInPackage("add", "string"))
}

// --- DefineQualifiedOnly with Package == "" ---

func TestScope_DefineQualifiedOnly_EmptyPackage(t *testing.T) {
	// When Package is empty, DefineQualifiedOnly falls back to Symbols.
	scope := NewScope(ScopeGlobal, nil, nil)
	sym := &Symbol{Name: "x", Kind: SymVariable}
	scope.DefineQualifiedOnly(sym)

	// Should be reachable via bare Symbols lookup.
	assert.Equal(t, sym, scope.Symbols["x"])
	// Should NOT appear in PackageSymbols (no qualified key).
	assert.Empty(t, scope.PackageSymbols)
}

// parseAndAnalyzeWithConfig is a test helper that parses source and runs
// analysis with a custom Config.
func parseAndAnalyzeWithConfig(t *testing.T, source string, cfg *Config) *Result {
	t.Helper()
	s := token.NewScanner("test.lisp", bytes.NewReader([]byte(source)))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	if cfg.Filename == "" {
		cfg.Filename = "test.lisp"
	}
	return Analyze(exprs, cfg)
}
