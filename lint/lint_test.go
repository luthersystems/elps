// Copyright © 2024 The ELPS authors

package lint

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// lintSource runs all default analyzers on the given source and returns diagnostics.
func lintSource(t *testing.T, source string) []Diagnostic {
	t.Helper()
	l := &Linter{Analyzers: DefaultAnalyzers()}
	diags, err := l.LintFile([]byte(source), "test.lisp")
	require.NoError(t, err)
	return diags
}

// lintCheck runs a single analyzer on the given source.
func lintCheck(t *testing.T, analyzer *Analyzer, source string) []Diagnostic {
	t.Helper()
	l := &Linter{Analyzers: []*Analyzer{analyzer}}
	diags, err := l.LintFile([]byte(source), "test.lisp")
	require.NoError(t, err)
	return diags
}

// assertHasDiag checks that at least one diagnostic contains the given substring.
func assertHasDiag(t *testing.T, diags []Diagnostic, substr string) {
	t.Helper()
	for _, d := range diags {
		if strings.Contains(d.Message, substr) {
			return
		}
	}
	var msgs []string
	for _, d := range diags {
		msgs = append(msgs, d.String())
	}
	t.Errorf("expected diagnostic containing %q, got: %v", substr, msgs)
}

// assertNoDiags checks that there are no diagnostics.
func assertNoDiags(t *testing.T, diags []Diagnostic) {
	t.Helper()
	if len(diags) > 0 {
		var msgs []string
		for _, d := range diags {
			msgs = append(msgs, d.String())
		}
		t.Errorf("expected no diagnostics, got %d: %v", len(diags), msgs)
	}
}

// assertDiagOnLine checks that a diagnostic exists on the given line with the given substring.
func assertDiagOnLine(t *testing.T, diags []Diagnostic, line int, substr string) {
	t.Helper()
	for _, d := range diags {
		if d.Pos.Line == line && strings.Contains(d.Message, substr) {
			return
		}
	}
	var msgs []string
	for _, d := range diags {
		msgs = append(msgs, fmt.Sprintf("line %d: %s", d.Pos.Line, d.Message))
	}
	t.Errorf("expected diagnostic on line %d containing %q, got: %v", line, substr, msgs)
}

// --- Position.String() ---

func TestPosition_String_FileOnly(t *testing.T) {
	p := Position{File: "test.lisp"}
	assert.Equal(t, "test.lisp", p.String())
}

func TestPosition_String_FileLine(t *testing.T) {
	p := Position{File: "test.lisp", Line: 10}
	assert.Equal(t, "test.lisp:10", p.String())
}

func TestPosition_String_FileLineCol(t *testing.T) {
	p := Position{File: "test.lisp", Line: 10, Col: 5}
	assert.Equal(t, "test.lisp:10:5", p.String())
}

// --- Diagnostic.String() ---

func TestDiagnostic_String(t *testing.T) {
	d := Diagnostic{
		Pos:      Position{File: "test.lisp", Line: 10},
		Message:  "use set!",
		Analyzer: "set-usage",
	}
	assert.Equal(t, "test.lisp:10: use set! (set-usage)", d.String())
}

// --- Analyzer error propagation ---

func TestLintFile_AnalyzerError(t *testing.T) {
	errAnalyzer := &Analyzer{
		Name: "fail",
		Doc:  "Always fails.",
		Run: func(pass *Pass) error {
			return fmt.Errorf("intentional failure")
		},
	}
	l := &Linter{Analyzers: []*Analyzer{errAnalyzer}}
	_, err := l.LintFile([]byte("(+ 1 2)"), "test.lisp")
	require.Error(t, err)
	assert.Contains(t, err.Error(), "intentional failure")
	assert.Contains(t, err.Error(), "fail")
}

// --- set-usage ---

func TestSetUsage_Positive_RepeatedSet(t *testing.T) {
	// Second set on the same symbol should be set!
	source := "(set 'x 1)\n(set 'x 2)"
	diags := lintCheck(t, AnalyzerSetUsage, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "already bound")
	assert.Equal(t, 2, diags[0].Pos.Line)
}

func TestSetUsage_Positive_RepeatedInBody(t *testing.T) {
	// Sequential set on the same symbol inside a function body
	source := `(defun foo () (set 'x 1) (set 'x 2))`
	diags := lintCheck(t, AnalyzerSetUsage, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "already bound")
}

func TestSetUsage_Negative_FirstSet(t *testing.T) {
	// First set creating a new binding is fine
	diags := lintCheck(t, AnalyzerSetUsage, `(set 'x 42)`)
	assertNoDiags(t, diags)
}

func TestSetUsage_Negative_DifferentSymbols(t *testing.T) {
	// Different symbols are fine
	source := `(set 'x 1) (set 'y 2)`
	diags := lintCheck(t, AnalyzerSetUsage, source)
	assertNoDiags(t, diags)
}

func TestSetUsage_Negative_SetBang(t *testing.T) {
	diags := lintCheck(t, AnalyzerSetUsage, `(set! x 42)`)
	assertNoDiags(t, diags)
}

func TestSetUsage_Negative_Let(t *testing.T) {
	diags := lintCheck(t, AnalyzerSetUsage, `(let ((x 42)) x)`)
	assertNoDiags(t, diags)
}

func TestSetUsage_Negative_DataList(t *testing.T) {
	// A quoted list containing the symbol "set" should not trigger
	diags := lintCheck(t, AnalyzerSetUsage, `'(set x 42)`)
	assertNoDiags(t, diags)
}

func TestSetUsage_Negative_DifferentPackages(t *testing.T) {
	// Same symbol set in different packages is fine — each package
	// has its own namespace so these are independent bindings.
	source := `(in-package 'pkg-a) (set 'x 1) (in-package 'pkg-b) (set 'x 2)`
	diags := lintCheck(t, AnalyzerSetUsage, source)
	assertNoDiags(t, diags)
}

func TestSetUsage_Positive_SamePackageRepeated(t *testing.T) {
	// Repeated set within the same package should still be flagged.
	source := `(in-package 'pkg-a) (set 'x 1) (set 'x 2)`
	diags := lintCheck(t, AnalyzerSetUsage, source)
	assert.Equal(t, 1, len(diags))
}

// --- in-package-toplevel ---

func TestInPackageToplevel_Positive_InDefun(t *testing.T) {
	source := "(defun foo ()\n  (in-package \"my-pkg\")\n  (+ 1 2))"
	diags := lintCheck(t, AnalyzerInPackageToplevel, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "top level")
	assert.Equal(t, 2, diags[0].Pos.Line)
}

func TestInPackageToplevel_Positive_InLet(t *testing.T) {
	source := "(let ((x 1))\n  (in-package \"other\"))"
	diags := lintCheck(t, AnalyzerInPackageToplevel, source)
	assert.Len(t, diags, 1)
	assert.Equal(t, 2, diags[0].Pos.Line)
}

func TestInPackageToplevel_Negative_TopLevel(t *testing.T) {
	diags := lintCheck(t, AnalyzerInPackageToplevel, `(in-package "user")`)
	assertNoDiags(t, diags)
}

func TestInPackageToplevel_Negative_MultipleTopLevel(t *testing.T) {
	source := "(in-package \"user\")\n(defun foo () 42)\n(in-package \"other\")"
	diags := lintCheck(t, AnalyzerInPackageToplevel, source)
	assertNoDiags(t, diags)
}

// --- if-arity ---

func TestIfArity_Positive_TooFew(t *testing.T) {
	diags := lintCheck(t, AnalyzerIfArity, `(if true 1)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "too few")
	assertHasDiag(t, diags, "2")
}

func TestIfArity_Positive_TooMany(t *testing.T) {
	diags := lintCheck(t, AnalyzerIfArity, `(if true 1 2 3)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "too many")
	assertHasDiag(t, diags, "4")
}

func TestIfArity_Positive_OnlyCondition(t *testing.T) {
	diags := lintCheck(t, AnalyzerIfArity, `(if true)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "too few")
	assertHasDiag(t, diags, "1")
}

func TestIfArity_Negative(t *testing.T) {
	diags := lintCheck(t, AnalyzerIfArity, `(if (= x 1) "yes" "no")`)
	assertNoDiags(t, diags)
}

func TestIfArity_Negative_Nested(t *testing.T) {
	source := "(if true\n  (if false \"a\" \"b\")\n  (if true \"c\" \"d\"))"
	diags := lintCheck(t, AnalyzerIfArity, source)
	assertNoDiags(t, diags)
}

// --- let-bindings ---

func TestLetBindings_Positive_MissingOuterParens(t *testing.T) {
	// (let (x 42) x) — binds x to nil and 42 to nil
	diags := lintCheck(t, AnalyzerLetBindings, `(let (x 42) x)`)
	assert.Len(t, diags, 2) // both x and 42 are "not a list"
	assertHasDiag(t, diags, "not a list")
}

func TestLetBindings_Positive_EmptyBinding(t *testing.T) {
	diags := lintCheck(t, AnalyzerLetBindings, `(let (()) x)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "empty")
}

func TestLetBindings_Positive_TooManyElements(t *testing.T) {
	diags := lintCheck(t, AnalyzerLetBindings, `(let ((x 1 2)) x)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "expected 2 elements")
}

func TestLetBindings_Positive_NonSymbolBinding(t *testing.T) {
	diags := lintCheck(t, AnalyzerLetBindings, `(let ((42 1)) x)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "must be a symbol")
}

func TestLetBindings_Positive_LetStar(t *testing.T) {
	diags := lintCheck(t, AnalyzerLetBindings, `(let* (x 42) x)`)
	assert.Len(t, diags, 2) // both x and 42 are "not a list"
	assertHasDiag(t, diags, "not a list")
}

func TestLetBindings_Positive_NoBody(t *testing.T) {
	diags := lintCheck(t, AnalyzerLetBindings, `(let)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "requires a binding list")
}

func TestLetBindings_Positive_NonListBindings(t *testing.T) {
	// Bindings must be a list, not a string
	diags := lintCheck(t, AnalyzerLetBindings, `(let "bindings" x)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "bindings must be a list")
}

func TestLetBindings_Positive_SingleElement(t *testing.T) {
	// (let ((x)) ...) — binding has only 1 element
	diags := lintCheck(t, AnalyzerLetBindings, `(let ((x)) x)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "expected 2 elements")
}

func TestLetBindings_Negative_UnquoteInBindingName(t *testing.T) {
	// (unquote sym) in binding position expands to a symbol at macro time
	source := `(let (((unquote mysym) 42)) x)`
	diags := lintCheck(t, AnalyzerLetBindings, source)
	assertNoDiags(t, diags)
}

func TestLetBindings_Negative(t *testing.T) {
	diags := lintCheck(t, AnalyzerLetBindings, `(let ((x 42) (y 0)) (+ x y))`)
	assertNoDiags(t, diags)
}

func TestLetBindings_Negative_LetStar(t *testing.T) {
	diags := lintCheck(t, AnalyzerLetBindings, `(let* ((x 1) (y (+ x 1))) y)`)
	assertNoDiags(t, diags)
}

func TestLetBindings_Negative_Empty(t *testing.T) {
	diags := lintCheck(t, AnalyzerLetBindings, `(let () 42)`)
	assertNoDiags(t, diags)
}

// --- defun-structure ---

func TestDefunStructure_Positive_TooFew(t *testing.T) {
	diags := lintCheck(t, AnalyzerDefunStructure, `(defun foo)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "at least a name and formals")
}

func TestDefunStructure_Positive_NoArgs(t *testing.T) {
	// (defun) — no args at all
	diags := lintCheck(t, AnalyzerDefunStructure, `(defun)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "at least a name and formals")
}

func TestDefunStructure_Positive_NonSymbolName(t *testing.T) {
	diags := lintCheck(t, AnalyzerDefunStructure, `(defun 42 (x) x)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "name must be a symbol")
}

func TestDefunStructure_Positive_NonListFormals(t *testing.T) {
	diags := lintCheck(t, AnalyzerDefunStructure, `(defun foo x x)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "formals must be a list")
}

func TestDefunStructure_Positive_Defmacro(t *testing.T) {
	diags := lintCheck(t, AnalyzerDefunStructure, `(defmacro 42 (x) x)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "name must be a symbol")
}

func TestDefunStructure_Negative(t *testing.T) {
	diags := lintCheck(t, AnalyzerDefunStructure, `(defun foo (x) (+ x 1))`)
	assertNoDiags(t, diags)
}

func TestDefunStructure_Negative_WithDocstring(t *testing.T) {
	diags := lintCheck(t, AnalyzerDefunStructure, `(defun foo (x) "Add one." (+ x 1))`)
	assertNoDiags(t, diags)
}

func TestDefunStructure_Negative_NoFormals(t *testing.T) {
	diags := lintCheck(t, AnalyzerDefunStructure, `(defun foo () 42)`)
	assertNoDiags(t, diags)
}

func TestDefunStructure_Negative_EmptyBody(t *testing.T) {
	// Empty-body defuns are valid no-ops
	diags := lintCheck(t, AnalyzerDefunStructure, `(defun noop (x y))`)
	assertNoDiags(t, diags)
}

func TestDefunStructure_Negative_InsideQuasiquote(t *testing.T) {
	// defun inside a quasiquote template is data, not a real definition.
	// (defun (unquote name) ...) should not be flagged.
	source := `(defmacro wrap (name) (quasiquote (defun (unquote name) (x) x)))`
	diags := lintCheck(t, AnalyzerDefunStructure, source)
	assertNoDiags(t, diags)
}

// --- cond-structure ---

func TestCondStructure_Positive_NonListClause(t *testing.T) {
	diags := lintCheck(t, AnalyzerCondStructure, `(cond true "yes")`)
	// both `true` and `"yes"` are non-list clauses
	assert.Len(t, diags, 2)
	assertHasDiag(t, diags, "not a list")
}

func TestCondStructure_Positive_EmptyClause(t *testing.T) {
	diags := lintCheck(t, AnalyzerCondStructure, `(cond ())`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "empty")
}

func TestCondStructure_Positive_MisplacedElse(t *testing.T) {
	source := "(cond\n  (else \"default\")\n  ((= x 1) \"one\"))"
	diags := lintCheck(t, AnalyzerCondStructure, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "else clause must be last")
	assert.Equal(t, 2, diags[0].Pos.Line)
}

func TestCondStructure_Positive_NoClauses(t *testing.T) {
	// (cond) — zero clauses, should produce no diagnostics (valid but useless)
	diags := lintCheck(t, AnalyzerCondStructure, `(cond)`)
	assertNoDiags(t, diags)
}

func TestCondStructure_Negative(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  ((= x 2) \"two\")\n  (else \"other\"))"
	diags := lintCheck(t, AnalyzerCondStructure, source)
	assertNoDiags(t, diags)
}

func TestCondStructure_Negative_NoElse(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  ((= x 2) \"two\"))"
	diags := lintCheck(t, AnalyzerCondStructure, source)
	assertNoDiags(t, diags)
}

func TestCondStructure_Negative_TrueDefault(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  (true \"default\"))"
	diags := lintCheck(t, AnalyzerCondStructure, source)
	assertNoDiags(t, diags)
}

// --- builtin-arity ---

func TestBuiltinArity_Positive_TooFew(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(car)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "requires at least 1")
}

func TestBuiltinArity_Positive_TooMany(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(car a b)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "accepts at most 1")
}

func TestBuiltinArity_Positive_ComparisonTooFew(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(= 1)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "requires at least 2")
}

func TestBuiltinArity_Positive_ComparisonTooMany(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(= 1 2 3)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "accepts at most 2")
}

func TestBuiltinArity_Positive_Cons(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(cons 1)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "requires at least 2")
}

func TestBuiltinArity_Positive_NotTooMany(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(not x y z)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "accepts at most 1")
}

func TestBuiltinArity_Positive_Gensym(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(gensym "x")`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "accepts at most 0")
}

func TestBuiltinArity_Negative_Car(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(car my-list)`)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_Variadic(t *testing.T) {
	// Variadic functions should accept any number of args
	diags := lintCheck(t, AnalyzerBuiltinArity, `(+ 1 2 3 4 5)`)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_VariadicZero(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(+)`)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_UnknownFunction(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(my-custom-func a b c)`)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_SetBang(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(set! x 42)`)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_Lambda(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(lambda (x) (+ x 1))`)
	assertNoDiags(t, diags)
}

// --- builtin-arity: user-defined shadow ---

func TestBuiltinArity_Negative_ShadowedByDefun(t *testing.T) {
	// User redefines map with 2 args — should not flag calls with 2 args
	source := "(defun map (fn seq) (cons (fn (car seq)) ()))\n(map #^(* 2 %) '(1 2 3))"
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_ShadowedByDefmacro(t *testing.T) {
	source := "(defmacro get (key) (list 'car key))\n(get x)"
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_ShadowedByParam(t *testing.T) {
	// Parameter name "reverse" shadows the builtin — calls should not be flagged
	source := "(defun get-pos (entity-id get-entity-func reverse)\n  (reverse max-date))"
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_ShadowedByLambdaParam(t *testing.T) {
	// Lambda parameter shadowing a builtin
	source := "(lambda (get) (get x))"
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Positive_NotShadowed(t *testing.T) {
	// A defun for a different name should not prevent checking map
	source := "(defun my-map (fn seq) ())\n(map fn seq)"
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "map requires at least 3")
}

// --- builtin-arity: formals list exclusion ---

func TestBuiltinArity_Negative_DefunFormals(t *testing.T) {
	// (expr) in defmacro formals is a parameter name, not a call
	source := `(defmacro delay (expr) expr)`
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_LambdaFormals(t *testing.T) {
	// (not) in lambda formals is a parameter name, not a call
	source := `(lambda (not) not)`
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Positive_NotInFormals(t *testing.T) {
	// (car) in defun body IS a call and should be checked
	source := `(defun foo (x) (car))`
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "car requires at least 1")
}

// --- builtin-arity: thread-first/thread-last exclusion ---

func TestBuiltinArity_Negative_ThreadFirst(t *testing.T) {
	// (get "key") inside thread-first gets v inserted: (get v "key")
	source := `(thread-first v (get "key") (nth 0))`
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Negative_ThreadLast(t *testing.T) {
	source := `(thread-last v (get "key"))`
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestBuiltinArity_Positive_ThreadFirstBadArity(t *testing.T) {
	// The initial value (first arg) is still checked normally
	source := `(thread-first (car) (get "key"))`
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "car requires at least 1")
}

// --- builtin-arity: deduplication with specific analyzers ---

func TestBuiltinArity_IfNotDuplicated(t *testing.T) {
	// `if` is checked by if-arity, should not also be in builtin-arity table
	_, ok := builtinArityTable["if"]
	assert.False(t, ok, "if should be removed from builtin arity table")
}

func TestBuiltinArity_CondNotDuplicated(t *testing.T) {
	// `cond` is checked by cond-structure, should not also be in builtin-arity table
	_, ok := builtinArityTable["cond"]
	assert.False(t, ok, "cond should be removed from builtin arity table")
}

// --- quote-call ---

func TestQuoteCall_Positive_UnquotedSet(t *testing.T) {
	diags := lintCheck(t, AnalyzerQuoteCall, `(set x 42)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "should be quoted")
	assert.NotEmpty(t, diags[0].Notes)
}

func TestQuoteCall_Positive_UnquotedDefconst(t *testing.T) {
	diags := lintCheck(t, AnalyzerQuoteCall, `(defconst x 42 "doc")`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "should be quoted")
}

func TestQuoteCall_Negative_QuotedSet(t *testing.T) {
	diags := lintCheck(t, AnalyzerQuoteCall, `(set 'x 42)`)
	assertNoDiags(t, diags)
}

func TestQuoteCall_Negative_SetBang(t *testing.T) {
	// set! takes unquoted symbols by design
	diags := lintCheck(t, AnalyzerQuoteCall, `(set! x 42)`)
	assertNoDiags(t, diags)
}

func TestQuoteCall_Negative_DynamicName(t *testing.T) {
	// An s-expression as the name arg is intentional (dynamic binding)
	diags := lintCheck(t, AnalyzerQuoteCall, `(set (compute-name) 42)`)
	assertNoDiags(t, diags)
}

// --- cond-missing-else ---

func TestCondMissingElse_Positive_NoDefault(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  ((= x 2) \"two\"))"
	diags := lintCheck(t, AnalyzerCondMissingElse, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "no default")
	assert.NotEmpty(t, diags[0].Notes)
}

func TestCondMissingElse_Negative_HasElse(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  (else \"default\"))"
	diags := lintCheck(t, AnalyzerCondMissingElse, source)
	assertNoDiags(t, diags)
}

func TestCondMissingElse_Negative_HasTrue(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  (true \"default\"))"
	diags := lintCheck(t, AnalyzerCondMissingElse, source)
	assertNoDiags(t, diags)
}

func TestCondMissingElse_Negative_EmptyCond(t *testing.T) {
	diags := lintCheck(t, AnalyzerCondMissingElse, `(cond)`)
	assertNoDiags(t, diags)
}

func TestCondMissingElse_Negative_SingleElse(t *testing.T) {
	source := "(cond (else \"only\"))"
	diags := lintCheck(t, AnalyzerCondMissingElse, source)
	assertNoDiags(t, diags)
}

func TestCondMissingElse_Negative_KeywordElse(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  (:else \"default\"))"
	diags := lintCheck(t, AnalyzerCondMissingElse, source)
	assertNoDiags(t, diags)
}

func TestCondMissingElse_Negative_KeywordTrue(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  (:true \"default\"))"
	diags := lintCheck(t, AnalyzerCondMissingElse, source)
	assertNoDiags(t, diags)
}

func TestCondStructure_Negative_KeywordElseLast(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  (:else \"default\"))"
	diags := lintCheck(t, AnalyzerCondStructure, source)
	assertNoDiags(t, diags)
}

func TestCondStructure_Positive_MisplacedKeywordElse(t *testing.T) {
	source := "(cond\n  (:else \"default\")\n  ((= x 1) \"one\"))"
	diags := lintCheck(t, AnalyzerCondStructure, source)
	assert.Len(t, diags, 1)
	assert.Contains(t, diags[0].Message, "else clause must be last")
}

// --- rethrow-context ---

func TestRethrowContext_Positive_TopLevel(t *testing.T) {
	diags := lintCheck(t, AnalyzerRethrowContext, `(rethrow)`)
	assert.Len(t, diags, 1)
	assertDiagOnLine(t, diags, 1, "outside handler-bind")
}

func TestRethrowContext_Positive_InDefun(t *testing.T) {
	source := "(defun my-handler (c &rest args)\n  (rethrow))"
	diags := lintCheck(t, AnalyzerRethrowContext, source)
	assert.Len(t, diags, 1)
	assertDiagOnLine(t, diags, 2, "outside handler-bind")
}

func TestRethrowContext_Positive_InLet(t *testing.T) {
	source := "(let ((x 1))\n  (rethrow))"
	diags := lintCheck(t, AnalyzerRethrowContext, source)
	assert.Len(t, diags, 1)
	assertDiagOnLine(t, diags, 2, "outside handler-bind")
}

func TestRethrowContext_Negative_InHandlerBind(t *testing.T) {
	source := `(handler-bind ((condition (lambda (c &rest args) (rethrow)))) (error 'test "data"))`
	diags := lintCheck(t, AnalyzerRethrowContext, source)
	assertNoDiags(t, diags)
}

func TestRethrowContext_Negative_InNestedHandlerBind(t *testing.T) {
	source := `(handler-bind ((condition (lambda (c &rest args)
                 (handler-bind ((condition (lambda (c2 &rest a2)
                                  (rethrow))))
                   (rethrow)))))
  (error 'test "data"))`
	diags := lintCheck(t, AnalyzerRethrowContext, source)
	assertNoDiags(t, diags)
}

func TestRethrowContext_Negative_InHandlerBindBody(t *testing.T) {
	// The linter deliberately permits (rethrow) anywhere inside a
	// handler-bind form, including the body. At runtime, a body-level
	// rethrow would fail unless an outer handler-bind is actively handling
	// an error, but static analysis cannot determine that — so the linter
	// avoids false positives by treating the entire handler-bind form as
	// a valid context.
	source := `(handler-bind ((condition (lambda (c &rest args) "caught")))
  (progn (do-stuff) (rethrow)))`
	diags := lintCheck(t, AnalyzerRethrowContext, source)
	assertNoDiags(t, diags)
}

func TestRethrowContext_Positive_HasNotes(t *testing.T) {
	diags := lintCheck(t, AnalyzerRethrowContext, `(rethrow)`)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes)
	assert.Contains(t, diags[0].Notes[0], "handler-bind")
}

func TestRethrowContext_Nolint(t *testing.T) {
	source := "(rethrow) ; nolint:rethrow-context\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

// --- unnecessary-progn ---

func TestUnnecessaryProgn_Positive_Lambda(t *testing.T) {
	source := `(lambda (x) (progn (print x) (+ x 1)))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in lambda body")
}

func TestUnnecessaryProgn_Positive_Defun(t *testing.T) {
	source := `(defun foo (x) (progn (print x) (+ x 1)))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in defun body")
}

func TestUnnecessaryProgn_Positive_Let(t *testing.T) {
	source := `(let ((x 1)) (progn (print x) x))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in let body")
}

func TestUnnecessaryProgn_Positive_LetStar(t *testing.T) {
	source := `(let* ((x 1)) (progn (print x) x))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in let* body")
}

func TestUnnecessaryProgn_Positive_HandlerBind(t *testing.T) {
	source := `(handler-bind ((condition (lambda (c &rest a) "caught"))) (progn (do-a) (do-b)))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in handler-bind body")
}

func TestUnnecessaryProgn_Positive_IgnoreErrors(t *testing.T) {
	source := `(ignore-errors (progn (do-a) (do-b)))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in ignore-errors body")
}

func TestUnnecessaryProgn_Positive_Dotimes(t *testing.T) {
	source := `(dotimes (i 10) (progn (print i) (do-stuff)))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in dotimes body")
}

func TestUnnecessaryProgn_Positive_NestedProgn(t *testing.T) {
	source := `(progn (progn (do-a) (do-b)))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "nested progn is redundant")
}

func TestUnnecessaryProgn_Positive_CondClause(t *testing.T) {
	source := "(cond\n  ((= x 1) (progn (print x) x))\n  (else \"default\"))"
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in cond clause body")
}

func TestUnnecessaryProgn_Positive_Flet(t *testing.T) {
	source := `(flet ((helper (x) (+ x 1))) (progn (helper 1) (helper 2)))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in flet body")
}

func TestUnnecessaryProgn_Positive_Labels(t *testing.T) {
	source := `(labels ((helper (x) (+ x 1))) (progn (helper 1) (helper 2)))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in labels body")
}

func TestUnnecessaryProgn_Positive_Macrolet(t *testing.T) {
	source := `(macrolet ((helper (x) x)) (progn (helper 1) (helper 2)))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "progn is unnecessary in macrolet body")
}

// --- unnecessary-progn: negative cases ---

func TestUnnecessaryProgn_Negative_IfThenBranch(t *testing.T) {
	// progn IS needed in if branches
	source := `(if true (progn (print "yes") 1) 0)`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assertNoDiags(t, diags)
}

func TestUnnecessaryProgn_Negative_IfElseBranch(t *testing.T) {
	source := `(if true 1 (progn (print "no") 0))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assertNoDiags(t, diags)
}

func TestUnnecessaryProgn_Negative_Defmacro(t *testing.T) {
	// defmacro only takes a single body expression, so progn IS needed
	source := `(defmacro m (x) (progn (print x) x))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assertNoDiags(t, diags)
}

func TestUnnecessaryProgn_Negative_MultipleBodyExprs(t *testing.T) {
	// If there are multiple body expressions (not just a single progn), no warning
	source := `(defun foo (x) (print x) (+ x 1))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assertNoDiags(t, diags)
}

func TestUnnecessaryProgn_Negative_SingleNonProgn(t *testing.T) {
	// A single body expression that's not progn is fine
	source := `(defun foo (x) (+ x 1))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assertNoDiags(t, diags)
}

func TestUnnecessaryProgn_Negative_TopLevelProgn(t *testing.T) {
	// A top-level progn is fine (not nested)
	source := `(progn (do-a) (do-b))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assertNoDiags(t, diags)
}

func TestUnnecessaryProgn_Negative_CondClauseMultipleBody(t *testing.T) {
	// cond clause with multiple body expressions (no wrapping progn) is fine
	source := "(cond\n  ((= x 1) (print x) x)\n  (else \"default\"))"
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	assertNoDiags(t, diags)
}

func TestUnnecessaryProgn_HasNotes(t *testing.T) {
	source := `(defun foo (x) (progn (print x) x))`
	diags := lintCheck(t, AnalyzerUnnecessaryProgn, source)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes)
	assert.Contains(t, diags[0].Notes[0], "remove the progn")
}

func TestUnnecessaryProgn_Nolint(t *testing.T) {
	source := "(defun foo (x) (progn (print x) x)) ; nolint:unnecessary-progn\n"
	diags := lintSource(t, source)
	// Filter only unnecessary-progn diagnostics
	var prognDiags []Diagnostic
	for _, d := range diags {
		if d.Analyzer == "unnecessary-progn" {
			prognDiags = append(prognDiags, d)
		}
	}
	assert.Empty(t, prognDiags)
}

// --- nolint suppression ---

func TestNolint_SuppressAll(t *testing.T) {
	source := "(if true 1) ; nolint\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

func TestNolint_SuppressSpecific(t *testing.T) {
	source := "(if true 1) ; nolint:if-arity\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

func TestNolint_DifferentCheck(t *testing.T) {
	// Suppressing a different check should not suppress if-arity.
	// The nolint:set-usage is also unused (no set-usage finding on this line).
	source := "(if true 1) ; nolint:set-usage\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 2)
	assertHasDiag(t, diags, "too few")
	assertHasDiag(t, diags, "does not suppress any diagnostic")
}

func TestNolint_MultipleNames(t *testing.T) {
	// Suppressing a comma-separated list including the right check
	source := "(if true 1) ; nolint:set-usage,if-arity\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

func TestNolint_MultipleNames_NotMatched(t *testing.T) {
	// Comma-separated list that doesn't include the triggered check.
	// The nolint directive is also unused (no set-usage or let-bindings finding).
	source := "(if true 1) ; nolint:set-usage,let-bindings\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 2)
	assertHasDiag(t, diags, "too few")
	assertHasDiag(t, diags, "does not suppress any diagnostic")
}

func TestNolint_SuppressSetUsage(t *testing.T) {
	// nolint should work for set-usage findings, not just if-arity
	source := "(set 'x 1)\n(set 'x 2) ; nolint:set-usage\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

func TestNolint_MultipleNamesWithSpaces(t *testing.T) {
	// Spaces after commas in the check list should be handled
	source := "(if true 1) ; nolint:set-usage, if-arity\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

func TestNolint_SuppressAll_NoErrorOnLine(t *testing.T) {
	// A line with ; nolint that has no actual finding should warn about unused nolint
	source := "(+ 1 2) ; nolint\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "does not suppress any diagnostic")
	assert.Equal(t, "unused-nolint", diags[0].Analyzer)
}

func TestNolint_SuppressSpecific_NoErrorOnLine(t *testing.T) {
	// A line with ; nolint:check-name that has no matching finding should warn about unused nolint
	source := "(+ 1 2) ; nolint:if-arity\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "does not suppress any diagnostic")
	assert.Equal(t, "unused-nolint", diags[0].Analyzer)
}

// --- output formatting ---

func TestFormatText(t *testing.T) {
	diags := []Diagnostic{
		{Pos: Position{File: "test.lisp", Line: 10}, Message: "use set!", Analyzer: "set-usage"},
		{Pos: Position{File: "test.lisp", Line: 20}, Message: "bad if", Analyzer: "if-arity"},
	}
	var buf bytes.Buffer
	FormatText(&buf, diags)
	output := buf.String()
	assert.Contains(t, output, "test.lisp:10: use set! (set-usage)")
	assert.Contains(t, output, "test.lisp:20: bad if (if-arity)")
}

func TestFormatJSON(t *testing.T) {
	diags := []Diagnostic{
		{Pos: Position{File: "test.lisp", Line: 10}, Message: "use set!", Analyzer: "set-usage", Severity: SeverityWarning},
	}
	var buf bytes.Buffer
	err := FormatJSON(&buf, diags)
	require.NoError(t, err)

	// Verify round-trip: unmarshal and compare
	var parsed []Diagnostic
	err = json.Unmarshal(buf.Bytes(), &parsed)
	require.NoError(t, err)
	assert.Equal(t, diags, parsed)
}

// --- integration test: multiple checks ---

func TestIntegration_MultipleFindings(t *testing.T) {
	source := "(set 'x 1)\n(set 'x 2)\n(if true 1)\n(defun foo (x) (in-package \"bad\") x)\n(let (y 1) y)\n(cond true)"
	diags := lintSource(t, source)
	// Should find: set-usage, if-arity, in-package-toplevel, let-bindings, cond-structure
	assert.GreaterOrEqual(t, len(diags), 5)

	analyzers := make(map[string]bool)
	for _, d := range diags {
		analyzers[d.Analyzer] = true
	}
	assert.True(t, analyzers["set-usage"], "expected set-usage finding")
	assert.True(t, analyzers["if-arity"], "expected if-arity finding")
	assert.True(t, analyzers["in-package-toplevel"], "expected in-package-toplevel finding")
	assert.True(t, analyzers["let-bindings"], "expected let-bindings finding")
	assert.True(t, analyzers["cond-structure"], "expected cond-structure finding")

	// Verify findings are on correct lines
	assertDiagOnLine(t, diags, 2, "already bound")
	assertDiagOnLine(t, diags, 3, "too few")
	assertDiagOnLine(t, diags, 4, "top level")
}

func TestIntegration_CleanCode(t *testing.T) {
	source := "(in-package \"user\")\n\n(defun add (a b)\n  \"Add two numbers.\"\n  (+ a b))\n\n(defun classify (x)\n  (cond\n    ((< x 0) \"negative\")\n    ((= x 0) \"zero\")\n    (true \"positive\")))\n\n(let ((result (add 1 2)))\n  (if (= result 3)\n    \"correct\"\n    \"wrong\"))\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

func TestIntegration_SortedByLine(t *testing.T) {
	// Diagnostics should be sorted by file then line
	// Line 1: if-arity, Line 2: first set (ok), Line 3: set-usage (repeated), Line 4: builtin-arity
	source := "(if true 1)\n(set x 1)\n(set x 2)\n(car)"
	diags := lintSource(t, source)
	assert.GreaterOrEqual(t, len(diags), 3)
	for i := 1; i < len(diags); i++ {
		assert.LessOrEqual(t, diags[i-1].Pos.Line, diags[i].Pos.Line,
			"diagnostics should be sorted by line")
	}
}

// --- edge cases ---

func TestEmptyFile(t *testing.T) {
	diags := lintSource(t, "")
	assertNoDiags(t, diags)
}

func TestCommentOnly(t *testing.T) {
	diags := lintSource(t, "; just a comment\n")
	assertNoDiags(t, diags)
}

func TestParseError(t *testing.T) {
	l := &Linter{Analyzers: DefaultAnalyzers()}
	_, err := l.LintFile([]byte("(unclosed"), "test.lisp")
	assert.Error(t, err)
}

func TestQuotedFormsIgnored(t *testing.T) {
	// Quoted forms should not be checked (they're data, not code)
	diags := lintSource(t, `'(set x 42)`)
	assertNoDiags(t, diags)
}

func TestBracketListIgnored(t *testing.T) {
	// Bracket lists [set x 42] are quoted and should not be checked
	diags := lintSource(t, `[set x 42]`)
	assertNoDiags(t, diags)
}

func TestDefaultAnalyzers(t *testing.T) {
	analyzers := DefaultAnalyzers()
	assert.Len(t, analyzers, 16)
	names := AnalyzerNames()
	assert.Equal(t, []string{
		"builtin-arity",
		"cond-missing-else",
		"cond-structure",
		"defun-structure",
		"if-arity",
		"in-package-toplevel",
		"let-bindings",
		"quote-call",
		"rethrow-context",
		"set-usage",
		"shadowing",
		"undefined-symbol",
		"unnecessary-progn",
		"unused-function",
		"unused-variable",
		"user-arity",
	}, names)
}

func TestAnalyzerDoc(t *testing.T) {
	doc := AnalyzerDoc()
	assert.Contains(t, doc, "set-usage")
	assert.Contains(t, doc, "builtin-arity")
	assert.NotEmpty(t, doc)
}

// --- walk helpers ---

func TestHeadSymbol_Empty(t *testing.T) {
	v := &lisp.LVal{Type: lisp.LSExpr}
	assert.Equal(t, "", HeadSymbol(v))
}

func TestUserDefined(t *testing.T) {
	// defun and defmacro for builtin names — calls should not be flagged
	source := "(defun map (fn seq) ())\n(defmacro get (key) ())\n(map inc '(1 2))\n(get x)"
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestUserDefined_NestedDefun(t *testing.T) {
	// defun inside a let — should still be detected
	source := "(let ((x 1))\n  (defun map (fn seq) ())\n  (map inc '(1 2 3)))"
	diags := lintCheck(t, AnalyzerBuiltinArity, source)
	assertNoDiags(t, diags)
}

func TestHeadSymbol_NonSExpr(t *testing.T) {
	v := &lisp.LVal{Type: lisp.LInt}
	assert.Equal(t, "", HeadSymbol(v))
}

func TestHeadSymbol_NonSymbolHead(t *testing.T) {
	v := &lisp.LVal{
		Type:  lisp.LSExpr,
		Cells: []*lisp.LVal{{Type: lisp.LInt}},
	}
	assert.Equal(t, "", HeadSymbol(v))
}

func TestArgCount_Empty(t *testing.T) {
	v := &lisp.LVal{Type: lisp.LSExpr}
	assert.Equal(t, 0, ArgCount(v))
}

func TestArgCount_HeadOnly(t *testing.T) {
	v := &lisp.LVal{
		Type:  lisp.LSExpr,
		Cells: []*lisp.LVal{{Type: lisp.LSymbol, Str: "foo"}},
	}
	assert.Equal(t, 0, ArgCount(v))
}

func TestSourceOf_PreferOwnSource(t *testing.T) {
	v := &lisp.LVal{
		Source: &token.Location{File: "test.lisp", Line: 5},
		Cells: []*lisp.LVal{
			{Source: &token.Location{File: "test.lisp", Line: 10}},
		},
	}
	result := SourceOf(v)
	assert.Equal(t, 5, result.Source.Line)
}

func TestSourceOf_FallbackToChild(t *testing.T) {
	v := &lisp.LVal{
		Source: nil,
		Cells: []*lisp.LVal{
			{Source: &token.Location{File: "test.lisp", Line: 10}},
		},
	}
	result := SourceOf(v)
	assert.Equal(t, 10, result.Source.Line)
}

func TestSourceOf_FallbackToSelf(t *testing.T) {
	// No source, no children — falls back to self
	v := &lisp.LVal{}
	result := SourceOf(v)
	assert.Same(t, v, result)
}

func TestSourceOf_FallbackZeroLine(t *testing.T) {
	// Source exists but line is 0 — should fall back to child
	v := &lisp.LVal{
		Source: &token.Location{File: "test.lisp", Line: 0},
		Cells: []*lisp.LVal{
			{Source: &token.Location{File: "test.lisp", Line: 7}},
		},
	}
	result := SourceOf(v)
	assert.Equal(t, 7, result.Source.Line)
}

// --- bindingSource fallback ---

func TestBindingSource_UsesBinding(t *testing.T) {
	binding := &lisp.LVal{Source: &token.Location{File: "a.lisp", Line: 3}}
	fallback := &lisp.LVal{Source: &token.Location{File: "b.lisp", Line: 1}}
	loc := bindingSource(binding, fallback)
	assert.Equal(t, 3, loc.Line)
	assert.Equal(t, "a.lisp", loc.File)
}

func TestBindingSource_FallbackNilSource(t *testing.T) {
	binding := &lisp.LVal{Source: nil}
	fallback := &lisp.LVal{Source: &token.Location{File: "b.lisp", Line: 1}}
	loc := bindingSource(binding, fallback)
	assert.Equal(t, 1, loc.Line)
	assert.Equal(t, "b.lisp", loc.File)
}

func TestBindingSource_FallbackZeroLine(t *testing.T) {
	binding := &lisp.LVal{Source: &token.Location{File: "a.lisp", Line: 0}}
	fallback := &lisp.LVal{Source: &token.Location{File: "b.lisp", Line: 5}}
	loc := bindingSource(binding, fallback)
	assert.Equal(t, 5, loc.Line)
	assert.Equal(t, "b.lisp", loc.File)
}

// --- buildArityTable ---

// --- LintFile edge cases ---

func TestLintFile_FileNameBackfill(t *testing.T) {
	// A custom analyzer that reports a diagnostic without setting Pos.File
	noFileAnalyzer := &Analyzer{
		Name: "no-file",
		Doc:  "Reports without file info.",
		Run: func(pass *Pass) error {
			if len(pass.Exprs) > 0 {
				pass.Report(Diagnostic{Message: "custom finding"})
			}
			return nil
		},
	}
	l := &Linter{Analyzers: []*Analyzer{noFileAnalyzer}}
	diags, err := l.LintFile([]byte("(+ 1 2)"), "backfill.lisp")
	require.NoError(t, err)
	require.Len(t, diags, 1)
	assert.Equal(t, "backfill.lisp", diags[0].Pos.File, "LintFile should backfill missing file")
}

func TestReportf_NilSource(t *testing.T) {
	// Reportf with nil source should produce a zero-position diagnostic
	pass := &Pass{
		Analyzer: &Analyzer{Name: "test"},
	}
	pass.Reportf(nil, "no location")
	require.Len(t, pass.diagnostics, 1)
	assert.Equal(t, Position{}, pass.diagnostics[0].Pos)
	assert.Equal(t, "no location", pass.diagnostics[0].Message)
}

// --- nolint edge cases ---

func TestNolint_RegularCommentDoesNotSuppress(t *testing.T) {
	// A regular trailing comment (non-nolint) should not suppress diagnostics
	source := "(if true 1) ; this is a regular comment\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "too few")
}

func TestNolint_LeadingCommentDoesNotSuppress(t *testing.T) {
	// nolint in a leading comment is on a different line than the expression,
	// so it does not suppress the if-arity finding. The nolint itself is unused.
	source := "; nolint\n(if true 1)\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 2)
	assertDiagOnLine(t, diags, 1, "does not suppress any diagnostic")
	assertDiagOnLine(t, diags, 2, "too few")
}

// --- buildArityTable ---

func TestBuildArityTable_HasEntries(t *testing.T) {
	table := buildArityTable()
	// Verify exact arity for known builtins
	assert.Equal(t, aritySpec{min: 1, max: 1}, table["car"])
	assert.Equal(t, aritySpec{min: 2, max: 2}, table["cons"])
	assert.Equal(t, aritySpec{min: 1, max: 1}, table["not"])
	// Variadic function should have max == -1
	plus, hasPlus := table["+"]
	assert.True(t, hasPlus, "arity table should include +")
	assert.Equal(t, -1, plus.max, "+ should be variadic (max == -1)")
}

// --- Notes ---

func TestDiagnostic_String_WithNotes(t *testing.T) {
	d := Diagnostic{
		Pos:      Position{File: "test.lisp", Line: 10},
		Message:  "use set!",
		Analyzer: "set-usage",
		Notes:    []string{"set creates a new binding; set! mutates an existing one"},
	}
	s := d.String()
	assert.Contains(t, s, "use set!")
	assert.Contains(t, s, "= note: set creates a new binding")
}

func TestSetUsage_HasNotes(t *testing.T) {
	source := "(set 'x 1)\n(set 'x 2)"
	diags := lintCheck(t, AnalyzerSetUsage, source)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes, "set-usage should provide hint notes")
}

func TestIfArity_TooFew_HasNotes(t *testing.T) {
	diags := lintCheck(t, AnalyzerIfArity, `(if true 1)`)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes, "if-arity should provide hint notes")
}

func TestBuiltinArity_HasNotes(t *testing.T) {
	diags := lintCheck(t, AnalyzerBuiltinArity, `(car)`)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes, "builtin-arity should provide hint notes")
	assert.Contains(t, diags[0].Notes[0], "help")
}

func TestFormatJSON_WithNotes(t *testing.T) {
	diags := []Diagnostic{
		{
			Pos:      Position{File: "test.lisp", Line: 1},
			Message:  "test msg",
			Analyzer: "test",
			Notes:    []string{"hint 1", "hint 2"},
		},
	}
	var buf bytes.Buffer
	err := FormatJSON(&buf, diags)
	require.NoError(t, err)
	assert.Contains(t, buf.String(), "hint 1")
	assert.Contains(t, buf.String(), "hint 2")
}

// --- severity ---

func TestSeverity_String(t *testing.T) {
	assert.Equal(t, "error", SeverityError.String())
	assert.Equal(t, "warning", SeverityWarning.String())
	assert.Equal(t, "info", SeverityInfo.String())
	assert.Equal(t, "unknown", Severity(0).String())  // severityUnset zero value
	assert.Equal(t, "unknown", Severity(99).String()) // out of range
}

func TestSeverity_AnalyzerDefaults(t *testing.T) {
	// Table-driven: verify each analyzer has the expected severity.
	expected := map[string]Severity{
		"set-usage":          SeverityWarning,
		"in-package-toplevel": SeverityWarning,
		"if-arity":           SeverityError,
		"let-bindings":       SeverityError,
		"defun-structure":    SeverityError,
		"cond-structure":     SeverityError,
		"builtin-arity":      SeverityError,
		"quote-call":         SeverityWarning,
		"cond-missing-else":  SeverityInfo,
		"rethrow-context":    SeverityError,
		"unnecessary-progn":  SeverityInfo,
		"undefined-symbol":   SeverityError,
		"unused-variable":    SeverityWarning,
		"unused-function":    SeverityWarning,
		"shadowing":          SeverityInfo,
		"user-arity":         SeverityError,
	}
	for _, a := range DefaultAnalyzers() {
		want, ok := expected[a.Name]
		if !ok {
			t.Errorf("analyzer %q has no expected severity in test", a.Name)
			continue
		}
		assert.Equal(t, want, a.Severity, "analyzer %s severity", a.Name)
	}
}

func TestSeverity_PropagatedToDiagnostic(t *testing.T) {
	// Verify that Report() sets the severity from the analyzer default.
	diags := lintCheck(t, AnalyzerIfArity, `(if true 1)`)
	require.Len(t, diags, 1)
	assert.Equal(t, SeverityError, diags[0].Severity)
}

func TestSeverity_PropagatedWarning(t *testing.T) {
	source := "(set 'x 1)\n(set 'x 2)"
	diags := lintCheck(t, AnalyzerSetUsage, source)
	require.Len(t, diags, 1)
	assert.Equal(t, SeverityWarning, diags[0].Severity)
}

func TestSeverity_PropagatedInfo(t *testing.T) {
	source := "(cond\n  ((= x 1) \"one\")\n  ((= x 2) \"two\"))"
	diags := lintCheck(t, AnalyzerCondMissingElse, source)
	require.Len(t, diags, 1)
	assert.Equal(t, SeverityInfo, diags[0].Severity)
}

func TestSeverity_PropagatedViaReportf(t *testing.T) {
	// Reportf delegates to Report which propagates the analyzer's default severity.
	pass := &Pass{
		Analyzer: &Analyzer{Name: "test-reportf", Severity: SeverityError},
	}
	pass.Reportf(&token.Location{File: "test.lisp", Line: 1, Col: 1}, "test %s", "msg")
	require.Len(t, pass.diagnostics, 1)
	assert.Equal(t, SeverityError, pass.diagnostics[0].Severity)
	assert.Equal(t, "test msg", pass.diagnostics[0].Message)
}

func TestSeverity_JSONRoundTrip(t *testing.T) {
	diags := []Diagnostic{
		{Pos: Position{File: "a.lisp", Line: 1}, Message: "err", Analyzer: "test", Severity: SeverityError},
		{Pos: Position{File: "a.lisp", Line: 2}, Message: "warn", Analyzer: "test", Severity: SeverityWarning},
		{Pos: Position{File: "a.lisp", Line: 3}, Message: "info", Analyzer: "test", Severity: SeverityInfo},
	}
	var buf bytes.Buffer
	err := FormatJSON(&buf, diags)
	require.NoError(t, err)

	var parsed []Diagnostic
	err = json.Unmarshal(buf.Bytes(), &parsed)
	require.NoError(t, err)
	assert.Equal(t, diags, parsed)

	// Verify the JSON contains string severity values
	output := buf.String()
	assert.Contains(t, output, `"error"`)
	assert.Contains(t, output, `"warning"`)
	assert.Contains(t, output, `"info"`)
}

func TestFormatText_WithNotes(t *testing.T) {
	diags := []Diagnostic{
		{Pos: Position{File: "test.lisp", Line: 10}, Message: "use set!", Analyzer: "set-usage", Notes: []string{"hint text"}},
	}
	var buf bytes.Buffer
	FormatText(&buf, diags)
	output := buf.String()
	assert.Contains(t, output, "use set!")
	assert.Contains(t, output, "= note: hint text")
}

// --- semantic analysis helpers ---

// lintCheckSemantic runs a single analyzer with semantic analysis enabled.
func lintCheckSemantic(t *testing.T, analyzer *Analyzer, source string) []Diagnostic {
	t.Helper()
	l := &Linter{Analyzers: []*Analyzer{analyzer}}
	diags, err := l.LintFileWithAnalysis([]byte(source), "test.lisp", nil)
	require.NoError(t, err)
	return diags
}

// --- LintFileWithContext ---

func TestLintFileWithContext_NilSemantics(t *testing.T) {
	// With nil semantics, semantic analyzers should be no-ops
	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol}}
	diags, err := l.LintFileWithContext([]byte(`(foo 1 2)`), "test.lisp", nil)
	require.NoError(t, err)
	assertNoDiags(t, diags)
}

// --- LintFileWithAnalysis ---

func TestLintFileWithAnalysis_Basic(t *testing.T) {
	l := &Linter{Analyzers: DefaultAnalyzers()}
	diags, err := l.LintFileWithAnalysis([]byte(`(+ 1 2)`), "test.lisp", nil)
	require.NoError(t, err)
	assertNoDiags(t, diags)
}

func TestLintFileWithAnalysis_NilConfig(t *testing.T) {
	l := &Linter{Analyzers: DefaultAnalyzers()}
	diags, err := l.LintFileWithAnalysis([]byte(`(+ 1 2)`), "test.lisp", nil)
	require.NoError(t, err)
	assertNoDiags(t, diags)
}

func TestLintFileWithAnalysis_ExternalSymbols(t *testing.T) {
	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol}}
	cfg := &analysis.Config{
		ExtraGlobals: []analysis.ExternalSymbol{
			{Name: "external-fn", Kind: analysis.SymFunction},
		},
	}
	diags, err := l.LintFileWithAnalysis([]byte(`(external-fn 1 2)`), "test.lisp", cfg)
	require.NoError(t, err)
	assertNoDiags(t, diags)
}

// --- undefined-symbol ---

func TestUndefinedSymbol_Positive_UnknownFunction(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(unknown-fn 1 2)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "undefined symbol: unknown-fn")
}

func TestUndefinedSymbol_Positive_UnknownVariable(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(+ x 1)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "undefined symbol: x")
}

func TestUndefinedSymbol_Negative_Builtin(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(+ 1 2)`)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_Negative_Defun(t *testing.T) {
	source := "(defun foo (x) (+ x 1))\n(foo 42)"
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, source)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_Negative_ForwardRef(t *testing.T) {
	source := "(defun bar () (foo))\n(defun foo () 42)"
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, source)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_Negative_Keyword(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(list :key :value)`)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_Negative_Qualified(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(math:floor 1.5)`)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_Negative_SetDefines(t *testing.T) {
	source := "(set 'x 42)\n(+ x 1)"
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, source)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_Negative_Let(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(let ((x 1)) (+ x 1))`)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_Negative_Lambda(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(lambda (x) (+ x 1))`)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_Negative_TrueFalse(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(if true 1 2)`)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_Negative_NoSemantics(t *testing.T) {
	// Without semantic analysis, should be a no-op
	diags := lintCheck(t, AnalyzerUndefinedSymbol, `(unknown-fn 1 2)`)
	assertNoDiags(t, diags)
}

func TestUndefinedSymbol_HasNotes(t *testing.T) {
	diags := lintCheckSemantic(t, AnalyzerUndefinedSymbol, `(unknown-fn)`)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes)
}

// --- unused-variable ---

func TestUnusedVariable_Positive_LetBinding(t *testing.T) {
	source := `(let ((x 1)) (+ 1 2))`
	diags := lintCheckSemantic(t, AnalyzerUnusedVariable, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "unused variable: x")
}

func TestUnusedVariable_Positive_Parameter(t *testing.T) {
	source := `(defun foo (x y) (+ y 1))`
	diags := lintCheckSemantic(t, AnalyzerUnusedVariable, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "unused parameter: x")
}

func TestUnusedVariable_Negative_Used(t *testing.T) {
	source := `(let ((x 1)) (+ x 1))`
	diags := lintCheckSemantic(t, AnalyzerUnusedVariable, source)
	assertNoDiags(t, diags)
}

func TestUnusedVariable_Negative_UnderscorePrefix(t *testing.T) {
	source := `(let ((_x 1)) (+ 1 2))`
	diags := lintCheckSemantic(t, AnalyzerUnusedVariable, source)
	assertNoDiags(t, diags)
}

func TestUnusedVariable_Negative_GlobalScope(t *testing.T) {
	// Top-level set bindings should not be flagged
	source := `(set 'x 42)`
	diags := lintCheckSemantic(t, AnalyzerUnusedVariable, source)
	assertNoDiags(t, diags)
}

func TestUnusedVariable_Negative_NoSemantics(t *testing.T) {
	diags := lintCheck(t, AnalyzerUnusedVariable, `(let ((x 1)) (+ 1 2))`)
	assertNoDiags(t, diags)
}

func TestUnusedVariable_HasNotes(t *testing.T) {
	source := `(let ((x 1)) (+ 1 2))`
	diags := lintCheckSemantic(t, AnalyzerUnusedVariable, source)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes)
	assert.Contains(t, diags[0].Notes[0], "_")
}

// --- unused-function ---

func TestUnusedFunction_Positive_NeverCalled(t *testing.T) {
	source := `(defun helper () 42)`
	diags := lintCheckSemantic(t, AnalyzerUnusedFunction, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "unused function: helper")
}

func TestUnusedFunction_Positive_UnusedMacro(t *testing.T) {
	source := `(defmacro my-macro (x) x)`
	diags := lintCheckSemantic(t, AnalyzerUnusedFunction, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "unused macro: my-macro")
}

func TestUnusedFunction_Negative_Called(t *testing.T) {
	source := "(defun helper () 42)\n(helper)"
	diags := lintCheckSemantic(t, AnalyzerUnusedFunction, source)
	assertNoDiags(t, diags)
}

func TestUnusedFunction_Negative_Exported(t *testing.T) {
	source := "(defun helper () 42)\n(export 'helper)"
	diags := lintCheckSemantic(t, AnalyzerUnusedFunction, source)
	assertNoDiags(t, diags)
}

func TestUnusedFunction_Negative_UnderscorePrefix(t *testing.T) {
	source := `(defun _internal () 42)`
	diags := lintCheckSemantic(t, AnalyzerUnusedFunction, source)
	assertNoDiags(t, diags)
}

func TestUnusedFunction_Negative_NoSemantics(t *testing.T) {
	diags := lintCheck(t, AnalyzerUnusedFunction, `(defun helper () 42)`)
	assertNoDiags(t, diags)
}

func TestUnusedFunction_HasNotes(t *testing.T) {
	source := `(defun helper () 42)`
	diags := lintCheckSemantic(t, AnalyzerUnusedFunction, source)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes)
	assert.Contains(t, diags[0].Notes[0], "export")
}

// --- shadowing ---

func TestShadowing_Positive_ParamShadowsBuiltin(t *testing.T) {
	source := `(defun foo (car) (+ car 1))`
	diags := lintCheckSemantic(t, AnalyzerShadowing, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "shadows")
	assertHasDiag(t, diags, "car")
}

func TestShadowing_Positive_LetShadowsParam(t *testing.T) {
	source := `(defun foo (x) (let ((x 2)) (+ x 1)))`
	diags := lintCheckSemantic(t, AnalyzerShadowing, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "shadows")
	assertHasDiag(t, diags, "x")
}

func TestShadowing_Negative_NoShadow(t *testing.T) {
	source := `(defun foo (x) (let ((y 2)) (+ x y)))`
	diags := lintCheckSemantic(t, AnalyzerShadowing, source)
	assertNoDiags(t, diags)
}

func TestShadowing_Negative_TopLevel(t *testing.T) {
	// Top-level defun redefining a builtin is OK (not shadowing)
	source := `(defun map (f l) (cons (f (car l)) ()))`
	diags := lintCheckSemantic(t, AnalyzerShadowing, source)
	assertNoDiags(t, diags)
}

func TestShadowing_Negative_NoSemantics(t *testing.T) {
	diags := lintCheck(t, AnalyzerShadowing, `(defun foo (car) car)`)
	assertNoDiags(t, diags)
}

func TestShadowing_HasNotes(t *testing.T) {
	source := `(defun foo (car) car)`
	diags := lintCheckSemantic(t, AnalyzerShadowing, source)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes)
	assert.Contains(t, diags[0].Notes[0], "rename")
}

func TestShadowing_Negative_ExternalSymbol(t *testing.T) {
	// Parameters should not trigger shadowing when the outer symbol
	// is from an external source (workspace scan / package import).
	source := `(defun foo (x) (+ x 1))`
	l := &Linter{Analyzers: []*Analyzer{AnalyzerShadowing}}
	cfg := &analysis.Config{
		ExtraGlobals: []analysis.ExternalSymbol{
			{Name: "x", Kind: analysis.SymVariable},
		},
	}
	diags, err := l.LintFileWithAnalysis([]byte(source), "test.lisp", cfg)
	require.NoError(t, err)
	assertNoDiags(t, diags)
}

// --- user-arity ---

func TestUserArity_Positive_TooFew(t *testing.T) {
	source := "(defun add (a b) (+ a b))\n(add 1)"
	diags := lintCheckSemantic(t, AnalyzerUserArity, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "requires at least 2")
}

func TestUserArity_Positive_TooMany(t *testing.T) {
	source := "(defun add (a b) (+ a b))\n(add 1 2 3)"
	diags := lintCheckSemantic(t, AnalyzerUserArity, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "accepts at most 2")
}

func TestUserArity_Negative_Correct(t *testing.T) {
	source := "(defun add (a b) (+ a b))\n(add 1 2)"
	diags := lintCheckSemantic(t, AnalyzerUserArity, source)
	assertNoDiags(t, diags)
}

func TestUserArity_Negative_Variadic(t *testing.T) {
	source := "(defun my-list (&rest items) items)\n(my-list 1 2 3 4 5)"
	diags := lintCheckSemantic(t, AnalyzerUserArity, source)
	assertNoDiags(t, diags)
}

func TestUserArity_Negative_Optional(t *testing.T) {
	source := "(defun greet (name &optional greeting) name)\n(greet \"Alice\")"
	diags := lintCheckSemantic(t, AnalyzerUserArity, source)
	assertNoDiags(t, diags)
}

func TestUserArity_Negative_Builtin(t *testing.T) {
	// Builtins should not be checked by user-arity (no Source)
	diags := lintCheckSemantic(t, AnalyzerUserArity, `(car)`)
	assertNoDiags(t, diags)
}

func TestUserArity_Negative_NoSemantics(t *testing.T) {
	source := "(defun add (a b) (+ a b))\n(add 1)"
	diags := lintCheck(t, AnalyzerUserArity, source)
	assertNoDiags(t, diags)
}

func TestUserArity_Negative_ThreadFirst(t *testing.T) {
	// thread-first inserts an extra arg — should not flag arity mismatch
	source := "(defun get-val (a b) (+ a b))\n(thread-first x (get-val 1))"
	diags := lintCheckSemantic(t, AnalyzerUserArity, source)
	assertNoDiags(t, diags)
}

func TestUserArity_Negative_ThreadLast(t *testing.T) {
	source := "(defun get-val (a b) (+ a b))\n(thread-last x (get-val 1))"
	diags := lintCheckSemantic(t, AnalyzerUserArity, source)
	assertNoDiags(t, diags)
}

func TestUserArity_HasNotes(t *testing.T) {
	source := "(defun add (a b) (+ a b))\n(add 1)"
	diags := lintCheckSemantic(t, AnalyzerUserArity, source)
	require.Len(t, diags, 1)
	assert.NotEmpty(t, diags[0].Notes)
	assert.Contains(t, diags[0].Notes[0], "defined at")
}

func TestUserArity_Negative_ExternalSymbol(t *testing.T) {
	// An external symbol (imported from workspace/package) that shadows a
	// local function name should not cause user-arity checks. This prevents
	// false positives when e.g. transient:get (1 arg) shadows builtin get (2 args).
	source := "(get rep \"key\")"
	l := &Linter{Analyzers: []*Analyzer{AnalyzerUserArity}}
	cfg := &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"transient": {
				{Name: "get", Kind: analysis.SymFunction, Package: "transient",
					Signature: &analysis.Signature{
						Params: []lisp.ParamInfo{{Name: "key", Kind: lisp.ParamRequired}},
					}},
			},
		},
	}
	// Simulate use-package to import the external symbol
	source = "(use-package 'transient)\n" + source
	diags, err := l.LintFileWithAnalysis([]byte(source), "test.lisp", cfg)
	require.NoError(t, err)
	assertNoDiags(t, diags)
}

// --- unused-nolint ---

func TestUnusedNolint_Unused(t *testing.T) {
	// nolint:set-usage on a line with no set-usage finding
	source := "(+ 1 2) ; nolint:set-usage\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "does not suppress any diagnostic")
	assert.Equal(t, "unused-nolint", diags[0].Analyzer)
	assert.Equal(t, SeverityWarning, diags[0].Severity)
}

func TestUnusedNolint_Used(t *testing.T) {
	// nolint:set-usage that actually suppresses a finding — no warning
	source := "(set 'x 1)\n(set 'x 2) ; nolint:set-usage\n"
	diags := lintSource(t, source)
	// Should have no diagnostics: set-usage is suppressed, nolint is used
	assertNoDiags(t, diags)
}

func TestUnusedNolint_UnknownAnalyzer(t *testing.T) {
	// nolint references a non-existent analyzer
	source := "(+ 1 2) ; nolint:nonexistent\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "unknown analyzer")
	assertHasDiag(t, diags, "nonexistent")
	assert.Equal(t, "unused-nolint", diags[0].Analyzer)
}

func TestUnusedNolint_SuppressAllUnused(t *testing.T) {
	// bare ; nolint on a clean line
	source := "(+ 1 2) ; nolint\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "does not suppress any diagnostic")
	assert.Equal(t, "unused-nolint", diags[0].Analyzer)
}

func TestUnusedNolint_SuppressAllUsed(t *testing.T) {
	// bare ; nolint that actually suppresses something — no warning
	source := "(if true 1) ; nolint\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

func TestUnusedNolint_SelfSuppression(t *testing.T) {
	// ; nolint:unused-nolint should suppress itself
	source := "(+ 1 2) ; nolint:unused-nolint\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

func TestUnusedNolint_HasNotes(t *testing.T) {
	source := "(+ 1 2) ; nolint:set-usage\n"
	diags := lintSource(t, source)
	require.Len(t, diags, 1)
	require.Len(t, diags[0].Notes, 2)
	assert.Contains(t, diags[0].Notes[0], "remove")
	assert.Contains(t, diags[0].Notes[1], "unused-nolint")
}

func TestUnusedNolint_MixedKnownUnknown(t *testing.T) {
	// Directive with a real analyzer that suppresses nothing + an unknown name
	source := "(+ 1 2) ; nolint:set-usage,bogus-check\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 1)
	// Should mention unknown because bogus-check doesn't exist
	assertHasDiag(t, diags, "unknown analyzer")
	assertHasDiag(t, diags, "bogus-check")
	// Known analyzer should NOT appear in the unknown message
	assert.NotContains(t, diags[0].Message, "set-usage")
}

func TestUnusedNolint_PartiallyUsed(t *testing.T) {
	// Directive suppresses if-arity but also lists a non-existent check.
	// The directive IS used (it suppresses if-arity), so no warning.
	// This is a deliberate design choice: when a directive successfully
	// suppresses at least one diagnostic, we don't warn about unused names
	// in the same directive. This avoids noise when an analyzer is only
	// sometimes triggered (e.g., a nolint that covers two checks but only
	// one fires on any given run).
	source := "(if true 1) ; nolint:if-arity,bogus-check\n"
	diags := lintSource(t, source)
	assertNoDiags(t, diags)
}

func TestUnusedNolint_MalformedDirective(t *testing.T) {
	// "; nolint-if-arity" is NOT a valid nolint directive (missing colon separator).
	// It should be ignored, so the if-arity finding is not suppressed.
	source := "(if true 1) ; nolint-if-arity\n"
	diags := lintSource(t, source)
	assertHasDiag(t, diags, "too few")
}

func TestUnusedNolint_Position(t *testing.T) {
	// Verify the reported position points to the nolint comment, not the expression
	source := "(+ 1 2) ; nolint:set-usage\n"
	diags := lintSource(t, source)
	require.Len(t, diags, 1)
	assert.Equal(t, 1, diags[0].Pos.Line)
	assert.Greater(t, diags[0].Pos.Col, 0, "should point to the nolint comment, not column 0")
}

func TestUnusedNolint_MultipleDirectives_MixedUsage(t *testing.T) {
	// Used nolint on line 1, unused nolint on line 2
	source := "(if true 1) ; nolint:if-arity\n(+ 1 2) ; nolint:set-usage\n"
	diags := lintSource(t, source)
	assert.Len(t, diags, 1)
	assert.Equal(t, 2, diags[0].Pos.Line) // only the unused one
	assert.Equal(t, "unused-nolint", diags[0].Analyzer)
}

// --- LintFiles ---

// writeTempLisp creates a .lisp file in dir and returns its path.
func writeTempLisp(t *testing.T, dir, name, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	err := os.WriteFile(path, []byte(content), 0o600)
	require.NoError(t, err)
	return path
}

func TestLintFiles_NilConfig_SyntacticOnly(t *testing.T) {
	dir := t.TempDir()
	f := writeTempLisp(t, dir, "test.lisp", "(if true 1)")

	l := &Linter{Analyzers: DefaultAnalyzers()}
	diags, err := l.LintFiles(nil, []string{f})
	require.NoError(t, err)
	// if-arity should fire (syntactic check)
	assertHasDiag(t, diags, "if")
}

func TestLintFiles_EmptyWorkspace_SyntacticOnly(t *testing.T) {
	dir := t.TempDir()
	f := writeTempLisp(t, dir, "test.lisp", "(if true 1)")

	l := &Linter{Analyzers: DefaultAnalyzers()}
	// Empty workspace string means no semantic analysis
	diags, err := l.LintFiles(&LintConfig{}, []string{f})
	require.NoError(t, err)
	assertHasDiag(t, diags, "if")
}

func TestLintFiles_WithWorkspace_SemanticChecks(t *testing.T) {
	dir := t.TempDir()
	// File A defines and exports a function
	writeTempLisp(t, dir, "a.lisp", `(in-package 'mylib)
(defun helper (x) (+ x 1))
(export 'helper)`)
	// File B uses the function from mylib
	b := writeTempLisp(t, dir, "b.lisp", `(use-package 'mylib)
(helper 42)`)

	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol}}
	diags, err := l.LintFiles(&LintConfig{Workspace: dir}, []string{b})
	require.NoError(t, err)
	// helper should resolve via workspace scanning — no undefined-symbol
	assertNoDiags(t, diags)
}

func TestLintFiles_Registry_ResolvesEmbedderSymbols(t *testing.T) {
	dir := t.TempDir()
	// File calls a function that only exists in the embedder registry
	f := writeTempLisp(t, dir, "test.lisp", `(use-package 'embedder)
(embedder-fn 1 2 3)`)

	// Build a registry with the embedder package
	env := lisp.NewEnv(nil)
	lisp.InitializeUserEnv(env)
	pkg := env.Runtime.Registry.DefinePackage("embedder")
	pkg.Symbols["embedder-fn"] = lisp.Fun("embedder-fn",
		lisp.Formals("a", "b", "c"),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			return lisp.Nil()
		})
	pkg.Externals = append(pkg.Externals, "embedder-fn")

	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol}}
	diags, err := l.LintFiles(&LintConfig{
		Workspace: dir,
		Registry:  env.Runtime.Registry,
	}, []string{f})
	require.NoError(t, err)
	// embedder-fn should resolve via registry — no undefined-symbol
	assertNoDiags(t, diags)
}

func TestLintFiles_Registry_WithoutWorkspace_NoSemanticAnalysis(t *testing.T) {
	dir := t.TempDir()
	f := writeTempLisp(t, dir, "test.lisp", `(unknown-fn 1 2)`)

	env := lisp.NewEnv(nil)
	lisp.InitializeUserEnv(env)

	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol}}
	// Registry is set but workspace is empty — semantic analysis is disabled
	diags, err := l.LintFiles(&LintConfig{
		Registry: env.Runtime.Registry,
	}, []string{f})
	require.NoError(t, err)
	// No semantic analysis → undefined-symbol is a no-op
	assertNoDiags(t, diags)
}

func TestLintFiles_MultipleFiles(t *testing.T) {
	dir := t.TempDir()
	f1 := writeTempLisp(t, dir, "a.lisp", "(if true 1)")
	f2 := writeTempLisp(t, dir, "b.lisp", "(if true 1)")

	l := &Linter{Analyzers: []*Analyzer{AnalyzerIfArity}}
	diags, err := l.LintFiles(nil, []string{f1, f2})
	require.NoError(t, err)
	assert.Len(t, diags, 2, "should report one diagnostic per file")
}

func TestLintFiles_FileNotFound(t *testing.T) {
	l := &Linter{Analyzers: DefaultAnalyzers()}
	_, err := l.LintFiles(nil, []string{"/nonexistent/file.lisp"})
	require.Error(t, err)
}

func TestBuildAnalysisConfig_Basic(t *testing.T) {
	dir := t.TempDir()
	writeTempLisp(t, dir, "lib.lisp", `(defun my-fn () 42)
(export 'my-fn)`)

	cfg, err := BuildAnalysisConfig(&LintConfig{Workspace: dir})
	require.NoError(t, err)
	require.NotNil(t, cfg)

	// Should have workspace globals
	assert.NotEmpty(t, cfg.ExtraGlobals)

	// Should have stdlib package exports
	assert.NotNil(t, cfg.PackageExports)
}

func TestBuildAnalysisConfig_WithRegistry(t *testing.T) {
	dir := t.TempDir()
	writeTempLisp(t, dir, "lib.lisp", `(defun my-fn () 42)
(export 'my-fn)`)

	env := lisp.NewEnv(nil)
	lisp.InitializeUserEnv(env)
	ccPkg := env.Runtime.Registry.DefinePackage("cc")
	ccPkg.Symbols["storage-put"] = lisp.Fun("storage-put",
		lisp.Formals("key", "value"),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			return lisp.Nil()
		})
	ccPkg.Externals = append(ccPkg.Externals, "storage-put")

	cfg, err := BuildAnalysisConfig(&LintConfig{
		Workspace: dir,
		Registry:  env.Runtime.Registry,
	})
	require.NoError(t, err)

	// cc package should appear in PackageExports from the registry
	ccSyms, ok := cfg.PackageExports["cc"]
	require.True(t, ok, "cc package should be in PackageExports")
	var names []string
	for _, s := range ccSyms {
		names = append(names, s.Name)
	}
	assert.Contains(t, names, "storage-put")
}

func TestBuildAnalysisConfig_StdlibExportsOverride(t *testing.T) {
	dir := t.TempDir()
	writeTempLisp(t, dir, "lib.lisp", "(+ 1 2)")

	custom := map[string][]analysis.ExternalSymbol{
		"custom-pkg": {
			{Name: "custom-fn", Kind: analysis.SymFunction},
		},
	}

	cfg, err := BuildAnalysisConfig(&LintConfig{
		Workspace:     dir,
		StdlibExports: custom,
	})
	require.NoError(t, err)

	// Should use custom exports, not load stdlib
	_, ok := cfg.PackageExports["custom-pkg"]
	assert.True(t, ok, "custom-pkg should be in PackageExports")
}
