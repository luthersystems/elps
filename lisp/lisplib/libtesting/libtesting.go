// Copyright © 2018 The ELPS authors

package libtesting

import (
	"fmt"
	"sync"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "testing"

const DefaultSuiteSymbol = "test-suite"

// CoreForms are the test definition forms.  They are core lisp forms (issue
// #736): a test body is code written in the caller's package, like a defun
// body, so the forms that wrap one in a lambda belong to package lisp, where
// they act in the caller's package.  LoadPackage re-exports each under
// package testing as the very same function value, so (use-package 'testing)
// and testing:test keep working and mean exactly lisp:test.
var CoreForms = []string{"test", "benchmark", "test-let", "test-let*", "benchmark-simple"}

// LoadPackage adds the testing package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	prevPkg := env.Runtime.Package.Name
	defer env.InPackage(lisp.Symbol(prevPkg))
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	env.SetPackageDoc(`Test framework: define named tests and benchmarks with assertion
		helpers (assert-equal, assert-nil, assert-not-nil, etc.).`)
	suite := NewTestSuite()
	//elpsvet:allow-native the per-VM test registry: publication rejects this mutable suite outright (TestSuite's doc; TestLoadLibraryTestingRegistryRejectsTemplate in lisp/lisplib/lisplib_test.go asserts the error), which is why LoadRuntimeLibrary omits this package and each VM loads its own
	env.PutGlobal(lisp.Symbol(DefaultSuiteSymbol), lisp.Native(suite))
	if e := reexportCoreForms(env); !e.IsNil() {
		return e
	}
	for _, fn := range assertMacros(suite) {
		env.AddMacros(true, fn)
	}
	return lisp.Nil()
}

// reexportCoreForms binds each of CoreForms in the current package (testing)
// to lisp's own function value and exports it.  The value keeps package lisp,
// so testing:test is not a special operator defined outside lisp; it is
// lisp:test under a second name.  A lisp package that lacks a form (an
// embedding that assembled lisp by hand) gets lisp's default definition
// registered here instead, which behaves the same.
func reexportCoreForms(env *lisp.LEnv) *lisp.LVal {
	lang := env.Runtime.Registry.Package(env.Runtime.Registry.Lang)
	for _, form := range CoreForms {
		if lang != nil {
			if v := lang.Get(lisp.Symbol(form)); v.Type == lisp.LFun {
				if e := env.PutGlobal(lisp.Symbol(form), v); e.IsError() {
					return e
				}
				env.Runtime.Package.Exports(form)
				continue
			}
		}
		def := coreDef(form)
		if def == nil {
			return env.Errorf("lisp defines no %s form", form)
		}
		if isCoreSpecialOp(form) {
			env.AddSpecialOps(true, def)
		} else {
			env.AddMacros(true, def)
		}
	}
	return lisp.Nil()
}

// coreDef returns lisp's default definition of a test form, or nil.
func coreDef(name string) lisp.LBuiltinDef {
	for _, def := range lisp.DefaultSpecialOps() {
		if def.Name() == name {
			return def
		}
	}
	for _, def := range lisp.DefaultMacros() {
		if def.Name() == name {
			return def
		}
	}
	return nil
}

func isCoreSpecialOp(name string) bool {
	for _, def := range lisp.DefaultSpecialOps() {
		if def.Name() == name {
			return true
		}
	}
	return false
}

// coreDefs returns lisp's default definitions of names, in order, skipping
// any lisp does not define.
func coreDefs(names ...string) []lisp.LBuiltinDef {
	defs := make([]lisp.LBuiltinDef, 0, len(names))
	for _, name := range names {
		if def := coreDef(name); def != nil {
			defs = append(defs, def)
		}
	}
	return defs
}

// TestSuite is an ordered set of named tests.
//
// A TestSuite's own bookkeeping is safe for concurrent use, so one suite may
// be registered into, and read from, any number of environments at once.
// LoadPackage gives every environment its own suite, so the ordinary path never
// shares one, but NewTestSuite and EnvTestSuite are exported with no stated
// scope: an embedder that installs one suite into several runtimes is doing
// something the API permits. Evaluating a `test` or `benchmark` form writes the
// suite's maps, and two goroutines writing one Go map is `fatal error:
// concurrent map writes`, which is thrown by the runtime and cannot be
// recovered. The mutex below is what keeps that from being reachable. See
// issue #420.
//
// What this does NOT promise is anything about running a registered test. A
// Test's Fun is a lambda closed over the environment that defined it, so
// evaluating it from a different runtime is environment sharing, which is a
// separate question from whether the suite is a safe container.
//
// Template publication rejects suites, even empty ones. Template-based runners
// load this package and register their tests separately in each VM.
type TestSuite struct {
	tests      map[string]*Test
	benchmarks map[string]*Test
	torder     []string
	border     []string

	// mu guards every field above it. Add and AddBenchmark run once per
	// definition form, at load time, never inside an assertion or inside a
	// running test body, so the lock is not on any hot path. It sits last
	// rather than first because fieldalignment is enforced across lisp/...
	// and a pointer-free mutex ahead of the maps would push the struct's
	// pointer bytes from 48 to 72.
	mu sync.RWMutex
}

// TransientNative marks *TestSuite as never saved by a durable dump: a
// suite holds the tests of one test run and its VM.
func (*TestSuite) TransientNative() {}

// A within-VM copy receives independent registry bookkeeping. Template
// publication rejects this mutable native; load testing separately in each VM.
var _ lisp.NativeCloner = (*TestSuite)(nil)

// NewTestSuite returns an empty suite. The result may be installed into any
// number of environments and used from all of them concurrently.
func NewTestSuite() *TestSuite {
	return &TestSuite{
		tests:      make(map[string]*Test),
		benchmarks: make(map[string]*Test),
	}
}

// CloneNative copies registry bookkeeping for within-VM value copies. Test
// functions are retained by pointer and still belong to their original VM.
// This does not transfer a populated suite across runtimes: Template rejects
// suites, and each VM must register and execute its own tests.
func (s *TestSuite) CloneNative() any {
	s.mu.RLock()
	defer s.mu.RUnlock()
	cp := NewTestSuite()
	cp.torder = make([]string, len(s.torder))
	copy(cp.torder, s.torder)
	cp.border = make([]string, len(s.border))
	copy(cp.border, s.border)
	for name, t := range s.tests {
		cp.tests[name] = t
	}
	for name, b := range s.benchmarks {
		cp.benchmarks[name] = b
	}
	return cp
}

func (s *TestSuite) Add(t *Test) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.tests[t.Name] != nil {
		return fmt.Errorf("test with the same name already defined: %v", t.Name)
	}
	s.torder = append(s.torder, t.Name)
	s.tests[t.Name] = t
	return nil
}

func (s *TestSuite) Len() int {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return len(s.torder)
}

func (s *TestSuite) Tests() []string {
	s.mu.RLock()
	defer s.mu.RUnlock()
	names := make([]string, len(s.torder))
	copy(names, s.torder)
	return names
}

func (s *TestSuite) Benchmarks() []string {
	s.mu.RLock()
	defer s.mu.RUnlock()
	names := make([]string, len(s.border))
	copy(names, s.border)
	return names
}

func (s *TestSuite) Test(i int) *Test {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.tests[s.torder[i]]
}

func (s *TestSuite) AddBenchmark(b *Test) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.benchmarks[b.Name] != nil {
		return fmt.Errorf("benchmark with the same name already defined: %v", b.Name)
	}
	s.border = append(s.border, b.Name)
	s.benchmarks[b.Name] = b
	return nil
}

func (s *TestSuite) Benchmark(i int) *Test {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.benchmarks[s.border[i]]
}

// DefineTest registers fun, a function of no arguments, as the test named
// name.  It is how lisp:test registers into the suite installed as
// testing:test-suite.
func (s *TestSuite) DefineTest(name string, fun *lisp.LVal) error {
	return s.Add(&Test{Name: name, Fun: fun})
}

// DefineBenchmark registers fun, a function of one argument (the iteration
// count), as the benchmark named name.  It is how lisp:benchmark registers
// into the suite installed as testing:test-suite.
func (s *TestSuite) DefineBenchmark(name string, fun *lisp.LVal) error {
	return s.AddBenchmark(&Test{Name: name, Fun: fun})
}

// Macros returns the testing package's macros for an embedder that assembles
// the package by hand: lisp's test-let, test-let* and benchmark-simple
// (see CoreForms), then the assert macros.  Register them with AddMacros
// next to the suite binding and Ops.  LoadPackage itself re-exports lisp's
// forms by value rather than registering them anew.
func (s *TestSuite) Macros() []lisp.LBuiltinDef {
	defs := coreDefs("test-let", "test-let*", "benchmark-simple")
	for _, fn := range assertMacros(s) {
		defs = append(defs, fn)
	}
	return defs
}

// assertMacros are the macros package testing defines itself.  They only
// build forms: their expansions are evaluated in the caller's package.
func assertMacros(s *TestSuite) []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.FunctionDoc("assert=", lisp.Formals("expect", "num"), s.MacroAssertNumEq,
			`Asserts that two expressions evaluate to numerically equal
			values. Both expect and num must evaluate to numbers (int or
			float). Reports the expected and actual values on failure.`),
		libutil.FunctionDoc("assert-string=", lisp.Formals("expect", "str"), s.MacroAssertStringEq,
			`Asserts that two expressions evaluate to equal strings.
			Both expect and str must evaluate to string values. Reports
			the expected and actual values on failure.`),
		libutil.FunctionDoc("assert-equal", lisp.Formals("expect", "expression"), s.MacroAssertEqual,
			`Asserts that two expressions are structurally equal using
			equal?. Works with any value types. Reports the expected
			and actual values on failure.`),
		libutil.FunctionDoc("assert-nil", lisp.Formals("expression"), s.MacroAssertNil,
			`Asserts that expression evaluates to nil. Reports the
			actual value on failure.`),
		libutil.FunctionDoc("assert-not-nil", lisp.Formals("expression"), s.MacroAssertNotNil,
			`Asserts that expression does not evaluate to nil. Reports
			the expression on failure.`),
		libutil.FunctionDoc("assert-not", lisp.Formals("expression"), s.MacroAssertNot,
			`Asserts that expression evaluates to a falsey value (nil or
			false). Reports the actual value on failure.`),
	}
}

// Ops returns lisp's test and benchmark special operators, for an embedder
// that assembles the testing package by hand.  They resolve
// testing:test-suite in the calling environment; embedders must install that
// binding as well as the ops.
//
// Deprecated: test and benchmark are core lisp forms since issue #736, and
// every environment with package lisp already has them.  Hand assembly needs
// only the suite binding and the assert macros; LoadPackage re-exports
// lisp's forms under package testing instead of registering these.
func (s *TestSuite) Ops() []lisp.LBuiltinDef {
	return coreDefs("test", "benchmark")
}

// MacroTestLet expands test-let.
//
// Deprecated: test-let is lisp's since issue #736.
func (s *TestSuite) MacroTestLet(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return coreDef("test-let").Eval(env, args)
}

// MacroTestLetSeq expands test-let*.
//
// Deprecated: test-let* is lisp's since issue #736.
func (s *TestSuite) MacroTestLetSeq(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return coreDef("test-let*").Eval(env, args)
}

// MacroBenchmarkSimple expands benchmark-simple.
//
// Deprecated: benchmark-simple is lisp's since issue #736.
func (s *TestSuite) MacroBenchmarkSimple(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return coreDef("benchmark-simple").Eval(env, args)
}

// OpTest is lisp:test.
//
// Deprecated: test is lisp's since issue #736.
func (s *TestSuite) OpTest(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return coreDef("test").Eval(env, args)
}

// OpBenchmark is lisp:benchmark.
//
// Deprecated: benchmark is lisp's since issue #736.
func (s *TestSuite) OpBenchmark(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return coreDef("benchmark").Eval(env, args)
}

// The assert macros' expansions.  Each binds the evaluated expressions to
// temporaries and asserts on them; the rendered source text of an expression
// is passed as a string argument, so a failure reports what the caller wrote.
var (
	assertStringEqForm = elpsutil.MustTemplate(`
(let (((unquote expect-sym) (unquote expect)) ((unquote s-sym) (unquote s)))
  (assert (lisp:string? (unquote expect-sym))
          "expression did not evaluate to a string\n\texpression: {}\n\t    result: {}"
          (unquote expect-text) (unquote expect-sym))
  (assert (lisp:string? (unquote s-sym))
          "expression did not evaluate to a string\n\texpression: {}\n\t    result: {}"
          (unquote s-text) (unquote s-sym))
  (assert (lisp:string= (unquote expect-sym) (unquote s-sym))
          "the string expressions are not equal\n\texpression: {}\n\t    result: {}\n\t  expected: {}"
          (unquote s-text) (unquote s-sym) (unquote expect-sym)))`,
		"expect-sym", "expect", "s-sym", "s", "expect-text", "s-text")

	assertNumEqForm = elpsutil.MustTemplate(`
(let (((unquote expect-sym) (unquote expect)) ((unquote n-sym) (unquote n)))
  (assert (lisp:number? (unquote expect-sym))
          "expression did not evaluate to a number\n\texpression: {}\n\t    result: {}"
          (unquote expect-text) (unquote expect-sym))
  (assert (lisp:number? (unquote n-sym))
          "expression did not evaluate to a number\n\texpression: {}\n\t    result: {}"
          (unquote n-text) (unquote n-sym))
  (assert (lisp:= (unquote expect-sym) (unquote n-sym))
          "the numeric expressions are not equal\n\texpression: {}\n\t    result: {}\n\t  expected: {}"
          (unquote n-text) (unquote n-sym) (unquote expect-sym)))`,
		"expect-sym", "expect", "n-sym", "n", "expect-text", "n-text")

	assertEqualForm = elpsutil.MustTemplate(`
(let (((unquote expect-sym) (unquote expect)) ((unquote expr-sym) (unquote expr)))
  (assert (lisp:equal? (unquote expect-sym) (unquote expr-sym))
          "the expressions are not `+"``"+`equal?''\n\texpression: {}\n\t    result: {}\n\t  expected: {}"
          (unquote expr-text) (unquote expr-sym) (unquote expect-sym)))`,
		"expect-sym", "expect", "expr-sym", "expr", "expr-text")

	assertNilForm = elpsutil.MustTemplate(`
(let (((unquote expr-sym) (unquote expr)))
  (assert (lisp:nil? (unquote expr-sym))
          "the expressions is not nil\n\texpression: {}\n\t    result: {}"
          (unquote expr-text) (unquote expr-sym)))`,
		"expr-sym", "expr", "expr-text")

	assertNotNilForm = elpsutil.MustTemplate(`
(let (((unquote expr-sym) (unquote expr)))
  (assert (not (lisp:nil? (unquote expr-sym)))
          "the expressions is nil\n\texpression: {}"
          (unquote expr-text)))`,
		"expr-sym", "expr", "expr-text")

	assertNotForm = elpsutil.MustTemplate(`
(let (((unquote expr-sym) (unquote expr)))
  (assert (not (unquote expr-sym))
          "the expressions is not falsey\n\texpression: {}\n\t    result: {}"
          (unquote expr-text) (unquote expr-sym)))`,
		"expr-sym", "expr", "expr-text")
)

func (s *TestSuite) MacroAssertStringEq(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	expectExpr, sExpr := args.Cells[0], args.Cells[1]
	expectSym, sSym := env.GenSym(), env.GenSym()
	return assertStringEqForm.Expand(expectSym, expectExpr, sSym, sExpr,
		lisp.String(env.Render(expectExpr)), lisp.String(env.Render(sExpr)))
}

func (s *TestSuite) MacroAssertNumEq(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	expectExpr, nExpr := args.Cells[0], args.Cells[1]
	expectSym, nSym := env.GenSym(), env.GenSym()
	return assertNumEqForm.Expand(expectSym, expectExpr, nSym, nExpr,
		lisp.String(env.Render(expectExpr)), lisp.String(env.Render(nExpr)))
}

func (s *TestSuite) MacroAssertEqual(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	expectExpr, exprExpr := args.Cells[0], args.Cells[1]
	expectSym, exprSym := env.GenSym(), env.GenSym()
	return assertEqualForm.Expand(expectSym, expectExpr, exprSym, exprExpr, lisp.String(env.Render(exprExpr)))
}

func (s *TestSuite) MacroAssertNil(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	exprExpr := args.Cells[0]
	return assertNilForm.Expand(env.GenSym(), exprExpr, lisp.String(env.Render(exprExpr)))
}

func (s *TestSuite) MacroAssertNotNil(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	exprExpr := args.Cells[0]
	return assertNotNilForm.Expand(env.GenSym(), exprExpr, lisp.String(env.Render(exprExpr)))
}

func (s *TestSuite) MacroAssertNot(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	exprExpr := args.Cells[0]
	return assertNotForm.Expand(env.GenSym(), exprExpr, lisp.String(env.Render(exprExpr)))
}

type Test struct {
	Fun  *lisp.LVal
	Name string
}

// EnvTestSuite returns the suite installed in env, or nil if there is none.
// The suite is returned by pointer and is safe to use while other environments
// holding the same suite are evaluating.
//
// "None" includes a runtime that never loaded this package at all.  The map
// lookup is nil-checked because Registry.Package returns a nil *Package for
// an unregistered name and Package.Get dereferences pkg.symbols, so using it unchecked
// turned "does this runtime have a suite?" -- the question the nil return
// advertises -- into a nil pointer dereference in the host.  See issue #425.
func EnvTestSuite(env *lisp.LEnv) *TestSuite {
	pkg := env.Runtime.Registry.Package(DefaultPackageName)
	if pkg == nil {
		return nil
	}
	suite, _ := lisp.NativeValue[*TestSuite](pkg.Get(lisp.Symbol(DefaultSuiteSymbol)))
	return suite
}
