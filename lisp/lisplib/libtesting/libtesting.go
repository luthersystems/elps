// Copyright © 2018 The ELPS authors

package libtesting

import (
	"fmt"
	"sync"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "testing"

const DefaultSuiteSymbol = "test-suite"

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
	env.PutGlobal(lisp.Symbol(DefaultSuiteSymbol), lisp.Native(suite))
	for _, fn := range suite.Ops() {
		env.AddSpecialOps(true, fn)
	}
	for _, fn := range suite.Macros() {
		env.AddMacros(true, fn)
	}
	return lisp.Nil()
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

// NewTestSuite returns an empty suite. The result may be installed into any
// number of environments and used from all of them concurrently.
func NewTestSuite() *TestSuite {
	return &TestSuite{
		tests:      make(map[string]*Test),
		benchmarks: make(map[string]*Test),
	}
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

func (s *TestSuite) Macros() []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.FunctionDoc("test-let", lisp.Formals("name", "bindings", lisp.VarArgSymbol, "exprs"), s.MacroTestLet,
			`Defines a named test with local let bindings. Expands to
			(test name (let (bindings) exprs...)). The bindings use
			parallel binding (let) semantics.`),
		libutil.FunctionDoc("test-let*", lisp.Formals("name", "bindings", lisp.VarArgSymbol, "exprs"), s.MacroTestLetSeq,
			`Defines a named test with local let* bindings. Like test-let
			but uses sequential binding (let*) semantics, so later
			bindings can reference earlier ones.`),
		libutil.FunctionDoc("benchmark-simple", lisp.Formals("name", lisp.VarArgSymbol, "exprs"), s.MacroBenchmarkSimple,
			`Defines a simple benchmark that runs exprs repeatedly.
			Expands to (benchmark name (count) (dotimes (_ count) exprs...)).
			The iteration count is provided by the benchmark harness.`),
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

func (s *TestSuite) Ops() []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.FunctionDoc("test", lisp.Formals("name", lisp.VarArgSymbol, "exprs"), s.OpTest,
			`Defines a named test case. name must be a string. The body
			expressions are wrapped in a lambda and registered with the
			test suite for later execution. Use assert macros inside
			the body to check conditions.`),
		libutil.FunctionDoc("benchmark", lisp.Formals("name", "args", lisp.VarArgSymbol, "exprs"), s.OpBenchmark,
			`Defines a named benchmark. name must be a string. args is a
			list containing a single symbol that receives the iteration
			count. The body should use dotimes or similar to run the
			benchmarked code count times. Prefer benchmark-simple for
			simple cases.`),
	}
}

func (s *TestSuite) MacroTestLet(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return s.macroTestLet(env, args, "let")
}

func (s *TestSuite) MacroTestLetSeq(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return s.macroTestLet(env, args, "let*")
}

func (s *TestSuite) MacroBenchmarkSimple(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name := args.Cells[0]
	exprs := args.Cells[1:]

	countsym := env.GenSym()
	body := list(
		lisp.Symbol("lisp:dotimes"),
		list(lisp.Symbol("_"), countsym),
	)
	body.Cells = append(body.Cells, exprs...) //elps:mutates body is freshly built by list() above; the helper hides its freshness from the intraprocedural rule
	return list(
		lisp.Symbol("benchmark"),
		name,
		list(countsym),
		body,
	)
}

func (s *TestSuite) MacroAssertStringEq(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	expectExpr, sExpr := args.Cells[0], args.Cells[1]
	expectSym, sSym := env.GenSym(), env.GenSym()
	return list(
		lisp.Symbol("let"),
		list(
			list(expectSym, expectExpr),
			list(sSym, sExpr),
		),
		list(
			lisp.Symbol("assert"),
			list(lisp.Symbol("lisp:string?"), expectSym),
			lisp.String("expression did not evaluate to a string\n\texpression: {}\n\t    result: {}"),
			lisp.String(expectExpr.String()),
			expectSym,
		),
		list(
			lisp.Symbol("assert"),
			list(lisp.Symbol("lisp:string?"), sSym),
			lisp.String("expression did not evaluate to a string\n\texpression: {}\n\t    result: {}"),
			lisp.String(sExpr.String()),
			sSym,
		),
		list(
			lisp.Symbol("assert"),
			list(lisp.Symbol("lisp:string="), expectSym, sSym),
			lisp.String("the string expressions are not equal\n\texpression: {}\n\t    result: {}\n\t  expected: {}"),
			lisp.String(sExpr.String()),
			sSym,
			expectSym,
		),
	)
}

func (s *TestSuite) MacroAssertNumEq(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	expectExpr, nExpr := args.Cells[0], args.Cells[1]
	expectSym, nSym := env.GenSym(), env.GenSym()
	return list(
		lisp.Symbol("let"),
		list(
			list(expectSym, expectExpr),
			list(nSym, nExpr),
		),
		list(
			lisp.Symbol("assert"),
			list(lisp.Symbol("lisp:number?"), expectSym),
			lisp.String("expression did not evaluate to a number\n\texpression: {}\n\t    result: {}"),
			lisp.String(expectExpr.String()),
			expectSym,
		),
		list(
			lisp.Symbol("assert"),
			list(lisp.Symbol("lisp:number?"), nSym),
			lisp.String("expression did not evaluate to a number\n\texpression: {}\n\t    result: {}"),
			lisp.String(nExpr.String()),
			nSym,
		),
		list(
			lisp.Symbol("assert"),
			list(lisp.Symbol("lisp:="), expectSym, nSym),
			lisp.String("the numeric expressions are not equal\n\texpression: {}\n\t    result: {}\n\t  expected: {}"),
			lisp.String(nExpr.String()),
			nSym,
			expectSym,
		),
	)
}

func (s *TestSuite) MacroAssertEqual(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	expectExpr, exprExpr := args.Cells[0], args.Cells[1]
	expectSym, exprSym := env.GenSym(), env.GenSym()
	return list(
		lisp.Symbol("let"),
		list(
			list(expectSym, expectExpr),
			list(exprSym, exprExpr),
		),
		list(
			lisp.Symbol("assert"),
			list(lisp.Symbol("lisp:equal?"), expectSym, exprSym),
			lisp.String("the expressions are not ``equal?''\n\texpression: {}\n\t    result: {}\n\t  expected: {}"),
			lisp.String(exprExpr.String()),
			exprSym,
			expectSym,
		),
	)
}

func (s *TestSuite) MacroAssertNil(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	exprExpr := args.Cells[0]
	exprSym := env.GenSym()
	return list(
		lisp.Symbol("let"),
		list(
			list(exprSym, exprExpr),
		),
		list(
			lisp.Symbol("assert"),
			list(lisp.Symbol("lisp:nil?"), exprSym),
			lisp.String("the expressions is not nil\n\texpression: {}\n\t    result: {}"),
			lisp.String(exprExpr.String()),
			exprSym,
		),
	)
}

func (s *TestSuite) MacroAssertNotNil(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	exprExpr := args.Cells[0]
	exprSym := env.GenSym()
	return list(
		lisp.Symbol("let"),
		list(
			list(exprSym, exprExpr),
		),
		list(
			lisp.Symbol("assert"),
			list(
				lisp.Symbol("not"),
				list(lisp.Symbol("lisp:nil?"), exprSym),
			),
			lisp.String("the expressions is nil\n\texpression: {}"),
			lisp.String(exprExpr.String()),
		),
	)
}

func (s *TestSuite) MacroAssertNot(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	exprExpr := args.Cells[0]
	exprSym := env.GenSym()
	return list(
		lisp.Symbol("let"),
		list(
			list(exprSym, exprExpr),
		),
		list(
			lisp.Symbol("assert"),
			list(lisp.Symbol("not"), exprSym),
			lisp.String("the expressions is not falsey\n\texpression: {}\n\t    result: {}"),
			lisp.String(exprExpr.String()),
			exprSym,
		),
	)
}

func (s *TestSuite) macroTestLet(env *lisp.LEnv, args *lisp.LVal, let string) *lisp.LVal {
	name, binds, exprs := args.Cells[0], args.Cells[1], args.Cells[2:]
	_, _, _ = name, binds, exprs
	if name.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", name.Type)
	}
	if binds.Type != lisp.LSExpr {
		return env.Errorf("second argument is not a list: %v", binds.Type)
	}
	for _, v := range binds.Cells {
		if v.Type != lisp.LSExpr {
			return env.Errorf("second argument is not a list of pairs: %v", v.Type)
		}
		if v.Len() != 2 {
			return env.Errorf("second argument is not a list of pairs: length %d", v.Len())
		}
	}
	letCells := make([]*lisp.LVal, 0, 2+len(exprs))
	letCells = append(letCells, lisp.Symbol(env.Runtime.Registry.Lang+":"+let), binds)
	letCells = append(letCells, exprs...)
	letExpr := lisp.SExpr(letCells)
	return lisp.SExpr([]*lisp.LVal{
		lisp.Symbol(DefaultPackageName + ":test"),
		name,
		letExpr,
	})
}

func (s *TestSuite) OpTest(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name, exprs := args.Cells[0], args.Cells[1:]
	if name.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", name.Type)
	}
	fun := env.Lambda(lisp.Nil(), exprs)
	test := &Test{
		Name: name.Str,
		Fun:  fun,
	}
	err := s.Add(test)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

func (s *TestSuite) OpBenchmark(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name := args.Cells[0]
	bargs := args.Cells[1]
	exprs := args.Cells[2:]
	if name.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", name.Type)
	}
	if bargs.Type != lisp.LSExpr {
		return env.Errorf("second argument is not a list: %v", bargs.Type)
	}
	for _, barg := range bargs.Cells {
		if barg.Type != lisp.LSymbol {
			return env.Errorf("second argument is not a list of symbols: %v", barg.Type)
		}
	}
	if bargs.Len() != 1 {
		return env.Errorf("benchmark doesn't take one argument: %v", bargs.Len())
	}
	fun := env.Lambda(bargs, exprs)
	test := &Test{
		Name: name.Str,
		Fun:  fun,
	}
	err := s.AddBenchmark(test)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
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
	lsuite := pkg.Get(lisp.Symbol(DefaultSuiteSymbol))
	if lsuite.Type != lisp.LNative {
		return nil
	}
	suite, _ := lsuite.Native.(*TestSuite)
	return suite
}

func list(v ...*lisp.LVal) *lisp.LVal {
	return lisp.SExpr(v)
}
