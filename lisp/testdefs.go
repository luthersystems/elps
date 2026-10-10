// Copyright © 2026 The ELPS authors

package lisp

// The test definition forms -- test, benchmark, test-let, test-let* and
// benchmark-simple -- are core (issue #736).  A test body is code written in
// the caller's package, exactly like a defun body, and a name resolves in the
// package of the code doing the lookup: core forms act in your package, while
// a builtin defined in package testing runs in package testing.  So the forms
// that wrap a body in a lambda belong here, next to lambda and defun.
//
// They need somewhere to put the test.  The test harness (lisp/lisplib/
// libtesting, loaded only by test runners) binds a per-VM registry as
// testing:test-suite; these forms find it through the registry by name, so a
// fork registers into its own VM's suite.  Outside a test run there is no such
// binding and they signal "no test suite".
//
// None of this costs anything to code that never runs a test: the forms are
// entries in the lisp tables, and the registry is looked up only when one is
// evaluated.

// testSuitePackage and testSuiteSymbol name the binding the test harness
// installs: libtesting.DefaultPackageName and libtesting.DefaultSuiteSymbol.
// libtesting's tests pin that the two agree.
const (
	testSuitePackage = "testing"
	testSuiteSymbol  = "test-suite"
)

// testRegistry is what the test forms register into.  libtesting's
// *TestSuite implements it.
type testRegistry interface {
	DefineTest(name string, fun *LVal) error
	DefineBenchmark(name string, fun *LVal) error
}

// envTestRegistry returns the registry installed in env's runtime, or an
// error naming why there is none.  op is the form asking, for the message.
func envTestRegistry(env *LEnv, op string) (testRegistry, *LVal) {
	pkg := env.Runtime.Registry.Package(testSuitePackage)
	if pkg == nil {
		return nil, env.Errorf("no test suite: %s can only be used while running tests", op)
	}
	v := pkg.Get(Symbol(testSuiteSymbol))
	if v.Type == LNative {
		if reg, ok := v.Native.(testRegistry); ok {
			return reg, nil
		}
	}
	return nil, env.Errorf("no test suite: %s:%s is not installed in the calling VM", testSuitePackage, testSuiteSymbol)
}

func opTest(env *LEnv, args *LVal) *LVal {
	name, exprs := args.Cells[0], args.Cells[1:]
	if name.Type != LString {
		return env.Errorf("first argument is not a string: %v", name.Type)
	}
	// Register into the suite the CALLING environment holds.  The lambda
	// below closes over env, so the two have to agree or a fork files its
	// own test, closed over its own environment, in the template's registry.
	reg, lerr := envTestRegistry(env, "test")
	if lerr != nil {
		return lerr
	}
	fun := env.Lambda(Nil(), exprs)
	if err := reg.DefineTest(name.Str, fun); err != nil {
		return env.Error(err)
	}
	return Nil()
}

func opBenchmark(env *LEnv, args *LVal) *LVal {
	name := args.Cells[0]
	bargs := args.Cells[1]
	exprs := args.Cells[2:]
	if name.Type != LString {
		return env.Errorf("first argument is not a string: %v", name.Type)
	}
	if bargs.Type != LSExpr {
		return env.Errorf("second argument is not a list: %v", bargs.Type)
	}
	for _, barg := range bargs.Cells {
		if barg.Type != LSymbol {
			return env.Errorf("second argument is not a list of symbols: %v", barg.Type)
		}
	}
	if bargs.Len() != 1 {
		return env.Errorf("benchmark doesn't take one argument: %v", bargs.Len())
	}
	// See opTest: the benchmark belongs to the calling environment's suite.
	reg, lerr := envTestRegistry(env, "benchmark")
	if lerr != nil {
		return lerr
	}
	fun := env.Lambda(bargs, exprs)
	// A Lambda error (a benchmark parameter named true, say) is registered
	// as the test function and reported when the test runs, as it was when
	// these forms lived in package testing.
	if err := reg.DefineBenchmark(name.Str, fun); err != nil {
		return env.Error(err)
	}
	return Nil()
}

func macroTestLet(env *LEnv, args *LVal) *LVal {
	return testLetExpansion(env, args, "let")
}

func macroTestLetSeq(env *LEnv, args *LVal) *LVal {
	return testLetExpansion(env, args, "let*")
}

// testLetExpansion expands (test-let name bindings exprs...) to
// (lisp:test name (lisp:let bindings exprs...)), with let* for test-let*.
func testLetExpansion(env *LEnv, args *LVal, let string) *LVal {
	name, binds, exprs := args.Cells[0], args.Cells[1], args.Cells[2:]
	if name.Type != LString {
		return env.Errorf("first argument is not a string: %v", name.Type)
	}
	if binds.Type != LSExpr {
		return env.Errorf("second argument is not a list: %v", binds.Type)
	}
	for _, v := range binds.Cells {
		if v.Type != LSExpr {
			return env.Errorf("second argument is not a list of pairs: %v", v.Type)
		}
		if v.Len() != 2 {
			return env.Errorf("second argument is not a list of pairs: length %d", v.Len())
		}
	}
	lang := env.Runtime.Registry.Lang
	letCells := make([]*LVal, 0, 2+len(exprs))
	letCells = append(letCells, Symbol(lang+":"+let), binds)
	letCells = append(letCells, exprs...)
	return Cells{Symbol(lang + ":test"), name, SExpr(letCells)}.SExpr()
}

// macroBenchmarkSimple expands (benchmark-simple name exprs...) to
// (lisp:benchmark name (n) (lisp:dotimes (_ n) exprs...)) with n a fresh
// symbol.
func macroBenchmarkSimple(env *LEnv, args *LVal) *LVal {
	name := args.Cells[0]
	exprs := args.Cells[1:]
	lang := env.Runtime.Registry.Lang
	countsym := env.GenSym()
	body := make([]*LVal, 0, 2+len(exprs))
	body = append(body, Symbol(lang+":dotimes"), Cells{Symbol("_"), countsym}.SExpr())
	body = append(body, exprs...)
	return Cells{
		Symbol(lang + ":benchmark"),
		name,
		Cells{countsym}.SExpr(),
		SExpr(body),
	}.SExpr()
}
