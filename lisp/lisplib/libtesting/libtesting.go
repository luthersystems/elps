// Copyright © 2018 The ELPS authors

package libtesting

import (
	"fmt"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "testing"

const DefaultSuiteSymbol = "test-suite"

// LoadPackage adds the time package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
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

var builtins = []*libutil.Builtin{}

// TestSuite is an ordered set of named tests.
type TestSuite struct {
	tests map[string]*Test
	order []string
}

func NewTestSuite() *TestSuite {
	return &TestSuite{
		tests: make(map[string]*Test),
	}
}

func (s *TestSuite) Add(t *Test) error {
	if s.tests[t.Name] != nil {
		return fmt.Errorf("test with the same name already defined: %v", t.Name)
	}
	s.order = append(s.order, t.Name)
	s.tests[t.Name] = t
	return nil
}

func (s *TestSuite) Len() int {
	return len(s.order)
}

func (s *TestSuite) Test(i int) *Test {
	return s.tests[s.order[i]]
}

func (s *TestSuite) Macros() []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.Function("test-let", lisp.Formals("name", "bindings", lisp.VarArgSymbol, "expers"), s.MacroTestLet),
		libutil.Function("test-let*", lisp.Formals("name", "bindings", lisp.VarArgSymbol, "expers"), s.MacroTestLetSeq),
		libutil.Function("assert=", lisp.Formals("expect", "num"), s.MacroAssertNumEq),
		libutil.Function("assert-string=", lisp.Formals("expect", "str"), s.MacroAssertStringEq),
		libutil.Function("assert-equal", lisp.Formals("expect", "expression"), s.MacroAssertEqual),
		libutil.Function("assert-nil", lisp.Formals("expression"), s.MacroAssertNil),
		libutil.Function("assert-not-nil", lisp.Formals("expression"), s.MacroAssertNotNil),
		libutil.Function("assert-not", lisp.Formals("expression"), s.MacroAssertNot),
	}
}

func (s *TestSuite) Ops() []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.Function("test", lisp.Formals("name", lisp.VarArgSymbol, "exprs"), s.OpTest),
	}
}

func (s *TestSuite) MacroTestLet(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return s.macroTestLet(env, args, "let")
}

func (s *TestSuite) MacroTestLetSeq(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return s.macroTestLet(env, args, "let*")
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

type Test struct {
	Name string
	Fun  *lisp.LVal
}

func EnvTestSuite(env *lisp.LEnv) *TestSuite {
	lsuite := env.Runtime.Registry.Packages[DefaultPackageName].Get(lisp.Symbol(DefaultSuiteSymbol))
	if lsuite.Type != lisp.LNative {
		return nil
	}
	suite, _ := lsuite.Native.(*TestSuite)
	return suite
}

func list(v ...*lisp.LVal) *lisp.LVal {
	return lisp.SExpr(v)
}
