// Copyright © 2018 The ELPS authors

package elpstest

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/parser"
)

func BenchmarkParse(path string, r func() lisp.Reader) func(*testing.B) {
	return func(b *testing.B) {
		buf, err := os.ReadFile(path) //#nosec G304
		if err != nil {
			b.Fatalf("Unable to read source file %v: %v", path, err)
		}
		b.SetBytes(int64(len(buf)))
		for i := 0; i < b.N; i++ {
			_, err := r().Read("test", bytes.NewReader(buf))
			if err != nil {
				b.Fatalf("Parse failure: %v", err)
			}
		}
	}
}

// Runner is a test runner.
type Runner struct {
	// Loader is the package loader used to initialize the test environment.
	// When Loader is nil lisplib.LoadLibrary is used.
	Loader func(*lisp.LEnv) *lisp.LVal

	// Teardown runs code to teardown an environment after each test declared
	// in the testing package has been run.  Any error returned by the teardown
	// function is reported as a test failure.
	Teardown func(*lisp.LEnv) *lisp.LVal
}

func (r *Runner) NewEnv(t testing.TB) (*lisp.LEnv, error) {
	logger := NewLogger(t)
	runtime := &lisp.Runtime{
		Registry: lisp.NewRegistry(),
		Stack:    &lisp.CallStack{},
		Reader:   parser.NewReader(),
		Stderr:   logger,
	}
	env := lisp.NewEnvRuntime(runtime)
	err := lisp.GoError(lisp.InitializeUserEnv(env))
	if err != nil {
		return nil, fmt.Errorf("failed to initialize lisp environment: %v", err)
	}
	env.InPackage(lisp.String(lisp.DefaultUserPackage))
	loader := r.Loader
	if loader == nil {
		loader = lisplib.LoadLibrary
	}
	err = lisp.GoError(loader(env))
	if err != nil {
		return nil, fmt.Errorf("failed to load package library: %v", err)
	}
	err = lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage)))
	if err != nil {
		return nil, fmt.Errorf("failed to switch into user package: %v", err)
	}

	return env, nil
}

func (r *Runner) loadTestSuite(t testing.TB, path string, source io.Reader) *libtesting.TestSuite {
	env, err := r.NewEnv(t)
	if err != nil {
		t.Fatal(err.Error())
	}
	defer env.Runtime.Stderr.(*Logger).Flush()

	err = lisp.GoError(env.Load(filepath.Base(path), source))
	if err != nil {
		r.LispError(t, err)
		t.FailNow()
	}
	suite := libtesting.EnvTestSuite(env)
	if suite == nil {
		t.Fatal("unable to locate test suite")
	}
	return suite
}

func (r *Runner) LoadTests(t *testing.T, path string, source io.Reader) []string {
	suite := r.loadTestSuite(t, path, source)
	return suite.Tests()
}

func (r *Runner) LoadBenchmarks(t *testing.B, path string, source io.Reader) []string {
	suite := r.loadTestSuite(t, path, source)
	return suite.Benchmarks()
}

// RunTest runs the test at index i read from source.  Path is only used to
// determine a file basename to use in LEnv.Load().  RunTest returns true if
// the test, and any teardown function given, completed successfully.
func (r *Runner) RunTest(t *testing.T, i int, path string, source io.Reader) {
	env, err := r.NewEnv(t)
	if err != nil {
		t.Error(err.Error())
		return
	}
	defer env.Runtime.Stderr.(*Logger).Flush()

	err = lisp.GoError(env.Load(filepath.Base(path), source))
	if err != nil {
		r.LispError(t, err)
		return
	}
	if r.Teardown != nil {
		defer r.Teardown(env)
	}
	suite := libtesting.EnvTestSuite(env)
	if suite == nil {
		t.Errorf("unable to locate test suite")
		return
	}
	ltest := suite.Test(i)
	err = lisp.GoError(env.Eval(lisp.SExpr([]*lisp.LVal{ltest.Fun})))
	if err != nil {
		r.LispError(t, err)
		return
	}
}

func (r *Runner) RunTestFile(t *testing.T, path string) {
	source, err := os.ReadFile(path) //#nosec G304
	if err != nil {
		t.Errorf("Unable to read test file: %v", err)
		return
	}

	var names []string
	ok := t.Run("$load", func(t *testing.T) {
		names = r.LoadTests(t, path, bytes.NewReader(source))
	})
	if !ok {
		return
	}

	for i := range names {
		// We don't check the result of t.Run here because we want all
		// independent tests to run during a single run of the suite.  An
		// assertion failure within a tests prevents futher evaluation of
		// expressions in that test, but does not halt the execution of the
		// suite as a whole.
		t.Run(names[i], func(t *testing.T) {
			r.RunTest(t, i, path, bytes.NewReader(source))
		})
	}
}

// RunBenchmark runs the benchmark at index i read from source.  Path is only
// used to determine a file basename to use in LEnv.Load().  RunBenchmark
// returns true if the test, and any teardown function given, completed
// successfully.
func (r *Runner) RunBenchmark(b *testing.B, i int, path string, source io.Reader) {
	b.StopTimer()
	env, err := r.NewEnv(b)
	if err != nil {
		b.Error(err.Error())
		return
	}
	defer env.Runtime.Stderr.(*Logger).Flush()

	err = lisp.GoError(env.Load(filepath.Base(path), source))
	if err != nil {
		r.LispError(b, err)
		return
	}
	if r.Teardown != nil {
		defer func() {
			b.StopTimer()
			r.Teardown(env)
			b.StartTimer()
		}()
	}
	suite := libtesting.EnvTestSuite(env)
	if suite == nil {
		b.Errorf("unable to locate test suite")
		return
	}
	ltest := suite.Benchmark(i)
	b.StartTimer()
	err = lisp.GoError(env.Eval(lisp.SExpr([]*lisp.LVal{ltest.Fun, lisp.Int(b.N)})))
	if err != nil {
		r.LispError(b, err)
		return
	}
}

func (r *Runner) RunBenchmarkFile(b *testing.B, path string) {
	b.StopTimer()

	source, err := os.ReadFile(path) //#nosec G304
	if err != nil {
		b.Errorf("Unable to read test file: %v", err)
		return
	}

	var names []string
	ok := b.Run("$load", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			names = r.LoadBenchmarks(b, path, bytes.NewReader(source))
		}
	})
	if !ok {
		return
	}

	for i := range names {
		// We don't check the result of t.Run here because we want all
		// independent benchmarks to run during a single run of the suite.  An
		// assertion failure within a tests prevents futher evaluation of
		// expressions in that test, but does not halt the execution of the
		// suite as a whole.
		b.Run(names[i], func(b *testing.B) {
			r.RunBenchmark(b, i, path, bytes.NewReader(source))
		})
	}
}

func (r *Runner) LispError(t testing.TB, err error) {
	lerr, ok := err.(*lisp.ErrorVal)
	if !ok {
		t.Error(err)
		return
	}
	var buf bytes.Buffer
	_, ioerr := lerr.WriteTrace(&buf)
	if ioerr != nil {
		t.Errorf("io error: %v", ioerr)
		t.Error(err)
		return
	}
	t.Error(buf.String())
}

// TestSequence is a sequence of lisp expressions which are evaluated sequentially
// by a lisp.LEnv.
type TestSequence []struct {
	Expr   string // a lisp expression
	Result string // the evaluated result
	Output string // debug output written to Runtime.Stderr
}

// TestSuite is a set of named TestSequences
type TestSuite []struct {
	Name string
	TestSequence
}

// RunTestSuite runs each TestSequence in tests on isolated lisp.LEnvs.
func RunTestSuite(t *testing.T, tests TestSuite) {
	for i, test := range tests {
		log.Printf("test %d -- %s", i, test.Name)
		env := lisp.NewEnv(nil)
		var exprBuf bytes.Buffer
		err := lisp.GoError(lisp.InitializeUserEnv(env,
			lisp.WithMaximumLogicalStackHeight(50000),
			lisp.WithMaximumPhysicalStackHeight(25000),
			lisp.WithReader(parser.NewReader()),
			lisp.WithStderr(io.MultiWriter(os.Stderr, &exprBuf)),
		))
		if err != nil {
			t.Errorf("test %d %q: %v", i, test.Name, err)
			continue
		}
		for j, expr := range test.TestSequence {
			exprBuf.Reset()
			v, err := env.Runtime.Reader.Read("test", strings.NewReader(expr.Expr))
			if err != nil {
				t.Errorf("test %d %q: expr %d: parse error: %v", i, test.Name, j, err)
				continue
			}
			if len(v) == 0 {
				t.Errorf("test %d %q: expr %d: no expression parsed", i, test.Name, j)
				continue
			}
			if len(v) != 1 {
				t.Errorf("test %d %q: expr %d: more than one expression parsed (%d)", i, test.Name, j, len(v))
				continue
			}
			result := env.Eval(v[0]).String()
			if result != expr.Result {
				t.Errorf("test %d %q: expr %d: expected result %s (got %s)", i, test.Name, j, expr.Result, result)
			}
			if exprBuf.String() != expr.Output {
				t.Errorf("test %d %q: expr %d: expected debug output %q (got %q)", i, test.Name, j, expr.Output, exprBuf.String())
			}
		}
	}
}

// RunBenchmark runs a standard benchmark that executes expressions parsed from
// source.
func RunBenchmark(b *testing.B, source string) {
	b.StopTimer()
	p := parser.NewReader()
	exprs, err := p.Read("benchmark", strings.NewReader(source))
	if err != nil {
		b.Fatalf("parse error: %v", err)
	}
	for i := 0; i < b.N; i++ {
		env := lisp.NewEnv(nil)
		err := lisp.GoError(lisp.InitializeUserEnv(env,
			lisp.WithMaximumLogicalStackHeight(50000),
			lisp.WithMaximumPhysicalStackHeight(25000),
			lisp.WithReader(p),
			lisp.WithStderr(io.Discard),
		))
		if err != nil {
			b.Fatal(err)
		}
		b.StartTimer()
		for i, expr := range exprs {
			lerr := env.Eval(expr)
			if lerr.Type == lisp.LError {
				b.Fatalf("expr %d: %v", i, lerr)
			}
		}
		b.StopTimer()
	}
}
