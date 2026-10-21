// Copyright © 2026 The ELPS authors

package elpsutil

import (
	"context"
	"io"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The FileLoader tests check two properties.  The loader parses its source
// once (TestFileLoaderParsesOnce, TestFileLoaderConcurrentLoadsParseOnce).
// Every environment gets the same sealed nodes, and the seal refuses Lisp
// writes to them (TestFileLoaderSharesSealedNodes,
// TestFileLoaderConcurrentLoadsShareSealedNodes).  A Go write to a shared
// node is not refused (see lisp/seal.go), so no test tries one.

// fileLoaderSrc binds quoted literals, which is how Lisp code reaches parsed
// nodes, and a function that closes over one of them.
const fileLoaderSrc = `
(in-package 'user)
(set 'shared-ql '(1 2 3))
(set 'shared-nest '("alpha" ("beta" "gamma")))
(defun shared-f (a) (cons a shared-ql))
`

// newFileLoaderEnv returns a new user environment.
func newFileLoaderEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	require.False(t, rc.IsError(), "%v", rc)
	return env
}

// loadedEnv returns a new user environment with l loaded into it.
func loadedEnv(t *testing.T, l *FileLoader) *lisp.LEnv {
	t.Helper()
	env := newFileLoaderEnv(t)
	rc := l.Load(env)
	require.False(t, rc.IsError(), "load: %v", rc)
	return env
}

// readSym returns the value env binds to sym.
func readSym(t *testing.T, env *lisp.LEnv, sym string) *lisp.LVal {
	t.Helper()
	v := env.EvalContext(context.Background(), lisp.Symbol(sym))
	require.False(t, v.IsError(), "read %s: %v", sym, v)
	return v
}

// TestFileLoaderParsesOnce loads one loader into eight environments and
// checks that it parsed its source once.
func TestFileLoaderParsesOnce(t *testing.T) {
	l := NewFileLoader([]byte(fileLoaderSrc), "shared.lisp")
	assert.Equal(t, uint64(0), l.ParseCount(), "parsed before the first load")

	for i := range 8 {
		env := loadedEnv(t, l)
		assert.Equal(t, `'(1 2 3)`, readSym(t, env, "shared-ql").String(), "env %d", i)
	}
	assert.Equal(t, uint64(1), l.ParseCount(), "parsed the source more than once")
}

// sealedMutations are Lisp writes to the loaded literals.  The seal refuses
// each with modify-literal-error.  They run without a handler, so a test
// fails when one fails for another reason.
var sealedMutations = []string{
	`(stable-sort > shared-ql)`,
	`(slice 'vector shared-ql 0 2)`,
	`(append 'vector shared-ql 9)`,
}

// TestFileLoaderSharesSealedNodes checks that two environments loaded from
// one FileLoader hold the same nodes, that the nodes are sealed and that
// the seal refuses each Lisp write to them.
func TestFileLoaderSharesSealedNodes(t *testing.T) {
	l := NewFileLoader([]byte(fileLoaderSrc), "shared.lisp")

	envA := loadedEnv(t, l)
	envB := loadedEnv(t, l)
	require.Equal(t, uint64(1), l.ParseCount())

	a := readSym(t, envA, "shared-ql")
	b := readSym(t, envB, "shared-ql")
	require.Equal(t, lisp.LSExpr, a.Type)
	require.Len(t, a.Cells, 3)
	require.Len(t, b.Cells, 3)
	for i := range a.Cells {
		require.Samef(t, a.Cells[i], b.Cells[i],
			"shared-ql.Cells[%d]: the environments hold different nodes", i)
	}
	nestA, nestB := readSym(t, envA, "shared-nest"), readSym(t, envB, "shared-nest")
	require.Len(t, nestA.Cells, 2)
	for i := range nestA.Cells {
		require.Samef(t, nestA.Cells[i], nestB.Cells[i],
			"shared-nest.Cells[%d]: the environments hold different nodes", i)
	}

	require.True(t, a.IsSealed(), "the loaded literal is not sealed")
	for i, c := range a.Cells {
		require.Truef(t, c.IsSealed(), "shared-ql.Cells[%d] is not sealed", i)
	}

	for _, mut := range sealedMutations {
		rc := envA.LoadStringContext(context.Background(), "mut.lisp", mut)
		require.Truef(t, rc.IsError(), "%s mutated the shared literal", mut)
		require.Equalf(t, lisp.CondModifyLiteral, rc.Str,
			"%s failed without modify-literal-error: %v", mut, rc)
	}

	assert.Equal(t, `'(1 2 3)`, readSym(t, envA, "shared-ql").String())
	assert.Equal(t, `'(1 2 3)`, readSym(t, envB, "shared-ql").String())

	// An environment loaded after the writes reads the parsed source.
	envC := loadedEnv(t, l)
	assert.Equal(t, `'(1 2 3)`, readSym(t, envC, "shared-ql").String())
	assert.Equal(t, uint64(1), l.ParseCount())
}

// TestFileLoaderConcurrentLoadsParseOnce loads one loader from several
// goroutines and checks that it parsed its source once.  Run it with -race.
func TestFileLoaderConcurrentLoadsParseOnce(t *testing.T) {
	const (
		goroutines = 8
		perRoutine = 6
	)
	l := NewFileLoader([]byte(fileLoaderSrc), "shared.lisp")

	var wg sync.WaitGroup
	wg.Add(goroutines)
	for g := range goroutines {
		go func(g int) {
			defer wg.Done()
			for i := range perRoutine {
				env := lisp.NewEnv(nil)
				env.Runtime.Reader = parser.NewReader()
				if rc := lisp.InitializeUserEnv(env); rc.IsError() {
					t.Errorf("goroutine %d iter %d: init: %v", g, i, rc)
					return
				}
				if rc := l.Load(env); rc.IsError() {
					t.Errorf("goroutine %d iter %d: load: %v", g, i, rc)
					return
				}
				v := env.EvalContext(context.Background(), lisp.Symbol("shared-ql"))
				if v.String() != `'(1 2 3)` {
					t.Errorf("goroutine %d iter %d: shared-ql = %v", g, i, v)
					return
				}
			}
		}(g)
	}
	wg.Wait()

	assert.Equal(t, uint64(1), l.ParseCount(), "concurrent loads parsed the source more than once")
}

// TestFileLoaderConcurrentLoadsShareSealedNodes evaluates the shared nodes
// from several goroutines at once, each trying the writes the seal refuses.
// Run it with -race.
func TestFileLoaderConcurrentLoadsShareSealedNodes(t *testing.T) {
	const goroutines = 8
	l := NewFileLoader([]byte(fileLoaderSrc), "shared.lisp")

	shared := readSym(t, loadedEnv(t, l), "shared-ql")
	require.Len(t, shared.Cells, 3)

	var wg sync.WaitGroup
	wg.Add(goroutines)
	for g := range goroutines {
		go func(g int) {
			defer wg.Done()
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			if rc := lisp.InitializeUserEnv(env); rc.IsError() {
				t.Errorf("goroutine %d: init: %v", g, rc)
				return
			}
			if rc := l.Load(env); rc.IsError() {
				t.Errorf("goroutine %d: load: %v", g, rc)
				return
			}
			v := env.EvalContext(context.Background(), lisp.Symbol("shared-ql"))
			if v.Type != lisp.LSExpr || len(v.Cells) != 3 {
				t.Errorf("goroutine %d: shared-ql = %v", g, v)
				return
			}
			for k := range shared.Cells {
				if v.Cells[k] != shared.Cells[k] {
					t.Errorf("goroutine %d: shared-ql.Cells[%d] is not the first environment's node", g, k)
					return
				}
			}
			for _, mut := range sealedMutations {
				rc := env.LoadStringContext(context.Background(), "mut.lisp", mut)
				if !rc.IsError() || rc.Str != lisp.CondModifyLiteral {
					t.Errorf("goroutine %d: the seal did not refuse %s: %v", g, mut, rc)
					return
				}
			}
			got := env.EvalContext(context.Background(), lisp.Symbol("shared-ql"))
			if got.String() != `'(1 2 3)` {
				t.Errorf("goroutine %d: shared-ql is %s after the writes", g, got)
			}
		}(g)
	}
	wg.Wait()

	env := loadedEnv(t, l)
	assert.Equal(t, `'(1 2 3)`, readSym(t, env, "shared-ql").String())
	assert.Equal(t, uint64(1), l.ParseCount())
}

// TestFileLoaderMatchesFreshParse checks that a loader on a later load
// serves the program that a new loader serves on its first load.
func TestFileLoaderMatchesFreshParse(t *testing.T) {
	shared := NewFileLoader([]byte(fileLoaderSrc), "shared.lisp")
	_ = loadedEnv(t, shared)

	probes := []string{
		`shared-ql`,
		`shared-nest`,
		`(shared-f 0)`,
		`(reverse 'list shared-ql)`,
		`(nth shared-nest 1)`,
	}

	envShared := loadedEnv(t, shared)
	envFresh := loadedEnv(t, NewFileLoader([]byte(fileLoaderSrc), "shared.lisp"))
	for _, p := range probes {
		want := envFresh.LoadStringContext(context.Background(), "probe.lisp", p)
		got := envShared.LoadStringContext(context.Background(), "probe.lisp", p)
		assert.Equal(t, want.String(), got.String(), "the shared parse differs on %s", p)
	}
	assert.Equal(t, uint64(1), shared.ParseCount())
}

// loadPanics calls l.Load twice and returns what each call panicked with.
// It fails t when a call returns.
func loadPanics(t *testing.T, l *FileLoader) [2]any {
	t.Helper()
	var got [2]any
	for i := range got {
		func() {
			defer func() { got[i] = recover() }()
			l.Load(newFileLoaderEnv(t))
			t.Fatalf("load %d did not panic", i)
		}()
	}
	return got
}

// TestFileLoaderParsePanicRepeats checks that a parser panic is reported by
// every Load, not only the first.  No input makes the parser panic, so the
// test injects a parser that does.
func TestFileLoaderParsePanicRepeats(t *testing.T) {
	l := NewFileLoader([]byte(fileLoaderSrc), "shared.lisp")
	l.newLoader = func(lisp.Reader, string, io.Reader) (lisp.Loader, error) {
		panic("parser exploded")
	}

	got := loadPanics(t, l)
	msgs := make([]string, len(got))
	for i, r := range got {
		require.NotNil(t, r, "load %d did not panic", i)
		err, ok := r.(error)
		require.True(t, ok, "load %d panic value is not an error: %#v", i, r)
		assert.Contains(t, err.Error(), "parser exploded", "load %d lost the parser panic", i)
		assert.Contains(t, err.Error(), "shared.lisp", "load %d does not name the source", i)
		msgs[i] = err.Error()
	}
	assert.Equal(t, msgs[0], msgs[1], "the loads report different panics")
	assert.Equal(t, uint64(0), l.ParseCount(), "a parser panic counted as a parse")
}

// TestFileLoaderParseErrorRepeats checks that a source that does not parse
// makes every Load panic with the same error, not only the first.
func TestFileLoaderParseErrorRepeats(t *testing.T) {
	l := NewFileLoader([]byte(`(set 'x '(1 2`), "broken.lisp")

	got := loadPanics(t, l)
	errs := make([]error, len(got))
	for i, r := range got {
		require.NotNil(t, r, "load %d did not panic", i)
		err, ok := r.(error)
		require.True(t, ok, "load %d panic value is not an error: %#v", i, r)
		errs[i] = err
	}
	assert.Equal(t, errs[0].Error(), errs[1].Error(), "the loads report different parse errors")
	assert.Contains(t, errs[0].Error(), "broken.lisp", "the parse error does not name the source")
	assert.Equal(t, uint64(0), l.ParseCount(), "a failed parse counted as a parse")
}
