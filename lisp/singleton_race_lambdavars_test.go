// Copyright © 2026 The ELPS authors

// Singleton mutation regression test for issue #333.
//
// Pre-fix, printing a lambda concatenated its formals with its bound
// variables and then unquoted the result in place:
//
//	s := SExpr([]*LVal{Quote(Symbol("list")), formals, bound})
//	s = builtinConcat(nil, s)
//	s.quoted = false // This is fine because builtinConcat returns a new list
//
// The comment was wrong. builtinConcatSeq short-circuits the empty case
// with `return Nil()` -- the process-wide shared singleton. So printing
// any lambda with no formals AND no bound vars wrote straight into
// singletonNil, racing with every concurrent (*LEnv).eval reading
// `v.quoted` off a Nil().
//
// The write stored the value the field already held, so it was
// invisible to SingletonSnapshot.Verify() / the elpscheck build, which
// compare values rather than observing writes. Only -race caught it.
//
// The mutation path no longer exists at all. A function's bound-variable
// list was always empty -- the scope it rendered belonged to a
// per-function environment nothing ever bound into -- so (*LVal).str
// prints the formals directly and concatenates, copies and unquotes
// nothing. These tests now pin that: printing a lambda writes nowhere.
//
//   - TestPrintingZeroArgLambdaWritesNothing is deterministic and needs
//     no -race: it asserts the rendered text and then that every shared
//     singleton is untouched.
//   - TestSingletonRacePrintingLambda drives the real production path
//     ((*LVal).String() on a zero-formal lambda) against concurrent
//     evaluation. Run with `go test -race` to observe the failure
//     pre-fix.

package lisp

import (
	"sync"
	"testing"
)

// zeroArgLambda builds a `(lambda () 1)` -- no formals, and a fresh
// closure scope with no bound vars. Its printed representation is the
// path that reached the offending write.
func zeroArgLambda(t *testing.T) (*LEnv, *LVal) {
	t.Helper()
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	fn := env.Lambda(QExpr(nil), []*LVal{Int(1)})
	if fn.Type == LError {
		t.Fatalf("Lambda: %v", fn)
	}
	return env, fn
}

// TestPrintingZeroArgLambdaWritesNothing pins what replaced the identity
// invariant. The empty-formals case is the one that reached the write, so
// it is the one worth pinning: printing it renders the same text as ever
// and leaves every shared singleton pristine.
func TestPrintingZeroArgLambdaWritesNothing(t *testing.T) {
	_, fn := zeroArgLambda(t)

	snap := TakeSingletonSnapshot()
	if got, want := fn.String(), "(lambda () 1)"; got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
	if bad := snap.Verify(); bad != "" {
		t.Errorf("printing a lambda mutated the shared singleton %s", bad)
	}
	// Verify compares values and so cannot see a write that stores the
	// value already there -- the shape #333 took.  The structural half of
	// the invariant is that printing reaches no singleton at all: the only
	// value it could reach is the formals list, and an empty formals list
	// is a fresh QExpr, never a singleton.
	if isSingleton(fn.Cells[0]) {
		t.Error("a lambda's formals list is a shared singleton; printing must not be able to reach one")
	}
}

// TestSingletonRacePrintingLambda reproduces issue #333 under `go test
// -race`: printers stringify a zero-formal lambda (the write side, back
// when printing concatenated and unquoted) while evaluators evaluate
// Nil() (read side, `if v.quoted` in (*LEnv).eval). Pre-fix both touch
// singletonNil and the race detector fires.
//
// Each goroutine owns its LEnv -- LEnv is not safe for concurrent use.
// The race is on the process-wide singleton, not on any env.
func TestSingletonRacePrintingLambda(t *testing.T) {
	const (
		printers   = 4
		evaluators = 4
		iterations = 3000
	)

	lambdas := make([]*LVal, printers)
	for i := range lambdas {
		_, lambdas[i] = zeroArgLambda(t)
	}
	evalEnvs := make([]*LEnv, evaluators)
	for i := range evalEnvs {
		evalEnvs[i], _ = zeroArgLambda(t)
	}

	var wg sync.WaitGroup
	start := make(chan struct{})

	wg.Add(printers)
	for i := range printers {
		go func(fn *LVal) {
			defer wg.Done()
			<-start
			for range iterations {
				// The former write side: str() -> lambdaVars ->
				// builtinConcat returned Nil() for the empty case and
				// the caller then set `s.quoted = false` on it.
				_ = fn.String()
			}
		}(lambdas[i])
	}

	wg.Add(evaluators)
	for i := range evaluators {
		go func(env *LEnv) {
			defer wg.Done()
			<-start
			for range iterations {
				// Read side: (*LEnv).eval reads `v.quoted` off the
				// same singletonNil.
				env.Eval(Nil())
			}
		}(evalEnvs[i])
	}

	close(start)
	wg.Wait()

	// Belt and braces: the singleton must still be pristine. This
	// cannot catch the same-value write that started #333 (see the
	// comment on checkSingleton), but it catches any future variant
	// that writes a *different* value.
	if singletonNil.quoted {
		t.Error("singletonNil.quoted was set by concurrent lambda printing")
	}
	if singletonNil.Len() != 0 {
		t.Errorf("singletonNil grew to %d cells", singletonNil.Len())
	}
}
