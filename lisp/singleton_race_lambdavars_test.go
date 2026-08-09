// Copyright © 2026 The ELPS authors

// Singleton mutation regression test for issue #333.
//
// Pre-fix, lambdaVars (lisp.go) did:
//
//	s := SExpr([]*LVal{Quote(Symbol("list")), formals, bound})
//	s = builtinConcat(nil, s)
//	s.Quoted = false // This is fine because builtinConcat returns a new list
//
// The comment was wrong. builtinConcatSeq short-circuits the empty case
// with `return Nil()` -- the process-wide shared singleton. So printing
// any lambda with no formals AND no bound vars wrote straight into
// singletonNil, racing with every concurrent (*LEnv).eval reading
// `v.Quoted` off a Nil().
//
// The write stored the value the field already held, so it was
// invisible to SingletonSnapshot.Verify() / the elpscheck build, which
// compare values rather than observing writes. Only -race caught it.
//
// Two tests here:
//
//   - TestLambdaVarsDoesNotReturnSingleton is deterministic and needs no
//     -race: it asserts the identity invariant directly.
//   - TestSingletonRaceLambdaVars drives the real production path
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

// TestLambdaVarsDoesNotReturnSingleton pins the identity invariant that
// makes the mutation safe. lambdaVars writes to the value it returns, so
// that value must never be one of the shared singletons -- regardless of
// what builtinConcat decides to hand back for the empty case.
func TestLambdaVarsDoesNotReturnSingleton(t *testing.T) {
	_, fn := zeroArgLambda(t)

	vars := lambdaVars(fn.Cells[0], boundVars(fn))
	if isSingleton(vars) {
		t.Fatalf("lambdaVars returned a shared singleton (%v); mutating it races with every concurrent reader", vars.Type)
	}
	if vars.Len() != 0 {
		t.Errorf("lambdaVars returned %d cells, want 0", vars.Len())
	}
	if got, want := fn.String(), "(lambda () 1)"; got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
}

// TestSingletonRaceLambdaVars reproduces issue #333 under `go test
// -race`: printers stringify a zero-formal lambda (write side, via
// lambdaVars) while evaluators evaluate Nil() (read side, `if v.Quoted`
// in (*LEnv).eval). Pre-fix both touch singletonNil and the race
// detector fires.
//
// Each goroutine owns its LEnv -- LEnv is not safe for concurrent use.
// The race is on the process-wide singleton, not on any env.
func TestSingletonRaceLambdaVars(t *testing.T) {
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
				// Write side: str() -> lambdaVars -> builtinConcat
				// returns Nil() for the empty case -> `s.Quoted = false`.
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
				// Read side: (*LEnv).eval reads `v.Quoted` off the
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
	if singletonNil.Quoted {
		t.Error("singletonNil.Quoted was set by concurrent lambda printing")
	}
	if singletonNil.Len() != 0 {
		t.Errorf("singletonNil grew to %d cells", singletonNil.Len())
	}
}
