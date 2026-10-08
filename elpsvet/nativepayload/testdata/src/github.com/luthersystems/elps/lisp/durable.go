package lisp

// This file makes the stub the kernel fixture of elpsdurablenative.  The
// only construction reported in the package is unmarked: kernel slot
// literals, the interface-typed Native, the generic NativeOf, a literal of
// another header and a call stack are not natives a dump can meet.

type CallStack struct{ frames []int }

func terminal(env *LEnv) *LVal { return &LVal{Type: LString, Native: env} }

func withStack(v *LVal, s *CallStack) { v.Native = s }

type handle struct{ n int }

func unmarked() *LVal {
	return Native(&handle{}) // want `lisp\.Native payload type \*lisp\.handle has no durable codec`
}
