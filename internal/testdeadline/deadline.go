// Package testdeadline bounds tests that assert termination.
//
// Several hardening regressions assert that an operation which used to hang
// or die now FINISHES. The assertion is "this terminates", not "this
// terminates within N seconds", so the budget only has to be large enough to
// never fire on a working build and small enough to catch a hang.
//
// The budget is CPU time, not wall time (#789). Wall time counts every moment
// the process waits for a CPU, so a wall-clock bound fails a correct build on
// a starved runner (-cpu=1 beside a CPU hog), and no margin is safe from
// that. [Within] and [Watch] bound the CPU time of the goroutine under test;
// [RunChild] bounds the CPU time of a re-executed child process.
//
// Instrumentation still costs CPU: the race detector slows a program by
// roughly an order of magnitude, and a budget picked for an ordinary build
// would then fail `make race` on correct code. Scale returns the budget to use,
// and every bound in this package applies it. Callers pass the ordinary-build
// value.
package testdeadline

import "time"

// Scale returns d adjusted for the instrumentation this binary was built
// with. It never returns less than d.
func Scale(d time.Duration) time.Duration {
	return time.Duration(float64(d) * factor)
}
