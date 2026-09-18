// Package testdeadline scales the wall-clock budget of a test that asserts
// termination.
//
// Several hardening regressions assert that an operation which used to hang
// or die now FINISHES: they re-execute the test binary under a context
// deadline and fail if the child is killed. The assertion is "this
// terminates", not "this terminates within N seconds", so the budget only has
// to be short enough to catch a hang and long enough to never fire on a
// working build. Instrumentation breaks that second half: the race detector
// slows a program by roughly an order of magnitude, and a budget picked for an
// ordinary build then fails `make race` on code that is perfectly correct.
//
// Scale returns the budget to use. Callers pass the ordinary-build value.
package testdeadline

import "time"

// Scale returns d adjusted for the instrumentation this binary was built
// with. It never returns less than d.
func Scale(d time.Duration) time.Duration {
	return time.Duration(float64(d) * factor)
}
