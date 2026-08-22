// Copyright © 2026 The ELPS authors

package lisp

// RegisterSealWatchForTest exposes the seal write watchdog's explicit
// registration to the external test package, which owns the tests that
// drive corruption shapes through the real parse/eval pipeline (the
// parser cannot be imported from inside package lisp).  A registered tree
// is read unsynchronized on every watchdog tick, so under -race any
// unsynchronized write to it is reported deterministically.  Test-only;
// compiled only into the lisp test binary.
func RegisterSealWatchForTest(roots ...*LVal) (unregister func()) {
	return registerSealWatch(roots...)
}

// PauseSealWatchdogForTest exposes pauseSealWatchdog to the external test
// package for tests that must deliberately mutate a sealed node.  The
// caller must restore the mutated bytes before resuming.
func PauseSealWatchdogForTest() (resume func()) {
	return pauseSealWatchdog()
}
