package lisp

const ElpsVersion = "1.7"

// Interface for a profiler
type Profiler interface {
	// Start the process, and returns a function to stop.
	Start(function *LVal) func()
}
