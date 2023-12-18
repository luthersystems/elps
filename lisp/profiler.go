package lisp

const ElpsVersion = "1.7"

// Interface for a profiler
type Profiler interface {
	// IsEnabled determines if the profiler enabled.
	IsEnabled() bool
	// Enable enables the profiler
	Enable() error
	// SetFile sets the file to output to
	SetFile(filename string) error
	// Complete ends the profiling session and output summary lines
	Complete() error
	// Start the process, and returns a function to stop.
	Start(function *LVal) func()
}
