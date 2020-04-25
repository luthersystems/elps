package lisp

const ElpsVersion = "1.7"

// Interface for a profiler
type Profiler interface {
	// Is the profiler enabled?
	IsEnabled() bool
	// Enable the profiler
	Enable() error
	// Set the file to output to
	SetFile(filename string) error
	// End the profiling session and output summary lines
	Complete() error
	// Marks the start of a process
	Start(function *LVal)
	// Marks the end of a process
	End(function *LVal)
}

