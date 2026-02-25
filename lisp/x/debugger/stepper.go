// Copyright © 2018 The ELPS authors

package debugger

// StepMode represents the current stepping behavior.
type StepMode int

const (
	// StepNone means no stepping is active (free-running).
	StepNone StepMode = iota
	// StepInto pauses on the next OnEval call regardless of depth.
	StepInto
	// StepOver pauses on the next OnEval where stack depth <= recorded depth.
	StepOver
	// StepOut pauses on the next OnEval where stack depth < recorded depth.
	StepOut
)

// Stepper implements the step state machine. It tracks the step mode and
// the reference stack depth to determine when stepping should pause.
//
// Thread safety: Stepper is NOT safe for concurrent use. All access must
// occur on the eval goroutine. The write path (SetStep*/Reset) runs inside
// WaitIfPaused after the channel receive, and the read path (ShouldPause)
// runs in OnEval before the next pause — both on the eval goroutine.
type Stepper struct {
	mode        StepMode
	depth       int    // stack depth when step command was issued
	granularity string // "instruction" for expression-level, anything else for line-level
	startFile   string // file where step was issued
	startLine   int    // line where step was issued
}

// NewStepper returns a stepper in the StepNone state.
func NewStepper() *Stepper {
	return &Stepper{}
}

// Mode returns the current step mode.
func (s *Stepper) Mode() StepMode {
	return s.mode
}

// Reset clears the stepper to StepNone (free-running).
func (s *Stepper) Reset() {
	s.mode = StepNone
	s.depth = 0
	s.granularity = ""
	s.startFile = ""
	s.startLine = 0
}

// SetStepInto configures the stepper to pause on the next OnEval.
// For line-level granularity (the default), it skips pauses on the same
// source line unless the stack depth increased (entered a function body).
// For "instruction" granularity, it pauses on every expression.
func (s *Stepper) SetStepInto(currentDepth int, granularity, file string, line int) {
	s.mode = StepInto
	s.depth = currentDepth
	s.granularity = granularity
	s.startFile = file
	s.startLine = line
}

// SetStepOver configures the stepper to pause on the next OnEval at the
// same or lesser stack depth.
// For line-level granularity (the default), it additionally skips pauses
// on the same source line. For "instruction" granularity, it pauses on
// every expression at the appropriate depth.
func (s *Stepper) SetStepOver(currentDepth int, granularity, file string, line int) {
	s.mode = StepOver
	s.depth = currentDepth
	s.granularity = granularity
	s.startFile = file
	s.startLine = line
}

// SetStepOut configures the stepper to pause on the next OnEval at a
// lesser stack depth (i.e., after the current function returns).
func (s *Stepper) SetStepOut(currentDepth int) {
	s.mode = StepOut
	s.depth = currentDepth
}

// Depth returns the reference stack depth recorded when the step command
// was issued. Used by Engine to detect the step-out condition in
// OnFunReturn (before the frame is popped).
func (s *Stepper) Depth() int {
	return s.depth
}

// ShouldPausePostCall returns true if a step-out should pause after a
// function call returns (post-call check in Eval). This is the primary
// mechanism for detecting step-out from tail-position functions, where
// the normal OnEval-based ShouldPause never fires because execution
// flows back through the call chain without visiting any new expressions
// at a lesser depth. After returning true, the stepper resets to StepNone.
func (s *Stepper) ShouldPausePostCall(currentDepth int) bool {
	if s.mode == StepOut && currentDepth < s.depth {
		s.mode = StepNone
		return true
	}
	return false
}

// isLineGranularity returns true if the stepper should use line-level
// stepping (skip same-line sub-expressions). All granularities except
// "instruction" map to line-level behavior.
func (s *Stepper) isLineGranularity() bool {
	return s.granularity != "instruction"
}

// isSameLine returns true if the given file and line match the step origin.
func (s *Stepper) isSameLine(file string, line int) bool {
	return file == s.startFile && line == s.startLine
}

// ShouldPause returns true if the stepper should cause a pause at the
// given stack depth and source location. After returning true, the stepper
// resets to StepNone.
//
// For line-level granularity (the default), StepInto and StepOver skip
// pauses on the same source line unless the stack depth increased (entered
// a function body). For "instruction" granularity, the original per-expression
// behavior is preserved.
func (s *Stepper) ShouldPause(currentDepth int, file string, line int) bool {
	switch s.mode {
	case StepNone:
		return false
	case StepInto:
		if s.isLineGranularity() && s.isSameLine(file, line) && currentDepth <= s.depth {
			// Same line, same or lesser depth — skip (sub-expression on same line).
			// But if depth increased, we entered a function body → pause.
			return false
		}
		s.mode = StepNone
		return true
	case StepOver:
		if currentDepth > s.depth {
			// Deeper than step origin — inside a called function, skip.
			return false
		}
		if currentDepth < s.depth {
			// Returned to a shallower depth — always pause regardless of
			// line, as the enclosing call completed.
			s.mode = StepNone
			return true
		}
		// currentDepth == s.depth: same depth as step origin.
		if s.isLineGranularity() && s.isSameLine(file, line) {
			// Same line, same depth — skip same-line sub-expressions.
			return false
		}
		s.mode = StepNone
		return true
	case StepOut:
		if currentDepth < s.depth {
			s.mode = StepNone
			return true
		}
		return false
	default:
		return false
	}
}
