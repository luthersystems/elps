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
	mode  StepMode
	depth int // stack depth when step command was issued
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
}

// SetStepInto configures the stepper to pause on the next OnEval.
func (s *Stepper) SetStepInto() {
	s.mode = StepInto
	s.depth = 0
}

// SetStepOver configures the stepper to pause on the next OnEval at the
// same or lesser stack depth.
func (s *Stepper) SetStepOver(currentDepth int) {
	s.mode = StepOver
	s.depth = currentDepth
}

// SetStepOut configures the stepper to pause on the next OnEval at a
// lesser stack depth (i.e., after the current function returns).
func (s *Stepper) SetStepOut(currentDepth int) {
	s.mode = StepOut
	s.depth = currentDepth
}

// ShouldPause returns true if the stepper should cause a pause at the
// given stack depth. After returning true, the stepper resets to StepNone.
func (s *Stepper) ShouldPause(currentDepth int) bool {
	switch s.mode {
	case StepNone:
		return false
	case StepInto:
		s.mode = StepNone
		return true
	case StepOver:
		if currentDepth <= s.depth {
			s.mode = StepNone
			return true
		}
		return false
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
