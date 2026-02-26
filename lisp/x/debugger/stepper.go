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

// StepLocation describes a source location for stepping operations.
// Using a struct (rather than positional arguments) keeps the API stable
// when new fields are added.
type StepLocation struct {
	Depth   int    // stack depth
	File    string // source file
	Line    int    // source line
	Col     int    // source column (1-based; 0 means unknown)
	IsSExpr bool   // true if the expression is an s-expression (not an atom)
	MacroID int64  // macro expansion node ID (0 = not in expansion)
}

// Stepper implements the step state machine. It tracks the step mode and
// the reference stack depth to determine when stepping should pause.
//
// Thread safety: Stepper is NOT safe for concurrent use. All access must
// occur on the eval goroutine. The write path (SetStep*/Reset) runs inside
// WaitIfPaused after the channel receive, and the read path (ShouldPause)
// runs in OnEval before the next pause — both on the eval goroutine.
type Stepper struct {
	mode        StepMode
	granularity string       // "instruction" for expression-level, anything else for line-level
	start       StepLocation // location when step command was issued
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
	s.granularity = ""
	s.start = StepLocation{}
}

// SetStepInto configures the stepper to pause on the next OnEval.
// For line-level granularity (the default), it skips pauses on the same
// source line unless the stack depth increased (entered a function body)
// or the macro expansion ID changed (progress within a macro expansion).
// For "instruction" granularity, it pauses on every expression.
func (s *Stepper) SetStepInto(loc StepLocation, granularity string) {
	s.mode = StepInto
	s.granularity = granularity
	s.start = loc
}

// SetStepOver configures the stepper to pause on the next OnEval at the
// same or lesser stack depth.
// For line-level granularity (the default), it additionally skips pauses
// on the same source line (macro ID changes are ignored — step-over treats
// entire macro expansions as same-line). For "instruction" granularity,
// it pauses on every expression at the appropriate depth.
func (s *Stepper) SetStepOver(loc StepLocation, granularity string) {
	s.mode = StepOver
	s.granularity = granularity
	s.start = loc
}

// SetStepOut configures the stepper to pause on the next OnEval at a
// lesser stack depth (i.e., after the current function returns).
func (s *Stepper) SetStepOut(currentDepth int) {
	s.mode = StepOut
	s.start = StepLocation{Depth: currentDepth}
}

// Depth returns the reference stack depth recorded when the step command
// was issued. Used by Engine to detect the step-out condition in
// OnFunReturn (before the frame is popped).
func (s *Stepper) Depth() int {
	return s.start.Depth
}

// ShouldPausePostCall returns true if a step-out should pause after a
// function call returns (post-call check in Eval). This is the primary
// mechanism for detecting step-out from tail-position functions, where
// the normal OnEval-based ShouldPause never fires because execution
// flows back through the call chain without visiting any new expressions
// at a lesser depth. After returning true, the stepper resets to StepNone.
func (s *Stepper) ShouldPausePostCall(currentDepth int) bool {
	if s.mode == StepOut && currentDepth < s.start.Depth {
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
	return file == s.start.File && line == s.start.Line
}

// ShouldPause returns true if the stepper should cause a pause at the
// given location. After returning true, the stepper resets to StepNone.
//
// For line-level granularity (the default), StepInto and StepOver skip
// pauses on the same source line unless the stack depth increased (entered
// a function body). StepInto additionally pauses when the macro expansion
// ID changed on the same line (progress within a macro expansion).
// StepOver ignores macro ID changes (treats entire expansion as same-line).
// For "instruction" granularity, the original per-expression behavior is
// preserved.
func (s *Stepper) ShouldPause(loc StepLocation) bool {
	switch s.mode {
	case StepNone:
		return false
	case StepInto:
		if s.isLineGranularity() && s.isSameLine(loc.File, loc.Line) && loc.Depth <= s.start.Depth {
			// Same line, same or lesser depth.
			// Check if we moved to a different s-expression on this line.
			// Col > 0 guards against expressions without column info.
			if loc.Col > 0 && loc.Col != s.start.Col && loc.IsSExpr {
				s.mode = StepNone
				return true
			}
			// Check if the macro expansion ID changed — if so, the debugger
			// has made progress within a macro expansion and should pause.
			if loc.MacroID != s.start.MacroID {
				s.mode = StepNone
				return true
			}
			// Same position, atom, or no column info — skip.
			return false
		}
		s.mode = StepNone
		return true
	case StepOver:
		if loc.Depth > s.start.Depth {
			// Deeper than step origin — inside a called function, skip.
			return false
		}
		if loc.Depth < s.start.Depth {
			// Returned to a shallower depth — always pause regardless of
			// line, as the enclosing call completed.
			s.mode = StepNone
			return true
		}
		// loc.Depth == s.start.Depth: same depth as step origin.
		if s.isLineGranularity() && s.isSameLine(loc.File, loc.Line) {
			// Same line, same depth — skip same-line sub-expressions.
			// Step-over intentionally ignores macro ID changes.
			return false
		}
		s.mode = StepNone
		return true
	case StepOut:
		if loc.Depth < s.start.Depth {
			s.mode = StepNone
			return true
		}
		return false
	default:
		return false
	}
}
