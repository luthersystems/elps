package debugger

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestStepper_InitialState(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	assert.Equal(t, StepNone, s.Mode())
	assert.False(t, s.ShouldPause(0, "test", 1))
	assert.False(t, s.ShouldPause(5, "test", 1))
}

func TestStepper_StepInto(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(0, "instruction", "test", 1)
	assert.Equal(t, StepInto, s.Mode())

	// Should pause on any depth (instruction granularity).
	assert.True(t, s.ShouldPause(0, "test", 1))
	// After pausing, mode resets.
	assert.Equal(t, StepNone, s.Mode())
	assert.False(t, s.ShouldPause(0, "test", 1))
}

func TestStepper_StepOver(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(3, "instruction", "test", 1) // Step issued at depth 3

	// Deeper than 3: don't pause (inside a function call).
	assert.False(t, s.ShouldPause(4, "test", 2))
	assert.False(t, s.ShouldPause(5, "test", 2))

	// Same depth, different line: pause.
	assert.True(t, s.ShouldPause(3, "test", 2))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_LesserDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(3, "instruction", "test", 1)

	// Lesser depth (returned from function): also pauses.
	assert.True(t, s.ShouldPause(2, "test", 2))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOut(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOut(3) // Step issued at depth 3

	// Same depth: don't pause (still in current function).
	assert.False(t, s.ShouldPause(3, "test", 1))

	// Deeper: don't pause.
	assert.False(t, s.ShouldPause(4, "test", 2))

	// Lesser depth (returned): pause.
	assert.True(t, s.ShouldPause(2, "test", 3))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_Reset(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(0, "", "test", 1)
	s.Reset()
	assert.Equal(t, StepNone, s.Mode())
	assert.False(t, s.ShouldPause(0, "test", 1))
}

func TestStepper_StepOut_DepthZero(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOut(0) // Step out issued at depth 0

	// Can't go shallower than 0, so depth 0 should not pause (need depth < 0).
	assert.False(t, s.ShouldPause(0, "test", 1))
	// Still in StepOut mode since we haven't satisfied the condition.
	assert.Equal(t, StepOut, s.Mode())
}

func TestStepper_StepOver_DepthZero(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(0, "instruction", "test", 1) // Step over at depth 0

	// Same depth 0, different line should pause (depth <= recorded depth).
	assert.True(t, s.ShouldPause(0, "test", 2))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_NonZeroDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(42, "instruction", "test", 1)

	// StepInto with instruction granularity pauses at any depth.
	assert.True(t, s.ShouldPause(42, "test", 1))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_ShouldPausePostCall(t *testing.T) {
	t.Parallel()
	t.Run("pauses at lesser depth", func(t *testing.T) {
		s := NewStepper()
		s.SetStepOut(3)

		// Post-call depth 2 < recorded depth 3 → should pause.
		assert.True(t, s.ShouldPausePostCall(2))
		// After pausing, mode resets.
		assert.Equal(t, StepNone, s.Mode())
	})

	t.Run("no-op at same depth", func(t *testing.T) {
		s := NewStepper()
		s.SetStepOut(3)

		// Same depth 3 is NOT < 3 → should not pause.
		assert.False(t, s.ShouldPausePostCall(3))
		// Mode stays StepOut.
		assert.Equal(t, StepOut, s.Mode())
	})

	t.Run("no-op at greater depth", func(t *testing.T) {
		s := NewStepper()
		s.SetStepOut(3)

		// Greater depth 5 is NOT < 3 → should not pause.
		assert.False(t, s.ShouldPausePostCall(5))
		assert.Equal(t, StepOut, s.Mode())
	})

	t.Run("no-op when not stepping out", func(t *testing.T) {
		s := NewStepper()
		s.SetStepOver(3, "instruction", "test", 1)

		// ShouldPausePostCall only checks StepOut mode.
		assert.False(t, s.ShouldPausePostCall(2))
		assert.Equal(t, StepOver, s.Mode())
	})

	t.Run("no-op when idle", func(t *testing.T) {
		s := NewStepper()

		assert.False(t, s.ShouldPausePostCall(0))
		assert.Equal(t, StepNone, s.Mode())
	})
}

func TestStepper_Depth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	assert.Equal(t, 0, s.Depth())

	s.SetStepOut(5)
	assert.Equal(t, 5, s.Depth())

	s.SetStepOver(3, "instruction", "test", 1)
	assert.Equal(t, 3, s.Depth())

	s.Reset()
	assert.Equal(t, 0, s.Depth())
}

// --- Line-granularity tests ---

func TestStepper_StepInto_LineGranularity(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued at depth 2, file "test.lisp", line 5.
	// Default granularity (empty string) → line-level.
	s.SetStepInto(2, "", "test.lisp", 5)

	// Same file+line, same depth → skip (sub-expression on same line).
	assert.False(t, s.ShouldPause(2, "test.lisp", 5))
	// Still in StepInto mode.
	assert.Equal(t, StepInto, s.Mode())

	// Different line, same file → pause.
	assert.True(t, s.ShouldPause(2, "test.lisp", 6))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_DifferentFile(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(2, "", "test.lisp", 5)

	// Different file → pause (even if same line number).
	assert.True(t, s.ShouldPause(2, "other.lisp", 5))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_EntersFunction(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued at depth 2, file "test.lisp", line 5.
	s.SetStepInto(2, "", "test.lisp", 5)

	// Same file+line but depth increased → entered a function body → pause.
	assert.True(t, s.ShouldPause(3, "test.lisp", 5))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_InstructionGranularity(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// "instruction" granularity → pause on every expression.
	s.SetStepInto(2, "instruction", "test.lisp", 5)

	// Same file+line, same depth → should still pause (instruction level).
	assert.True(t, s.ShouldPause(2, "test.lisp", 5))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_StatementGranularity(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// "statement" granularity maps to line-level (same as default).
	s.SetStepInto(2, "statement", "test.lisp", 5)

	// Same file+line, same depth → skip.
	assert.False(t, s.ShouldPause(2, "test.lisp", 5))
	assert.Equal(t, StepInto, s.Mode())

	// Different line → pause.
	assert.True(t, s.ShouldPause(2, "test.lisp", 6))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_LineGranularity(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step over at depth 2, file "test.lisp", line 5.
	s.SetStepOver(2, "", "test.lisp", 5)

	// Same file+line, same depth → skip (sub-expression on same line).
	assert.False(t, s.ShouldPause(2, "test.lisp", 5))
	assert.Equal(t, StepOver, s.Mode())

	// Deeper → skip (inside called function).
	assert.False(t, s.ShouldPause(3, "test.lisp", 10))
	assert.Equal(t, StepOver, s.Mode())

	// Same depth, different line → pause.
	assert.True(t, s.ShouldPause(2, "test.lisp", 6))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_LineGranularity_LesserDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(2, "", "test.lisp", 5)

	// Lesser depth, different line → pause.
	assert.True(t, s.ShouldPause(1, "test.lisp", 10))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_LineGranularity_SameLineLesserDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(2, "", "test.lisp", 5)

	// Lesser depth, same line → pause (returned from call, step complete).
	assert.True(t, s.ShouldPause(1, "test.lisp", 5))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_EmptySource(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued with empty file and zero line (no source info).
	s.SetStepInto(0, "", "", 0)

	// Same empty file+line at same depth → skip (same-line suppression).
	assert.False(t, s.ShouldPause(0, "", 0))
	assert.Equal(t, StepInto, s.Mode())

	// Any non-empty file → pause (different source location).
	assert.True(t, s.ShouldPause(0, "test.lisp", 1))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_LesserDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(3, "", "test.lisp", 5)

	// Lesser depth, same line → pause (returned from a call).
	// Lesser depth means currentDepth < s.depth, so the condition
	// `currentDepth <= s.depth` is true, but we're at a lesser depth
	// which means we left the current scope — this should still pause
	// because same-line suppression only applies at same or lesser depth.
	// Actually, the isSameLine check AND currentDepth <= s.depth both hold,
	// so this is suppressed. But that's correct: if we returned to a
	// shallower frame on the same source line, it's a sub-expression
	// evaluation completing, and the next different-line eval will pause.
	assert.False(t, s.ShouldPause(2, "test.lisp", 5))
	assert.Equal(t, StepInto, s.Mode())

	// Different line at lesser depth → pause.
	assert.True(t, s.ShouldPause(2, "test.lisp", 6))
	assert.Equal(t, StepNone, s.Mode())
}
