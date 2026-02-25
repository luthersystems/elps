package debugger

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestStepper_InitialState(t *testing.T) {
	s := NewStepper()
	assert.Equal(t, StepNone, s.Mode())
	assert.False(t, s.ShouldPause(0))
	assert.False(t, s.ShouldPause(5))
}

func TestStepper_StepInto(t *testing.T) {
	s := NewStepper()
	s.SetStepInto()
	assert.Equal(t, StepInto, s.Mode())

	// Should pause on any depth.
	assert.True(t, s.ShouldPause(0))
	// After pausing, mode resets.
	assert.Equal(t, StepNone, s.Mode())
	assert.False(t, s.ShouldPause(0))
}

func TestStepper_StepOver(t *testing.T) {
	s := NewStepper()
	s.SetStepOver(3) // Step issued at depth 3

	// Deeper than 3: don't pause (inside a function call).
	assert.False(t, s.ShouldPause(4))
	assert.False(t, s.ShouldPause(5))

	// Same depth: pause.
	assert.True(t, s.ShouldPause(3))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_LesserDepth(t *testing.T) {
	s := NewStepper()
	s.SetStepOver(3)

	// Lesser depth (returned from function): also pauses.
	assert.True(t, s.ShouldPause(2))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOut(t *testing.T) {
	s := NewStepper()
	s.SetStepOut(3) // Step issued at depth 3

	// Same depth: don't pause (still in current function).
	assert.False(t, s.ShouldPause(3))

	// Deeper: don't pause.
	assert.False(t, s.ShouldPause(4))

	// Lesser depth (returned): pause.
	assert.True(t, s.ShouldPause(2))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_Reset(t *testing.T) {
	s := NewStepper()
	s.SetStepInto()
	s.Reset()
	assert.Equal(t, StepNone, s.Mode())
	assert.False(t, s.ShouldPause(0))
}

func TestStepper_StepOut_DepthZero(t *testing.T) {
	s := NewStepper()
	s.SetStepOut(0) // Step out issued at depth 0

	// Can't go shallower than 0, so depth 0 should not pause (need depth < 0).
	assert.False(t, s.ShouldPause(0))
	// Still in StepOut mode since we haven't satisfied the condition.
	assert.Equal(t, StepOut, s.Mode())
}

func TestStepper_StepOver_DepthZero(t *testing.T) {
	s := NewStepper()
	s.SetStepOver(0) // Step over issued at depth 0

	// Same depth 0 should pause (depth <= recorded depth).
	assert.True(t, s.ShouldPause(0))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_NonZeroDepth(t *testing.T) {
	s := NewStepper()
	s.SetStepInto()

	// StepInto pauses at any depth, not just depth 0.
	assert.True(t, s.ShouldPause(42))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_ShouldPausePostCall(t *testing.T) {
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
		s.SetStepOver(3)

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
	s := NewStepper()
	assert.Equal(t, 0, s.Depth())

	s.SetStepOut(5)
	assert.Equal(t, 5, s.Depth())

	s.SetStepOver(3)
	assert.Equal(t, 3, s.Depth())

	s.Reset()
	assert.Equal(t, 0, s.Depth())
}
