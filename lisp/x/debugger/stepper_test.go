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
