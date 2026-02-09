// Copyright © 2018 The ELPS authors

package lisp

import "testing"

func TestStandardRuntimeStackLimits(t *testing.T) {
	rt := StandardRuntime()
	if rt.Stack.MaxHeightLogical != DefaultMaxLogicalStackHeight {
		t.Errorf("MaxHeightLogical = %d, want %d", rt.Stack.MaxHeightLogical, DefaultMaxLogicalStackHeight)
	}
	if rt.Stack.MaxHeightPhysical != DefaultMaxPhysicalStackHeight {
		t.Errorf("MaxHeightPhysical = %d, want %d", rt.Stack.MaxHeightPhysical, DefaultMaxPhysicalStackHeight)
	}
}

func TestAtomicCounterAdd(t *testing.T) {
	var c atomicCounter
	// Add various values and verify they accumulate correctly.
	got := c.Add(1)
	if got != 1 {
		t.Errorf("Add(1) = %d, want 1", got)
	}
	got = c.Add(5)
	if got != 6 {
		t.Errorf("Add(5) = %d, want 6", got)
	}
	got = c.Add(10)
	if got != 16 {
		t.Errorf("Add(10) = %d, want 16", got)
	}
}

func TestNewEnvDefaultStackLimits(t *testing.T) {
	env := NewEnv(nil)
	if env.Runtime.Stack.MaxHeightLogical != DefaultMaxLogicalStackHeight {
		t.Errorf("NewEnv(nil) MaxHeightLogical = %d, want %d",
			env.Runtime.Stack.MaxHeightLogical, DefaultMaxLogicalStackHeight)
	}
	if env.Runtime.Stack.MaxHeightPhysical != DefaultMaxPhysicalStackHeight {
		t.Errorf("NewEnv(nil) MaxHeightPhysical = %d, want %d",
			env.Runtime.Stack.MaxHeightPhysical, DefaultMaxPhysicalStackHeight)
	}
}
