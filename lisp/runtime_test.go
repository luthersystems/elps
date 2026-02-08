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
