// Copyright © 2025 The ELPS authors

//go:build elpscheck

package lisp

import (
	"fmt"
)

// initSnapshot is the bit-pattern of all three singleton LVals captured
// at package init time, before any user code can mutate them.
var initSnapshot SingletonSnapshot

func init() {
	initSnapshot = TakeSingletonSnapshot()
}

// checkSingleton verifies that all three singleton LVals are
// bit-identical to their init-time snapshot. Called from Bool() and
// Nil() on every invocation — so any mutation is detected at the next
// read of any singleton, localizing the corruption to within a handful
// of evaluations of the offending write.
//
// Cost: three struct-field compares per call. Only enabled under the
// `elpscheck` build tag.
func checkSingleton(_ *LVal) {
	if drift := initSnapshot.Verify(); drift != "" {
		panic(fmt.Sprintf("singleton corruption detected: %s mutated\n  current Nil=%+v\n  current True=%+v\n  current False=%+v",
			drift, singletonNil, singletonTrue, singletonFalse))
	}
}
