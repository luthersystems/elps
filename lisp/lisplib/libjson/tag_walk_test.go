// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"runtime"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func TestTagRejectedArrayAllocationBound(t *testing.T) {
	v := lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(1024), lisp.Int(1024)}), nil)
	opts := []TypedOption{WithTypedMaxValues(3)}
	run := func() {
		t.Helper()
		if _, err := Tag(v, opts...); !errors.Is(err, ErrTypedLimit) {
			t.Fatalf("Tag error = %v, want value limit", err)
		}
	}
	run() // Warm error formatting before measuring.
	var before, after runtime.MemStats
	runtime.ReadMemStats(&before)
	const runs = 20
	for range runs {
		run()
	}
	runtime.ReadMemStats(&after)
	allocated := (after.TotalAlloc - before.TotalAlloc) / runs
	if allocated > 64<<10 {
		t.Fatalf("rejected array allocated %d bytes per call, want at most 65536", allocated)
	}
	t.Logf("rejected array: %d bytes per call", allocated)
}
