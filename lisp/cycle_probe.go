package lisp

// PROBE BUILD ONLY -- never merged, never committed to the PR branch.
//
// Answers the question the microbenchmarks cannot: does the real production
// workload ever drive a guarded walk past cycleGuardDepth (64), the depth at
// which the guard stops being a bare int compare and starts allocating and
// touching a map per node?
//
// Instrumentation sits in cycleGuard.descend / pairGuard.descend, which every
// guarded walk (String/render, Equal, JSON encode, macro stamping, GoValue)
// calls once per container node it enters, so g.depth is exactly the nesting
// depth of the value being walked at that point.  A new maximum prints to
// stderr; the last such line in a run's log is the workload's true maximum.

import (
	"fmt"
	"os"
	"sync/atomic"
)

var (
	probeMaxValueDepth atomic.Int64
	probeMaxPairDepth  atomic.Int64
	probeValueDescends atomic.Int64
	probePairDescends  atomic.Int64
	// Descends at or past cycleGuardDepth -- the ones that pay the map.
	probeDeepEvents atomic.Int64
)

func probeRecord(kind string, depth int, max *atomic.Int64, total *atomic.Int64) {
	d := int64(depth)
	total.Add(1)
	if d >= cycleGuardDepth {
		probeDeepEvents.Add(1)
	}
	for {
		old := max.Load()
		if d <= old {
			return
		}
		if max.CompareAndSwap(old, d) {
			fmt.Fprintf(os.Stderr,
				"ELPS-DEPTH-PROBE kind=%s new_max_depth=%d guard_depth=%d deep_events=%d value_descends=%d pair_descends=%d\n",
				kind, d, cycleGuardDepth, probeDeepEvents.Load(),
				probeValueDescends.Load(), probePairDescends.Load())
			return
		}
	}
}

// ProbeReport returns the run totals, for a caller that wants them at exit.
func ProbeReport() string {
	return fmt.Sprintf(
		"ELPS-DEPTH-PROBE FINAL max_value_depth=%d max_pair_depth=%d guard_depth=%d deep_events=%d value_descends=%d pair_descends=%d",
		probeMaxValueDepth.Load(), probeMaxPairDepth.Load(), cycleGuardDepth,
		probeDeepEvents.Load(), probeValueDescends.Load(), probePairDescends.Load())
}
