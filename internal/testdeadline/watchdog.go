package testdeadline

import (
	"runtime/metrics"
	"time"
)

// heapObjects is the runtime metric Watch polls: bytes in live and
// not-yet-swept heap objects.  Reading it does not stop the world.
const heapObjects = "/memory/classes/heap/objects:bytes"

// heapBytes reads heapObjects.
func heapBytes() uint64 {
	sample := []metrics.Sample{{Name: heapObjects}}
	metrics.Read(sample)
	if sample[0].Value.Kind() != metrics.KindUint64 {
		return 0
	}
	return sample[0].Value.Uint64()
}

// Watch runs fn and PANICS -- ending the test binary -- if fn has used
// Scale(d) of CPU time without returning, or if the heap grows by more than
// maxHeap bytes while it runs.
//
// It is for regressions whose failure mode is a walk that does not come
// back: exponential work inside one builtin step, which ignores a context
// deadline and the step budget, and may allocate as it goes.  Such a walk
// cannot be interrupted from outside, so a test that merely waited for it
// would hang the suite, and one that waited on a timer and then returned
// would leave it running -- and allocating -- behind the next test until the
// host ran out of memory.  Ending the binary is the only bounded failure.
// A working build returns from fn in milliseconds of CPU, so neither bound
// fires.
//
// The budget is CPU time on fn's thread, not wall time (#789): see Within.
// A walk that does not come back keeps using CPU, so it still reaches the
// budget; a correct walk on a starved process does not, however long it
// waits for a CPU.
//
// Watch returns the CPU time fn used.
func Watch(name string, d time.Duration, maxHeap uint64, fn func()) time.Duration {
	used, reason := watch(Scale(d), maxHeap, fn)
	if reason != "" {
		panic(name + ": " + reason)
	}
	return used
}
