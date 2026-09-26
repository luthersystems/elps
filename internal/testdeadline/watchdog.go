package testdeadline

import (
	"fmt"
	"runtime/metrics"
	"time"
)

// heapObjects is the runtime metric Watch polls: bytes in live and
// not-yet-swept heap objects.  Reading it does not stop the world.
const heapObjects = "/memory/classes/heap/objects:bytes"

// Watch runs fn and PANICS -- ending the test binary -- if it has not
// returned within Scale(d), or if the heap grows by more than maxHeap bytes
// while it runs.
//
// It is for regressions whose failure mode is a walk that does not come
// back: exponential work inside one builtin step, which ignores a context
// deadline and the step budget, and may allocate as it goes.  Such a walk
// cannot be interrupted from outside, so a test that merely waited for it
// would hang the suite, and one that waited on a timer and then returned
// would leave it running -- and allocating -- behind the next test until the
// host ran out of memory.  Ending the binary is the only bounded failure.
// A working build returns from fn in milliseconds, so neither bound fires.
func Watch(name string, d time.Duration, maxHeap uint64, fn func()) {
	sample := []metrics.Sample{{Name: heapObjects}}
	heap := func() uint64 {
		metrics.Read(sample)
		if sample[0].Value.Kind() != metrics.KindUint64 {
			return 0
		}
		return sample[0].Value.Uint64()
	}
	base := heap()
	done := make(chan struct{})
	go func() {
		defer close(done)
		fn()
	}()
	budget := Scale(d)
	deadline := time.NewTimer(budget)
	defer deadline.Stop()
	tick := time.NewTicker(10 * time.Millisecond)
	defer tick.Stop()
	for {
		select {
		case <-done:
			return
		case <-deadline.C:
			panic(fmt.Sprintf("%s: did not return within %v", name, budget))
		case <-tick.C:
			if h := heap(); h > base && h-base > maxHeap {
				panic(fmt.Sprintf("%s: heap grew by %d MB while it ran (limit %d MB)", name, (h-base)>>20, maxHeap>>20))
			}
		}
	}
}
