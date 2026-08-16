// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// BenchmarkDeleteCompaction measures what the #471 fix costs.
//
// The fix replaced an in-place compaction — which allocated nothing, because
// it shifted the tail down inside the caller's own array — with one that
// builds the result in a slice of its own.  That is one allocation on a path
// that previously did none, so it is measured rather than asserted to be
// free.  Issue #452's rule applies: allocations are not exempt from the gate,
// and a clamp or a copy that forces one has to show the number.
//
// The two arms alternate inside a single process, as BenchmarkCycleGuardCost
// does, so machine drift moves both together and benchstat -col /arm reads
// the difference rather than the noise.  The "inplace" arm is the pre-fix
// body, kept alive as inPlaceViewDelete in path_view_alias_test.go and pinned
// to the shipped answer by TestInPlaceReplicaMatchesTheShippedAnswer — so the
// comparison is against what actually used to run, not a sketch of it.
//
// Both arms are given a fresh sequence per iteration, since both consume the
// one they are handed.  The allocation that setup costs is charged to both
// equally and is what the /arm comparison cancels.
func BenchmarkDeleteCompaction(b *testing.B) {
	steps := []struct {
		name string
		step *lisp.LVal
	}{
		{"index", lisp.Int(0)},
		{"range1", rangeStep(0, 1)},
		{"range3", rangeStep(0, 3)},
	}
	arms := []struct {
		name string
		del  viewDeleteFn
	}{
		{"allocating", shippedViewDelete},
		{"inplace", inPlaceViewDelete},
	}
	for _, n := range []int{4, 16, 64} {
		for _, st := range steps {
			for _, arm := range arms {
				name := "n=" + itoa(n) + "/step=" + st.name + "/arm=" + arm.name
				b.Run(name, func(b *testing.B) {
					// The elements are built once.  Per iteration the work
					// slice is refilled by copy -- no allocation, and the same
					// cost in both arms -- so the /arm delta is the
					// compaction's own allocation and nothing else.  A
					// StopTimer'd setup would not do: b.ReportAllocs counts
					// allocations whether the timer runs or not, and a fresh
					// n-element slice per iteration would bury the single
					// allocation this benchmark exists to measure.
					template := make([]*lisp.LVal, n)
					for i := range template {
						template[i] = lisp.Int(i)
					}
					work := make([]*lisp.LVal, n)

					b.ReportAllocs()
					for b.Loop() {
						copy(work, template)
						seq := lisp.Array(nil, work[0:n:n])
						if _, err := arm.del(st.step, seq); err != nil {
							b.Fatal(err)
						}
					}
				})
			}
		}
	}
}

func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	var buf [20]byte
	i := len(buf)
	for n > 0 {
		i--
		buf[i] = byte('0' + n%10)
		n /= 10
	}
	return string(buf[i:])
}
