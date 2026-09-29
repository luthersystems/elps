package lisp

import "testing"

// TestGetDefaultExpansionAllocs pins get-default's expansion cost to the
// hand-built expansion it replaced: one header per template node and one
// cells slice per list, plus the two gensyms and the quote header.  Ranging
// over template children by value made every child escape (26 extra
// allocations per call, a +25% allocs/op regression in Workload/sortedmap).
func TestGetDefaultExpansionAllocs(t *testing.T) {
	env := NewEnv(nil)
	args := SExpr([]*LVal{Symbol("m"), String("a"), Int(0)})
	if got := testing.AllocsPerRun(1000, func() { macroGetDefault(env, args) }); got > 30 {
		t.Fatalf("get-default expansion allocates %v times, want <= 30", got)
	}
}
