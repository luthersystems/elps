// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"os"
	"os/exec"
	"runtime/debug"
	"strings"
	"testing"
	"time"
)

// TestRenderMillionDeepValues isolates fatal stack overflows from the test
// runner. Each 1M-deep runtime value and renderer has an external 30s deadline.
func TestRenderMillionDeepValues(t *testing.T) {
	for _, kind := range []string{"list", "vector", "map", "dag"} {
		for _, renderer := range []string{"string", "bounded"} {
			if kind == "dag" && renderer == "string" {
				// The expanded DAG is exponential even with the depth cap;
				// only the byte-bounded renderer promises to reject it cheaply.
				continue
			}
			t.Run(kind+"/"+renderer, func(t *testing.T) {
				ctx, cancel := context.WithTimeout(t.Context(), 30*time.Second)
				defer cancel()
				//nolint:gosec // os.Args[0] is this test binary
				cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestRenderDepthHelper$")
				cmd.Env = append(os.Environ(), "ELPS_TEST_RENDER_DEPTH="+kind+"/"+renderer)
				out, err := cmd.CombinedOutput()
				if err != nil {
					t.Fatalf("deep rendering failed (%v; deadline: %v):\n%s", err, ctx.Err(), out)
				}
			})
		}
	}
}

func TestRenderDepthHelper(t *testing.T) {
	mode := os.Getenv("ELPS_TEST_RENDER_DEPTH")
	if mode == "" {
		t.Skip("subprocess helper")
	}
	// A bounded renderer needs much less than this. The smaller stack makes
	// the pre-fix process death cheap without weakening the 1M-depth input.
	debug.SetMaxStack(16 << 20)
	parts := strings.Split(mode, "/")
	v := Int(7)
	left := "("
	for range 1000000 {
		switch parts[0] {
		case "list":
			v = SExpr([]*LVal{v})
		case "dag":
			v = SExpr([]*LVal{v, v})
		case "vector":
			v = Vector([]*LVal{v})
			left = "(vector "
		case "map":
			m := SortedMap()
			if err := m.MapSet("k", v); err.Type == LError {
				t.Fatal(err)
			}
			v = m
			left = `(sorted-map "k" `
		default:
			t.Fatal("unknown fixture")
		}
	}
	if parts[0] == "dag" {
		// Strict rendering's abbreviated DAG fits this budget, but no
		// cycle can justify returning it. The probe must reject without
		// unrolling the 2^1024 depth-limited branches.
		if got, ok := v.boundedString(16 << 10); ok || got != "" {
			t.Fatal("bounded renderer accepted an abbreviated acyclic DAG")
		}
		return
	}
	// The literal 1024 pins the documented public rendering rule independently
	// of the implementation constant. Neither full output nor an empty result
	// is acceptable; preserve delimiters and identify the omitted subtree.
	want := strings.Repeat(left, 1024) + "#<depth-limit>" + strings.Repeat(")", 1024)
	var got string
	switch parts[1] {
	case "string":
		got = v.String()
	case "bounded":
		var ok bool
		got, ok = v.boundedString(32 << 20)
		if !ok || got != want {
			t.Fatal("large byte budget did not preserve depth-limited representation")
		}
		got, ok = v.boundedString(len(want))
		if !ok {
			t.Fatal("exact byte budget rejected depth-limited representation")
		}
		if short, ok := v.boundedString(len(want) - 1); ok || short != "" {
			t.Fatal("short byte budget accepted partial representation")
		}
	default:
		t.Fatal("unknown renderer")
	}
	if got != want {
		t.Fatalf("render mismatch: got %d bytes, want %d bytes", len(got), len(want))
	}
}

func TestRenderDepthBoundary(t *testing.T) {
	for _, depth := range []int{1023, 1024, 1025} {
		v := Int(7)
		for range depth {
			v = SExpr([]*LVal{v})
		}
		leaf := "7"
		count := depth
		if depth > 1024 {
			leaf = "#<depth-limit>"
			count = 1024
		}
		want := strings.Repeat("(", count) + leaf + strings.Repeat(")", count)
		if got := v.String(); got != want {
			t.Errorf("depth %d: unexpected String representation", depth)
		}
		if got, ok := v.boundedString(len(want)); !ok || got != want {
			t.Errorf("depth %d: unexpected bounded representation", depth)
		}
	}
}

func TestBoundedStringAgreesWithStringOnLongCycle(t *testing.T) {
	for _, tc := range []struct {
		name           string
		prefix, length int
		shallowSibling bool
	}{
		{"long-ring", 0, 2000, false},
		{"entered-after-cap", 1100, 2000, false},
		{"spans-cap", 500, 600, false},
		{"lazy-tracking-boundary", 0, 1000, false},
		{"shared-cycle-spans-cap", 500, 900, true},
		{"shared-cycle-at-lazy-boundary", 500, 960, true},
		{"shared-cycle-past-lazy-boundary", 500, 961, true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			v := SExpr(nil)
			tail := v
			for i := 1; i < tc.prefix+tc.length; i++ {
				next := SExpr(nil)
				tail.Cells = []*LVal{next}
				tail = next
			}
			entry := v
			for range tc.prefix {
				entry = entry.Cells[0]
			}
			tail.Cells = []*LVal{entry}
			if tc.shallowSibling {
				// The first path hits the cap inside the ring. The second
				// enters it shallowly enough for String to discover the cycle.
				v = SExpr([]*LVal{v, entry})
			}
			want := v.String()
			if got, ok := v.boundedString(len(want)); !ok || got != want {
				t.Errorf("exact budget: ok=%v, got %d bytes, want String's %d bytes", ok, len(got), len(want))
			}
			if got, ok := v.boundedString(len(want) - 1); ok || got != "" {
				t.Errorf("short budget: ok=%v, got %d bytes, want false and empty", ok, len(got))
			}
		})
	}
}

func TestBoundedRenderCycleProbePreservesErrorRecovery(t *testing.T) {
	bad := SExpr([]*LVal{nil})
	first := ErrorConditionf("first", "placeholder")
	first.Cells = []*LVal{bad}
	second := ErrorConditionf("second", "placeholder")
	ring := SExpr(nil)
	tail := ring
	for range 899 {
		next := SExpr(nil)
		tail.Cells = []*LVal{next}
		tail = next
	}
	tail.Cells = []*LVal{ring}
	deep := ring
	for range 500 {
		deep = SExpr([]*LVal{deep})
	}
	// Neither occurrence may render past bad. The hidden shared cycle
	// would require the graph probe, rather than the strict active path.
	second.Cells = []*LVal{bad, SExpr([]*LVal{deep, ring})}
	dag := Int(7)
	for range 12 {
		dag = SExpr([]*LVal{dag, dag})
	}
	v := SExpr([]*LVal{dag, first, second})
	want := v.String()
	if strings.Contains(want, cycleMark) || !strings.Contains(want, corruptedNativeMessage) {
		t.Fatal("fixture must recover before reaching its hidden cycle")
	}
	if got, ok := v.boundedString(len(want)); !ok || got != want {
		t.Fatal("exact budget must retain both recovered error messages")
	}
	if got, ok := v.boundedString(len(want) - 1); ok || got != "" {
		t.Fatal("short budget accepted a cycle hidden behind error recovery")
	}
}

func TestBoundedRenderCycleProbePreservesDepthDependentRecovery(t *testing.T) {
	var bad *LVal
	for range 900 {
		bad = SExpr([]*LVal{bad})
	}
	e := ErrorConditionf("bad", "placeholder")
	ring := make([]*LVal, 900)
	for i := range ring {
		ring[i] = SExpr(nil)
	}
	for i := range len(ring) - 1 {
		ring[i].Cells = []*LVal{ring[i+1]}
	}
	ring[len(ring)-1].Cells = []*LVal{e}
	e.Cells = []*LVal{bad, ring[0]}
	deep := e
	for range 200 {
		deep = SExpr([]*LVal{deep})
	}
	// The deep visit caps bad and enters the ring. The shallow visit
	// recovers from bad before entering it. Those paths cannot be combined.
	v := SExpr([]*LVal{deep, ring[800]})
	want := v.String()
	if strings.Contains(want, cycleMark) || !strings.Contains(want, corruptedNativeMessage) {
		t.Fatal("fixture must cap or recover before detecting a cycle")
	}
	if got, ok := v.boundedString(len(want)); !ok || got != want {
		t.Fatal("exact budget changed depth-dependent error recovery")
	}
	if got, ok := v.boundedString(len(want) - 1); ok || got != "" {
		t.Fatal("short budget accepted a cycle assembled from incompatible recovery paths")
	}
}

// Quoting, errors, and tagged values also recurse through the same renderer.
// A cyclic retry must retain the depth cap, and a sibling must still render.
func TestRenderDepthNestedKindsAndStrictRetry(t *testing.T) {
	for _, kind := range []string{"quote", "error", "tagged", "cycle-sibling"} {
		t.Run(kind, func(t *testing.T) {
			v := Int(7)
			for range 2048 {
				switch kind {
				case "quote":
					v = &LVal{Type: LQuote, Cells: []*LVal{v}}
				case "error":
					e := ErrorConditionf("deep", "placeholder")
					e.Cells = []*LVal{v}
					v = e
				case "tagged":
					v = &LVal{Type: LTaggedVal, Str: "tag", Cells: []*LVal{v}}
				default:
					v = SExpr([]*LVal{v})
				}
			}
			if kind == "cycle-sibling" {
				cyc := SExpr(nil)
				cyc.Cells = []*LVal{cyc}
				v = SExpr([]*LVal{cyc, v, Int(9)})
			} else {
				v = SExpr([]*LVal{v, Int(9)})
			}
			got := v.String()
			if !strings.Contains(got, "#<depth-limit>") || !strings.HasSuffix(got, " 9)") {
				t.Fatal("depth marker or trailing sibling missing")
			}
			if kind == "cycle-sibling" && !strings.Contains(got, "#<cycle>") {
				t.Fatal("cycle marker missing")
			}
			if bounded, ok := v.boundedString(len(got)); !ok || bounded != got {
				t.Fatal("bounded and String depth rules differ")
			}
		})
	}
}

// The lazy cycle guard starts tracking at depth 64, so String rediscovers a
// ring's entry only at depth max(entry, 64)+length. A ring that closes after
// the rendering cap is never a cycle as far as String is concerned: it renders
// as depth-limited output. The byte-budget retry walks strictly, tracking from
// depth 1, so it closes the same ring earlier and produces a shorter string.
// It must not offer that string as String's representation.
//
// Ring lengths straddle the boundary in both arms of that max: entry 1 (the
// root is on the ring) detects at 64+length, and a ring entered at or below
// depth 64 detects at entry+length.
func TestBoundedRenderLongCycleMatchesString(t *testing.T) {
	for _, ring := range []struct {
		prefix, length int
	}{
		{0, 960}, {0, 961}, {0, 1000}, {0, 1024}, {0, 1025},
		{100, 900}, {100, 923}, {100, 924},
		{500, 523}, {500, 524},
	} {
		// prefix acyclic lists, then a ring of length cells closing on
		// its own first node. The ring's entry sits at depth prefix+1.
		root := SExpr(nil)
		tail := root
		for i := 1; i < ring.prefix+ring.length; i++ {
			next := SExpr(nil)
			tail.Cells = []*LVal{next}
			tail = next
		}
		entry := root
		for range ring.prefix {
			entry = entry.Cells[0]
		}
		tail.Cells = []*LVal{entry}
		want := root.String()
		// Whichever rule String applied, the marker it chose is the one
		// the bounded renderer has to reproduce.
		cyclic := max(ring.prefix+1, cycleGuardDepth)+ring.length <= maxRenderDepth
		if strings.Contains(want, cycleMark) != cyclic {
			t.Errorf("ring %+v: String used the wrong truncation marker", ring)
		}
		if got, ok := root.boundedString(len(want)); !ok || got != want {
			t.Errorf("ring %+v: exact String budget differed", ring)
		}
		if got, ok := root.boundedString(len(want) - 1); ok || got != "" {
			t.Errorf("ring %+v: accepted a representation shorter than String", ring)
		}
	}
}

func TestRenderDepthRootQuoteBoundary(t *testing.T) {
	for _, depth := range []int{1024, 1025} {
		v := Int(7)
		for range depth {
			v = SExpr([]*LVal{v})
		}
		v = &LVal{Type: LQuote, Cells: []*LVal{v}}
		leaf := "7"
		if depth == 1025 {
			leaf = "#<depth-limit>"
		}
		want := "''" + strings.Repeat("(", 1024) + leaf + strings.Repeat(")", 1024)
		if got := v.String(); got != want {
			t.Errorf("quoted depth %d: String boundary changed", depth)
		}
		if got, ok := v.boundedString(len(want)); !ok || got != want {
			t.Errorf("quoted depth %d: bounded boundary changed", depth)
		}
	}
}
