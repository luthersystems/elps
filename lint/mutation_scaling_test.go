// Copyright © 2026 The ELPS authors

package lint

import (
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

// These bounds cover actual analysis work, not merely deduplicated output.
// The old flattened descendant cache and repeated distinct-collection scans
// produced the right findings while doing quadratic work (issue #649).
func TestMutationChecks_NestedWorkBound(t *testing.T) {
	const depth = 800
	for _, head := range []string{"stable-sort", "map"} {
		t.Run(head, func(t *testing.T) {
			source := compactMutationNest(depth, head)
			analyzer, count := AnalyzerComparatorMutation, 2*depth-1
			if head == "map" {
				analyzer, count = AnalyzerIterationMutation, depth
			}
			assertMutationWorkBound(t, analyzer, source, count)
		})
	}
}

func TestMutationChecks_DistinctCollectionsWorkBound(t *testing.T) {
	const size = 800
	assertMutationWorkBound(t, AnalyzerIterationMutation, distinctCollectionSource(size), size)
}

func compactMutationNest(depth int, head string) []byte {
	// Indentation must not make the input itself quadratic in depth.
	lines := strings.Split(mutationNestSource(depth, head), "\n")
	for i := range lines {
		lines[i] = strings.TrimSpace(lines[i])
	}
	return []byte(strings.Join(lines, "\n"))
}

func distinctCollectionSource(size int) []byte {
	var source strings.Builder
	source.WriteString("(defun cb (x)\n")
	for range size {
		source.WriteString("(assoc! x \"key\" 1)\n")
	}
	source.WriteString("x)\n")
	for i := range size {
		fmt.Fprintf(&source, "(map 'list cb xs%d)\n", i)
	}
	return []byte(source.String())
}

func assertMutationWorkBound(t *testing.T, analyzer *Analyzer, source []byte, count int) {
	t.Helper()
	linter := &Linter{Analyzers: []*Analyzer{analyzer}}
	allocs := testing.AllocsPerRun(2, func() {
		diags, err := linter.LintFile(source, "scaling.lisp")
		require.NoError(t, err)
		require.Equal(t, count, diagCount(diags))
	})
	// Includes parsing, lint setup and output. This intentionally allows
	// ample linear overhead across toolchains while rejecting the old
	// millions of repeated allocations for these sub-64 KB inputs.
	require.Less(t, allocs, float64(220_000), "analysis must not redo nested or shared callback work")
	t.Logf("source=%d bytes diagnostics=%d allocations=%.0f", len(source), count, allocs)
}

func TestIterationMutation_CollectionPrecedenceIsPerUse(t *testing.T) {
	diags := lintCheck(t, AnalyzerIterationMutation, `(defun cb (x)
  (assoc! x "key" 1))
(map 'list cb x)
(map 'list cb ys)
(map 'list cb x)
(select 'list cb ys)`)
	require.Len(t, diags, 3)
	assertHasDiag(t, diags, "assoc! mutates x while map iterates over it")
	assertHasDiag(t, diags, "assoc! mutates the element x of map")
	assertHasDiag(t, diags, "assoc! mutates the element x of select")
}

func TestIterationMutation_NestedCapturedTargets(t *testing.T) {
	diags := lintCheck(t, AnalyzerIterationMutation, `(map 'list (lambda (outer)
  (map 'list (lambda (inner)
    (assoc! outer "key" 1)
    (assoc! inner "key" 2)
    (append! xs inner)) ys)) xs)`)
	require.Len(t, diags, 3)
	assertHasDiag(t, diags, "assoc! mutates the element outer of map")
	assertHasDiag(t, diags, "assoc! mutates the element inner of map")
	assertHasDiag(t, diags, "append! mutates xs while map iterates over it")
}

func TestMutationChecks_DiagnosticOrder(t *testing.T) {
	// Different query groups report at the same site and on the same line.
	// The global linter sort does not break such ties; map order must not leak.
	const source = `(defun cb (a b) (assoc! a "key" 1) (append! b a)) (map 'list cb a) (select 'list cb b) (map 'list cb c) (stable-sort cb xs) (insert-sorted 'list xs cb x)`
	for _, analyzer := range []*Analyzer{AnalyzerComparatorMutation, AnalyzerIterationMutation} {
		want := lintCheck(t, analyzer, source)
		count := 4
		if analyzer == AnalyzerIterationMutation {
			count = 5
		}
		require.Equal(t, count, diagCount(want))
		for range 30 {
			require.Equal(t, want, lintCheck(t, analyzer, source))
		}
	}
}

func TestMutationSpans_UnionVisitsEachSiteOnce(t *testing.T) {
	// An independent bitset model covers nesting, duplicates, adjacency,
	// disjoint spans and reverse reference order. Counting visits BEFORE
	// diagnostic dedup catches a removed merge even if output stays correct.
	var candidates []mutationSpan
	for start := range 4 {
		for end := start + 1; end <= 4; end++ {
			candidates = append(candidates, mutationSpan{start: start, end: end})
		}
	}
	for _, a := range candidates {
		for _, b := range candidates {
			for _, c := range candidates {
				spans := []mutationSpan{a, b, c}
				var want, got [4]int
				for _, span := range spans {
					for i := span.start; i < span.end; i++ {
						want[i] = 1
					}
				}
				visitMutationSpans(spans, func(span mutationSpan) {
					for i := span.start; i < span.end; i++ {
						got[i]++
					}
				})
				require.Equal(t, want, got, "spans %v, %v, %v", a, b, c)
			}
		}
	}
	visitMutationSpans(nil, func(mutationSpan) { t.Fatal("empty input must not visit a span") })
}

func BenchmarkMutationChecksScaling(b *testing.B) {
	for _, size := range []int{200, 800} {
		for _, shape := range []struct {
			name     string
			analyzer *Analyzer
			source   []byte
			findings int
		}{
			{"NestedComparator", AnalyzerComparatorMutation, compactMutationNest(size, "stable-sort"), 2*size - 1},
			{"NestedIteration", AnalyzerIterationMutation, compactMutationNest(size, "map"), size},
			{"DistinctCollections", AnalyzerIterationMutation, distinctCollectionSource(size), size},
		} {
			b.Run(fmt.Sprintf("%s/%d", shape.name, size), func(b *testing.B) {
				linter := &Linter{Analyzers: []*Analyzer{shape.analyzer}}
				b.ReportAllocs()
				b.SetBytes(int64(len(shape.source)))
				for b.Loop() {
					diags, err := linter.LintFile(shape.source, "scaling.lisp")
					if err != nil || len(diags) != shape.findings {
						b.Fatalf("findings=%d want=%d err=%v", len(diags), shape.findings, err)
					}
				}
			})
		}
	}
}
