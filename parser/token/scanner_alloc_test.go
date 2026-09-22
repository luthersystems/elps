// Copyright © 2026 The ELPS authors

package token

import "testing"

func TestScannerAllocationCounts(t *testing.T) {
	// Empty text excludes string allocation. Each run starts fresh so slab
	// refills, including the partially used last chunk, are measured.
	// Chunks grow 8, 16, 32, 64, 64, ... (nextChunk), so cumulative capacity
	// runs 8, 24, 56, 120, 184, 248, 312: n tokens take as many Token chunks
	// as that ladder needs, and EmitToken also draws one Location per token
	// from the same ladder, so it allocates twice as often as LocStart alone.
	for _, tc := range []struct {
		name                         string
		n, wantTokens, wantLocations int
	}{
		{"one", 1, 2, 1},
		{"first-chunk", 8, 2, 1},
		{"grow", 9, 4, 2},
		{"64", 64, 8, 4},
		{"128", 128, 10, 5},
		{"256", 256, 14, 7},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var tok *Token
			allocs := testing.AllocsPerRun(200, func() {
				s := Scanner{file: "test", startLine: 1}
				for range tc.n {
					tok = s.EmitToken(SYMBOL)
				}
			})
			if tok.Type != SYMBOL || tok.Text != "" || tok.Source.File != "test" {
				t.Fatalf("unexpected token: %+v", tok)
			}
			if allocs != float64(tc.wantTokens) {
				t.Errorf("EmitToken allocated %v times, want %d", allocs, tc.wantTokens)
			}
			var loc *Location
			allocs = testing.AllocsPerRun(200, func() {
				s := Scanner{file: "test", startLine: 1}
				for range tc.n {
					loc = s.LocStart()
				}
			})
			if *loc != (Location{File: "test", Line: 1, Col: 1}) {
				t.Fatalf("unexpected location: %+v", loc)
			}
			if allocs != float64(tc.wantLocations) {
				t.Errorf("LocStart allocated %v times, want %d", allocs, tc.wantLocations)
			}
		})
	}
}
