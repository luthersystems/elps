// Copyright © 2026 The ELPS authors

package token

import "testing"

func TestScannerAllocationCounts(t *testing.T) {
	// Empty text excludes string allocation. Each run starts fresh so slab
	// refills, including the partially used last chunk, are measured.
	for _, tc := range []struct {
		name                         string
		n, wantTokens, wantLocations int
	}{
		{"one", 1, 2, 1},
		{"full", 64, 2, 1},
		{"refill", 65, 4, 2},
		{"two", 128, 4, 2},
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
