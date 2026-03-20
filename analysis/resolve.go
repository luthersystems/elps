// Copyright © 2024 The ELPS authors

package analysis

import (
	"cmp"
	"slices"
)

// PreferredDefinition returns the preferred definition from a set of symbols
// with the same name. The winner is chosen by: lexicographically smallest
// Source.File, then earliest Source.Line, then earliest Source.Col.
// Symbols with nil Source are ranked last. Returns nil if the slice is empty.
// Note: sorts the input slice in-place as a side effect.
func PreferredDefinition(syms []ExternalSymbol) *ExternalSymbol {
	if len(syms) == 0 {
		return nil
	}
	SortDefinitions(syms)
	return &syms[0]
}

// SortDefinitions sorts external symbols in-place by deterministic priority:
// lexicographically smallest Source.File, then earliest Source.Line, then
// earliest Source.Col. Symbols with nil Source are pushed to the end.
func SortDefinitions(syms []ExternalSymbol) {
	slices.SortStableFunc(syms, compareExternalSymbol)
}

// compareExternalSymbol is the ordering used by SortDefinitions.
func compareExternalSymbol(a, b ExternalSymbol) int {
	aHas := a.Source != nil
	bHas := b.Source != nil
	if !aHas && !bHas {
		return 0
	}
	if !aHas {
		return 1 // nil Source goes to end
	}
	if !bHas {
		return -1
	}
	if c := cmp.Compare(a.Source.File, b.Source.File); c != 0 {
		return c
	}
	if c := cmp.Compare(a.Source.Line, b.Source.Line); c != 0 {
		return c
	}
	return cmp.Compare(a.Source.Col, b.Source.Col)
}
