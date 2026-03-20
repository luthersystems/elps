// Copyright © 2024 The ELPS authors

package analysis

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestPreferredDefinition_DifferentFiles(t *testing.T) {
	syms := []ExternalSymbol{
		{Name: "fn", Source: &token.Location{File: "z.lisp", Line: 1, Col: 1}},
		{Name: "fn", Source: &token.Location{File: "a.lisp", Line: 1, Col: 1}},
		{Name: "fn", Source: &token.Location{File: "m.lisp", Line: 1, Col: 1}},
	}
	winner := PreferredDefinition(syms)
	require.NotNil(t, winner)
	assert.Equal(t, "a.lisp", winner.Source.File)
}

func TestPreferredDefinition_SameFileDifferentLines(t *testing.T) {
	syms := []ExternalSymbol{
		{Name: "fn", Source: &token.Location{File: "lib.lisp", Line: 10, Col: 1}},
		{Name: "fn", Source: &token.Location{File: "lib.lisp", Line: 3, Col: 1}},
		{Name: "fn", Source: &token.Location{File: "lib.lisp", Line: 7, Col: 1}},
	}
	winner := PreferredDefinition(syms)
	require.NotNil(t, winner)
	assert.Equal(t, 3, winner.Source.Line)
}

func TestPreferredDefinition_SameFileSameLineDifferentCols(t *testing.T) {
	syms := []ExternalSymbol{
		{Name: "fn", Source: &token.Location{File: "lib.lisp", Line: 1, Col: 10}},
		{Name: "fn", Source: &token.Location{File: "lib.lisp", Line: 1, Col: 3}},
	}
	winner := PreferredDefinition(syms)
	require.NotNil(t, winner)
	assert.Equal(t, 3, winner.Source.Col)
}

func TestPreferredDefinition_NilSourceLast(t *testing.T) {
	syms := []ExternalSymbol{
		{Name: "fn", Source: nil},
		{Name: "fn", Source: &token.Location{File: "z.lisp", Line: 1, Col: 1}},
	}
	winner := PreferredDefinition(syms)
	require.NotNil(t, winner)
	assert.Equal(t, "z.lisp", winner.Source.File)
}

func TestPreferredDefinition_SingleEntry(t *testing.T) {
	syms := []ExternalSymbol{
		{Name: "fn", Source: &token.Location{File: "only.lisp", Line: 5, Col: 1}},
	}
	winner := PreferredDefinition(syms)
	require.NotNil(t, winner)
	assert.Equal(t, "only.lisp", winner.Source.File)
}

func TestPreferredDefinition_Empty(t *testing.T) {
	winner := PreferredDefinition(nil)
	assert.Nil(t, winner)
}

func TestSortDefinitions_StableOrder(t *testing.T) {
	syms := []ExternalSymbol{
		{Name: "c", Source: &token.Location{File: "z.lisp", Line: 1, Col: 1}},
		{Name: "a", Source: &token.Location{File: "a.lisp", Line: 5, Col: 1}},
		{Name: "b", Source: nil},
		{Name: "d", Source: &token.Location{File: "a.lisp", Line: 1, Col: 1}},
	}
	SortDefinitions(syms)

	// a.lisp:1 < a.lisp:5 < z.lisp:1 < nil
	assert.Equal(t, "d", syms[0].Name)
	assert.Equal(t, "a", syms[1].Name)
	assert.Equal(t, "c", syms[2].Name)
	assert.Equal(t, "b", syms[3].Name)
}

func TestPreferredDefinition_AllNilSource(t *testing.T) {
	syms := []ExternalSymbol{
		{Name: "fn1", Source: nil},
		{Name: "fn2", Source: nil},
	}
	winner := PreferredDefinition(syms)
	require.NotNil(t, winner)
	// First one wins when all are nil (stable sort)
	assert.Equal(t, "fn1", winner.Name)
}
