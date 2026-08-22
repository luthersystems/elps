// Copyright © 2024 The ELPS authors

package parser

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fmtraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNewReader_Standard(t *testing.T) {
	r := NewReader()
	exprs, err := r.Read("test", strings.NewReader("(+ 1 2)"))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	assert.Equal(t, lisp.LSExpr, exprs[0].Type)
	// Standard reader does not populate formatting metadata.
	assert.Nil(t, fmtraw.Meta(exprs[0]))
}

func TestNewReader_FormatPreserving(t *testing.T) {
	r := NewReader(WithFormatPreserving())
	exprs, err := r.Read("test", strings.NewReader("; comment\n(+ 1 2)"))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	// Format-preserving reader populates Meta with comments and bracket type.
	m := fmtraw.Meta(exprs[0])
	require.NotNil(t, m)
	assert.Equal(t, '(', m.BracketType)
	require.Len(t, m.LeadingComments, 1)
	assert.Contains(t, m.LeadingComments[0].Text, "comment")
}

func TestNewReader_FormatPreserving_BracketList(t *testing.T) {
	r := NewReader(WithFormatPreserving())
	exprs, err := r.Read("test", strings.NewReader("[a b c]"))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	m := fmtraw.Meta(exprs[0])
	require.NotNil(t, m)
	assert.Equal(t, '[', m.BracketType)
}

func TestNewReader_BackwardsCompat(t *testing.T) {
	// Calling NewReader() with no args should work identically to the old API.
	r := NewReader()
	exprs, err := r.Read("test", strings.NewReader("42"))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	assert.Equal(t, lisp.LInt, exprs[0].Type)
	assert.Equal(t, 42, exprs[0].Int)
}

func TestNewReader_FormatPreserving_LocationReader(t *testing.T) {
	r := NewReader(WithFormatPreserving())
	lr, ok := r.(lisp.LocationReader)
	require.True(t, ok, "format-preserving reader should implement LocationReader")

	exprs, err := lr.ReadLocation("logical", "/path/to/file.lisp", strings.NewReader("(foo)"))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	loc, ok := exprs[0].Source()
	require.True(t, ok)
	assert.Equal(t, "logical", loc.File)
	assert.Equal(t, "/path/to/file.lisp", loc.Path)
}

func TestNewReader_Standard_ParseError(t *testing.T) {
	r := NewReader()
	_, err := r.Read("test", strings.NewReader("(unclosed"))
	assert.Error(t, err)
}

func TestNewReader_FormatPreserving_ParseError(t *testing.T) {
	r := NewReader(WithFormatPreserving())
	_, err := r.Read("test", strings.NewReader("(unclosed"))
	assert.Error(t, err)
}

func TestNewReader_Standard_LocationReader(t *testing.T) {
	r := NewReader()
	lr, ok := r.(lisp.LocationReader)
	require.True(t, ok, "standard reader should implement LocationReader")

	exprs, err := lr.ReadLocation("logical", "/path/to/file.lisp", strings.NewReader("(bar)"))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	loc, ok := exprs[0].Source()
	require.True(t, ok)
	assert.Equal(t, "logical", loc.File)
	assert.Equal(t, "/path/to/file.lisp", loc.Path)
}
