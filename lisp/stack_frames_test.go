// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestPushFIDWritesEveryField: PushFID and Pop write frames field by field.
// A pushed frame holds exactly what PushFID was given, even in a slot an
// embedder left dirty by truncating Frames, and Pop returns the frame and
// leaves no pointer in the slot it frees.
func TestPushFIDWritesEveryField(t *testing.T) {
	// PushFID writes, and Pop clears, the fields by name.  A new CallFrame
	// field must be written in PushFID, cleared in Pop if it holds a
	// pointer, and then added to this list.
	var fields []string
	for f := range reflect.TypeFor[CallFrame]().Fields() {
		fields = append(fields, f.Name)
	}
	require.Equal(t, []string{"Source", "FID", "Package", "Name", "HeightLogical", "Terminal", "TROBlock", "lispFun", "TailIterations"}, fields)
	var s CallStack
	src := &token.Location{File: "frames.lisp", Line: 2}
	require.NoError(t, s.PushFID(src, "fid1", "pkg1", "name1"))
	top := s.Top()
	top.Terminal, top.TROBlock, top.lispFun, top.TailIterations = true, true, true, 9
	s.Frames = s.Frames[:0] // truncated without Pop
	require.NoError(t, s.PushFID(nil, "fid2", "pkg2", "name2"))
	assert.Equal(t, CallFrame{FID: "fid2", Package: "pkg2", Name: "name2"}, s.Frames[0])
	s.Frames = s.Frames[:0]
	require.NoError(t, s.pushFrame(nil, "fid2", "pkg2", "name2", true))
	assert.Equal(t, CallFrame{FID: "fid2", Package: "pkg2", Name: "name2", lispFun: true}, s.Frames[0])
	s.Frames = s.Frames[:0]
	require.NoError(t, s.PushFID(nil, "fid2", "pkg2", "name2"))
	require.NoError(t, s.PushFID(src, "fid3", "pkg3", "name3"))
	want := CallFrame{Source: src, FID: "fid3", Package: "pkg3", Name: "name3", HeightLogical: 1}
	assert.Equal(t, want, s.Frames[1])
	s.Top().Terminal = true
	want.Terminal = true
	assert.Equal(t, want, s.Pop())
	require.Len(t, s.Frames, 1)
	freed := s.Frames[:2][1]
	assert.Nil(t, freed.Source)
	assert.Empty(t, freed.FID)
	assert.Empty(t, freed.Package)
	assert.Empty(t, freed.Name)
	assert.Equal(t, CallFrame{FID: "fid2", Package: "pkg2", Name: "name2"}, s.Pop())
	assert.Empty(t, s.Frames)
}
