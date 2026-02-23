package debugger

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
)

func TestBreakpointStore_SetAndMatch(t *testing.T) {
	store := NewBreakpointStore()

	bp := store.Set("test.lisp", 10, "")
	assert.Equal(t, 1, bp.ID)
	assert.Equal(t, "test.lisp", bp.File)
	assert.Equal(t, 10, bp.Line)
	assert.True(t, bp.Enabled)

	// Match with matching location.
	loc := &token.Location{File: "test.lisp", Line: 10}
	matched := store.Match(loc)
	assert.NotNil(t, matched)
	assert.Equal(t, bp.ID, matched.ID)

	// No match on different line.
	loc2 := &token.Location{File: "test.lisp", Line: 11}
	assert.Nil(t, store.Match(loc2))

	// No match on different file.
	loc3 := &token.Location{File: "other.lisp", Line: 10}
	assert.Nil(t, store.Match(loc3))

	// Nil source returns nil.
	assert.Nil(t, store.Match(nil))
}

func TestBreakpointStore_Remove(t *testing.T) {
	store := NewBreakpointStore()
	store.Set("test.lisp", 10, "")

	ok := store.Remove("test.lisp", 10)
	assert.True(t, ok)

	loc := &token.Location{File: "test.lisp", Line: 10}
	assert.Nil(t, store.Match(loc))

	// Removing non-existent breakpoint returns false.
	ok = store.Remove("test.lisp", 10)
	assert.False(t, ok)
}

func TestBreakpointStore_SetForFile(t *testing.T) {
	store := NewBreakpointStore()

	// Set initial breakpoints.
	store.Set("test.lisp", 5, "")
	store.Set("test.lisp", 10, "")
	store.Set("other.lisp", 15, "")

	// Replace all breakpoints for test.lisp.
	bps := store.SetForFile("test.lisp", []int{20, 25}, []string{"(> x 5)", ""})
	assert.Len(t, bps, 2)
	assert.Equal(t, 20, bps[0].Line)
	assert.Equal(t, "(> x 5)", bps[0].Condition)
	assert.Equal(t, 25, bps[1].Line)
	assert.Empty(t, bps[1].Condition)

	// Old breakpoints in test.lisp are gone.
	loc5 := &token.Location{File: "test.lisp", Line: 5}
	assert.Nil(t, store.Match(loc5))
	loc10 := &token.Location{File: "test.lisp", Line: 10}
	assert.Nil(t, store.Match(loc10))

	// New breakpoints are active.
	loc20 := &token.Location{File: "test.lisp", Line: 20}
	assert.NotNil(t, store.Match(loc20))

	// Other file is unaffected.
	loc15 := &token.Location{File: "other.lisp", Line: 15}
	assert.NotNil(t, store.Match(loc15))
}

func TestBreakpointStore_ClearFile(t *testing.T) {
	store := NewBreakpointStore()
	store.Set("test.lisp", 5, "")
	store.Set("test.lisp", 10, "")
	store.Set("other.lisp", 15, "")

	store.ClearFile("test.lisp")
	assert.Nil(t, store.Match(&token.Location{File: "test.lisp", Line: 5}))
	assert.Nil(t, store.Match(&token.Location{File: "test.lisp", Line: 10}))
	assert.NotNil(t, store.Match(&token.Location{File: "other.lisp", Line: 15}))
}

func TestBreakpointStore_ExceptionBreak(t *testing.T) {
	store := NewBreakpointStore()
	assert.Equal(t, ExceptionBreakNever, store.ExceptionBreak())

	store.SetExceptionBreak(ExceptionBreakAll)
	assert.Equal(t, ExceptionBreakAll, store.ExceptionBreak())

	store.SetExceptionBreak(ExceptionBreakNever)
	assert.Equal(t, ExceptionBreakNever, store.ExceptionBreak())
}

func TestBreakpointStore_All(t *testing.T) {
	store := NewBreakpointStore()
	store.Set("a.lisp", 1, "")
	store.Set("b.lisp", 2, "")

	all := store.All()
	assert.Len(t, all, 2)
}

func TestBreakpointStore_IDsAreUnique(t *testing.T) {
	store := NewBreakpointStore()
	bp1 := store.Set("test.lisp", 1, "")
	bp2 := store.Set("test.lisp", 2, "")
	assert.NotEqual(t, bp1.ID, bp2.ID)
}

func TestBreakpointStore_SetUpdatesExisting(t *testing.T) {
	store := NewBreakpointStore()
	bp1 := store.Set("test.lisp", 10, "")
	bp2 := store.Set("test.lisp", 10, "(> x 5)")

	// Same breakpoint ID, updated condition.
	assert.Equal(t, bp1.ID, bp2.ID)
	assert.Equal(t, "(> x 5)", bp2.Condition)
}
