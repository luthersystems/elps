package debugger

import (
	"sort"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestBreakpointStore_SetAndMatch(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	store := NewBreakpointStore()
	assert.Equal(t, ExceptionBreakNever, store.ExceptionBreak())

	store.SetExceptionBreak(ExceptionBreakAll)
	assert.Equal(t, ExceptionBreakAll, store.ExceptionBreak())

	store.SetExceptionBreak(ExceptionBreakNever)
	assert.Equal(t, ExceptionBreakNever, store.ExceptionBreak())
}

func TestBreakpointStore_All(t *testing.T) {
	t.Parallel()
	store := NewBreakpointStore()
	bp1 := store.Set("a.lisp", 1, "")
	bp2 := store.Set("b.lisp", 2, "")

	all := store.All()
	assert.Len(t, all, 2)

	// Verify returned breakpoint IDs and files, not just count.
	sort.Slice(all, func(i, j int) bool { return all[i].ID < all[j].ID })
	assert.Equal(t, bp1.ID, all[0].ID)
	assert.Equal(t, "a.lisp", all[0].File)
	assert.Equal(t, 1, all[0].Line)
	assert.Equal(t, bp2.ID, all[1].ID)
	assert.Equal(t, "b.lisp", all[1].File)
	assert.Equal(t, 2, all[1].Line)
}

func TestBreakpointStore_IDsAreUnique(t *testing.T) {
	t.Parallel()
	store := NewBreakpointStore()
	bp1 := store.Set("test.lisp", 1, "")
	bp2 := store.Set("test.lisp", 2, "")
	assert.NotEqual(t, bp1.ID, bp2.ID)
}

func TestBreakpointStore_SetUpdatesExisting(t *testing.T) {
	t.Parallel()
	store := NewBreakpointStore()
	bp1 := store.Set("test.lisp", 10, "")
	bp2 := store.Set("test.lisp", 10, "(> x 5)")

	// Same breakpoint ID, updated condition.
	assert.Equal(t, bp1.ID, bp2.ID)
	assert.Equal(t, "(> x 5)", bp2.Condition)
}

// newConditionTestEnv creates a minimal env with a reader for condition evaluation.
func newConditionTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "InitializeUserEnv failed: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil(), "LoadLibrary failed: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "InPackage failed: %v", rc)
	return env
}

func TestEvalCondition_Empty(t *testing.T) {
	t.Parallel()
	env := newConditionTestEnv(t)
	// Empty condition → always true (unconditional breakpoint).
	assert.True(t, EvalCondition(env, ""))
}

func TestEvalCondition_True(t *testing.T) {
	t.Parallel()
	env := newConditionTestEnv(t)
	assert.True(t, EvalCondition(env, "(> 5 3)"))
}

func TestEvalCondition_False(t *testing.T) {
	t.Parallel()
	env := newConditionTestEnv(t)
	assert.False(t, EvalCondition(env, "(> 3 5)"))
}

func TestEvalCondition_Nil(t *testing.T) {
	t.Parallel()
	env := newConditionTestEnv(t)
	// Condition that evaluates to nil → false.
	assert.False(t, EvalCondition(env, "()"))
}

func TestEvalCondition_Error(t *testing.T) {
	t.Parallel()
	env := newConditionTestEnv(t)
	// Condition that errors → false (error is falsey).
	assert.False(t, EvalCondition(env, "undefined-symbol-xyz"))
}

func TestEvalCondition_ParseError(t *testing.T) {
	t.Parallel()
	env := newConditionTestEnv(t)
	// Malformed expression → treated as unconditional (true) per design.
	assert.True(t, EvalCondition(env, "(+ 1"))
}

func TestEvalCondition_NoReader(t *testing.T) {
	t.Parallel()
	env := lisp.NewEnv(nil)
	// No reader → treated as unconditional (true).
	assert.True(t, EvalCondition(env, "(> 5 3)"))
}

func TestBreakpointStore_DisabledBreakpoint(t *testing.T) {
	t.Parallel()
	store := NewBreakpointStore()
	bp := store.Set("test.lisp", 10, "")
	require.True(t, bp.Enabled)

	// Manually disable the breakpoint.
	bp.Enabled = false

	// Match should return nil for disabled breakpoints.
	loc := &token.Location{File: "test.lisp", Line: 10}
	assert.Nil(t, store.Match(loc), "disabled breakpoint should not match")
}

func TestBreakpointStore_PathNormalization(t *testing.T) {
	t.Parallel()
	store := NewBreakpointStore()

	// Set breakpoint with absolute path (IDE-style).
	store.Set("/Users/dev/project/phylum/organisation.lisp", 10, "")

	// Match with basename (ELPS runtime-style).
	loc := &token.Location{File: "organisation.lisp", Line: 10}
	assert.NotNil(t, store.Match(loc), "basename should match absolute path breakpoint")

	// Match with relative path.
	loc2 := &token.Location{File: "../../../phylum/organisation.lisp", Line: 10}
	assert.NotNil(t, store.Match(loc2), "relative path should match absolute path breakpoint")

	// Match with different absolute path (same basename).
	loc3 := &token.Location{File: "/other/path/organisation.lisp", Line: 10}
	assert.NotNil(t, store.Match(loc3), "different absolute path with same basename should match")

	// No match on different file.
	loc4 := &token.Location{File: "utils.lisp", Line: 10}
	assert.Nil(t, store.Match(loc4), "different basename should not match")

	// No match on different line.
	loc5 := &token.Location{File: "organisation.lisp", Line: 11}
	assert.Nil(t, store.Match(loc5), "different line should not match")
}

func TestBreakpointStore_SetForFile_PathNormalization(t *testing.T) {
	t.Parallel()
	store := NewBreakpointStore()

	// Set breakpoints with absolute path.
	store.SetForFile("/Users/dev/project/phylum/test.lisp", []int{5, 10}, []string{"", ""})

	// Match with basename.
	loc := &token.Location{File: "test.lisp", Line: 5}
	assert.NotNil(t, store.Match(loc), "basename should match SetForFile absolute path")

	// Replace using a different absolute path (same basename).
	bps := store.SetForFile("/other/path/test.lisp", []int{20}, []string{""})
	assert.Len(t, bps, 1)

	// Old breakpoints should be gone (same normalized name).
	assert.Nil(t, store.Match(&token.Location{File: "test.lisp", Line: 5}), "old breakpoint should be cleared")
	assert.Nil(t, store.Match(&token.Location{File: "test.lisp", Line: 10}), "old breakpoint should be cleared")

	// New breakpoint should match.
	assert.NotNil(t, store.Match(&token.Location{File: "test.lisp", Line: 20}), "new breakpoint should match")
}

func TestParseHitCondition(t *testing.T) {
	t.Parallel()
	tests := []struct {
		input string
		op    hitOp
		val   int
	}{
		{"", hitOpNone, 0},
		{"5", hitOpEQ, 5},
		{"==10", hitOpEQ, 10},
		{">3", hitOpGT, 3},
		{">=7", hitOpGTE, 7},
		{"%2", hitOpMod, 2},
		{" >= 5 ", hitOpGTE, 5},
		{"abc", hitOpNone, 0},
		{">abc", hitOpNone, 0},
		{">-1", hitOpGT, -1},    // negative numbers are parsed
		{"==-3", hitOpEQ, -3},   // negative numbers are parsed
		{"%0", hitOpMod, 0},      // zero modulus parsed but guarded at check time
		{"   ", hitOpNone, 0},    // whitespace-only
		{"==0", hitOpEQ, 0},      // zero value
		{"999999", hitOpEQ, 999999}, // large value
	}
	for _, tt := range tests {
		op, val := parseHitCondition(tt.input)
		assert.Equal(t, tt.op, op, "op for %q", tt.input)
		assert.Equal(t, tt.val, val, "val for %q", tt.input)
	}
}

func TestBreakpoint_HitCount(t *testing.T) {
	t.Parallel()
	bp := &Breakpoint{
		parsedHitOp:  hitOpEQ,
		parsedHitVal: 3,
	}
	assert.False(t, bp.IncrementHitCount()) // 1 != 3
	assert.False(t, bp.IncrementHitCount()) // 2 != 3
	assert.True(t, bp.IncrementHitCount())  // 3 == 3
	assert.False(t, bp.IncrementHitCount()) // 4 != 3

	// Test modulo.
	bp2 := &Breakpoint{
		parsedHitOp:  hitOpMod,
		parsedHitVal: 2,
	}
	assert.False(t, bp2.IncrementHitCount()) // 1 % 2 != 0
	assert.True(t, bp2.IncrementHitCount())  // 2 % 2 == 0
	assert.False(t, bp2.IncrementHitCount()) // 3 % 2 != 0
	assert.True(t, bp2.IncrementHitCount())  // 4 % 2 == 0

	// Test > (greater than).
	bp3 := &Breakpoint{
		parsedHitOp:  hitOpGT,
		parsedHitVal: 2,
	}
	assert.False(t, bp3.IncrementHitCount()) // 1 > 2 false
	assert.False(t, bp3.IncrementHitCount()) // 2 > 2 false
	assert.True(t, bp3.IncrementHitCount())  // 3 > 2 true
	assert.True(t, bp3.IncrementHitCount())  // 4 > 2 true

	// Test >= (greater than or equal).
	bp4 := &Breakpoint{
		parsedHitOp:  hitOpGTE,
		parsedHitVal: 2,
	}
	assert.False(t, bp4.IncrementHitCount()) // 1 >= 2 false
	assert.True(t, bp4.IncrementHitCount())  // 2 >= 2 true
	assert.True(t, bp4.IncrementHitCount())  // 3 >= 2 true

	// Test no hit condition.
	bp5 := &Breakpoint{}
	assert.True(t, bp5.IncrementHitCount())
	assert.True(t, bp5.IncrementHitCount())
}

func TestInterpolateLogMessage(t *testing.T) {
	t.Parallel()
	env := newConditionTestEnv(t)

	// Simple text with no interpolation.
	assert.Equal(t, "hello world", InterpolateLogMessage(env, "hello world"))

	// Single expression.
	assert.Equal(t, "x is 3", InterpolateLogMessage(env, "x is {(+ 1 2)}"))

	// Multiple expressions.
	assert.Equal(t, "1 and 2", InterpolateLogMessage(env, "{1} and {2}"))

	// Unterminated brace.
	assert.Equal(t, "start {rest", InterpolateLogMessage(env, "start {rest"))

	// Empty template.
	assert.Equal(t, "", InterpolateLogMessage(env, ""))

	// Expression that errors — should render as error.
	result := InterpolateLogMessage(env, "val={undefined_sym}")
	assert.Contains(t, result, "<error:")
}

func TestBreakpointStore_SetForFileSpecs(t *testing.T) {
	t.Parallel()
	store := NewBreakpointStore()

	specs := []BreakpointSpec{
		{Line: 5, Condition: "(> x 3)", HitCondition: ">2", LogMessage: "hit {x}"},
		{Line: 10},
	}
	bps := store.SetForFileSpecs("test.lisp", specs)
	assert.Len(t, bps, 2)
	assert.Equal(t, "(> x 3)", bps[0].Condition)
	assert.Equal(t, ">2", bps[0].HitCondition)
	assert.Equal(t, "hit {x}", bps[0].LogMessage)
	assert.Equal(t, hitOpGT, bps[0].parsedHitOp)
	assert.Equal(t, 2, bps[0].parsedHitVal)
	assert.Empty(t, bps[1].HitCondition)
}

func TestBreakpointStore_ClearFile_PathNormalization(t *testing.T) {
	t.Parallel()
	store := NewBreakpointStore()

	store.Set("test.lisp", 5, "")
	store.Set("/Users/dev/project/other.lisp", 10, "")

	// Clear using absolute path for test.lisp.
	store.ClearFile("/some/path/test.lisp")

	// test.lisp breakpoint should be gone.
	assert.Nil(t, store.Match(&token.Location{File: "test.lisp", Line: 5}), "cleared breakpoint should not match")

	// other.lisp should still be there.
	assert.NotNil(t, store.Match(&token.Location{File: "other.lisp", Line: 10}), "unrelated file should not be cleared")
}
