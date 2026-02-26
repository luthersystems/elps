package debugger

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// loc is a test helper for building StepLocation values.
func loc(depth int, file string, line int) StepLocation {
	return StepLocation{Depth: depth, File: file, Line: line}
}

// locm is a test helper for building StepLocation values with a macro ID.
func locm(depth int, file string, line int, macroID int64) StepLocation {
	return StepLocation{Depth: depth, File: file, Line: line, MacroID: macroID}
}

func TestStepper_InitialState(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	assert.Equal(t, StepNone, s.Mode())
	assert.False(t, s.ShouldPause(loc(0, "test", 1)))
	assert.False(t, s.ShouldPause(loc(5, "test", 1)))
}

func TestStepper_StepInto(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(loc(0, "test", 1), "instruction")
	assert.Equal(t, StepInto, s.Mode())

	// Should pause on any depth (instruction granularity).
	assert.True(t, s.ShouldPause(loc(0, "test", 1)))
	// After pausing, mode resets.
	assert.Equal(t, StepNone, s.Mode())
	assert.False(t, s.ShouldPause(loc(0, "test", 1)))
}

func TestStepper_StepOver(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(loc(3, "test", 1), "instruction") // Step issued at depth 3

	// Deeper than 3: don't pause (inside a function call).
	assert.False(t, s.ShouldPause(loc(4, "test", 2)))
	assert.False(t, s.ShouldPause(loc(5, "test", 2)))

	// Same depth, different line: pause.
	assert.True(t, s.ShouldPause(loc(3, "test", 2)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_LesserDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(loc(3, "test", 1), "instruction")

	// Lesser depth (returned from function): also pauses.
	assert.True(t, s.ShouldPause(loc(2, "test", 2)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOut(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOut(3) // Step issued at depth 3

	// Same depth: don't pause (still in current function).
	assert.False(t, s.ShouldPause(loc(3, "test", 1)))

	// Deeper: don't pause.
	assert.False(t, s.ShouldPause(loc(4, "test", 2)))

	// Lesser depth (returned): pause.
	assert.True(t, s.ShouldPause(loc(2, "test", 3)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_Reset(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(loc(0, "test", 1), "")
	s.Reset()
	assert.Equal(t, StepNone, s.Mode())
	assert.False(t, s.ShouldPause(loc(0, "test", 1)))
}

func TestStepper_StepOut_DepthZero(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOut(0) // Step out issued at depth 0

	// Can't go shallower than 0, so depth 0 should not pause (need depth < 0).
	assert.False(t, s.ShouldPause(loc(0, "test", 1)))
	// Still in StepOut mode since we haven't satisfied the condition.
	assert.Equal(t, StepOut, s.Mode())
}

func TestStepper_StepOver_DepthZero(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(loc(0, "test", 1), "instruction") // Step over at depth 0

	// Same depth 0, different line should pause (depth <= recorded depth).
	assert.True(t, s.ShouldPause(loc(0, "test", 2)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_NonZeroDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(loc(42, "test", 1), "instruction")

	// StepInto with instruction granularity pauses at any depth.
	assert.True(t, s.ShouldPause(loc(42, "test", 1)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_ShouldPausePostCall(t *testing.T) {
	t.Parallel()
	t.Run("pauses at lesser depth", func(t *testing.T) {
		s := NewStepper()
		s.SetStepOut(3)

		// Post-call depth 2 < recorded depth 3 → should pause.
		assert.True(t, s.ShouldPausePostCall(2))
		// After pausing, mode resets.
		assert.Equal(t, StepNone, s.Mode())
	})

	t.Run("no-op at same depth", func(t *testing.T) {
		s := NewStepper()
		s.SetStepOut(3)

		// Same depth 3 is NOT < 3 → should not pause.
		assert.False(t, s.ShouldPausePostCall(3))
		// Mode stays StepOut.
		assert.Equal(t, StepOut, s.Mode())
	})

	t.Run("no-op at greater depth", func(t *testing.T) {
		s := NewStepper()
		s.SetStepOut(3)

		// Greater depth 5 is NOT < 3 → should not pause.
		assert.False(t, s.ShouldPausePostCall(5))
		assert.Equal(t, StepOut, s.Mode())
	})

	t.Run("no-op when not stepping out", func(t *testing.T) {
		s := NewStepper()
		s.SetStepOver(loc(3, "test", 1), "instruction")

		// ShouldPausePostCall only checks StepOut mode.
		assert.False(t, s.ShouldPausePostCall(2))
		assert.Equal(t, StepOver, s.Mode())
	})

	t.Run("no-op when idle", func(t *testing.T) {
		s := NewStepper()

		assert.False(t, s.ShouldPausePostCall(0))
		assert.Equal(t, StepNone, s.Mode())
	})
}

func TestStepper_Depth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	assert.Equal(t, 0, s.Depth())

	s.SetStepOut(5)
	assert.Equal(t, 5, s.Depth())

	s.SetStepOver(loc(3, "test", 1), "instruction")
	assert.Equal(t, 3, s.Depth())

	s.Reset()
	assert.Equal(t, 0, s.Depth())
}

// --- Line-granularity tests ---

func TestStepper_StepInto_LineGranularity(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued at depth 2, file "test.lisp", line 5.
	// Default granularity (empty string) → line-level.
	s.SetStepInto(loc(2, "test.lisp", 5), "")

	// Same file+line, same depth → skip (sub-expression on same line).
	assert.False(t, s.ShouldPause(loc(2, "test.lisp", 5)))
	// Still in StepInto mode.
	assert.Equal(t, StepInto, s.Mode())

	// Different line, same file → pause.
	assert.True(t, s.ShouldPause(loc(2, "test.lisp", 6)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_DifferentFile(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(loc(2, "test.lisp", 5), "")

	// Different file → pause (even if same line number).
	assert.True(t, s.ShouldPause(loc(2, "other.lisp", 5)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_EntersFunction(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued at depth 2, file "test.lisp", line 5.
	s.SetStepInto(loc(2, "test.lisp", 5), "")

	// Same file+line but depth increased → entered a function body → pause.
	assert.True(t, s.ShouldPause(loc(3, "test.lisp", 5)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_InstructionGranularity(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// "instruction" granularity → pause on every expression.
	s.SetStepInto(loc(2, "test.lisp", 5), "instruction")

	// Same file+line, same depth → should still pause (instruction level).
	assert.True(t, s.ShouldPause(loc(2, "test.lisp", 5)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_StatementGranularity(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// "statement" granularity maps to line-level (same as default).
	s.SetStepInto(loc(2, "test.lisp", 5), "statement")

	// Same file+line, same depth → skip.
	assert.False(t, s.ShouldPause(loc(2, "test.lisp", 5)))
	assert.Equal(t, StepInto, s.Mode())

	// Different line → pause.
	assert.True(t, s.ShouldPause(loc(2, "test.lisp", 6)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_LineGranularity(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step over at depth 2, file "test.lisp", line 5.
	s.SetStepOver(loc(2, "test.lisp", 5), "")

	// Same file+line, same depth → skip (sub-expression on same line).
	assert.False(t, s.ShouldPause(loc(2, "test.lisp", 5)))
	assert.Equal(t, StepOver, s.Mode())

	// Deeper → skip (inside called function).
	assert.False(t, s.ShouldPause(loc(3, "test.lisp", 10)))
	assert.Equal(t, StepOver, s.Mode())

	// Same depth, different line → pause.
	assert.True(t, s.ShouldPause(loc(2, "test.lisp", 6)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_LineGranularity_LesserDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(loc(2, "test.lisp", 5), "")

	// Lesser depth, different line → pause.
	assert.True(t, s.ShouldPause(loc(1, "test.lisp", 10)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_LineGranularity_SameLineLesserDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepOver(loc(2, "test.lisp", 5), "")

	// Lesser depth, same line → pause (returned from call, step complete).
	assert.True(t, s.ShouldPause(loc(1, "test.lisp", 5)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_EmptySource(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued with empty file and zero line (no source info).
	s.SetStepInto(loc(0, "", 0), "")

	// Same empty file+line at same depth → skip (same-line suppression).
	assert.False(t, s.ShouldPause(loc(0, "", 0)))
	assert.Equal(t, StepInto, s.Mode())

	// Any non-empty file → pause (different source location).
	assert.True(t, s.ShouldPause(loc(0, "test.lisp", 1)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_LesserDepth(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(loc(3, "test.lisp", 5), "")

	// Lesser depth, same line → suppressed. Same-line check applies at
	// any depth <= s.depth, so a sub-expression completing at a shallower
	// frame on the same line is still "same line" and gets skipped.
	assert.False(t, s.ShouldPause(loc(2, "test.lisp", 5)))
	assert.Equal(t, StepInto, s.Mode())

	// Different line at lesser depth → pause.
	assert.True(t, s.ShouldPause(loc(2, "test.lisp", 6)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_InstructionGranularity_SameLine(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// StepOver with instruction granularity at same depth, same line.
	s.SetStepOver(loc(2, "test.lisp", 5), "instruction")

	// Same file+line, same depth → should pause (instruction level skips
	// the same-line suppression).
	assert.True(t, s.ShouldPause(loc(2, "test.lisp", 5)))
	assert.Equal(t, StepNone, s.Mode())
}

// --- Expression-level stepping tests (Col + IsSExpr) ---

// loce is a test helper for building StepLocation values with Col and IsSExpr.
func loce(depth int, file string, line, col int, isSExpr bool) StepLocation {
	return StepLocation{Depth: depth, File: file, Line: line, Col: col, IsSExpr: isSExpr}
}

func TestStepper_StepInto_LineGranularity_DifferentSExprSameLine(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued at col 1 on an s-expression.
	s.SetStepInto(loce(2, "test.lisp", 5, 1, true), "")

	// Same line, different col, s-expression → pause (different expression).
	assert.True(t, s.ShouldPause(loce(2, "test.lisp", 5, 10, true)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_AtomSameLine(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued at col 1 on an s-expression.
	s.SetStepInto(loce(2, "test.lisp", 5, 1, true), "")

	// Same line, different col, atom → skip (atoms aren't interesting).
	assert.False(t, s.ShouldPause(loce(2, "test.lisp", 5, 10, false)))
	assert.Equal(t, StepInto, s.Mode())

	// Same line, different col, s-expression → pause.
	assert.True(t, s.ShouldPause(loce(2, "test.lisp", 5, 15, true)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_SameColSameLine(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued at col 5 on an s-expression.
	s.SetStepInto(loce(2, "test.lisp", 5, 5, true), "")

	// Same line, same col → skip (same expression position).
	assert.False(t, s.ShouldPause(loce(2, "test.lisp", 5, 5, true)))
	assert.Equal(t, StepInto, s.Mode())
}

func TestStepper_StepInto_LineGranularity_ZeroColFallsBack(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued with Col=0 (no column info).
	s.SetStepInto(loce(2, "test.lisp", 5, 0, true), "")

	// Same line, Col=0 → falls back to line-level behavior (skip).
	assert.False(t, s.ShouldPause(loce(2, "test.lisp", 5, 0, true)))
	assert.Equal(t, StepInto, s.Mode())

	// Different line → pause.
	assert.True(t, s.ShouldPause(loce(2, "test.lisp", 6, 0, true)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_LineGranularity_IncomingZeroColSkips(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued with Col=5 (has column info).
	s.SetStepInto(loce(2, "test.lisp", 5, 5, true), "")

	// Incoming expression has Col=0 (unknown) — should skip since we can't
	// confirm it's a different expression without column info.
	assert.False(t, s.ShouldPause(loce(2, "test.lisp", 5, 0, true)))
	assert.Equal(t, StepInto, s.Mode())
}

func TestStepper_StepInto_LineGranularity_ExprLevelEntersFunction(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	s.SetStepInto(loce(2, "test.lisp", 5, 1, true), "")

	// Greater depth → pause (entered function body), regardless of col.
	assert.True(t, s.ShouldPause(loce(3, "test.lisp", 5, 1, true)))
	assert.Equal(t, StepNone, s.Mode())
}

// --- Macro expansion ID tests ---

func TestStepper_StepInto_MacroID_PausesOnChange(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step-into at same line, with macro ID 10.
	s.SetStepInto(locm(2, "test.lisp", 5, 10), "")

	// Same file+line, same depth, same macro ID → skip.
	assert.False(t, s.ShouldPause(locm(2, "test.lisp", 5, 10)))
	assert.Equal(t, StepInto, s.Mode())

	// Same file+line, same depth, different macro ID → pause (progress).
	assert.True(t, s.ShouldPause(locm(2, "test.lisp", 5, 11)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_MacroID_ZeroToNonZero(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step issued outside macro expansion (ID=0).
	s.SetStepInto(locm(2, "test.lisp", 5, 0), "")

	// Same line, now in a macro expansion (ID=1) → pause.
	assert.True(t, s.ShouldPause(locm(2, "test.lisp", 5, 1)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepOver_MacroID_IgnoresChange(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// Step-over at same line, with macro ID 10.
	s.SetStepOver(locm(2, "test.lisp", 5, 10), "")

	// Same file+line, same depth, different macro ID → skip (step-over
	// ignores macro ID changes to treat entire expansion as same-line).
	assert.False(t, s.ShouldPause(locm(2, "test.lisp", 5, 11)))
	assert.Equal(t, StepOver, s.Mode())

	// Different line → pause.
	assert.True(t, s.ShouldPause(locm(2, "test.lisp", 6, 12)))
	assert.Equal(t, StepNone, s.Mode())
}

func TestStepper_StepInto_MacroID_InstructionGranularity(t *testing.T) {
	t.Parallel()
	s := NewStepper()
	// With instruction granularity, macro ID is irrelevant — always pauses.
	s.SetStepInto(locm(2, "test.lisp", 5, 10), "instruction")

	assert.True(t, s.ShouldPause(locm(2, "test.lisp", 5, 10)))
	assert.Equal(t, StepNone, s.Mode())
}
