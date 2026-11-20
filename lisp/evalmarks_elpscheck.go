// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

// evalMarksReused is false under elpscheck: a release poisons each consumed
// mark and keeps none, so every mark allocates and an allocation pin counts
// one more than in the default build.
const evalMarksReused = false

// poisonEvalMark is the elpscheck form of giving back a consumed trampoline
// mark: the mark is never reused.  It becomes an invalid value, so code that
// kept the mark past the frame that read it reads an invalid value instead
// of the next mark's call.
func poisonEvalMark(m *LVal) {
	*m = LVal{Type: LInvalid, Str: "released evaluator mark"} //elps:mutates a consumed trampoline mark, which only the frame that read it held
}
