// Copyright © 2026 The ELPS authors

package lisp

import "fmt"

// MaxValueDepth bounds recursive walks over program-built values independently
// of evaluator nesting, cycle detection and allocation budgets. This is also
// the renderer's hard ceiling; no option may raise or disable it.
//
// Walker inventory: walker | file:line | guard | regression.
// Paths are relative to the repository root. Existing input-only limits are
// retained; this ceiling governs recursive walks of program-built values.
// rendering / format-string | lisp/render_bounded.go:281 | counter / marker | TestValueWalkDepth/format-string
// copy / detach | lisp/detach.go:128 | counter | TestValueWalkDepth/copy,detach
// LVal.Copy | lisp/copier.go:271 | counter | TestValueWalkDepth/Copy
// GoValue / GoSlice / GoMap | lisp/embed.go:42 | counter | TestValueWalkDepth/GoValue
// JSON dump | lisp/lisplib/libjson/encode.go:313 | counter, both passes | TestJSONValueDepth/dump-string
// JSON Go conversion | lisp/lisplib/libjson/json.go:718 | counter | TestJSONValueDepth/GoValue
// template admission (values / sealed / envs) | lisp/template.go:303 | counter | TestValueWalkDepth/template
// template compilation / instantiation | lisp/template_plan.go:113 | indexed loops | TestTemplatePlanGraph
// equality | lisp/lisp.go:1516 | counter, both passes | TestValueWalkDepth/equal
// macro stamping | lisp/macro.go:624 | counter | TestValueWalkDepth/stamp
// quasiquote | lisp/macro.go:777 | counter | TestValueWalkDepth/quasiquote
// sealing | lisp/seal.go:147 | explicit stack at cap | TestValueWalkDepth/seal
// macro source locations | lisp/macro.go:471 | explicit stack at cap | TestValueWalkDepth/locate
// package classification | lisp/package_admit.go:206 | counter | TestValueWalkDepth/classify
// elpspath copy | lisp/lisplib/libelpspath/path.go:68 | counter | TestPathValueDepth
// elpspath copying chains | lisp/lisplib/libelpspath/path.go:860 | remaining step count | TestPathTraversalDepth
// elpspath nested iterators | lisp/lisplib/libelpspath/query.go:125 | iterator count on Lisp admission | TestPathTraversalDepth
// reader admission | lisp/loader.go:673 | existing loaderWalkMaxDepth | TestLoadCacheDepthCapSurvivesMemoHit
// program seal classification | lisp/program.go:333 | existing reader admission | TestLoadCacheDepthCapSurvivesMemoHit
// seal fingerprint | lisp/sealfp.go:166 | existing depth / work limit | TestSealedASTFingerprintCycleTerminates
// JSON load conversion | lisp/lisplib/libjson/json.go:368 | existing encoding/json input limit | TestLoadNestingLimitIsWhereWeThinkItIs
const MaxValueDepth = 1024

// ValueDepthError reports an exhausted recursive value walk. It is an ordinary
// error; Lisp builtins wrap it as an ELPS condition. Its value is the limit.
type ValueDepthError int

func (e ValueDepthError) Error() string {
	return fmt.Sprintf("value nesting depth exceeds maximum: %d", int(e))
}

func valueDepthError() *LVal { return Error(ValueDepthError(MaxValueDepth)) }

// ValueDepthLimit returns the effective recursive value-walk limit. A nil
// runtime, nonpositive value, or value above the hard ceiling uses MaxValueDepth.
func (r *Runtime) ValueDepthLimit() int {
	if r != nil && r.MaxValueDepth > 0 && r.MaxValueDepth < MaxValueDepth {
		return r.MaxValueDepth
	}
	return MaxValueDepth
}
