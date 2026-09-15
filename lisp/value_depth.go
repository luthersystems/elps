// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"fmt"
)

// MaxValueDepth is the default limit for iterative walks over program-built
// values, independently of evaluator nesting, cycle detection and allocation
// budgets. Runtime walkers accept WithMaxValueDepth overrides of at least 1024,
// including values above this default: their stacks live on the heap.
// Rendered text has a separate fixed maxRenderDepth of 1024.
//
// Walker inventory: walker | file:line | guard | regression.
// Paths are relative to the repository root. Input and path-composition limits remain separate from value depth.
// rendered text | lisp/render_bounded.go:17 | separate 1024 counter / marker | TestValueWalkDepth/100000/format-string
// format value conversion | lisp/value_depth.go:64 | explicit stack / runtime counter | TestValueWalkDepth/{100000,3000000}/format-string
// copy / detach | lisp/detach.go:146 | explicit jobs / runtime counter | TestValueWalkDepth/{100000,3000000}/{copy,detach}
// LVal.Copy | lisp/copier.go:307 | explicit jobs / runtime counter | TestValueWalkDepth/{100000,3000000}/Copy; TestValueDepthRaisedLimit
// GoValue / GoSlice / GoMap | lisp/embed.go:50 | explicit stack / default counter | TestValueWalkDepth/{100000,3000000}/GoValue; TestGoConversionRootDepth
// JSON dump | lisp/lisplib/libjson/encode.go:313 | explicit stack / counter in both passes | TestJSONValueDepth; TestJSONValueDepthRaisedLimit
// JSON Go conversion | lisp/lisplib/libjson/json.go:717 | explicit stack / default counter | TestJSONValueDepth
// template admission (values / sealed / envs) | lisp/template.go:234 | explicit jobs / runtime counter | TestValueWalkDepth/{100000,3000000}/{template,template-sealed}
// template compilation / instantiation | lisp/template_plan.go:113 | indexed loops | TestTemplatePlanGraph; TestValueWalkDepth
// equality | lisp/lisp.go:1493 | explicit stack / runtime counter / pair memo | TestValueWalkDepth/{100000,3000000}/equal; TestAllowedValuesDepthError
// macro stamping | lisp/macro.go:595 | explicit stack / runtime counter | TestValueWalkDepth/{100000,3000000}/stamp
// quasiquote | lisp/macro.go:723 | explicit stack / runtime counter | TestValueWalkDepth/{100000,3000000}/quasiquote
// sealing | lisp/seal.go:145 | explicit stack / sealed memo (void API completes) | TestValueWalkDepth/{100000,3000000}/seal
// macro source locations | lisp/macro.go:471 | explicit stack / identity memo (void API completes) | TestValueWalkDepth/{100000,3000000}/locate
// package classification | lisp/package_admit.go:206 | explicit stack / default counter | TestValueWalkDepth/{100000,3000000}/classify
// elpspath copy | lisp/lisplib/libelpspath/path.go:71 | explicit stack / default counter | TestPathValueDepth
// elpspath copying chains | lisp/lisplib/libelpspath/path.go:933 | separate maxPathSteps (1024) | TestPathTraversalDepth
// elpspath nested iterators | lisp/lisplib/libelpspath/query.go:125 | separate maxPathSteps (1024) | TestPathTraversalDepth
// reader admission | lisp/loader.go:673 | existing loaderWalkMaxDepth | TestLoadCacheDepthCapSurvivesMemoHit
// program seal classification | lisp/program.go:333 | existing reader admission | TestLoadCacheDepthCapSurvivesMemoHit
// seal fingerprint | lisp/sealfp.go:166 | existing depth / work limit | TestSealedASTFingerprintCycleTerminates
// JSON load conversion | lisp/lisplib/libjson/json.go:368 | existing encoding/json input limit | TestLoadNestingLimitIsWhereWeThinkItIs
const MaxValueDepth = 1_000_000

// ValueDepthError reports an exhausted value walk. It is an ordinary
// error; Lisp builtins wrap it as an ELPS condition. Its value is the limit.
type ValueDepthError int

func (e ValueDepthError) Error() string {
	return fmt.Sprintf("value nesting depth exceeds maximum: %d", int(e))
}

func valueDepthError() *LVal { return Error(ValueDepthError(MaxValueDepth)) }

// ValueDepthLimit returns the effective iterative value-walk limit. A nil
// runtime or unset/invalid field uses MaxValueDepth. WithMaxValueDepth rejects
// invalid settings; a valid override may exceed the default.
func (r *Runtime) ValueDepthLimit() int {
	if r != nil && r.MaxValueDepth >= 1024 {
		return r.MaxValueDepth
	}
	return MaxValueDepth
}

// checkValueDepth validates the conversion graph after bounded text rendering succeeds.
// A visited node closes cycles; the counter bounds acyclic descent.
func checkValueDepth(v *LVal, limit int, ctx context.Context) error {
	if ctx != nil {
		if err := ctx.Err(); err != nil {
			return err
		}
	}
	if v == nil {
		return nil
	}
	if limit <= 0 {
		return ValueDepthError(limit)
	}
	if len(v.Cells) == 0 && v.Type != LSortMap {
		return nil
	}
	return checkContainerDepth(v, limit, ctx)
}

func checkContainerDepth(v *LVal, limit int, ctx context.Context) error {
	type frame struct {
		cells, entries, pair []*LVal
	}
	// Only ancestors need cursors; only containers need identity tracking.
	// Keep the visited set for DAGs as well as cycles, so shared expansions
	// remain linear in graph size. Scalar siblings need neither structure.
	pending := make([]frame, 0, 16)
	seen := make(map[*LVal]bool)
	visits := 0
walk:
	for {
		visits++
		if visits%4096 == 0 && ctx != nil {
			if err := ctx.Err(); err != nil {
				return err
			}
		}
		if v != nil {
			container := len(v.Cells) > 0 || v.Type == LSortMap
			if !container || !seen[v] {
				if len(pending) >= limit {
					return ValueDepthError(limit)
				}
				if container {
					seen[v] = true
					f := frame{cells: v.Cells}
					if v.Type == LSortMap {
						entries := sortedMapEntries(v.Map())
						if entries.Type == LError {
							return GoError(entries)
						}
						f.entries = entries.Cells
					}
					pending = append(pending, f)
				}
			}
		}
		for len(pending) > 0 {
			f := &pending[len(pending)-1]
			// Preserve the previous last-in-first-out order: map pairs and
			// their values first, then the header's cells, all in reverse.
			for len(f.pair) == 0 && len(f.entries) > 0 {
				i := len(f.entries) - 1
				f.pair, f.entries = f.entries[i].Cells, f.entries[:i]
			}
			if n := len(f.pair); n > 0 {
				v, f.pair = f.pair[n-1], f.pair[:n-1]
				continue walk
			}
			if n := len(f.cells); n > 0 {
				v, f.cells = f.cells[n-1], f.cells[:n-1]
				continue walk
			}
			*f = frame{}
			pending = pending[:len(pending)-1]
		}
		return nil
	}
}
