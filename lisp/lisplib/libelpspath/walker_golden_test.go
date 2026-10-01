// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

type goldenPathCase struct {
	fixture    goldenFixture
	name       string
	limit      int
	startDepth int
	work       int
	iterWork   int
	iter       bool
	stepLimit  int64
	stepBudget int64
	repeat     bool
}

func goldenPathCases() []goldenPathCase {
	fixtures := goldenFixtures()
	byName := make(map[string]goldenFixture)
	var cases []goldenPathCase
	for _, f := range fixtures {
		byName[f.name] = f
		cases = append(cases, goldenPathCase{fixture: f, name: "corpus/" + f.name})
	}
	for _, limit := range []int{-1, 0, 1, 1023, 1024} {
		for _, kind := range []string{"list", "vector", "map", "tagged", "quote"} {
			for _, n := range []int{1024, 1025} {
				f := byName[fmt.Sprintf("depth-%s-%d", kind, n)]
				cases = append(cases, goldenPathCase{fixture: f, name: fmt.Sprintf("limit-%d/%s", limit, f.name), limit: limit})
			}
		}
	}
	// A guard at depth 63 tracks the next container. A shallow cycle then repeats once.
	for _, depth := range []int{62, 63, 64, 1023, 1024} {
		for _, fixture := range []string{"self-list", "self-vector", "self-map", "self-tagged", "vector-nil-cell", "array-multi"} {
			cases = append(cases, goldenPathCase{fixture: byName[fixture], name: fmt.Sprintf("guard-depth-%d/%s", depth, fixture), startDepth: depth, limit: 1024})
		}
	}
	// Wide containers cross the memo budget before repeated visits.
	for _, kind := range []string{"list", "vector", "map"} {
		for _, width := range []int{2047, 2048, 4095, 4096} {
			f := goldenFixture{fmt.Sprintf("wide-%s-%d", kind, width), func(*goldenInput) *lisp.LVal {
				leaf := lisp.Int(1)
				cells := make([]*lisp.LVal, width)
				for i := range cells {
					cells[i] = leaf
				}
				v := lisp.SExpr(cells)
				if kind == "vector" {
					v = lisp.Vector(cells)
				}
				if kind == "map" {
					v = lisp.SortedMap()
					for i := range width {
						v.MapSetLVal(lisp.Int(i), leaf)
					}
				}
				return lisp.SExpr([]*lisp.LVal{v, v, v})
			}}
			cases = append(cases, goldenPathCase{fixture: f, name: f.name})
		}
	}
	for _, work := range []int{4095, 4096, 4097} {
		for _, fixture := range []string{"shared-small", "shared-dag-30-list", "shared-dag-30-vector", "host-map-replace-sibling"} {
			cases = append(cases, goldenPathCase{fixture: byName[fixture], name: fmt.Sprintf("memo-work-%d/%s", work, fixture), work: work, repeat: true})
		}
	}
	// Reuse a finished copy at a greater depth to exercise its stored height.
	for _, depth := range []int{1022, 1023, 1024} {
		cases = append(cases, goldenPathCase{fixture: byName["shared-small"], name: fmt.Sprintf("memo-height-%d", depth), startDepth: depth, limit: 1024, work: 4097, repeat: true})
	}
	for _, fixture := range []string{"vector", "map-string-keys", "self-vector", "array-multi", "tagged-no-cell", "host-map-error"} {
		for _, work := range []int{iterWorkAllowance - 3, iterWorkAllowance, iterWorkAllowance + 1} {
			cases = append(cases, goldenPathCase{fixture: byName[fixture], name: fmt.Sprintf("iterator-work-%d/%s", work, fixture), iter: true, iterWork: work, stepBudget: 1})
		}
		cases = append(cases, goldenPathCase{fixture: byName[fixture], name: "precedence-step-limit-budget/" + fixture, iter: true, iterWork: iterWorkAllowance, stepLimit: 1, stepBudget: 1})
		cases = append(cases, goldenPathCase{fixture: byName[fixture], name: "precedence-depth-budget/" + fixture, iter: true, iterWork: iterWorkAllowance, stepBudget: 1, startDepth: 1024, limit: 1024})
	}
	return cases
}

func TestValueWalkerGoldens(t *testing.T) {
	for _, walker := range []string{"validator", "copy"} {
		t.Run(walker, func(t *testing.T) {
			var records []goldenRecord
			for _, c := range goldenPathCases() {
				in, v := newGoldenInput(c.fixture)
				var st cycleState
				op := newCopyOp(c.limit)
				op.work, op.iterWork = c.work, c.iterWork
				st.work = c.work
				if c.iter {
					env := lisp.NewEnv(nil)
					if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
						t.Fatal(rc)
					}
					if c.stepLimit > 0 {
						if rc := lisp.WithMaxSteps(c.stepLimit)(env); rc.Type == lisp.LError {
							t.Fatal(rc)
						}
					}
					env.Runtime.SetStepBudget(c.stepBudget)
					op.env = env
					op.enterIter()
				}
				g := newCycleGuardOp(&st, op)
				g.depth = c.startDepth
				r := goldenObserve(c.name, in, func() (*lisp.LVal, []byte, error) {
					if walker == "validator" {
						err := okSimpleTypeGuarded(v, g)
						if err == nil && c.repeat {
							err = okSimpleTypeGuarded(v, g)
						}
						if err != nil {
							return nil, nil, err
						}
						return lisp.Symbol("valid"), nil, nil
					}
					out, err := copyGuarded(v, g)
					if err == nil && c.repeat {
						// The second walk shares the operation memo and enters one level deeper.
						var second cycleState
						g2 := newCycleGuardOp(&second, op)
						g2.depth = c.startDepth + 1
						out, err = copyGuarded(v, g2)
					}
					return out, nil, err
				}, map[string]error{"host": goldenHostError, "cycle": errCyclicValue, "depth": lisp.ValueDepthError(g.valueDepthLimit()), "operation-stopped": errIterStopped})
				if walker == "validator" {
					r.State = fmt.Sprintf("work=%d,valid=%d,path=%d", st.work, len(st.valid), len(st.path))
				} else {
					r.State = fmt.Sprintf("work=%d,copies=%d,iterator-work=%d,path=%d", op.work, len(op.copies), op.iterWork, len(st.path))
					if op.env != nil {
						_, used := op.env.Runtime.StepBudget()
						r.State += fmt.Sprintf(",steps=%d,budget-used=%d", op.env.Runtime.Steps(), used)
					}
					if op.stop != nil {
						r.Condition = op.stop.Str
						for _, cell := range op.stop.Cells {
							r.ConditionData = append(r.ConditionData, cell.Str)
						}
					}
				}
				records = append(records, r)
			}
			checkWalkerGolden(t, "walker-"+walker, records)
		})
	}
}

// A shared graph and its tree expansion must agree before the memo budget is crossed.
func TestValueWalkerDifferential(t *testing.T) {
	for _, vector := range []bool{false, true} {
		for depth := range 8 {
			dag := goldenDAG(depth, vector)
			tree := goldenExpand(dag)
			de, te := okSimpleType(dag), okSimpleType(tree)
			if fmt.Sprint(de) != fmt.Sprint(te) {
				t.Fatalf("depth %d: DAG %v; tree %v", depth, de, te)
			}
			dc, de := copyLVal(dag, 1024)
			tc, te := copyLVal(tree, 1024)
			if fmt.Sprint(de) != fmt.Sprint(te) {
				t.Fatalf("depth %d: DAG copy %v; tree copy %v", depth, de, te)
			}
			if !lisp.True(dc.Equal(tc)) {
				t.Fatalf("depth %d: copies differ", depth)
			}
		}
	}
}

func goldenExpand(v *lisp.LVal) *lisp.LVal {
	switch v.Type {
	case lisp.LSExpr:
		cells := make([]*lisp.LVal, len(v.Cells))
		for i, c := range v.Cells {
			cells[i] = goldenExpand(c)
		}
		return lisp.SExpr(cells)
	case lisp.LArray:
		cells := make([]*lisp.LVal, len(v.Cells[1].Cells))
		for i, c := range v.Cells[1].Cells {
			cells[i] = goldenExpand(c)
		}
		return lisp.Vector(cells)
	default:
		return v
	}
}
