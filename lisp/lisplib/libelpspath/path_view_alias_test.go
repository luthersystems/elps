// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// The aliasing battery for issue #471: the VIEW axis.
//
// path_alias_test.go's battery writes through COPIES, and says so in its own
// header.  A view is not a copy, and the difference is the whole of #471.
//
// A view is an ordinary array LVal whose cells are a WINDOW onto a longer
// backing array.  Two producers hand one out — the kernel's
// (slice 'vector ...) and this package's own rangePath.Get — and nothing in
// an LVal marks it, so every mutating builtin accepts one and cannot tell.
// The copy battery has no arm that mutates a view, so the one shape in which
// an in-place compaction is observable was outside it entirely.  #471 lived
// in exactly that unreachable place: `append(cells[:index], cells[index+1:]...)`
// shifted the view's tail left THROUGH the source's array, so
//
//	(set 'v (vector 1 2 3 4 5))
//	(set 'w (slice 'vector v 0 3))
//	(?del! w 0)
//	  view (vector 2 3)   src (vector 2 3 3 4 5)
//
// The source cannot shrink, so it was not shortened — it was scrambled, with
// 1 gone and 3 duplicated, and nothing raised.
//
// THE PROPERTY.  A mutating operation on a view may change the backing array
// only at the positions its path NAMES.  It is stated that way, rather than
// as "must not change the source", because element assignment through a view
// is legitimate and documented: (?set! w 0 97) sets the source's element 0,
// exactly as writing through any alias does.  #471 is not that — it moved
// every element after the deleted one, positions no path named.
//
// Red-proof: TestViewBatteryDetectsInPlaceCompaction runs the identical sweep
// against replicas of the pre-#471 in-place bodies and requires it to fail.
// A battery that cannot fail is not evidence — the rule this file exists
// because the copy battery followed everywhere except here.

// viewDeleteFn is the delete-through-a-view under test.  The battery runs the
// shipped operation; the red-proof substitutes a replica of the pre-#471
// in-place compaction.
type viewDeleteFn func(step *lisp.LVal, in *lisp.LVal) (*lisp.LVal, error)

// shippedViewDelete is the real thing, reached exactly as a builtin reaches
// it.
func shippedViewDelete(step *lisp.LVal, in *lisp.LVal) (*lisp.LVal, error) {
	p, err := argToStep(step)
	if err != nil {
		return nil, err
	}
	return p.DeleteMutate(in)
}

// inPlaceViewDelete replicates the two deleteMutate bodies as they stood
// before #471 was fixed: the compaction built by appending onto the input's
// own prefix.
//
// It is a replica rather than a git revert so the red-proof runs in the same
// binary as the green one.  TestInPlaceReplicaMatchesTheShippedAnswer pins
// that it differs from the shipped code only in what it writes THROUGH — the
// answer it returns is identical, which is precisely why the defect was
// silent.
func inPlaceViewDelete(step *lisp.LVal, in *lisp.LVal) (*lisp.LVal, error) {
	if err := errMutateList(in); err != nil {
		return nil, err
	}
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	n := len(cells)
	switch p := mustStep(step).(type) {
	case *indexPath:
		index, ok := resolveIndex(n, p.index)
		if !ok {
			return lisp.Nil(), nil
		}
		vals := append(cells[:index], cells[index+1:]...) //nolint:gocritic // the pre-#471 body, on purpose
		storeCells(in, vals)
		return in, nil
	case *rangePath:
		from, to, err := validateRange(n, p.from, p.to, p.implicitTo)
		if err != nil {
			return nil, err
		}
		vals := cells[:from]
		if to < n {
			vals = append(vals, cells[to:]...)
		}
		storeCells(in, vals)
		return in, nil
	default:
		return nil, fmt.Errorf("unsupported replica step: %T", p)
	}
}

func mustStep(step *lisp.LVal) Path {
	p, err := argToStep(step)
	if err != nil {
		panic(fmt.Sprintf("bad step %v: %v", step, err))
	}
	return p
}

// viewWindow is one view under test: a window onto a longer backing array,
// plus the before-snapshot of that array.
type viewWindow struct {
	view     *lisp.LVal
	backing  []*lisp.LVal
	snapshot []*lisp.LVal
	n        int
}

// newViewWindow builds an n-element view over an (n+pad)-element array.
//
// The window is clamped three-index, exactly as the kernel's slice and
// rangePath.Get produce it, so the view carries NO spare capacity.  That
// keeps this battery aimed at #471 and not at #369/#373: every write it can
// catch is within len, where no capacity clamp reaches.
func newViewWindow(n, pad int) *viewWindow {
	backing := make([]*lisp.LVal, n+pad)
	for i := range backing {
		if i < n {
			backing[i] = lisp.Int(i)
		} else {
			backing[i] = lisp.String(fmt.Sprintf("PAD%d", i-n))
		}
	}
	snapshot := make([]*lisp.LVal, len(backing))
	copy(snapshot, backing)
	return &viewWindow{
		view:     lisp.Array(nil, backing[0:n:n]),
		backing:  backing,
		snapshot: snapshot,
		n:        n,
	}
}

// changedOutside lists the backing positions that moved without being named.
//
// Identity, not value: a correct operation never rewrites a position it does
// not name, so pointer comparison is both the strictest available check and
// free of false positives from two equal-valued elements.
func (w *viewWindow) changedOutside(named map[int]bool) []int {
	var out []int
	for i := range w.snapshot {
		if w.backing[i] == w.snapshot[i] || named[i] {
			continue
		}
		out = append(out, i)
	}
	return out
}

func (w *viewWindow) render() string {
	parts := make([]string, len(w.backing))
	for i, c := range w.backing {
		parts[i] = c.String()
	}
	return "[" + strings.Join(parts, " ") + "]"
}

// viewSteps is the step set swept: every in-range index, both signs, and a
// spread of ranges including empty ones, suffixes and the whole window.
func viewSteps(n int) []*lisp.LVal {
	var steps []*lisp.LVal
	for i := range n {
		steps = append(steps, lisp.Int(i))
		steps = append(steps, lisp.Int(i-n)) // the same position, named negatively
	}
	for from := 0; from <= n; from++ {
		for to := from; to <= n; to++ {
			steps = append(steps, rangeStep(from, to))
		}
	}
	return steps
}

// runViewBattery sweeps window sizes, pad sizes and steps, applying del and
// reporting every case in which the backing array moved somewhere the step
// did not name.
func runViewBattery(t *testing.T, del viewDeleteFn) (int, []string) {
	t.Helper()
	var failures []string
	ran := 0
	for _, n := range []int{1, 2, 3, 4, 5} {
		for _, pad := range []int{0, 1, 3} {
			for _, step := range viewSteps(n) {
				w := newViewWindow(n, pad)
				named, ok := namedPositions(n, step)
				if !ok {
					continue
				}
				if _, err := del(step, w.view); err != nil {
					continue
				}
				ran++
				if bad := w.changedOutside(named); len(bad) > 0 {
					failures = append(failures, fmt.Sprintf(
						"n=%d pad=%d step=%s: backing positions %v moved and the step names %v\n  before: %s\n  after:  %s",
						n, pad, step, bad, sortedNamed(named),
						renderCells(w.snapshot), w.render()))
				}
			}
		}
	}
	return ran, failures
}

func renderCells(cells []*lisp.LVal) string {
	parts := make([]string, len(cells))
	for i, c := range cells {
		parts[i] = c.String()
	}
	return "[" + strings.Join(parts, " ") + "]"
}

// viewMutateOps are the three in-place builtins, each reached through a view.
//
// Delete is the one #471 was in, but a clean verdict about the package has to
// cover the other two as well: they write through the same toCells backing
// and the same storeCells, and nothing but this sweep would say so.
//
// The expectation differs per operation and that is the point of stating the
// property as "only the positions the path names":
//
//   - ?set! and ?nil! at an INDEX legitimately write the source's element at
//     that index.  That is ordinary aliasing, it is documented, and the
//     battery permits exactly it and nothing more.
//   - ?set! and ?nil! at a RANGE write nothing, because setMutate builds its
//     splice in a slice it allocates (fixed earlier, same class as #471).
//   - ?del! writes nothing, after this change.
var viewMutateOps = []struct {
	name string
	run  viewDeleteFn
}{
	{"?del!", shippedViewDelete},
	{"?set!", func(step *lisp.LVal, in *lisp.LVal) (*lisp.LVal, error) {
		p, err := argToStep(step)
		if err != nil {
			return nil, err
		}
		return p.SetMutate(in, lisp.Vector([]*lisp.LVal{lisp.String("W0"), lisp.String("W1")}))
	}},
	{"?nil!", func(step *lisp.LVal, in *lisp.LVal) (*lisp.LVal, error) {
		p, err := argToStep(step)
		if err != nil {
			return nil, err
		}
		return p.NilMutate(in)
	}},
}

// TestViewMutateBattery is the requirement for all three mutating operations:
// through a view, each writes only where its path points.
func TestViewMutateBattery(t *testing.T) {
	t.Parallel()

	for _, op := range viewMutateOps {
		t.Run(op.name, func(t *testing.T) {
			t.Parallel()
			ran, failures := runViewBattery(t, op.run)
			require.Greater(t, ran, 100,
				"%s: the view battery ran only %d cases", op.name, ran)
			if len(failures) > 0 {
				const sample = 8
				msgs := failures
				if len(msgs) > sample {
					msgs = append(msgs[:sample:sample],
						fmt.Sprintf("... and %d more", len(failures)-sample))
				}
				t.Errorf("%s: %d operations through a view wrote outside the step's own"+
					" positions:\n%s", op.name, len(failures), strings.Join(msgs, "\n"))
				return
			}
			t.Logf("%s: %d operations through a view, none wrote outside the step's own positions",
				op.name, ran)
		})
	}
}

// TestViewBatteryDetectsInPlaceCompaction is the red-proof.  Without it a
// clean TestViewDeleteBattery would be consistent with a battery that never
// writes anywhere observable.
func TestViewBatteryDetectsInPlaceCompaction(t *testing.T) {
	t.Parallel()

	ran, failures := runViewBattery(t, inPlaceViewDelete)
	require.Greater(t, ran, 100,
		"the red-proof deleted through only %d views", ran)
	require.NotEmpty(t, failures,
		"the pre-#471 in-place compaction must fail this battery, or a clean run "+
			"of TestViewDeleteBattery proves nothing")
	t.Logf("red-proof: the pre-#471 in-place compaction writes outside the step's "+
		"positions in %d of %d cases, e.g.\n%s", len(failures), ran, failures[0])
}

// TestInPlaceReplicaMatchesTheShippedAnswer pins the replica as a replica.
//
// The two must return the SAME value for every case in the sweep and differ
// only in what they write through — that identity is the whole reason #471
// was silent, and it is also what makes the red-proof above evidence about
// the shipped code rather than about a straw man that is merely broken.
func TestInPlaceReplicaMatchesTheShippedAnswer(t *testing.T) {
	t.Parallel()

	compared := 0
	for _, n := range []int{1, 2, 3, 4, 5} {
		for _, step := range viewSteps(n) {
			// No pad: with nothing aliased beyond the window the two bodies
			// have nowhere to differ, so any difference is in the answer.
			a := newViewWindow(n, 0)
			b := newViewWindow(n, 0)
			gotA, errA := shippedViewDelete(step, a.view)
			gotB, errB := inPlaceViewDelete(step, b.view)
			require.Equalf(t, errA == nil, errB == nil,
				"n=%d step=%s: replica and shipped disagree on error", n, step)
			if errA != nil {
				continue
			}
			compared++
			require.Equalf(t, gotA.String(), gotB.String(),
				"n=%d step=%s: the replica is not a replica — it returns a different answer",
				n, step)
		}
	}
	require.Greater(t, compared, 50, "compared only %d cases", compared)
	t.Logf("replica returns the shipped answer in all %d compared cases; it differs "+
		"only in what it writes through", compared)
}

// TestIssue471Reproduction is the issue's own example at the Go surface, both
// step forms, both producers.
func TestIssue471Reproduction(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	for _, tc := range []struct {
		name string
		step *lisp.LVal
	}{
		{"index", lisp.Int(0)},
		{"range", rangeStep(0, 1)},
	} {
		t.Run(tc.name, func(t *testing.T) {
			for _, producer := range []string{"kernel-slice", "elpspath-range"} {
				t.Run(producer, func(t *testing.T) {
					backing := make([]*lisp.LVal, 5)
					for i := range backing {
						backing[i] = lisp.Int(i + 1)
					}
					src := lisp.Array(nil, backing)

					var view *lisp.LVal
					if producer == "kernel-slice" {
						// what (slice 'vector src 0 3) hands back: a clamped
						// window onto src's own array
						view = lisp.Array(nil, backing[0:3:3])
					} else {
						got, err := Range(0, 3, false).Get(src)
						require.NoError(t, err)
						view = got
					}

					res := callBuiltin(env, BuiltinQueryDeleteMutate, view, tc.step)
					require.NotEqualf(t, lisp.LError, res.Type, "?del! errored: %v", res)

					require.Equal(t, "(vector 2 3)", res.String(),
						"the view's own answer was always correct; that is why this was silent")
					require.Equal(t, "(vector 1 2 3 4 5)", src.String(),
						"deleting through a view scrambled the source it aliases (#471)")
				})
			}
		})
	}
}

// TestViewStepsCoverBothForms is the battery's own gate: viewSteps is its
// only source of steps, so a form missing from it is a whole route the sweep
// silently cannot reach — the lesson TestEnumeratePathsCoversEveryStepForm
// records for the copy battery.
func TestViewStepsCoverBothForms(t *testing.T) {
	t.Parallel()

	forms := map[string]int{}
	negatives := 0
	for _, s := range viewSteps(4) {
		switch s.Type { //nolint:exhaustive // viewSteps emits exactly these two
		case lisp.LInt:
			forms["index"]++
			if s.Int < 0 {
				negatives++
			}
		case lisp.LSExpr:
			forms["range"]++
		}
	}
	for _, want := range []string{"index", "range"} {
		require.Positivef(t, forms[want], "viewSteps emits no %s step: %v", want, forms)
	}
	require.Positive(t, negatives,
		"viewSteps emits no negative index, so the fold in resolveIndex is unswept here")
	t.Logf("viewSteps(4): %s", stepFormSummary(forms, negatives))
}

func stepFormSummary(forms map[string]int, negatives int) string {
	return "index=" + strconv.Itoa(forms["index"]) +
		" (negative=" + strconv.Itoa(negatives) + ")" +
		" range=" + strconv.Itoa(forms["range"])
}
