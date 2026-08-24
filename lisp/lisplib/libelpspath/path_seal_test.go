// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// This file covers issue #392: the READ-ONLY ?-getter laundering the sealed
// flag off a program literal.
//
// rangePath.Get takes a two-index slice of the input sequence's LIVE cell
// backing and wraps it in a fresh LVal header. lisp.SExpr/lisp.Array mint
// that header with sealed == false. Every kernel copy-on-write site keys off
// list.sealed (lisp/seal.go), so once the flag is gone none of them fire and
// the shared parse is mutated in place for every environment that evaluates
// it -- the luthersystems/substrate#378 class, reached through a getter.
//
// The kernel does the identical aliasing correctly in builtinSlice,
// builtinCdr and builtinRest: it copies the source's flag onto the new
// header, because "a two-index slice keeps the original backing array (and
// its spare capacity), so a sealed input's constraint travels with the
// intermediate value".
//
// Three layers of coverage here, weakest to strongest:
//
//	(1) TestGetSealProvenance -- a structural oracle over the whole Path
//	    surface: for every non-mutating op on a sealed source, any result
//	    whose cell storage OVERLAPS the source's backing must itself be
//	    sealed. It does not need anyone to have anticipated which op leaks.
//	(2) TestGetDoesNotCorruptLiteral -- the four confirmed lisp-level
//	    reproductions, each asserting the parsed AST is byte-identical
//	    after evaluation.
//	(3) TestGetCrossEnvironment -- one parse, two fresh LEnvs, each
//	    reporting the literal BEFORE it mutates anything. This is the
//	    property that actually matters for a host with a parse cache.

// sealLaunderVariant is one confirmed reproduction from issue #392.
//
// Each is a defun whose body is a quoted list literal, a read of that
// literal, and then an expression that queries a window out of it with
// elpspath:? and hands the window to a kernel builtin whose copy-on-write
// guard keys off the sealed flag.
type sealLaunderVariant struct {
	name string
	// lit is the literal in the defun body.
	lit string
	// mutate is the expression that queries a window and mutates it.
	mutate string
	// want is the rendering of the value mutate must evaluate to in the
	// RUNTIME-LIST control program. Pinned as a literal string rather than
	// re-derived, so a change that quietly altered WHAT ? returns cannot
	// satisfy the test by moving the target. On the literal program the
	// mutate expression is instead refused with modify-literal-error
	// (issue #378) — the refusal firing is itself the proof that the seal
	// travelled through the ?-getter's window.
	want string
}

// sealLaunderVariants are the four variants confirmed on the unfixed tree.
// Every one of them printed a CHANGED literal, and every one of them
// panicked a -tags elpscheck binary with "sealed program AST mutated after
// parse".
var sealLaunderVariants = []sealLaunderVariant{
	{
		name:   "range+append-vector",
		lit:    `'(10 20 30 40)`,
		mutate: `(append 'vector (elpspath:? (cfg) '(range 0 2)) 999)`,
		want:   `(vector 10 20 999)`,
	},
	{
		name:   "range+stable-sort",
		lit:    `'(10 20 30 40)`,
		mutate: `(stable-sort '> (elpspath:? (cfg) '(range 0 3)))`,
		want:   `(30 20 10)`,
	},
	{
		name:   "range+slice-vector+append!",
		lit:    `'(1 2 3 4)`,
		mutate: `(append! (slice 'vector (elpspath:? (cfg) '(range 0 2)) 0 2) 777)`,
		want:   `(vector 1 2 777)`,
	},
	{
		// The window is reached through a RUNTIME array that merely holds
		// the literal: (vector (cfg)) is unsealed, indexPath.Get hands back
		// the sealed element, and rangePath.Get launders it from there. A
		// fix that only looked at the outermost value would miss this.
		name:   "nested-under-runtime-array",
		lit:    `'(10 20 30 40)`,
		mutate: `(stable-sort '> (elpspath:? (vector (cfg)) 0 '(range 0 3)))`,
		want:   `(30 20 10)`,
	},
}

// program renders a variant as three top-level expressions:
//
//	0: the defun holding the literal
//	1: a read of the literal, evaluated BEFORE anything mutates
//	2: the mutating expression
//
// Expression 1 is what the cross-environment test compares between runs.
func (v sealLaunderVariant) program() string {
	return "(defun cfg () " + v.lit + ")\n(cfg)\n" + v.mutate + "\n"
}

// runtimeControl is the same program with the literal replaced by an
// equivalent list built at runtime. A runtime list is not sealed, so no
// copy-on-write fires and the aliasing path runs unguarded -- which is the
// documented behaviour for runtime data and must not change. Its value for
// the mutating expression is the reference answer: fixing the seal leak must
// not change WHAT ? returns, only whose storage it is allowed to share.
func (v sealLaunderVariant) runtimeControl() string {
	lit := v.lit
	// '(10 20 30 40) -> (list 10 20 30 40)
	return "(defun cfg () (list " + lit[2:len(lit)-1] + "))\n(cfg)\n" + v.mutate + "\n"
}

// evalSnapshot evaluates exprs in order, rendering each result to a string
// IMMEDIATELY after that expression is evaluated.
//
// Rendering has to happen inline. The values these programs return are
// pointers into the shared parse, so a caller that collected the *LVal and
// stringified it after the whole run would read post-mutation state and see
// nothing wrong. Snapshotting expression 1 before expression 2 executes is
// exactly what makes the cross-environment divergence observable.
//
// Go panics are recovered so a corrupted tree can still be inspected.
func evalSnapshot(env *lisp.LEnv, exprs []*lisp.LVal) (snaps []string, panicked interface{}) {
	defer func() { panicked = recover() }()
	for _, e := range exprs {
		snaps = append(snaps, env.EvalContext(context.Background(), e).String())
	}
	return snaps, nil
}

// TestGetDoesNotCorruptLiteral is the direct regression test for the four
// confirmed variants of #392.
//
// The assertion is on the AST fingerprint rather than on a printed value:
// fpAST digests the parsed expressions themselves, so it fires on ANY
// in-place write to the shared tree, whether or not the program happens to
// read the literal back afterwards.
//
// It also pins the query's ANSWER against the runtime-list control, so the
// fix cannot pass by making ? return something different.
func TestGetDoesNotCorruptLiteral(t *testing.T) {
	t.Parallel()
	for _, tc := range sealLaunderVariants {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			exprs := parseOnce(t, tc.program())
			pristine := fpAST(exprs)

			env := newMutateTestEnv(t)
			snaps, panicked := evalSnapshot(env, exprs)
			if panicked != nil {
				t.Fatalf("evaluation panicked: %v", panicked)
			}

			if got := fpAST(exprs); got != pristine {
				t.Errorf("sealed program literal was mutated in place by a read-only query\n"+
					"  program:        %s\n"+
					"  literal before: %s\n"+
					"  literal after:  %s",
					tc.mutate, snaps[1], evalLiteral(t, env))
			}

			// On the literal program the mutate expression must be REFUSED
			// with the modify-literal-error condition (issue #378): the
			// guard firing through the ?-getter's window is the proof that
			// the seal was not laundered off. Before the flip this asserted
			// the copy-on-write answer instead.
			if !strings.Contains(snaps[2], lisp.CondModifyLiteral) {
				t.Errorf("mutating a window onto the literal was not refused with %s: got %s",
					lisp.CondModifyLiteral, snaps[2])
			}

			// The runtime-list control keeps the documented unguarded
			// behaviour for runtime data, which must not change.
			ctrl := newMutateTestEnv(t)
			ctrlSnaps, ctrlPanic := evalSnapshot(ctrl, parseOnce(t, tc.runtimeControl()))
			if ctrlPanic != nil {
				t.Fatalf("runtime-list control panicked: %v", ctrlPanic)
			}
			if ctrlSnaps[2] != tc.want {
				t.Errorf("runtime-list control answer changed: got %s, want %s", ctrlSnaps[2], tc.want)
			}
		})
	}
}

// TestGetCrossEnvironment is the property that actually matters: one parse,
// many environments.
//
// A host with a parse cache (substrate's, the motivating consumer in
// lisp/seal.go) hands the SAME []*lisp.LVal to every warm environment. A
// mutation of that tree in one environment is a silent, permanent,
// cross-transaction change to the program every other environment runs. The
// test models it exactly: parse once, evaluate the identical expressions in
// two fresh LEnvs, and compare what each one saw when it read the literal
// before touching anything.
//
// On the unfixed tree the two runs disagree -- run 2 opens on the wreckage
// run 1 left behind.
func TestGetCrossEnvironment(t *testing.T) {
	t.Parallel()
	for _, tc := range sealLaunderVariants {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			// ONE parse, shared by both environments.
			exprs := parseOnce(t, tc.program())

			snaps1, panic1 := evalSnapshot(newMutateTestEnv(t), exprs)
			if panic1 != nil {
				t.Fatalf("run 1 panicked: %v", panic1)
			}
			snaps2, panic2 := evalSnapshot(newMutateTestEnv(t), exprs)
			if panic2 != nil {
				t.Fatalf("run 2 panicked: %v", panic2)
			}

			if snaps1[1] != snaps2[1] {
				t.Errorf("DIVERGE %s: the same parse produced different literals in two environments\n"+
					"  run1=%s\n  run2=%s", tc.name, snaps1[1], snaps2[1])
			}
			if snaps1[2] != snaps2[2] {
				t.Errorf("DIVERGE %s: the same parse produced different answers in two environments\n"+
					"  run1=%s\n  run2=%s", tc.name, snaps1[2], snaps2[2])
			}
		})
	}
}

// evalLiteral reads the literal back out of env, for failure messages.
func evalLiteral(t *testing.T, env *lisp.LEnv) string {
	t.Helper()
	return evalString(t, env, "(cfg)")
}

// evalString evaluates src in env and renders the result.
func evalString(t *testing.T, env *lisp.LEnv, src string) string {
	t.Helper()
	v := env.LoadStringContext(context.Background(), "expected.lisp", src)
	if v.Type == lisp.LError {
		t.Fatalf("eval %q: %v", src, v)
	}
	return v.String()
}

// ---------------------------------------------------------------------------
// The structural oracle
// ---------------------------------------------------------------------------

// cellsOf returns the cell storage a sequence LVal actually holds its
// elements in. An array is [dims, data] and keeps them one level down; a
// list holds them directly. Anything else has no sequence storage.
func cellsOf(v *lisp.LVal) []*lisp.LVal {
	switch {
	case v == nil:
		return nil
	case v.Type == lisp.LArray:
		if len(v.Cells) < 2 {
			return nil
		}
		return v.Cells[1].Cells
	case v.Type == lisp.LSExpr:
		return v.Cells
	default:
		return nil
	}
}

// sharesBacking reports whether a and b are windows onto the same backing
// array.
//
// Comparing len-bounded slices is not enough: the whole defect is a
// TWO-INDEX slice, cells[from:to], whose first element may sit in the middle
// of the source and whose spare capacity runs off the end of its own length.
// Both slices are therefore re-expanded to their full capacity before the
// element addresses are compared, and the comparison runs in both directions
// because either one may be the sub-window of the other.
func sharesBacking(a, b []*lisp.LVal) bool {
	if len(a) == 0 || len(b) == 0 {
		return false
	}
	af, bf := a[:cap(a)], b[:cap(b)]
	for i := range af {
		if &af[i] == &bf[0] {
			return true
		}
	}
	for i := range bf {
		if &bf[i] == &af[0] {
			return true
		}
	}
	return false
}

// TestSharesBackingOracle is the anti-vacuity check for sharesBacking. An
// oracle that never reported an overlap would make TestGetSealProvenance
// pass on any tree at all, including the corrupting one.
func TestSharesBackingOracle(t *testing.T) {
	t.Parallel()
	cells := []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4)}
	fresh := []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4)}

	// The exact shape rangePath.Get produces: a two-index window starting
	// past the front of the source.
	if !sharesBacking(cells, cells[1:3]) {
		t.Error("a two-index window was not detected as sharing its source's backing")
	}
	if !sharesBacking(cells[1:3], cells) {
		t.Error("overlap detection is not symmetric")
	}
	if !sharesBacking(cells, cells[:2]) {
		t.Error("a prefix window was not detected as sharing its source's backing")
	}
	if sharesBacking(cells, fresh) {
		t.Error("two independent slices were reported as sharing backing")
	}
	if sharesBacking(cells, nil) || sharesBacking(nil, cells) {
		t.Error("an empty slice was reported as sharing backing")
	}
}

// sealedSource parses src, seals it as the parser would, and returns the
// quoted literal inside it.
//
// The value comes from a real parse rather than a hand-built tree so the
// test is pinned to what actually reaches this package at runtime, sealed
// flags and cell layout included.
func sealedSource(t *testing.T, src string) *lisp.LVal {
	t.Helper()
	exprs := parseOnce(t, src)
	if len(exprs) != 1 {
		t.Fatalf("parse %q: got %d expressions, want 1", src, len(exprs))
	}
	v := exprs[0]
	// The parser wraps a literal in a quote form; unwrap to the list.
	for v.Type == lisp.LQuote && len(v.Cells) == 1 {
		v = v.Cells[0]
	}
	if !v.IsSealed() {
		t.Fatalf("anti-vacuity: the parsed literal %q is not sealed", src)
	}
	if v.Type != lisp.LSExpr {
		t.Fatalf("parsed literal %q is a %v, want a list", src, v.Type)
	}
	return v
}

// TestGetSealProvenance is the package-wide audit, expressed as an
// executable rule rather than a list of known-bad call sites:
//
//	any LVal a Path op mints over storage BORROWED from a sealed source
//	must itself report IsSealed.
//
// It runs every non-mutating op of every path kind against a sealed list
// literal and checks the rule on the result. An op that satisfies it by
// copying (Set/Delete/Nil, which work on a private copy) passes because
// there is no overlap to inherit; an op that satisfies it by propagating
// (Get) passes because the flag travelled. An op that does NEITHER -- which
// is exactly what rangePath.Get did -- fails, and would have failed the day
// it was written, without anyone knowing which builtin downstream was going
// to write through the hole.
//
// The mutating (*Mutate) ops are excluded on purpose: errMutateList rejects
// them on lists outright (substrate#378, TestMutateListRejected), so they
// never mint anything over a sealed literal's backing.
func TestGetSealProvenance(t *testing.T) {
	t.Parallel()

	paths := []struct {
		name string
		path Path
	}{
		{"index", Root(Chain(Index(1)))},
		{"index-negative", Root(Chain(Index(-1)))},
		{"range", Root(Chain(Range(0, 2, false)))},
		{"range-full", Root(Chain(Range(0, 4, false)))},
		{"range-implicit-to", Root(Chain(Range(1, 0, true)))},
		{"range-suffix", Root(Chain(Range(2, 4, false)))},
		{"iter", Root(Chain(Iter()))},
		{"root-empty", Root(Chain())},
	}
	ops := []struct {
		name string
		call func(Path, *lisp.LVal) (*lisp.LVal, error)
	}{
		{"Get", func(p Path, in *lisp.LVal) (*lisp.LVal, error) { return p.Get(in) }},
		{"Set", func(p Path, in *lisp.LVal) (*lisp.LVal, error) {
			return p.Set(in, lisp.Vector([]*lisp.LVal{lisp.Int(7), lisp.Int(8)}))
		}},
		{"Delete", func(p Path, in *lisp.LVal) (*lisp.LVal, error) { return p.Delete(in) }},
		{"Nil", func(p Path, in *lisp.LVal) (*lisp.LVal, error) { return p.Nil(in) }},
	}

	for _, pc := range paths {
		for _, oc := range ops {
			t.Run(pc.name+"/"+oc.name, func(t *testing.T) {
				t.Parallel()
				// A fresh parse per subtest: the ops below are allowed to
				// write to a private copy, and sharing one source across
				// subtests would let a real defect hide behind another
				// subtest's copy.
				src := sealedSource(t, `'(10 20 30 40)`)
				before := fpAST([]*lisp.LVal{src})

				out, err := oc.call(pc.path, src)
				if err != nil {
					// A path that does not apply is not interesting here;
					// what matters is that it minted nothing.
					t.Skipf("%s: %v", oc.name, err)
				}

				if after := fpAST([]*lisp.LVal{src}); after != before {
					t.Fatalf("%s.%s mutated the sealed source in place", pc.name, oc.name)
				}
				if sharesBacking(cellsOf(src), cellsOf(out)) && !out.IsSealed() {
					t.Errorf("%s.%s minted an UNSEALED value over the sealed source's backing:\n"+
						"  source: %s\n  result: %s\n"+
						"the kernel's copy-on-write sites key off the sealed flag, so this "+
						"result is a mutable window onto a shared program literal",
						pc.name, oc.name, src, out)
				}
			})
		}
	}
}

// TestGetUnsealedStaysUnsealed is the other half of the set-point. The fix
// must attach the constraint to values that share SEALED storage and to
// nothing else: a window onto ordinary runtime data has to stay an ordinary
// mutable window, or every mutating builtin downstream starts copying data
// it was free to write.
func TestGetUnsealedStaysUnsealed(t *testing.T) {
	t.Parallel()
	runtimeList := lisp.QExpr([]*lisp.LVal{lisp.Int(10), lisp.Int(20), lisp.Int(30), lisp.Int(40)})
	if runtimeList.IsSealed() {
		t.Fatal("anti-vacuity: a hand-built list is sealed")
	}
	out, err := Range(0, 2, false).Get(runtimeList)
	if err != nil {
		t.Fatalf("Get: %v", err)
	}
	if out.IsSealed() {
		t.Error("a window onto unsealed runtime data came back sealed")
	}
	if !sharesBacking(runtimeList.Cells, out.Cells) {
		t.Error("the window no longer aliases its source: ? stopped being an O(1) view")
	}
}

// TestGetSealedWindowIsAView pins the chosen fix, propagation, against the
// alternative that was considered and rejected (returning a copy). A copy
// would also be correct; it would also be a silently different cost model
// for ?, which substrate runs on the transaction path. If someone later
// swaps propagation for copying, this fails and they have to say so.
func TestGetSealedWindowIsAView(t *testing.T) {
	t.Parallel()
	src := sealedSource(t, `'(10 20 30 40)`)
	out, err := Range(0, 2, false).Get(src)
	if err != nil {
		t.Fatalf("Get: %v", err)
	}
	if !out.IsSealed() {
		t.Fatal("the window onto a sealed literal is not sealed")
	}
	if !sharesBacking(src.Cells, out.Cells) {
		t.Error("? on a sealed literal now allocates a copy; " +
			"that is a deliberate change to a hot path, not a refactor")
	}
}
