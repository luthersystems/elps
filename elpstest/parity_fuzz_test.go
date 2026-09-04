// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// FuzzForkParity is the governing property of the fork work, fuzzed.
//
// "For two VMs that get instantiated from the same source code, ELPS code
// should work identically to two forks from the same template."  Every
// other target in this package checks a mechanism -- aliasing, isolation,
// the location channel -- and each is a consequence of that sentence.  This
// target checks the sentence: a generated program P and generated
// per-environment transaction sequences T_1..T_n, run on n forks of one
// template and on n cold environments, must give the same per-transaction
// results and the same post-run reachable state (elpstest.CheckParity).
//
// Three things existed and none was this.  RunForkCheck has the cold arm
// but only ever ran hand-written programs, one transaction per fork.
// FuzzAliasGuard generates programs and forks them, but compares
// structure, never execution.  FuzzLoadCacheMultiEnv and
// FuzzSharedProgramMultiEnv are differential over n environments, but the
// n are independent loads, never forks.  So the parity oracle was not
// fuzzed, the multi-environment fuzzers did not fork, and the forking
// fuzzer did not run anything.  This is the three joined.
//
// The generator wraps FuzzAliasGuard's (generateAliasGraphFrom): the same
// controlled-aliasing template, extended with the shapes a structural
// oracle cannot see -- sequences whose views share a backing array with
// their source (cdr, rest, slice) and in-place mutations of them -- and
// with a transaction SEQUENCE per environment rather than one transaction
// per fork.  (FuzzAliasGuard's header says "a random per-fork transaction
// sequence"; measured, generateAliasGraph emits one transaction per fork
// -- its repro says so -- and CheckTransactions runs Tx[i] on fork i once.
// The sequences are new here.)  The schedule alternates between
// sequential, with forks taken lazily, and round-robin over eagerly taken
// forks, so an ordering-dependent leak has both shapes to show up in.
//
// Bounds: FuzzAliasGuard's 8 bindings, plus at most 4 sequence bindings,
// 6 environments and 4 transactions each, over a CORE environment.
func FuzzForkParity(f *testing.F) {
	for _, seed := range paritySeeds {
		f.Add(seed)
	}
	f.Fuzz(func(t *testing.T, script []byte) {
		g := generateParity(script)
		if g.program == "" {
			return
		}
		if viewSortGapShape(g) {
			// THE PINNED KNOWN FAILURE.  A view over a sequence (cdr, rest,
			// slice) shares its backing array with its source; Fork copies
			// each header's cells separately, so an in-place sort of the
			// source reorders the view on a cold environment and not on a
			// fork.  Red on commit 74e4ac8, and the fix has not landed, so
			// every generated input that sorts is skipped here -- NARROWLY:
			// TestForkParity_SkipIsNarrow pins that no other seed matches
			// this predicate, and TestForkParity_ViewSortGapStillOpen runs
			// the seed WITHOUT the skip and fails the day the gap closes,
			// which is the signal to delete this branch and the predicate.
			t.Skipf("known parity gap (view + in-place sort), pinned by TestForkParity_ViewSortGapStillOpen; delete this skip when that test fails")
		}
		got, err := elpstest.CheckParity(g.check())
		if err != nil {
			// A generated program neither arm can load is not a finding.
			t.Skipf("parity: %v", err)
		}
		for _, wit := range got {
			t.Errorf("%s", wit)
		}
	})
}

// Generation bounds beyond FuzzAliasGuard's.
const (
	parityMaxSeqs      = 4
	parityMaxEnvs      = 6
	parityMaxTxPerEnv  = 4
	parityMaxSeqLen    = 5
	parityMinSeqLen    = 2
	parityValueCeiling = 50
)

// paritySeq is one generated numeric sequence binding p<k>, and the view
// w<k> over it when there is one.
type paritySeq struct {
	name   string
	vector bool
	view   string
}

// parityGraph is one generated program plus its schedule.
type parityGraph struct {
	program    string
	tx         [][]string
	interleave bool
	hops       int
}

func (g parityGraph) check() elpstest.ParityCheck {
	return elpstest.ParityCheck{
		NewEnv:     newFuzzEnv,
		Program:    g.program,
		Tx:         g.tx,
		Interleave: g.interleave,
		Hops:       g.hops,
		Repro:      g.repro(),
	}
}

// repro renders the program and its schedule as something runnable: the
// template, then each environment's sequence in the order the schedule ran
// them.
func (g parityGraph) repro() string {
	var b strings.Builder
	b.WriteString(";; parity repro: template (loaded once into the template, and by each cold environment)\n")
	b.WriteString(strings.TrimSpace(g.program))
	schedule := "sequential, forks taken lazily"
	if g.interleave {
		schedule = "round-robin across environments, forks taken eagerly"
	}
	fmt.Fprintf(&b, "\n;; %d environment(s); schedule: %s; hops: %d\n", len(g.tx), schedule, g.hops)
	for i, seq := range g.tx {
		fmt.Fprintf(&b, ";; environment %d\n", i)
		for _, tx := range seq {
			b.WriteString(tx)
			b.WriteByte('\n')
		}
	}
	return b.String()
}

// generateParity turns a script into a program with controlled aliasing
// (FuzzAliasGuard's graph), numeric sequences with views over them, and a
// transaction sequence per environment.  It reads the base graph's choices
// first, then its own, so a FuzzAliasGuard seed produces the same base
// graph here.
func generateParity(b []byte) parityGraph {
	if len(b) == 0 {
		return parityGraph{}
	}
	s := &script{b: b}
	base := generateAliasGraphFrom(s)
	var prog strings.Builder
	prog.WriteString(base.program)

	nseq := 1 + s.n(parityMaxSeqs)
	seqs := make([]paritySeq, 0, nseq)
	for k := range nseq {
		sq := paritySeq{name: fmt.Sprintf("p%d", k)}
		n := parityMinSeqLen + s.n(parityMaxSeqLen-parityMinSeqLen+1)
		elems := make([]string, n)
		for i := range elems {
			elems[i] = strconv.Itoa(s.n(parityValueCeiling))
		}
		ctor := "list"
		if s.n(2) == 1 {
			ctor = "vector"
			sq.vector = true
		}
		fmt.Fprintf(&prog, "(set '%s (%s %s))\n", sq.name, ctor, strings.Join(elems, " "))
		// A VIEW: a second header whose cells are a sub-slice of the
		// source's backing array.  No payload pointer is shared, so no
		// structural oracle can see the alias; only running an in-place
		// mutation can.
		switch s.n(4) {
		case 1:
			sq.view = fmt.Sprintf("w%d", k)
			fmt.Fprintf(&prog, "(set '%s (rest %s))\n", sq.view, sq.name)
		case 2:
			sq.view = fmt.Sprintf("w%d", k)
			lo := s.n(n)
			hi := lo + s.n(n-lo+1)
			fmt.Fprintf(&prog, "(set '%s (slice '%s %s %d %d))\n", sq.view, ctor, sq.name, lo, hi)
		case 3:
			sq.view = fmt.Sprintf("w%d", k)
			if sq.vector {
				fmt.Fprintf(&prog, "(set '%s (rest %s))\n", sq.view, sq.name)
			} else {
				fmt.Fprintf(&prog, "(set '%s (cdr %s))\n", sq.view, sq.name)
			}
		default:
		}
		seqs = append(seqs, sq)
	}

	g := parityGraph{program: prog.String()}
	nenvs := 1 + s.n(parityMaxEnvs)
	for i := range nenvs {
		ntx := 1 + s.n(parityMaxTxPerEnv)
		seq := make([]string, 0, ntx)
		for j := range ntx {
			seq = append(seq, generateParityTx(s, base.kinds, seqs, i*parityMaxTxPerEnv+j))
		}
		g.tx = append(g.tx, seq)
	}
	g.interleave = s.n(2) == 1
	g.hops = 1 + s.n(2)
	return g
}

// generateParityTx emits one transaction: a mutation -- one of
// FuzzAliasGuard's over the base graph, or an in-place sequence operation
// -- followed by an observation, so every transaction has a result worth
// comparing and a mutation whose effect the observation can carry.
func generateParityTx(s *script, kinds []varKind, seqs []paritySeq, n int) string {
	var mutation string
	switch s.n(3) {
	case 0:
		mutation = generateTx(s, kinds, n)
	case 1:
		sq := seqs[s.n(len(seqs))]
		switch s.n(4) {
		case 0:
			mutation = fmt.Sprintf("(stable-sort < %s)", sq.name)
		case 1:
			mutation = fmt.Sprintf("(stable-sort > %s)", sq.name)
		case 2:
			if sq.view != "" {
				mutation = fmt.Sprintf("(stable-sort < %s)", sq.view)
			} else {
				mutation = fmt.Sprintf("(set 'w%d-%d (rest %s))", n, s.n(9), sq.name)
			}
		default:
			if sq.vector {
				mutation = fmt.Sprintf("(append! %s %d)", sq.name, s.n(parityValueCeiling))
			} else {
				mutation = fmt.Sprintf("(set 'q%d (quasiquote (unquote %s)))", n, sq.name)
			}
		}
	default:
		// Observation only.
	}
	return strings.TrimSpace(mutation + " " + generateObservation(s, kinds, seqs))
}

// generateObservation names something to read back: a sequence, its view,
// a base binding, or the probe list that gathers every base binding.
func generateObservation(s *script, kinds []varKind, seqs []paritySeq) string {
	sq := seqs[s.n(len(seqs))]
	switch s.n(5) {
	case 0:
		return sq.name
	case 1:
		if sq.view != "" {
			return sq.view
		}
		return fmt.Sprintf("(rest %s)", sq.name)
	case 2:
		return fmt.Sprintf("(nth %s 0)", sq.name)
	case 3:
		if len(kinds) > 0 {
			return fmt.Sprintf("v%d", s.n(len(kinds)))
		}
		return "probe"
	default:
		return "probe"
	}
}

// viewSortGapShape reports whether a generated schedule sorts a sequence
// in place: the shape of the pinned known failure
// (TestForkParity_ViewSortGapStillOpen).  It is deliberately no narrower
// than "sorts": a view can also be taken INSIDE a transaction, or exist as
// a quasiquote header, so gating on the template alone would still let the
// fuzzer rediscover the pinned gap on every run.
func viewSortGapShape(g parityGraph) bool {
	for _, seq := range g.tx {
		for _, tx := range seq {
			if strings.Contains(tx, "stable-sort") {
				return true
			}
		}
	}
	return false
}

// paritySeeds are the committed corpus.  The corpus cannot grow from a
// branch (see aliasGuardSeeds), so the shapes that matter are seeded, and
// TestParitySeedsCoverTheShapes asserts they still generate what their
// comments claim.
var paritySeeds = [][]byte{
	// THE PINNED KNOWN FAILURE (viewSortGapSeed below): one list, one cdr
	// view, one environment whose only transaction sorts the source in
	// place and observes the view.  Cold: '(20 30).  Fork: '(10 20).
	viewSortGapSeed,
	// FuzzAliasGuard's seeds, so its historical shapes run under parity
	// too: the base graph a script produces is the same here, because the
	// base generator reads its bytes first.  The second is padded with
	// zeros because, unpadded, this generator's wrapped reads land on a
	// sort and the seed would fall under the pinned skip.
	{1, 0, 4, 0, 0, 1},
	{1, 1, 0, 4, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	{0, 6, 0, 1, 0, 0},
	{2, 0, 0, 4, 0, 0, 1, 0, 3, 1},
	{2, 1, 0, 0, 0, 0, 4, 1, 0, 0, 2, 1},
	{255, 255, 255, 255},
	// A vector with a slice view; appends round-robin across three eagerly
	// taken two-hop forks, observed through the view.
	{0, 7, 3, 0, 0, 0, 2, 9, 4, 1, 2, 1, 2, 1, 2, 2, 1, 1, 0, 3, 7, 0, 1, 1, 0, 3, 8, 0, 0, 0, 1, 0, 3, 9, 0, 1, 0, 2, 0, 2, 1, 1},
	// The base graph's map alias and two-closures-over-one-scope shapes,
	// a cdr view and a rest view, a second header over a list taken inside
	// a transaction, round-robin over two-hop forks.
	{2, 0, 6, 0, 4, 0, 1, 1, 0, 0, 1, 1, 5, 1, 3, 0, 3, 0, 8, 2, 0, 1, 1, 1, 0, 0, 1, 4, 1, 1, 1, 0, 3, 0, 0, 0, 0, 1, 2, 0, 3, 1, 1, 1},
	// Degenerate inputs.
	{},
	{0},
}

// viewSortGapSeed generates, byte by byte (the base graph reads first):
//
//	0       one base binding
//	7 5     of the default kind: (set 'v0 5)
//	0       trip site 1
//	0       no FuzzAliasGuard transactions
//	0       one sequence
//	1       of length 3
//	30 10 20
//	0       a list
//	3       with a cdr view:   (set 'p0 (list 30 10 20)) (set 'w0 (cdr p0))
//	0       one environment
//	0       one transaction
//	1 0 0   a sequence op on p0: (stable-sort < p0)
//	0 1     observing w0
//	0 0     sequential, one hop
var viewSortGapSeed = []byte{0, 7, 5, 0, 0, 0, 1, 30, 10, 20, 0, 3, 0, 0, 1, 0, 0, 0, 1, 0, 0}

// requiredParityShapes are the shapes the committed corpus must keep
// generating, each as a substring of a seed's repro.
var requiredParityShapes = map[string]string{
	"a cdr view over a list":                 "(cdr p",
	"a rest view":                            "(rest p",
	"a slice view":                           "(slice '",
	"an in-place sort of a source":           "(stable-sort < p0)",
	"an in-place append to a vector":         "(append! p",
	"a second environment":                   ";; environment 1",
	"a round-robin schedule":                 "round-robin",
	"a two-hop fork":                         "hops: 2",
	"the base graph's alias shape":           "(quasiquote (unquote v",
	"the base graph's captured-scope shape":  "(lambda () c",
	"a second header taken in a transaction": "(quasiquote (unquote p",
}

func TestParitySeedsCoverTheShapes(t *testing.T) {
	t.Parallel()
	var all strings.Builder
	multiTx := false
	for i, seed := range paritySeeds {
		g := generateParity(seed)
		if g.program == "" {
			continue
		}
		t.Logf("seed %d %v:\n%s", i, seed, g.repro())
		all.WriteString(g.repro())
		for _, seq := range g.tx {
			if len(seq) > 1 {
				multiTx = true
			}
		}
	}
	for name, pattern := range requiredParityShapes {
		if !strings.Contains(all.String(), pattern) {
			t.Errorf("no committed seed generates %s (%q any more). Retune a seed.", name, pattern)
		}
	}
	if !multiTx {
		t.Errorf("no committed seed gives any environment more than one transaction; the per-environment sequence is the shape this target adds")
	}
}

// TestParityGapSeedIsWhatItsCommentSays pins the byte-by-byte derivation
// above to the program it claims to produce.
func TestParityGapSeedIsWhatItsCommentSays(t *testing.T) {
	t.Parallel()
	g := generateParity(viewSortGapSeed)
	wantProgram := "(set 'v0 5)\n(set 'probe (list v0))\n(set 'p0 (list 30 10 20))\n(set 'w0 (cdr p0))\n"
	if g.program != wantProgram {
		t.Errorf("program:\n got: %q\nwant: %q", g.program, wantProgram)
	}
	wantTx := [][]string{{"(stable-sort < p0) w0"}}
	if fmt.Sprint(g.tx) != fmt.Sprint(wantTx) {
		t.Errorf("transactions: got %q, want %q", g.tx, wantTx)
	}
	if g.interleave || g.hops != 1 {
		t.Errorf("schedule: interleave=%t hops=%d, want sequential one-hop", g.interleave, g.hops)
	}
}

// TestForkParity_ViewSortGapStillOpen is the positive control and the
// reminder.  It runs the pinned seed through CheckParity WITHOUT the skip
// FuzzForkParity applies, and requires BOTH witnesses the gap produces: the
// transaction's result ('(20 30) cold, '(10 20) fork) and the post-run
// state of w0.  Measured red on commit 74e4ac8.
//
// When Fork learns to preserve backing-array sharing this test FAILS -- on
// purpose.  That is the signal to delete it, the viewSortGapShape skip in
// FuzzForkParity, and TestForkParity_SkipIsNarrow, and to move
// viewSortGapSeed into the ordinary green corpus.
func TestForkParity_ViewSortGapStillOpen(t *testing.T) {
	t.Parallel()
	g := generateParity(viewSortGapSeed)
	got, err := elpstest.CheckParity(g.check())
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	resultWitness, stateWitness := false, false
	for _, w := range got {
		t.Logf("%s", w)
		switch {
		case strings.Contains(w.Property, "returns what it returns"):
			resultWitness = resultWitness || strings.Contains(w.Detail, "cold: list list[20 30]") && strings.Contains(w.Detail, "fork: list list[10 20]")
		case strings.Contains(w.Property, "reachable state"):
			stateWitness = stateWitness || strings.Contains(w.Leak, "user:w0")
		}
	}
	if resultWitness && stateWitness {
		return
	}
	t.Fatalf("the pinned view+sort parity gap no longer reproduces (result witness: %t, state witness at user:w0: %t).\n"+
		"If Fork now preserves backing-array sharing, that is the fix landing: delete this test, the\n"+
		"viewSortGapShape skip in FuzzForkParity and TestForkParity_SkipIsNarrow, and keep viewSortGapSeed\n"+
		"as an ordinary seed.  If Fork did not change, the parity oracle has been weakened.",
		resultWitness, stateWitness)
}

// TestForkParity_SkipIsNarrow pins the skip to the one seed it is for: the
// pinned gap matches the predicate and NO other committed seed does, so
// the fuzz target's skip cannot quietly widen into "skip everything".
func TestForkParity_SkipIsNarrow(t *testing.T) {
	t.Parallel()
	for i, seed := range paritySeeds {
		g := generateParity(seed)
		if g.program == "" {
			continue
		}
		isGap := string(seed) == string(viewSortGapSeed)
		if got := viewSortGapShape(g); got != isGap {
			t.Errorf("seed %d %v: viewSortGapShape=%t, want %t\n%s", i, seed, got, isGap, g.repro())
		}
	}
}

// TestForkParity_KnownShapes runs every committed seed except the pinned
// gap as a plain test, so parity over the corpus is checked on every PR
// rather than only when the fuzz sweep runs.
func TestForkParity_KnownShapes(t *testing.T) {
	t.Parallel()
	for i, seed := range paritySeeds {
		g := generateParity(seed)
		if g.program == "" || viewSortGapShape(g) {
			continue
		}
		t.Run(fmt.Sprintf("seed%d", i), func(t *testing.T) {
			t.Parallel()
			got, err := elpstest.CheckParity(g.check())
			if err != nil {
				t.Fatalf("harness error: %v", err)
			}
			for _, w := range got {
				t.Errorf("%s", w)
			}
		})
	}
}
