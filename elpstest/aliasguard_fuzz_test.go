// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// FuzzAliasGuard is the shapes nobody enumerated.
//
// The deterministic tests in this package cover the failure modes that have
// actually shipped — and they are the better instrument for those, because
// they run on every PR and do not depend on a generator stumbling on the
// right graph.  This target exists for the rest: a random value graph with
// CONTROLLED ALIASING (two names for one payload, a payload that reaches
// itself, aliases nested inside maps, lists, vectors and captured closure
// scopes) plus a random per-fork transaction sequence, held to the same
// properties.
//
// The generator emits LISP SOURCE rather than building LVals from Go, for
// two reasons: the aliasing shapes that matter are the ones a program can
// actually produce (`(quasiquote (unquote a))` is the whole of issue #576),
// and a failing input then renders as a runnable repro that can be pasted
// into a test or the REPL.  Every witness a failure produces carries that
// program in its Repro field.
//
// Bounds: at most 8 bindings, 6 transactions and 6 forks, over a CORE
// environment rather than a full standard library.  The properties do not
// need lisplib and the fingerprint over a core environment is a tenth the
// size, which is what keeps the per-execution cost inside the shard budget.
func FuzzAliasGuard(f *testing.F) {
	for _, seed := range aliasGuardSeeds {
		f.Add(seed)
	}
	f.Fuzz(func(t *testing.T, script []byte) {
		g := generateAliasGraph(script)
		if g.program == "" {
			return
		}
		repro := g.repro()

		// Layer 2a: every registered walker rebuilds this graph with the
		// same values and the same sharing, and a write through the copy is
		// seen exactly where it is seen through the source.
		for _, w := range elpstest.Walkers() {
			got, err := elpstest.CheckWalker(w, elpstest.AliasCheck{
				NewEnv:  newFuzzEnv,
				Program: g.program,
				Repro:   repro,
			})
			if err != nil {
				// A generated program the walker legitimately cannot
				// process is not a finding.
				t.Skipf("%s: %v", w.Name, err)
			}
			for _, wit := range got {
				t.Errorf("%s", wit)
			}
		}

		if len(g.tx) == 0 {
			return
		}
		// Layer 2c: transaction isolation over the same graph.
		got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
			NewEnv:                newFuzzEnv,
			Program:               g.program,
			Tx:                    g.tx,
			ExpectNoSharedNatives: true,
			Repro:                 repro,
		})
		if err != nil {
			t.Skipf("transaction isolation: %v", err)
		}
		for _, wit := range got {
			t.Errorf("%s", wit)
		}

		// Layer 2b: the location channel over the generated graph.  No
		// WantSite: the anti-vacuity assertion belongs to the deterministic
		// test, which aims its trip point at a known body entry; here the
		// trip point is generated and any rendering will do as a baseline,
		// because the property is that stamping changes NOTHING.
		locs, err := elpstest.CheckLocations(elpstest.LocationCheck{
			NewEnv:  newFuzzEnv,
			Program: g.program + "\n" + fuzzLocationSuffix,
			Probe:   `(fuzz-outer 3)`,
			Trip:    g.trip,
			Repro:   repro,
		})
		if err != nil {
			t.Skipf("location channel: %v", err)
		}
		for _, wit := range locs {
			t.Errorf("%s", wit)
		}
	})
}

// fuzzLocationSuffix adds the closures the location sweep needs: one over a
// let scope reached directly, one reached through a container, and a caller
// that enters both.
const fuzzLocationSuffix = `
(let ([fk 2]) (set 'fuzz-inner (lambda (m) (* m fk))))
(let ([fj 5]) (set 'fuzz-held (lambda (m) (+ m fj))))
(set 'fuzz-holder (sorted-map "fn" fuzz-held))
(defun fuzz-outer (x) (+ (fuzz-inner x) 1))
`

// newFuzzEnv builds a CORE environment: the kernel's own builtins, no
// lisplib.  Everything the generator emits — sorted-map, to-bytes, vector,
// quasiquote, lambda, let — is a kernel builtin, and the fingerprint over a
// core environment is a fraction of the size of one over a loaded standard
// library, which is what keeps an execution cheap enough to fuzz.
func newFuzzEnv() (*lisp.LEnv, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return env, nil
}

// aliasGraph is one generated template plus its transactions.
type aliasGraph struct {
	program string
	tx      []string
	trip    int
	// kinds[i] is what the generator BELIEVES v<i> evaluates to, and
	// quoted[i] whether that value carries the quote flag.  Both are
	// static predictions about a program that has not run yet, and both
	// are load-bearing: kinds decides which mutation a transaction may
	// emit, and quoted decides the kind of a quasiquote OVER the binding.
	// A wrong prediction does not fail loudly -- it makes the generated
	// program raise, which the fuzz target reads as "a program the walker
	// legitimately cannot process" and SKIPS.  Exposed on the graph so
	// TestTheGeneratorsPredictionsMatchTheRuntime can check them against a
	// real evaluation.
	kinds  []varKind
	quoted []bool
}

// repro renders the generated graph and its transactions as a runnable
// program: the deliverable a bug fix gets attached to, so contamination is
// shown by example rather than described.
func (g aliasGraph) repro() string {
	var b strings.Builder
	b.WriteString(";; alias-guard repro: template\n")
	b.WriteString(strings.TrimSpace(g.program))
	b.WriteString("\n;; one transaction per fork; each runs on its own fork of the template above\n")
	for i, tx := range g.tx {
		fmt.Fprintf(&b, ";; fork %d\n%s\n", i, tx)
	}
	return b.String()
}

// script is a byte reader that never runs out: it wraps, so a short input
// still drives a whole generation and the mutator's small edits stay
// meaningful.
type script struct {
	b []byte
	i int
}

func (s *script) byte() byte {
	if len(s.b) == 0 {
		return 0
	}
	v := s.b[s.i%len(s.b)]
	s.i++
	return v
}

func (s *script) n(mod int) int {
	if mod <= 0 {
		return 0
	}
	return int(s.byte()) % mod
}

// Generation bounds.  Small on purpose: the properties are quadratic in the
// number of probe sites, and a fuzz shard's budget is wall clock.
const (
	fuzzMaxVars = 8
	fuzzMaxTx   = 6
)

// varKind is what a generated binding holds, so the transactions the
// generator emits are type-correct and a program that raises is a finding
// rather than the norm.
type varKind int

const (
	kindMap varKind = iota
	kindBytes
	kindList
	kindVector
	kindClosure
	kindInt
	// kindClosure2 is not produced by the generator; it names the
	// transaction that rebinds an existing function under a second name,
	// which the closure case is the natural target for.
	kindClosure2
	// kindQuote is a value that quasiquote WRAPPED rather than re-headed:
	// the doubly-quasiquoted form. It carries no mutation of its own, but
	// it is still a second path to the payload underneath, so the shape is
	// generated and fingerprinted -- just never offered to an operation
	// that needs a map, bytes or a vector. See the case-4 comment.
	kindQuote
)

// generateAliasGraph turns a fuzzer script into a template that builds a
// value graph with controlled aliasing, plus one transaction per fork.
func generateAliasGraph(b []byte) aliasGraph {
	if len(b) == 0 {
		return aliasGraph{}
	}
	return generateAliasGraphFrom(&script{b: b})
}

// generateAliasGraphFrom is generateAliasGraph over a script the caller
// keeps: a generator that extends this graph (parity_fuzz_test.go) reads
// its own choices from the same script AFTER this one has read its own, so
// the base graph a script produces is byte-identical whether or not it is
// extended, and every seed here keeps generating the shape its comment
// claims (TestFuzzSeedsCoverTheHistoricalShapes).
func generateAliasGraphFrom(s *script) aliasGraph {
	var (
		prog  strings.Builder
		kinds []varKind
		// quoted[i] records whether v<i>'s VALUE is already quoted, which
		// is what decides the kind of a quasiquote OF it. Not derivable
		// from kinds: a level-one (quasiquote (unquote vj)) over a map is
		// still a map, and is quoted.
		quoted []bool
	)
	nvars := 1 + s.n(fuzzMaxVars)
	// ref returns an expression naming an earlier binding, or a literal
	// when there is none.  Naming an earlier binding is what makes the
	// graph a graph: the same payload reached from several places.
	ref := func(i int) string {
		if i == 0 {
			return "1"
		}
		return fmt.Sprintf("v%d", s.n(i))
	}
	for i := range nvars {
		var expr string
		kind := kindInt
		isQuoted := false
		switch s.n(8) {
		case 0:
			expr = fmt.Sprintf(`(sorted-map "k0" %s "k1" %s)`, ref(i), ref(i))
			kind = kindMap
		case 1:
			expr = fmt.Sprintf(`(to-bytes "b%d%s")`, i, strings.Repeat("x", 1+s.n(4)))
			kind = kindBytes
		case 2:
			expr = fmt.Sprintf(`(list %s %s)`, ref(i), ref(i))
			kind = kindList
			// `list` returns a QUOTED list, which is why a quasiquote over
			// one is a wrap rather than a re-head.  Measured, not assumed:
			// TestTheGeneratorsPredictionsMatchTheRuntime.
			isQuoted = true
		case 3:
			expr = fmt.Sprintf(`(vector %s %s)`, ref(i), ref(i))
			kind = kindVector
		case 4:
			// A SECOND HEADER over an earlier binding's payload: the shape
			// of issues #576 and #585, and the one no value comparison can
			// see.
			if i == 0 {
				expr = "7"
			} else {
				j := s.n(i)
				expr = fmt.Sprintf(`(quasiquote (unquote v%d))`, j)
				// THE KIND DEPENDS ON WHETHER vj IS ALREADY QUOTED, because
				// lisp.Quote has two branches.  An UNQUOTED value is copied
				// with the flag set, so the copy keeps its type: a
				// quasiquote of a sorted map is still a sorted map, a second
				// header over the same payload, which is the #576/#585 shape
				// this case exists for.  An ALREADY-QUOTED value is instead
				// WRAPPED in an LQuote, whose type is `quote`; no map, bytes
				// or vector operation accepts one.
				//
				// Recording kinds[j] unconditionally was a generator defect.
				// It labelled the wrapper with the payload's type, the
				// generator then emitted a mutation for that type -- the
				// cycle step's (assoc! v<i> ...) on a supposed map is the
				// loudest -- the program raised, and FuzzAliasGuard read the
				// raise as "a program the walker legitimately cannot
				// process" and SKIPPED the input.  Lost coverage, silently,
				// in the one generator case whose entire purpose is the
				// alias shape.
				if quoted[j] {
					kind = kindQuote
				} else {
					kind = kinds[j]
				}
				isQuoted = true
			}
		case 5:
			// A closure over a captured scope, which Fork must copy and
			// which `copy` and detach must refuse.
			expr = fmt.Sprintf(`(let ([c%d %s]) (lambda () c%d))`, i, ref(i), i)
			kind = kindClosure
		case 6:
			// TWO closures over ONE captured scope: the environment-level
			// form of the aliasing question.  Fork must give the pair one
			// copied environment, not two, or a write through the first
			// closure's binding stops being visible to the second.
			expr = fmt.Sprintf(`(let ([c%d %s]) (list (lambda () c%d) (lambda (x) (set 'c%d x))))`, i, ref(i), i, i)
			kind = kindList
			isQuoted = true
		default:
			expr = strconv.Itoa(s.n(97))
		}
		fmt.Fprintf(&prog, "(set 'v%d %s)\n", i, expr)
		kinds = append(kinds, kind)
		quoted = append(quoted, isQuoted)
	}
	// Cycles: a map that reaches itself, directly or through the second
	// header over it.  The *LVal memo bounds the walk but not the number of
	// clones, which is how the self-referential shape of #576 and #585
	// nested one copy inside the next.
	for i := range kinds {
		if kinds[i] != kindMap || s.n(3) != 0 {
			continue
		}
		fmt.Fprintf(&prog, "(assoc! v%d \"self\" v%d)\n", i, s.n(len(kinds)))
	}
	// One binding that gathers everything, so the walk starts at a single
	// root and every payload is reachable from it.
	prog.WriteString("(set 'probe (list")
	for i := range kinds {
		fmt.Fprintf(&prog, " v%d", i)
	}
	prog.WriteString("))\n")

	g := aliasGraph{program: prog.String(), trip: 1 + s.n(12), kinds: kinds, quoted: quoted}
	ntx := s.n(fuzzMaxTx + 1)
	for i := range ntx {
		g.tx = append(g.tx, generateTx(s, kinds, i))
	}
	return g
}

// generateTx emits one transaction: a mutation that is type-correct for the
// binding it targets, so a raised error is a finding rather than the
// generator's own noise.
func generateTx(s *script, kinds []varKind, n int) string {
	if len(kinds) == 0 {
		return fmt.Sprintf("(defun tx%d-fn (x) x)", n)
	}
	i := s.n(len(kinds))
	switch kinds[i] {
	case kindMap:
		if s.n(3) == 0 {
			return fmt.Sprintf(`(dissoc! v%d "k0")`, i)
		}
		return fmt.Sprintf(`(assoc! v%d "tx%d" %d)`, i, n, s.n(97))
	case kindBytes:
		return fmt.Sprintf(`(append! v%d %d)`, i, s.n(200))
	case kindVector:
		return fmt.Sprintf(`(append! v%d %d)`, i, s.n(97))
	case kindClosure:
		if s.n(2) == 0 {
			return fmt.Sprintf("(set 'tx%d-alias v%d)", n, i)
		}
		return fmt.Sprintf(`(v%d)`, i)
	case kindClosure2:
		// Binding an EXISTING function under a second name REWRITES that
		// function's entry in the package's FID→name index.  A fork that
		// shared the index instead of copying it would rename the
		// template's function under every later transaction.
		return fmt.Sprintf("(set 'tx%d-alias v%d)", n, i)
	default:
		// A definition, which also writes the FID→name index.
		return fmt.Sprintf("(defun tx%d-fn (x) (+ x %d)) (tx%d-fn 1)", n, s.n(9), n)
	}
}

// aliasGuardSeeds are the historical shapes, committed so the corpus starts
// at the bugs rather than at random bytes.
//
// Committing them matters for a reason particular to this repository's fuzz
// setup: corpus growth is restricted to main and the nightly schedule, so
// anything the fuzzer discovers on a branch does NOT travel.  The shapes
// that have actually shipped bugs are therefore seeds, and
// TestFuzzSeedsCoverTheHistoricalShapes asserts they still generate what
// their comments claim — a generator change that stopped producing a shape
// would otherwise silently drop it.
//
// The generator is a pure function of its script, so each seed is a
// reproducible graph; the shapes are shown in that test's log output.
var aliasGuardSeeds = [][]byte{
	// #576 / #585: two names for one sorted map, and the map then reaches
	// itself THROUGH the second header — the shape whose *LVal-only memo
	// bounded the walk but not the number of clones.
	{1, 0, 4, 0, 0, 1},
	// #576's second payload kind: two names for one bytes value, with a
	// transaction growing it in place through each name.
	{1, 1, 0, 4, 0, 2, 2, 0},
	// Two closures over ONE captured scope: the environment-level form of
	// the same question, which only Fork has to answer.
	{0, 6, 0, 1, 0, 0},
	// Maps aliased into maps, with cycles.
	{2, 0, 0, 4, 0, 0, 1, 0, 3, 1},
	// An aliased bytes value nested inside a map, plus a second header over
	// that map: the alias two levels down.
	{2, 1, 0, 0, 0, 0, 4, 1, 0, 0, 2, 1},
	// A wide vector diamond: the same payload reached from many places,
	// which is what keeps the walk honest about being linear.
	{255, 255, 255, 255},
	// Degenerate inputs.
	{},
	{0},
}

// requiredSeedShapes are the historical failure modes the committed corpus
// must keep producing, each as a substring of the generated template.
//
// Two historical shapes are NOT here, and deliberately:
//
//   - Two headers over one pointer NATIVE (#576's third payload kind).
//     Lisp cannot express it — a native is bound from Go — so it is covered
//     by TestAliasGuardNativePayloadAcrossWalkers and by the
//     TestGuardDetectsDealiasedNativePayload control instead.
//   - The two-hop fork (#579).  It is not a graph shape: CheckWalker takes
//     every fork one level deeper on every input, so every seed exercises
//     it.
var requiredSeedShapes = map[string]string{
	"two names for one sorted map":      `(set 'v1 (quasiquote (unquote v0)))`,
	"a cycle closed through the alias":  `(assoc! v0 "self" v1)`,
	"two names for one bytes value":     "(set 'v0 (to-bytes",
	"two closures over one scope":       `(list (lambda () c0)`,
	"an alias nested inside a map":      `(sorted-map "k0" v0 "k1" v0)`,
	"a transaction that grows in place": "(append! v",
}

func TestFuzzSeedsCoverTheHistoricalShapes(t *testing.T) {
	t.Parallel()
	var all strings.Builder
	for i, seed := range aliasGuardSeeds {
		g := generateAliasGraph(seed)
		if g.program == "" {
			continue
		}
		t.Logf("seed %d %v:\n%s", i, seed, g.repro())
		all.WriteString(g.program)
		for _, tx := range g.tx {
			all.WriteString(tx)
			all.WriteString("\n")
		}
	}
	for name, pattern := range requiredSeedShapes {
		if !strings.Contains(all.String(), pattern) {
			t.Errorf("no committed seed generates %s (%q any more).\n"+
				"The corpus is the deterministic half of this target: a branch cannot grow the shared\n"+
				"corpus, so a shape that stops being seeded stops being fuzzed. Retune a seed.",
				name, pattern)
		}
	}
}

// kindRuntimeType is the elps type each generator kind claims its binding
// will evaluate to.  kindClosure2 is absent: it names a transaction shape,
// not a binding, and the generator never records it.
var kindRuntimeType = map[varKind]string{
	kindMap:     "sorted-map",
	kindBytes:   "bytes",
	kindList:    "list",
	kindVector:  "array",
	kindClosure: "function",
	kindInt:     "int",
	kindQuote:   "quote",
}

// TestTheGeneratorsPredictionsMatchTheRuntime is the poka-yoke for the
// generator's static predictions.
//
// generateAliasGraph decides, WITHOUT evaluating anything, what each
// binding will hold: its kind, and whether its value carries the quote
// flag.  Both decisions steer later generation — kinds picks the mutation a
// transaction emits and gates the cycle step, quoted picks the kind of a
// quasiquote over the binding — so a wrong prediction emits an operation
// the value does not accept and the program raises.
//
// A raise is not a finding: FuzzAliasGuard skips it as "a program the
// walker legitimately cannot process".  That is the failure mode this test
// exists for.  A prediction that drifts from evaluation costs coverage and
// says nothing while it does it, which is exactly the shape of bug the
// whole PR is about.  Measured here instead: every generated program must
// LOAD, and every prediction must match what the value turns out to be.
func TestTheGeneratorsPredictionsMatchTheRuntime(t *testing.T) {
	t.Parallel()
	seen := map[varKind]int{}
	checked, loadFailures := 0, 0
	for seed := range 600 {
		b := []byte{byte(seed), byte(seed >> 8), byte(seed * 7), byte(seed*31 + 11), byte(seed*13 + 3)}
		g := generateAliasGraph(b)
		if strings.TrimSpace(g.program) == "" {
			continue
		}
		env, err := newFuzzEnv()
		if err != nil {
			t.Fatal(err)
		}
		if rc := env.LoadString("f.lisp", g.program); rc.Type == lisp.LError {
			loadFailures++
			t.Errorf("seed %d: the generated TEMPLATE does not load: %v\n"+
				"Every raise here is an input FuzzAliasGuard skips rather than checks.\n"+
				"program:\n%s", seed, rc, g.program)
			continue
		}
		checked++
		for i, kind := range g.kinds {
			v := env.Get(lisp.Symbol(fmt.Sprintf("v%d", i)))
			if v == nil || v.Type == lisp.LError {
				t.Errorf("seed %d: v%d is %v\nprogram:\n%s", seed, i, v, g.program)
				continue
			}
			seen[kind]++
			if got, want := v.Type.String(), kindRuntimeType[kind]; got != want {
				t.Errorf("seed %d: v%d is predicted %s (kind %d) but evaluates to %s.\n"+
					"The generator will emit a %s operation against a %s, the program will raise,\n"+
					"and the fuzz target will SKIP the input instead of checking it.\nprogram:\n%s",
					seed, i, want, kind, got, want, got, g.program)
			}
			if got, want := v.IsQuoted(), g.quoted[i]; got != want {
				t.Errorf("seed %d: v%d is predicted quoted=%v but evaluates to quoted=%v.\n"+
					"quoted decides the kind of a quasiquote OVER this binding: an unquoted value is\n"+
					"copied and keeps its type, an already-quoted one is wrapped in an LQuote.\n"+
					"program:\n%s", seed, i, want, got, g.program)
			}
		}
		// The transactions are the other consumer of kinds, and their
		// raises are skipped by the same rule -- one level further on,
		// where CheckTransactions loads them onto a fork.
		for j, tx := range g.tx {
			if rc := env.LoadString(fmt.Sprintf("tx%d.lisp", j), tx); rc.Type == lisp.LError {
				t.Errorf("seed %d: transaction %d does not load: %v\n"+
					"generateTx emitted an operation the binding's kind does not license.\n"+
					"tx: %s\nprogram:\n%s", seed, j, rc, tx, g.program)
			}
		}
	}
	if checked == 0 {
		t.Fatal("no generated program loaded; this test checked nothing")
	}
	if loadFailures != 0 {
		t.Errorf("%d of %d generated templates failed to load", loadFailures, checked+loadFailures)
	}
	// Non-vacuity: every kind the generator can record must actually be
	// recorded, or this sweep is silently not covering it.  kindQuote is
	// the one this test was written for -- the nested quasiquote, which
	// only appears when a quasiquote falls on an already-quoted binding.
	for kind := range kindRuntimeType {
		if seen[kind] == 0 {
			t.Errorf("no generated binding had kind %d (%s) across the sweep, so this test does not\n"+
				"cover it. Widen the sweep or retune the generator.", kind, kindRuntimeType[kind])
		}
	}
	t.Logf("checked %d templates, %d bindings by kind: %v", checked, len(seen), seen)
}
