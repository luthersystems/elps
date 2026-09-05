// Copyright © 2026 The ELPS authors

package elpstest

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/internal/walkraw"
	"github.com/luthersystems/elps/lisp"
)

// The alias guard: a class-level oracle for the value-graph aliasing bugs.
//
// # Why pointer comparison is not the test
//
// The harness this file generalises compared PAYLOAD IDENTITY: it walked two
// graphs and asked whether "same object" held for the same pairs of
// positions.  That is an inference about semantics.  The semantics
// themselves are "does a write through A show up at B", and that is what
// this file tests: it enumerates every mutable payload reachable from a
// value, writes a sentinel into one, and records WHICH OTHER SITES SEE IT.
// The set of sites that see a write is the alias equivalence class, measured
// rather than inferred, and a walker preserves aliasing exactly when the
// class it produces is the class it was given.
//
// The difference is not academic.  A walker could preserve pointer identity
// and still break the semantics (a map implementation that copies on read),
// or break pointer identity harmlessly (Fork mints a fresh *funData per
// function header, which nothing can observe).  Measuring the write is
// blind to both.
//
// # Per-walker contracts
//
// The four walkers do NOT promise the same thing, so one uniform oracle
// would be wrong in two directions at once — too strict for `copy`, which
// refuses closures, and too loose for the macro stamper, which does not
// produce a copy at all.  Each walker therefore declares its contract, and
// the oracle reads it:
//
//	Fork    closures IN SCOPE, backing rebuilt, full isolation required
//	copy    closures REFUSED (shared, not copied), backing rebuilt
//	Detach  closures REFUSED (rejected with an error), backing rebuilt
//	stamp   not a copier: the walk must not mutate anything reachable
//	        outside the output it allocated.  It deliberately SHARES every
//	        payload behind a pointer with its source (lisp/macro.go), so
//	        "must not alias source structure" would be the wrong rule here
//	        and is not asserted.
//
// Adding a fifth walker is a one-line registration in Walkers().
//
// # The witness
//
// A failing check that says "the graphs differ" leaves an operator to find
// the binding themselves.  Every failure here carries a Witness naming the
// walker, the probe site that was written, the affected-site set that was
// expected against the one observed, and a rendered path from a named
// binding to the payload that leaked — `a -> map entry "k" -> bytes[3]`.

// ClosureScope says what a walker does with a function value.
type ClosureScope int

const (
	// ClosuresInScope means the walker copies closures and the environments
	// they captured, so a captured binding is a probe site and two closures
	// that captured one environment must still share it afterwards.  Fork.
	ClosuresInScope ClosureScope = iota
	// ClosuresRefused means the walker does not copy a closure: it shares
	// it by reference (`copy`, lisp/copy.go) or rejects it outright
	// (detach).  The oracle asserts the refusal still happens, with
	// unchanged error text, rather than comparing a copy that is never
	// made.
	ClosuresRefused
)

// BackingPolicy says what a walker does with the storage behind an
// unchanged node.
type BackingPolicy int

const (
	// BackingRebuilt means every container in the output is fresh storage,
	// so no mutable payload is shared between input and output.
	//
	// This is where a documented exception lives rather than a failure: a
	// list or vector's Cells BACKING ARRAY is deliberately not preserved
	// across `copy` and detach — two headers that shared a backing array
	// (what cdr, rest and (slice 'list …) produce) land in the copy with
	// separate arrays.  It is stated in lisp/copy.go's doc comment and in
	// docs/func.md, pinned by TestCopyDoesNotPreserveBackingArraySharing,
	// and is the safe direction (strictly fewer accidental aliases).  The
	// guard therefore does not probe backing-array sharing at all; it
	// probes the payloads the walkers DO promise to preserve.
	BackingRebuilt BackingPolicy = iota
	// BackingPreserved means an unchanged node is shared with the input
	// rather than copied.  The macro stamper: it replaces only the nodes it
	// stamps, and shares everything else including the Cells backing array
	// (lisp/macro.go).
	BackingPreserved
)

// WalkerKind selects the shape of a walker's operation.
type WalkerKind int

const (
	// WalkerCopy takes a value and returns an independent copy of it.
	WalkerCopy WalkerKind = iota
	// WalkerFork takes an environment and returns an independent one; the
	// "copy" of a value is the fork's binding of the same name.
	WalkerFork
	// WalkerStamp takes a value and returns a rewritten one.  It is not a
	// copier: it may share with its input, but it must not MUTATE its input
	// or anything reachable from it.
	WalkerStamp
)

// Walker is one value-rebuilding primitive plus the contract the oracle
// holds it to.
type Walker struct {
	// Name identifies the walker in failures.
	Name string
	// Kind selects which of Copy and Fork is called.
	Kind WalkerKind
	// Copy rebuilds v within env.  Set for WalkerCopy and WalkerStamp.
	Copy func(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error)
	// Prepare, when set, runs before the "nothing outside the output
	// moved" baseline is taken.  A WalkerStamp needs it: driving the macro
	// stamp means DEFINING a macro first, and a definition legitimately
	// changes the environment, so it must happen on the baseline's side of
	// the line.
	Prepare func(env *lisp.LEnv, v *lisp.LVal) error
	// Fork builds an independent environment.  Set for WalkerFork.
	Fork func(env *lisp.LEnv) (*lisp.LEnv, error)
	// Closures is the walker's promise about function values.
	Closures ClosureScope
	// Refusal is the substring the walker's error must contain when
	// Closures is ClosuresRefused AND the walker rejects rather than
	// shares.  Empty means the walker shares closures instead of failing.
	Refusal string
	// Backing is the walker's promise about the storage behind an unchanged
	// node.
	Backing BackingPolicy
	// Memoises is the walker's declared payload-memo set, mirrored from the
	// production registry in package lisp so a walker registered here
	// without a memo set — or with the wrong one — fails the drift guard.
	Memoises []lisp.PayloadKind
	// Doc points at the prose that governs the walker.
	Doc string
}

// Walkers is the registry: every value-rebuilding primitive the guard
// covers.  A new walker is one row.  Nothing broken is ever registered here
// — the deliberately-broken reference walkers that prove the oracle can see
// each historical failure mode live in aliasguard_broken_test.go and are
// only ever passed to CheckWalker directly.
func Walkers() []Walker {
	return []Walker{
		{
			Name:     "Fork",
			Kind:     WalkerFork,
			Fork:     func(env *lisp.LEnv) (*lisp.LEnv, error) { return env.Fork() },
			Closures: ClosuresInScope,
			Backing:  BackingRebuilt,
			Memoises: lisp.WalkerMemoKinds("forker"),
			Doc:      "lisp/fork.go, docs/fork.md",
		},
		{
			Name:     "copy",
			Kind:     WalkerCopy,
			Copy:     copyBuiltin,
			Closures: ClosuresRefused,
			Backing:  BackingRebuilt,
			Memoises: lisp.WalkerMemoKinds("detacher"),
			Doc:      "lisp/copy.go, docs/func.md",
		},
		{
			Name:     "Detach",
			Kind:     WalkerCopy,
			Copy:     func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return walkraw.Detach(v) },
			Closures: ClosuresRefused,
			Refusal:  "cannot be detached",
			Backing:  BackingRebuilt,
			Memoises: lisp.WalkerMemoKinds("detacher"),
			Doc:      "lisp/detach.go",
		},
		{
			Name:     "macro-stamp",
			Kind:     WalkerStamp,
			Copy:     stampWalker,
			Prepare:  stampPrepare,
			Closures: ClosuresRefused,
			Backing:  BackingPreserved,
			Memoises: lisp.WalkerMemoKinds("macroStamper"),
			Doc:      "lisp/macro.go, docs/sealed-ast.md §4.5",
		},
	}
}

// copyInputSymbol is where the oracle parks the value it hands the `copy`
// builtin.  A binding rather than a quoted literal: `(copy (quote v))` would
// hand the builtin the QUOTE node, and the oracle would then be comparing a
// copy of a wrapper against the value it wraps.
const copyInputSymbol = "alias-guard-copy-input"

// copyBuiltin drives the lisp `copy` builtin through evaluation, which is
// the only way lisp code reaches it, so the oracle tests the path a program
// takes rather than a Go entry point beside it.
func copyBuiltin(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
	if rc := env.PutGlobal(lisp.Symbol(copyInputSymbol), v); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	res := env.LoadString("copy.lisp", "(copy "+copyInputSymbol+")")
	if res.Type == lisp.LError {
		return nil, lisp.GoError(res)
	}
	return res, nil
}

// ProbeKind names a class of mutable payload the oracle can write a
// sentinel into and undo exactly.
type ProbeKind string

const (
	// ProbeSortedMapEntry writes a sentinel over one sorted-map entry.
	ProbeSortedMapEntry ProbeKind = "sorted-map-entry"
	// ProbeBytesElement writes a sentinel over one byte of an LBytes
	// backing array.
	ProbeBytesElement ProbeKind = "bytes-element"
	// ProbeCapturedBinding rebinds one symbol in an environment a closure
	// captured.  Only walkers whose Closures is ClosuresInScope have these.
	ProbeCapturedBinding ProbeKind = "captured-binding"
)

// ProbeSite is one place the oracle can write a sentinel and undo it.
// Sites are enumerated in the fingerprint's walk order, so the i'th site of
// a graph and the i'th site of its copy are the same position.
type ProbeSite struct {
	// Kind is the payload class.
	Kind ProbeKind
	// Path is the rendered route from a named binding to the payload, e.g.
	//	a -> map entry "k" -> bytes[3]
	Path string

	write func(sentinel int)
	read  func() string
	reset func()
}

// String renders a site for a failure message.
func (s ProbeSite) String() string { return string(s.Kind) + " at " + s.Path }

// Witness is the evidence for one failed property: enough for a human to
// act on without re-running anything.
type Witness struct {
	// Walker is the walker whose contract was broken.
	Walker string
	// Property names the property that failed.
	Property string
	// Site is the probe site that was written, when the failure is
	// site-specific.
	Site ProbeSite
	// WantAffected and GotAffected are the alias equivalence classes: the
	// sites that saw the write in the source graph, and the ones that saw
	// it in the copy.  Rendered as paths.
	WantAffected, GotAffected []string
	// Leak is the rendered path from a named binding to the payload that
	// leaked, when there is one.
	Leak string
	// Baseline and Observed are the two renderings of a location-channel
	// failure, side by side.
	Baseline, Observed string
	// Detail is free text for failures with no site.
	Detail string
	// Repro is a runnable lisp program that rebuilds the graph and applies
	// the mutation sequence, when the graph came from the generator.
	Repro string
}

func (w Witness) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "%s: %s", w.Walker, w.Property)
	if w.Site.Path != "" {
		fmt.Fprintf(&b, "\n  wrote a sentinel at: %s", w.Site)
	}
	if w.WantAffected != nil || w.GotAffected != nil {
		fmt.Fprintf(&b, "\n  sites that saw the write in the source: %s", renderSet(w.WantAffected))
		fmt.Fprintf(&b, "\n  sites that saw the write in the copy:   %s", renderSet(w.GotAffected))
	}
	if w.Leak != "" {
		fmt.Fprintf(&b, "\n  leaked payload reachable at: %s", w.Leak)
	}
	if w.Baseline != "" || w.Observed != "" {
		fmt.Fprintf(&b, "\n  baseline:    %s\n  contaminated: %s", clip(w.Baseline), clip(w.Observed))
	}
	if w.Detail != "" {
		fmt.Fprintf(&b, "\n  %s", w.Detail)
	}
	if w.Repro != "" {
		fmt.Fprintf(&b, "\n  repro:\n%s", indentLines(w.Repro))
	}
	return b.String()
}

func renderSet(s []string) string {
	if len(s) == 0 {
		return "{}"
	}
	return "{" + strings.Join(s, ", ") + "}"
}

func indentLines(s string) string {
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i := range lines {
		lines[i] = "    " + lines[i]
	}
	return strings.Join(lines, "\n")
}

// DefaultMaxProbeSites bounds the O(n²) mutation-probe sweep.  A graph
// with more mutable payloads than this has its sweep truncated -- loudly,
// with a partial-coverage witness -- rather than its runtime exploding.
//
// Truncation is not silent, and the comment here once claimed it could
// safely be: it said the fingerprint still covered the whole graph, so a
// shortened sweep could not hide a leak.  The second review of #599
// falsified that by running code.  A hundred unique-content buffers
// followed by four IDENTICAL-content ones is 104 probe sites; a copier
// that shares the source's buffer for just the duplicates puts the defect
// past the cap, and then the fingerprints agree (equal content, per-walk
// ordinals), the source looks isolated, the copy leaks, and the oracle
// reported nothing at all.  See TestGuardAnnouncesATruncatedProbeSweep.
//
// The value is 256 rather than a tighter number for the same reason
// DefaultMaxEnvironments is 128: a cap an ordinary program trips is a
// failure that is not a bug, and that trains an embedder to raise the cap
// reflexively, which is the opposite of what a loud signal is for.  A
// sorted map of 96 int entries is 96 probe sites (pinned by
// TestASortedMapEntryIsAProbeSite, in both directions), so 96 was inside
// ordinary range.
//
// Cost, measured on a 4-core box over a list of n buffers, best of three.
// The sweep is quadratic in the site count, and the figures are PER
// WALKER — the default check runs all four, so the last column is what an
// embedder actually pays:
//
//	sites    Fork    copy   Detach   stamp    all four
//	   24   6.4ms    3.0ms   3.3ms   7.0ms      20ms
//	  256   325ms    160ms   151ms   8.8ms     645ms
//
// The fuzzer cannot reach either constant (fuzzMaxVars is 8), so the
// controls for this cap are deterministic and committed.
//
// Raise it per check with AliasCheck.MaxProbeSites.
const DefaultMaxProbeSites = 256

// AliasCheck describes one run of the oracle.
type AliasCheck struct {
	// NewEnv builds the environment.  Nil means NewForkCheckEnv.
	NewEnv func() (*lisp.LEnv, error)
	// Program is loaded into that environment.  It must bind Symbol in the
	// user package.
	Program string
	// Symbol is the binding the oracle rebuilds and probes.  Empty means
	// "probe".
	Symbol string
	// Walkers to run.  Nil means Walkers().
	Walkers []Walker
	// MaxProbeSites bounds the mutation-probe sweep, which is O(n²) in
	// the number of mutable payloads.  Zero or negative means
	// DefaultMaxProbeSites.
	//
	// Exceeding it does not silently shorten the sweep: the check reports
	// a partial-coverage witness naming this field, because a probe site
	// past the cap is never written and a leak there is invisible.
	MaxProbeSites int
	// Repro, when set, is attached to every witness: the runnable program
	// that rebuilds this graph.  The generator sets it.
	Repro string
}

// maxSites is the check's probe-site cap, defaulted.
func (c AliasCheck) maxSites() int {
	if c.MaxProbeSites > 0 {
		return c.MaxProbeSites
	}
	return DefaultMaxProbeSites
}

func (c AliasCheck) symbol() string {
	if c.Symbol == "" {
		return "probe"
	}
	return c.Symbol
}

func (c AliasCheck) build() (*lisp.LEnv, error) {
	newEnv := c.NewEnv
	if newEnv == nil {
		newEnv = NewForkCheckEnv
	}
	env, err := newEnv()
	if err != nil {
		return nil, err
	}
	if rc := env.LoadString("program.lisp", c.Program); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return env, nil
}

// RunAliasCheck runs the oracle for every walker in c and reports each
// witness with t.Errorf, so one run reports every property that failed
// rather than the first.
func RunAliasCheck(t TestingTB, c AliasCheck) {
	t.Helper()
	ws := c.Walkers
	if ws == nil {
		ws = Walkers()
	}
	for _, w := range ws {
		got, err := CheckWalker(w, c)
		if err != nil {
			t.Fatalf("%s: %v", w.Name, err)
			return
		}
		for _, wit := range got {
			t.Errorf("%s", wit)
		}
	}
}

// TestingTB is the slice of testing.TB the guard uses.  It is an interface
// so that the guard-on-the-guard tests can run the oracle and inspect its
// witnesses instead of failing the test that ran it.
type TestingTB interface {
	Helper()
	Errorf(format string, args ...any)
	Fatalf(format string, args ...any)
}

// CheckWalker runs every property the walker's contract carries and returns
// one witness per failure.  An empty slice means the walker met its
// contract.  The error return is for a harness failure — an environment
// that would not build, a program that would not load — which is not a
// finding about the walker.
func CheckWalker(w Walker, c AliasCheck) ([]Witness, error) {
	env, err := c.build()
	if err != nil {
		return nil, fmt.Errorf("build environment: %w", err)
	}
	sym := c.symbol()
	src := env.Get(lisp.Symbol(sym))
	if src == nil || src.Type == lisp.LError {
		return nil, fmt.Errorf("program must bind %s: %v", sym, src)
	}

	switch w.Kind {
	case WalkerStamp:
		return checkStamp(w, c, env, src)
	case WalkerFork:
		return checkFork(w, c, env, sym, src)
	case WalkerCopy:
		return checkCopy(w, c, env, src)
	}
	return nil, fmt.Errorf("walker %s: unknown kind %d", w.Name, w.Kind)
}

// copyOptsFor is the fingerprint configuration for comparing a value
// against a copy of it: values and sharing only.  The seal is excluded
// because `copy` clears it by contract, and package metadata is not
// reachable from a value.  A walker that SHARES closures additionally
// stops the walk at a function value — see
// FingerprintOptions.SkipCapturedEnvironments for the documented behaviour
// that would otherwise be reported as a defect.
func copyOptsFor(scope ClosureScope) FingerprintOptions {
	return FingerprintOptions{SkipCapturedEnvironments: scope == ClosuresRefused}
}

func checkCopy(w Walker, c AliasCheck, env *lisp.LEnv, src *lisp.LVal) ([]Witness, error) {
	var out []Witness
	if w.Closures == ClosuresRefused && w.Refusal != "" {
		wit, err := checkRefusal(w, c, env)
		if err != nil {
			return nil, err
		}
		out = append(out, wit...)
	}
	cp, err := w.Copy(env, src)
	if err != nil {
		// A walker that legitimately refuses this graph has already been
		// checked above; anything else is a finding.
		if w.Refusal != "" && strings.Contains(err.Error(), w.Refusal) {
			return out, nil
		}
		return nil, fmt.Errorf("walker %s: %w", w.Name, err)
	}
	return append(out, comparePair(w, c, "value", src, cp, w.Closures)...), nil
}

// checkRefusal asserts that a walker declaring ClosuresRefused with a
// Refusal string still refuses a function value, with unchanged error text.
// Comparing a copy that is never made would be vacuous; the refusal itself
// is the contract.
func checkRefusal(w Walker, c AliasCheck, env *lisp.LEnv) ([]Witness, error) {
	fn := env.Eval(lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("lambda"),
		lisp.QExpr([]*lisp.LVal{lisp.Symbol("x")}),
		lisp.Symbol("x"),
	}))
	if fn.Type != lisp.LFun {
		return nil, fmt.Errorf("walker %s: building a probe closure: %v", w.Name, fn)
	}
	_, err := w.Copy(env, fn)
	if err == nil {
		return []Witness{{
			Walker:   w.Name,
			Property: "closures are refused",
			Detail: "the walker declares ClosuresRefused with refusal text " +
				strconv.Quote(w.Refusal) + " but copied a closure without error; " +
				"a closure carries its defining environment and through it the whole runtime",
			Repro: c.Repro,
		}}, nil
	}
	if !strings.Contains(err.Error(), w.Refusal) {
		return []Witness{{
			Walker:   w.Name,
			Property: "closure refusal text is unchanged",
			Baseline: w.Refusal,
			Observed: err.Error(),
			Repro:    c.Repro,
		}}, nil
	}
	return nil, nil
}

func checkFork(w Walker, c AliasCheck, env *lisp.LEnv, sym string, src *lisp.LVal) ([]Witness, error) {
	fork, err := w.Fork(env)
	if err != nil {
		return nil, fmt.Errorf("walker %s: %w", w.Name, err)
	}
	cp := fork.Get(lisp.Symbol(sym))
	if cp == nil || cp.Type == lisp.LError {
		return nil, fmt.Errorf("walker %s: the fork does not bind %s: %v", w.Name, sym, cp)
	}
	out := comparePair(w, c, "value", src, cp, w.Closures)

	// Every fork is checked one level deeper: defence in depth against a
	// class a single hop cannot exhibit.  See ForkCheck's doc comment in
	// forkcheck.go for why the previous "issue #579" justification here was
	// false; it is stated once, there, rather than restated here.
	fork2, err := w.Fork(fork)
	if err != nil {
		return nil, fmt.Errorf("walker %s: fork of fork: %w", w.Name, err)
	}
	cp2 := fork2.Get(lisp.Symbol(sym))
	if cp2 == nil || cp2.Type == lisp.LError {
		return nil, fmt.Errorf("walker %s: the fork of a fork does not bind %s: %v", w.Name, sym, cp2)
	}
	for _, wit := range comparePair(w, c, "value (two fork hops)", cp, cp2, w.Closures) {
		wit.Property = "two fork hops: " + wit.Property
		out = append(out, wit)
	}
	// A fork check compares two pairs over the same graph, so a graph that
	// exceeds the probe cap yields the identical partial-coverage witness
	// from each. One condition, one finding.
	return dedupeTruncation(out), nil
}

// dedupeTruncation collapses repeated probe-truncation witnesses, which
// carry no per-occurrence information: they report a property of the GRAPH
// (more mutable payloads than the cap), not of a particular comparison.
func dedupeTruncation(ws []Witness) []Witness {
	seen := false
	out := ws[:0]
	for _, w := range ws {
		if strings.Contains(w.Property, "covers every mutable payload") {
			if seen {
				continue
			}
			seen = true
		}
		out = append(out, w)
	}
	return out
}

// comparePair is the heart of the oracle: fingerprint equality, then the
// site-by-site mutation probes, then isolation in both directions.
func comparePair(w Walker, c AliasCheck, what string, src, cp *lisp.LVal, scope ClosureScope) []Witness {
	var out []Witness
	opts := copyOptsFor(scope)
	fpSrc := FingerprintValue(src, opts)
	fpCp := FingerprintValue(cp, opts)
	if !fpSrc.Equal(fpCp) {
		out = append(out, Witness{
			Walker:   w.Name,
			Property: "the copy has the same values and the same sharing as the source",
			Detail:   "fingerprint diverges (sharing is part of the encoding, so this fires on a de-aliased payload as well as on a changed value)\n" + fpSrc.Diff(fpCp),
			Repro:    c.Repro,
		})
	}

	sSites, sTrunc := probeSites(src, scope, c.symbol(), c.maxSites())
	cSites, cTrunc := probeSites(cp, scope, c.symbol(), c.maxSites())
	if sTrunc || cTrunc {
		out = append(out, probeTruncationWitness(w, c, what))
	}
	if len(sSites) != len(cSites) {
		// A payload the copy split in two (or merged into one) shows up
		// here as a different NUMBER of mutable payloads, before any
		// sentinel is written.  Render both lists: the site that appears
		// on one side and not the other IS the leak.
		return append(out, Witness{
			Walker:   w.Name,
			Property: "the copy has the same mutable payloads as the source",
			Detail: fmt.Sprintf("%s: source has %d probe sites, the copy has %d",
				what, len(sSites), len(cSites)),
			WantAffected: allPaths(sSites),
			GotAffected:  allPaths(cSites),
			Leak:         firstExtraPath(sSites, cSites),
			Repro:        c.Repro,
		})
	}
	for i := range sSites {
		if sSites[i].Kind != cSites[i].Kind {
			out = append(out, Witness{
				Walker:   w.Name,
				Property: "probe sites line up",
				Detail: fmt.Sprintf("site %d is %s in the source and %s in the copy",
					i, sSites[i].Kind, cSites[i].Kind),
				Repro: c.Repro,
			})
			return out
		}
	}
	if len(sSites) == 0 {
		return out
	}

	sBase := readAll(sSites)
	cBase := readAll(cSites)
	for i := range sSites {
		wantAff, wit := affectedSet(w, c, opts, sSites, sBase, i, fpCp, cp, "the copy")
		out = append(out, wit...)
		gotAff, wit := affectedSet(w, c, opts, cSites, cBase, i, fpSrc, src, "the source")
		out = append(out, wit...)
		if !sameIndexSet(wantAff, gotAff) {
			out = append(out, Witness{
				Walker:       w.Name,
				Property:     "a write through the copy is seen exactly where it is seen through the source",
				Site:         sSites[i],
				WantAffected: paths(sSites, wantAff),
				GotAffected:  paths(cSites, gotAff),
				Leak:         leakPath(sSites, cSites, wantAff, gotAff),
				Repro:        c.Repro,
			})
		}
	}
	return out
}

// affectedSet writes a sentinel at site i, records which sites see it,
// asserts the OTHER graph did not move while it was written, and undoes the
// write exactly.
func affectedSet(w Walker, c AliasCheck, opts FingerprintOptions, sites []ProbeSite, base []string, i int, otherFP *Fingerprint, other *lisp.LVal, otherName string) ([]int, []Witness) {
	var out []Witness
	before := fingerprintOf(sites)
	sites[i].write(sentinelFor(i))
	var affected []int
	for j := range sites {
		if sites[j].read() != base[j] {
			affected = append(affected, j)
		}
	}
	if got := FingerprintValue(other, opts); !otherFP.Equal(got) {
		out = append(out, Witness{
			Walker:   w.Name,
			Property: "a write on one side is invisible on the other",
			Site:     sites[i],
			Detail:   "writing this site moved " + otherName + "\n" + otherFP.Diff(got),
			Repro:    c.Repro,
		})
	}
	sites[i].reset()
	if after := fingerprintOf(sites); before != after {
		out = append(out, Witness{
			Walker:   w.Name,
			Property: "the probe undoes its own write exactly",
			Site:     sites[i],
			Baseline: before,
			Observed: after,
			Detail:   "this is a harness defect, not a walker defect, but it invalidates every probe after it",
			Repro:    c.Repro,
		})
	}
	return affected, out
}

// checkStamp holds the macro-expansion stamper to its own contract.  It is
// not a copier: it may share with its input, and does.  What it must never
// do is WRITE into anything reachable from its input — the whole history of
// issues #274, #370, #431, #517, #582 and #583 is that walk writing into
// storage that belonged to someone else.
func checkStamp(w Walker, c AliasCheck, env *lisp.LEnv, src *lisp.LVal) ([]Witness, error) {
	if w.Prepare != nil {
		if err := w.Prepare(env, src); err != nil {
			return nil, fmt.Errorf("walker %s: prepare: %w", w.Name, err)
		}
	}
	// Everything reachable from the package bindings, not just the probe
	// value: the #582 shape is a macro body returning a GLOBAL binding, so
	// the storage at risk is reachable from a name the expansion never
	// mentions.
	before := FingerprintEnv(env, templateOpts)
	if _, err := w.Copy(env, src); err != nil {
		return nil, fmt.Errorf("walker %s: %w", w.Name, err)
	}
	after := FingerprintEnv(env, templateOpts)
	if before.Equal(after) {
		return nil, nil
	}
	return []Witness{{
		Walker:   w.Name,
		Property: "expansion mutates nothing reachable outside its own output",
		Detail:   "a binding moved while the expansion was being stamped\n" + before.Diff(after),
		Leak:     firstDivergentPath(before, after),
		Repro:    c.Repro,
	}}, nil
}

// stampPrepare binds the probe value and defines the macro whose body
// returns it.  It runs BEFORE the baseline fingerprint, because defining a
// macro legitimately changes the environment and the property under test is
// what the EXPANSION changes.
func stampPrepare(env *lisp.LEnv, v *lisp.LVal) error {
	if rc := env.PutGlobal(lisp.Symbol("stamp-probe-value"), v); rc.Type == lisp.LError {
		return lisp.GoError(rc)
	}
	if rc := env.LoadString("stamp.lisp", `(defmacro stamp-probe-macro () stamp-probe-value)`); rc.Type == lisp.LError {
		return lisp.GoError(rc)
	}
	return nil
}

// stampWalker drives the macro-expansion stamp the only way lisp code
// reaches it: by expanding a macro whose body returns the probe value.
//
// The distinction issue #586 established is respected rather than
// flattened: a GO macro's expansion is located IN PLACE before the stamp
// sees it (locateExpansionTree), because its output is fresh by contract,
// while a LISP macro's expansion is copy-on-write.  This walker drives the
// LISP macro path, which is the one that can be handed a binding — the #582
// shape, and the only one where "did it write into storage it does not own"
// is a live question.
func stampWalker(env *lisp.LEnv, _ *lisp.LVal) (*lisp.LVal, error) {
	res := env.LoadString("stamp-call.lisp", `(macroexpand-1 '(stamp-probe-macro))`)
	if res.Type == lisp.LError {
		return nil, lisp.GoError(res)
	}
	return res, nil
}

// templateOpts is the fingerprint configuration for template-level
// comparisons: everything, including the seal bit and the per-package
// metadata tables.
var templateOpts = FingerprintOptions{Seal: true, PackageMetadata: true}

func firstDivergentPath(a, b *Fingerprint) string {
	d := a.Diff(b)
	if i := strings.Index(d, "at "); i >= 0 {
		rest := d[i+3:]
		if j := strings.IndexByte(rest, ' '); j > 0 {
			return rest[:j]
		}
	}
	return ""
}

func readAll(sites []ProbeSite) []string {
	out := make([]string, len(sites))
	for i := range sites {
		out[i] = sites[i].read()
	}
	return out
}

func fingerprintOf(sites []ProbeSite) string {
	return strings.Join(readAll(sites), "\x00")
}

// sentinelFor picks a value that cannot collide with what a generated graph
// holds: the generator's integers are small and its bytes are ASCII.
func sentinelFor(i int) int { return 0x5E7719 + i }

// sameIndexSet compares two alias equivalence classes, as sorted index
// slices.
//
// DO NOT DELETE THIS ARM AS REDUNDANT.  An earlier version of this comment
// said exactly that — that the fingerprint catches every shape lisp can
// express, so this was defence in depth with no end-to-end control.  That
// was wrong, and the adversarial re-review of #599 falsified it by running
// code.
//
// The reasoning only covered DE-aliasing (a copy that splits a shared
// payload), where the fingerprint is genuinely redundant.  It missed
// OVER-aliasing at the BACKING-ARRAY level, which the fingerprint cannot
// see at all: two distinct *[]byte headers over one backing array get two
// distinct identity ordinals, so the encoding says "not shared" while the
// memory is shared.  Only writing through one and reading the other can
// tell.  The shape is ordinary —
//
//	(set 'p (to-bytes "abc"))
//	(set 'q (to-bytes "abc"))
//	(set 'probe (list p q))
//
// — and the walker defect is a plausible copy-path optimisation: intern
// equal-content buffers onto one array.  Fingerprints agree, the source is
// isolated, the copy leaks, and THIS ARM IS THE ONLY THING THAT REPORTS IT.
// Make the comparison permissive and the oracle returns zero witnesses for
// that program; see TestGuardDetectsACopyThatInternsEqualBuffers.
//
// No live elps walker does this: detach.go allocates with
// make([]byte, len(*b)) and fork.go with append([]byte(nil), *b...), both
// fresh arrays.  This is guard coverage against a change that has not
// happened, on a codebase where byte-slice sizing changes do land.
func sameIndexSet(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// allPaths renders every site's path, for a witness about the site list
// itself rather than about one write.
func allPaths(sites []ProbeSite) []string {
	out := make([]string, 0, len(sites))
	for i := range sites {
		out = append(out, sites[i].Path)
	}
	return out
}

// firstExtraPath names the first site one list has and the other does not,
// which for a de-aliased payload is the second name the copy grew.
func firstExtraPath(a, b []ProbeSite) string {
	count := map[string]int{}
	for _, s := range a {
		count[s.Path]++
	}
	for _, s := range b {
		if count[s.Path] == 0 {
			return s.Path + "  (a payload the copy split: the source reaches it under one name)"
		}
		count[s.Path]--
	}
	for _, s := range a {
		if count[s.Path] > 0 {
			return s.Path
		}
	}
	return ""
}

func paths(sites []ProbeSite, idx []int) []string {
	out := make([]string, 0, len(idx))
	for _, i := range idx {
		out = append(out, sites[i].Path)
	}
	return out
}

// leakPath renders the path to the first site where the two alias classes
// disagree: the payload that leaked, or the one that stopped being shared.
func leakPath(sSites, cSites []ProbeSite, want, got []int) string {
	in := func(s []int, v int) bool {
		for _, x := range s {
			if x == v {
				return true
			}
		}
		return false
	}
	for _, i := range want {
		if !in(got, i) {
			return sSites[i].Path + "  (shared in the source, not in the copy)"
		}
	}
	for _, i := range got {
		if !in(want, i) {
			return cSites[i].Path + "  (shared in the copy, not in the source)"
		}
	}
	return ""
}

// probeSites enumerates the mutable payloads reachable from v, in the
// fingerprint's walk order, so that the i'th site of a graph and the i'th
// site of its copy are the same position.
func probeSites(v *lisp.LVal, scope ClosureScope, root string, limit int) ([]ProbeSite, bool) {
	// Collect one site PAST the limit so "truncated" means "more sites
	// exist than the cap allows" rather than "the walk met something after
	// reaching the cap".  A graph holding exactly `limit` mutable payloads
	// was swept completely and must not be reported as partial -- that is
	// what makes the witness's remediation (raise the cap to the count you
	// measured) actually work.
	p := &siteWalker{scope: scope, seen: map[any]bool{}, limit: oneMore(limit)}
	p.path = []string{root}
	p.value(v)
	truncated := len(p.sites) > limit
	if truncated {
		p.sites = p.sites[:limit]
	}
	return p.sites, truncated
}

type siteWalker struct {
	scope ClosureScope
	seen  map[any]bool
	path  []string
	sites []ProbeSite
	limit int
}

func (p *siteWalker) here() string { return strings.Join(p.path, " -> ") }

func (p *siteWalker) push(seg string) { p.path = append(p.path, seg) }
func (p *siteWalker) pop()            { p.path = p.path[:len(p.path)-1] }

// full reports whether the walk has collected all it is going to.
//
// It does NOT decide truncation.  An earlier version set a truncated flag
// here, reasoning that "every caller asks in order to stop, so a true
// answer means a payload went unexamined" -- which is false, and the third
// review of #599 measured it: full() is asked at the top of value() BEFORE
// the callee is classified, so a value contributing no probe site at all
// (an int, an already-seen buffer, an empty list) flipped the flag merely
// by being reached after the cap-th site.  A bare 96-entry sorted map --
// the shape DefaultMaxProbeSites cites as ordinary -- then FAILED the
// guard with a partial-coverage witness whose text was untrue and whose
// remediation did not work.
//
// Truncation is decided in probeSites, by collecting one site past the
// limit and comparing counts, exactly as reachableEnvs does.
func (p *siteWalker) full() bool { return len(p.sites) >= p.limit }

func (p *siteWalker) value(v *lisp.LVal) {
	if v == nil || p.full() {
		return
	}
	if p.seen[v] {
		return
	}
	p.seen[v] = true
	switch v.Type {
	case lisp.LSortMap:
		p.sortedMap(v)
	case lisp.LBytes:
		p.bytes(v)
	case lisp.LNative:
		// Opaque: the guard cannot write inside it.  Its SHARING is
		// covered by the fingerprint's identity ordinal and by the
		// cross-fork native census (aliasguard_isolation.go).
		//
		// DELIBERATELY still keyed on the TYPE here, where the census and
		// the fingerprint were widened to key on the payload.  This arm
		// decides where the guard can WRITE a sentinel, and a payload it
		// cannot write into is not a probe site whatever header carries
		// it.  Widening the key here would do harm rather than good: an
		// LSExpr carrying an embedder annotation, and every cell view
		// once PR #602 lands, would enter the opaque arm and the walk
		// would STOP -- losing the cells below it, which are real probe
		// sites.  Measured: the probe-site count for a graph holding an
		// aliased map, aliased bytes, a native and an annotated sealed
		// node is 2 before and after the widening, and that is the right
		// answer.
	case lisp.LFun:
		if p.scope == ClosuresInScope {
			p.push("closure env")
			p.env(funraw.Env(v))
			p.pop()
		}
	default:
		for i, c := range v.Cells {
			p.push("cell " + strconv.Itoa(i))
			p.value(c)
			p.pop()
		}
	}
}

func (p *siteWalker) sortedMap(v *lisp.LVal) {
	md := v.Map()
	if md == nil || p.seen[md] {
		return
	}
	p.seen[md] = true
	for _, k := range md.Keys().Cells {
		if p.full() {
			return
		}
		key := k
		seg := "map entry " + quoteKey(key)
		p.push(seg)
		orig, _ := md.Get(key)
		path := p.here()
		p.sites = append(p.sites, ProbeSite{
			Kind:  ProbeSortedMapEntry,
			Path:  path,
			write: func(s int) { md.Set(key, lisp.Int(s)) },
			read: func() string {
				cur, _ := md.Get(key)
				return renderProbeValue(cur)
			},
			reset: func() { md.Set(key, orig) },
		})
		p.value(orig)
		p.pop()
	}
}

func (p *siteWalker) bytes(v *lisp.LVal) {
	buf, ok := v.Native.(*[]byte)
	if !ok || buf == nil || len(*buf) == 0 || p.seen[buf] {
		return
	}
	p.seen[buf] = true
	p.push("bytes[0]")
	orig := (*buf)[0]
	b := buf
	p.sites = append(p.sites, ProbeSite{
		Kind:  ProbeBytesElement,
		Path:  p.here(),
		write: func(s int) { (*b)[0] = byte(s) },
		read:  func() string { return strconv.Itoa(int((*b)[0])) },
		reset: func() { (*b)[0] = orig },
	})
	p.pop()
}

func (p *siteWalker) env(e *lisp.LEnv) {
	if e == nil || p.seen[e] || e.Parent() == nil {
		// The global boundary: its bindings are the whole standard
		// library and are not this value's state.
		return
	}
	p.seen[e] = true
	keys, vals := sortedBindings(e)
	for _, k := range keys {
		if p.full() {
			return
		}
		name, env := k, e
		orig := vals[k]
		p.push("binding " + name)
		p.sites = append(p.sites, ProbeSite{
			Kind:  ProbeCapturedBinding,
			Path:  p.here(),
			write: func(s int) { env.Put(lisp.Symbol(name), lisp.Int(s)) },
			read:  func() string { return renderProbeValue(bindingOf(env, name)) },
			reset: func() { env.Put(lisp.Symbol(name), orig) },
		})
		p.value(orig)
		p.pop()
	}
	p.push("parent")
	p.env(e.Parent())
	p.pop()
}

func bindingOf(e *lisp.LEnv, name string) *lisp.LVal {
	for k, v := range e.Bindings() {
		if k == name {
			return v
		}
	}
	return nil
}

// renderProbeValue renders a probe read.  It must distinguish the sentinel
// from whatever was there before and must not recurse into a cyclic graph,
// so it renders the value's own identity and type rather than its contents.
func renderProbeValue(v *lisp.LVal) string {
	if v == nil {
		return "<nil>"
	}
	switch v.Type {
	case lisp.LInt:
		return "int:" + strconv.Itoa(v.Int)
	case lisp.LSortMap:
		return fmt.Sprintf("map:%p", v.Map())
	case lisp.LBytes:
		return fmt.Sprintf("bytes:%p", v.Native)
	case lisp.LNative:
		return fmt.Sprintf("native:%p", v.Native)
	default:
		return fmt.Sprintf("%s:%p", v.Type, v)
	}
}

// probeTruncationWitness reports a mutation-probe sweep that stopped at
// the site cap.  It is a FAILURE, not a note: a partial sweep and a clean
// sweep are indistinguishable to a reader, and a payload past the cap is
// never written through, so a leak there is invisible.  Every truncation
// site routes through here so the wording and the remediation cannot drift
// apart -- the location channel learned that lesson one commit earlier.
func probeTruncationWitness(w Walker, c AliasCheck, what string) Witness {
	return Witness{
		Walker:   w.Name,
		Property: "the mutation-probe sweep covers every mutable payload",
		// Leak renders as "leaked payload reachable at:", so it holds a
		// path or a region, not a sentence.  There is no leaked payload
		// here -- the finding is absence of coverage.
		// Site paths elsewhere in this file are 0-based, so the first
		// site the sweep did not write is index maxSites(), not +1.
		Leak:     fmt.Sprintf("<probe sites %d..n, never written>", c.maxSites()),
		Baseline: "every mutable payload probed",
		Observed: fmt.Sprintf("the sweep stopped at MaxProbeSites=%d (%s)", c.maxSites(), what),
		Detail: fmt.Sprintf("This graph holds more than %d mutable payloads, so the sweep is PARTIAL: "+
			"a payload the copy shares with its source past the cap is never written through, and the "+
			"fingerprint cannot substitute for the write (equal contents fingerprint equally). Raise "+
			"AliasCheck.MaxProbeSites -- the sweep is O(n^2) in the site count -- or narrow the graph.",
			c.maxSites()),
		Repro: c.Repro,
	}
}

// oneMore returns limit+1, SATURATING at math.MaxInt.
//
// Both capped walks collect one item past their limit so that "truncated"
// can mean "more exist than the cap allows" rather than "the walk met
// something after reaching the cap".  Computing that as a bare limit+1
// overflows to math.MinInt when a caller passes math.MaxInt, and the
// consequences are the worst kind: the walk then finds itself instantly
// full, collects NOTHING, and reports truncated=false because zero is not
// greater than math.MaxInt.  The sweep is disarmed and says nothing.
//
// math.MaxInt is the Go idiom for "no cap", and both the doc comments and
// the truncation witness tell an embedder to RAISE the cap, so this is
// advice the guard itself hands out.  Measured before the fix, against a
// walker that genuinely leaks: 512 -> 8 witnesses, MaxInt-1 -> 8
// witnesses, MaxInt -> 0 witnesses and no truncation notice either.
//
// Saturating makes math.MaxInt behave as the unlimited cap a caller
// intends: nothing is ever full, so nothing is skipped, and the count can
// never exceed the limit so nothing is reported partial.  Pinned by
// TestAnUnlimitedCapDoesNotDisarmTheSweep.
func oneMore(limit int) int {
	if limit >= math.MaxInt {
		return math.MaxInt
	}
	return limit + 1
}

// quoteKey renders a sorted-map key for a probe path.  A STRING key's
// String() is already quoted, so passing it through strconv.Quote a second
// time renders `map entry "\"k\""` where the doc comment, the witnesses and
// the revert-proof transcripts all say `map entry "k"`.  Every other key
// type renders unquoted and wants the quoting.
func quoteKey(key *lisp.LVal) string {
	if key.Type == lisp.LString {
		return strconv.Quote(key.Str)
	}
	return strconv.Quote(key.String())
}
