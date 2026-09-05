// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
)

// The trip point at which a countdown cancellation lands exactly at bb's
// body entry in locationProgram, and the location the resulting error must
// report: the lambda's DEFINITION site (line 3, column 23), not its
// defining environment's live register (line 3, column 14) and not the call
// site.  The two differ, which is what makes the definition-site property
// observable at all — see PR #578's review finding F1 and lisp/funloc_test.go.
const (
	bodyEntryTrip = 8
	bodyEntrySite = "program.lisp:3:23"
)

// ---------------------------------------------------------------------------
// The historical shapes, run against every registered walker.
// ---------------------------------------------------------------------------

// Issue #598: the alias oracle used to drive Fork alone, so the identical
// de-aliasing defect stayed live in the detach/copy walker for the whole
// time the Fork guard was green (issue #585).  Every registered walker now
// meets the same contract over the same graph.
func TestAliasGuardOverEveryWalker(t *testing.T) {
	t.Parallel()
	elpstest.RunAliasCheck(t, elpstest.AliasCheck{Program: aliasProgram})
}

// The third payload kind, which lisp cannot express: two headers over one
// native handle.  Bound from Go, as issue #576's own regression test does.
func TestAliasGuardNativePayloadAcrossWalkers(t *testing.T) {
	t.Parallel()
	elpstest.RunAliasCheck(t, elpstest.AliasCheck{
		NewEnv:  nativeAliasEnv,
		Program: nativeAliasProgram,
	})
}

// A closure over a captured scope, reached both directly and through a
// container: Fork copies the captured environment, so a captured binding is
// a probe site, and two closures that captured one environment must still
// share it after the fork.
func TestAliasGuardClosureState(t *testing.T) {
	t.Parallel()
	elpstest.RunAliasCheck(t, elpstest.AliasCheck{
		Program: `
(let ([outer (sorted-map "n" 0)])
  (set 'bump! (lambda () (assoc! outer "n" 1)))
  (set 'peek (lambda () (get outer "n"))))
(set 'probe (list bump! peek (sorted-map "b" bump!)))
`,
	})
}

// A map that reaches itself through a second header: the shape whose
// *LVal-only memo bounded the walk but not the number of clones, one per
// header, each containing the next (issues #576 and #585).
func TestAliasGuardSelfReferentialMap(t *testing.T) {
	t.Parallel()
	elpstest.RunAliasCheck(t, elpstest.AliasCheck{
		Program: `
(set 'm (sorted-map))
(set 'alias (quasiquote (unquote m)))
(assoc! m "self" alias)
(set 'probe (list m alias))
`,
	})
}

// ---------------------------------------------------------------------------
// The location channel.
// ---------------------------------------------------------------------------

// Locations must not bleed between environments, a fork must start with an
// empty evaluator location register, and a function's definition-site
// snapshot must stay frozen when its defining environment's live register
// moves.  See aliasguard_location.go for why the three are tested by
// consequence rather than by comparing registers.
func TestLocationChannelHasNoBleed(t *testing.T) {
	t.Parallel()
	elpstest.RunLocationCheck(t, elpstest.LocationCheck{
		Program:  locationProgram,
		Probe:    `(cc 3)`,
		Trip:     bodyEntryTrip,
		WantSite: bodyEntrySite,
	})
}

// ---------------------------------------------------------------------------
// Transaction isolation.
// ---------------------------------------------------------------------------

// The guarantee the whole guard serves: an embedder loads once into a
// template and runs every transaction on a fresh fork, and no transaction
// may observe or affect another.  The transactions below each mutate
// something the template holds, in a different way — a sorted map, a bytes
// value, a captured closure binding, a new definition (which writes the
// package's FID→name index, a table Fork copies rather than shares).
func TestTransactionIsolation(t *testing.T) {
	t.Parallel()
	elpstest.RunTransactionCheck(t, elpstest.TransactionCheck{
		Program: `
(set 'shared-map (sorted-map "k" 1))
(set 'shared-alias (quasiquote (unquote shared-map)))
(set 'buf (to-bytes "abc"))
(let ([n (sorted-map "count" 0)])
  (set 'bump! (lambda () (assoc! n "count" (+ 1 (get n "count")))))
  (set 'peek (lambda () (get n "count"))))
`,
		Tx: []string{
			`(assoc! shared-map "tx0" 1) (get shared-alias "tx0")`,
			`(append! buf 7) (length buf)`,
			`(bump!) (peek)`,
			`(defun tx3-fn (x) (* x 2)) (tx3-fn 4)`,
			`(dissoc! shared-alias "k") (get shared-map "k")`,
			// Binding an EXISTING function under a second name rewrites
			// that function's entry in the package's FID->name index — a
			// table Fork copies rather than shares (lisp/fork.go, issue
			// #397).  A fork that shared it instead would rename the
			// template's function, and every later transaction would see
			// the new name in its stack traces.  Nothing compared that
			// table before the fingerprint's package-metadata channel.
			`(set 'peek-alias peek) (peek-alias)`,
		},
		// A program over the standard library alone should share no native
		// payload between two forks: the one native type a loaded stdlib
		// leaves reachable is libtesting's suite, and it is a NativeCloner.
		ExpectNoSharedNatives: true,
	})
}

// ---------------------------------------------------------------------------
// Every native payload type must declare its sharing semantics.
// ---------------------------------------------------------------------------

// nativeDeclarationExemptions is SHRINK-ONLY.  A row states a payload type
// reachable from a loaded standard library that declares neither
// NativeCloner nor RuntimeBound and is not a basic type, together with the
// reason sharing it between every transaction is nevertheless safe.  A new
// row is a design decision that belongs in a review; it is not the way to
// make this test green.
var nativeDeclarationExemptions = map[string]string{}

// Fork shares a native payload by reference unless the payload implements
// NativeCloner or the embedder substitutes it, so a stateful payload that
// declares nothing is shared by every transaction of every fork — silently,
// and in every build.
//
// The runtime-affinity protocol (lisp.RuntimeBound, issue #546) is the
// kernel's answer to this, and it is a good one, but it has two gaps this
// test closes rather than duplicates.  It is OPT-IN, so a payload that
// forgets to declare is never checked at all; and its enforcement is
// compiled only under `-tags elpscheck`, so no production build checks even
// the payloads that did declare.  Requiring every reachable payload TYPE to
// have declared something — a duplication protocol, a runtime affinity, or
// a basic underlying type with no state to share — closes the class
// deterministically, on every PR, without the fuzzer having to stumble on
// the right shape.
func TestEveryReachableNativeDeclaresItsSharing(t *testing.T) {
	t.Parallel()
	env, err := elpstest.NewForkCheckEnv()
	if err != nil {
		t.Fatal(err)
	}
	decls := elpstest.NativeDeclarations(env)
	if len(decls) == 0 {
		t.Fatal("no native payload is reachable from a loaded standard library; " +
			"this test would pass vacuously — check the walk, not the library")
	}
	for _, d := range decls {
		if d.Declared() {
			continue
		}
		if reason, ok := nativeDeclarationExemptions[d.Type]; ok {
			t.Logf("exempt: %s (%s)", d.Type, reason)
			continue
		}
		t.Errorf("native payload type %s declares nothing about being shared.\n"+
			"  reached at: %s\n"+
			"  Fork shares an undeclared payload by reference, so every transaction on every fork\n"+
			"  holds this one object.  Implement lisp.NativeCloner (say what a duplicate is) or\n"+
			"  lisp.RuntimeBound (say which Runtime it belongs to), or add a row to\n"+
			"  nativeDeclarationExemptions saying why sharing it is safe.",
			d.Type, d.Path)
	}
	// The exemption list may only shrink: a row for a type that is no
	// longer reachable is dead weight that hides the next one.
	for typ := range nativeDeclarationExemptions {
		found := false
		for _, d := range decls {
			if d.Type == typ {
				found = true
			}
		}
		if !found {
			t.Errorf("nativeDeclarationExemptions has a row for %s, which is no longer reachable; delete it", typ)
		}
	}
}

// ---------------------------------------------------------------------------
// The fingerprint's own contract.
// ---------------------------------------------------------------------------

// Sharing is in the fingerprint: two graphs that are `equal?` and differ
// only in how they share must fingerprint differently.  Without this the
// whole oracle is blind to the bug class it exists for.
func TestFingerprintEncodesSharing(t *testing.T) {
	t.Parallel()
	load := func(prog string) *lisp.LEnv {
		t.Helper()
		env, err := elpstest.NewForkCheckEnv()
		if err != nil {
			t.Fatal(err)
		}
		if rc := env.LoadString("p.lisp", prog); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		return env
	}
	aliased := load(`(set 'a (sorted-map "k" 1)) (set 'b (quasiquote (unquote a))) (set 'probe (list a b))`)
	dealiased := load(`(set 'a (sorted-map "k" 1)) (set 'b (sorted-map "k" 1)) (set 'probe (list a b))`)

	// Premise: lisp cannot tell the two apart by value.
	eq := aliased.LoadString("q.lisp", `(equal? (first probe) (second probe))`)
	if eq.Type == lisp.LError || eq.IsNil() {
		t.Fatalf("premise: the two names must be equal? in the aliased program: %v", eq)
	}
	fa := elpstest.FingerprintValue(aliased.Get(lisp.Symbol("probe")), elpstest.FingerprintOptions{})
	fd := elpstest.FingerprintValue(dealiased.Get(lisp.Symbol("probe")), elpstest.FingerprintOptions{})
	if fa.Equal(fd) {
		t.Fatalf("the fingerprint cannot tell two names over one map from two maps:\n%s", fa)
	}
	if fa.Hash() == fd.Hash() {
		t.Errorf("the hashes agree where the streams do not: %s", fa.Hash())
	}
	// The diff must point at a path, not just report inequality.
	d := fa.Diff(fd)
	if !strings.Contains(d, "at ") {
		t.Errorf("the diff carries no path: %s", d)
	}
	t.Logf("diff:\n%s", d)

	// The same distinction must hold for EVERY payload kind the walkers
	// memoise, not only sorted maps.  Each of these is a negative control
	// for one identity ordinal in the encoding: drop the bytes ordinal and
	// the bytes arm goes red, drop the native ordinal and the native arm
	// goes red.  Before they existed, the adversarial review of #599
	// removed the bytes ordinal and the whole suite stayed green.
	t.Run("bytes", func(t *testing.T) {
		t.Parallel()
		al := load(`(set 'a (to-bytes "abc")) (set 'b (quasiquote (unquote a))) (set 'probe (list a b))`)
		de := load(`(set 'a (to-bytes "abc")) (set 'b (to-bytes "abc")) (set 'probe (list a b))`)
		eq := al.LoadString("q.lisp", `(equal? (first probe) (second probe))`)
		if eq.Type == lisp.LError || eq.IsNil() {
			t.Fatalf("premise: the two names must be equal? in the aliased program: %v", eq)
		}
		fa := elpstest.FingerprintValue(al.Get(lisp.Symbol("probe")), elpstest.FingerprintOptions{})
		fd := elpstest.FingerprintValue(de.Get(lisp.Symbol("probe")), elpstest.FingerprintOptions{})
		if fa.Equal(fd) {
			t.Fatalf("the fingerprint cannot tell two names over one BYTES payload from two equal ones.\n"+
				"The bytes identity ordinal has been dropped from the encoding, so a walker that\n"+
				"de-aliases bytes is now invisible to it:\n%s", fa)
		}
		if fa.Hash() == fd.Hash() {
			t.Errorf("the bytes hashes agree where the streams do not: %s", fa.Hash())
		}
	})
	t.Run("native", func(t *testing.T) {
		t.Parallel()
		// lisp cannot express two headers over one native payload, so the
		// aliased side is built in Go.  The dealiased side is two distinct
		// payloads of the same Go type — which is exactly what a walker
		// missing its native memo produces.
		alEnv, err := nativeAliasEnvForFingerprint()
		if err != nil {
			t.Fatal(err)
		}
		deEnv, err := nativeDistinctEnvForFingerprint()
		if err != nil {
			t.Fatal(err)
		}
		fa := elpstest.FingerprintValue(alEnv.Get(lisp.Symbol("probe")), elpstest.FingerprintOptions{})
		fd := elpstest.FingerprintValue(deEnv.Get(lisp.Symbol("probe")), elpstest.FingerprintOptions{})
		if fa.Equal(fd) {
			t.Fatalf("the fingerprint cannot tell two names over one NATIVE payload from two distinct\n"+
				"payloads of the same Go type. The native identity ordinal has been dropped, and it is\n"+
				"the only channel that can see native sharing — no probe can look inside a native:\n%s", fa)
		}
		if fa.Hash() == fd.Hash() {
			t.Errorf("the native hashes agree where the streams do not: %s", fa.Hash())
		}
	})
}

// nativeAliasEnvForFingerprint puts two headers over ONE native payload
// into `probe`.
func nativeAliasEnvForFingerprint() (*lisp.LEnv, error) {
	env, err := elpstest.NewForkCheckEnv()
	if err != nil {
		return nil, err
	}
	a := lisp.Native(&fpNative{})
	b := *a
	return putProbePair(env, a, &b)
}

// nativeDistinctEnvForFingerprint puts two headers over TWO payloads of the
// same Go type into `probe` — the shape a walker with no native memo
// produces from the aliased one.
func nativeDistinctEnvForFingerprint() (*lisp.LEnv, error) {
	env, err := elpstest.NewForkCheckEnv()
	if err != nil {
		return nil, err
	}
	return putProbePair(env, lisp.Native(&fpNative{}), lisp.Native(&fpNative{}))
}

func putProbePair(env *lisp.LEnv, a, b *lisp.LVal) (*lisp.LEnv, error) {
	if rc := env.PutGlobal(lisp.Symbol("na"), a); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	if rc := env.PutGlobal(lisp.Symbol("nb"), b); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	if rc := env.LoadString("n.lisp", `(set 'probe (list na nb))`); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return env, nil
}

// fpNative is a stateless native payload used only to give the fingerprint
// two distinguishable-by-identity values of one Go type.
type fpNative struct{ n int }

func (f *fpNative) CloneNative() any { return &fpNative{n: f.n} }

// A shared subtree is walked once, so a diamond-shaped graph stays linear
// instead of costing one walk per path in.
func TestFingerprintIsLinearOnDiamonds(t *testing.T) {
	t.Parallel()
	load := func(prog string) *lisp.LVal {
		t.Helper()
		env, err := elpstest.NewForkCheckEnv()
		if err != nil {
			t.Fatal(err)
		}
		if rc := env.LoadString("p.lisp", prog); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		return env.Get(lisp.Symbol("probe"))
	}
	base := load(`(set 'probe (list 1))`)
	deep := load(`(set 'probe (list 1)) (dotimes (i 40) (set 'probe (list probe probe)))`)
	nb := len(elpstest.FingerprintValue(base, elpstest.FingerprintOptions{}).Tokens())
	nd := len(elpstest.FingerprintValue(deep, elpstest.FingerprintOptions{}).Tokens())
	if nd-nb > 40*20 {
		t.Fatalf("forty diamond levels grew the stream by %d tokens; the shared subtree is being re-walked", nd-nb)
	}
}

// A fork is indistinguishable from its template under the template-level
// fingerprint — every value, every sharing relation, the seal bits and the
// per-package metadata tables.  It is the fork contract in one comparison.
func TestForkFingerprintsIdenticallyToItsTemplate(t *testing.T) {
	t.Parallel()
	env, err := elpstest.NewForkCheckEnv()
	if err != nil {
		t.Fatal(err)
	}
	if rc := env.LoadString("p.lisp", aliasProgram); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	opts := elpstest.FingerprintOptions{Seal: true, PackageMetadata: true}
	before := elpstest.FingerprintEnv(env, opts)
	fork, err := env.Fork()
	if err != nil {
		t.Fatal(err)
	}
	got := elpstest.FingerprintEnv(fork, opts)
	if !before.Equal(got) {
		t.Fatalf("a fresh fork differs from its template:\n%s", before.Diff(got))
	}
}

// Every registered walker must carry a memo declaration from the
// production registry in package lisp, and a doc reference.  Memoises is
// filled by lisp.WalkerMemoKinds, which returns nil for a name it does not
// know, so a walker registered here under a name the registry has never
// heard of arrives with an empty set — and that is the shape a fifth
// walker takes when someone adds it to the guard and forgets to declare
// its memos beside the walk itself.
func TestEveryRegisteredWalkerDeclaresItsMemos(t *testing.T) {
	t.Parallel()
	ws := elpstest.Walkers()
	if len(ws) < 4 {
		t.Fatalf("the walker registry holds %d walkers; it has lost one", len(ws))
	}
	for _, w := range ws {
		if len(w.Memoises) == 0 {
			t.Errorf("walker %q declares no memoised payload kinds.\n"+
				"Memoises comes from lisp.WalkerMemoKinds, which returns nil for an unknown name:\n"+
				"either the name does not match a row in walkerMemos (lisp/walkers.go), or the walk\n"+
				"has no memo declaration beside it.", w.Name)
		}
		if w.Doc == "" {
			t.Errorf("walker %q carries no doc reference", w.Name)
		}
		switch w.Kind {
		case elpstest.WalkerFork:
			if w.Fork == nil {
				t.Errorf("walker %q is a fork walker with no Fork function", w.Name)
			}
		case elpstest.WalkerCopy, elpstest.WalkerStamp:
			if w.Copy == nil {
				t.Errorf("walker %q has no Copy function", w.Name)
			}
		}
	}
}

// The fingerprint's file comment claims "a Go map is never ranged
// directly", which is what makes the encoding deterministic: Go randomises
// map iteration order per range, so one direct range would make the whole
// oracle flaky — a value would stop matching its own copy at random.
// Nothing tested it.
//
// What this test does and does not demonstrate, measured rather than
// asserted:
//
//   - Introducing a genuine raw `for k := range someGoMap` into the walk
//     DOES fail it, on the environment arm, within the repeat budget. That
//     is the risk the claim is about.
//   - Deleting the existing sort.Strings calls over PackageNames() and
//     SymbolNames() does NOT fail it: those accessors already return
//     deterministic order, so the sorts are defensive rather than
//     load-bearing. Do not read a green run here as proof that a
//     particular sort is unnecessary.
func TestFingerprintIsDeterministic(t *testing.T) {
	t.Parallel()
	env, err := elpstest.NewForkCheckEnv()
	if err != nil {
		t.Fatal(err)
	}
	// A graph reaching every map-backed structure the walk can visit.
	prog := `
(set 'm (sorted-map "b" 2 "a" 1 "c" 3 "d" 4 "e" 5 "f" 6))
(defun handler (x) (+ x 1))
(set 'clo (let ([s (vector 1 2 3)]) (lambda () s)))
(set 'probe (list m clo handler (sorted-map "nested" m "buf" (to-bytes "abc"))))
`
	if rc := env.LoadString("p.lisp", prog); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	v := env.Get(lisp.Symbol("probe"))
	opts := elpstest.FingerprintOptions{PackageMetadata: true}

	first := elpstest.FingerprintValue(v, opts)
	for i := range 32 {
		again := elpstest.FingerprintValue(v, opts)
		if !first.Equal(again) {
			t.Fatalf("fingerprinting the same value twice disagreed on repeat %d.\n"+
				"Something in the walk now ranges a Go map directly: iteration order is randomised\n"+
				"per range, so the oracle would fail at random on correct code.\n%s",
				i, first.Diff(again))
		}
	}

	// The environment walk reaches the package tables too.
	fe := elpstest.FingerprintEnv(env, opts)
	for range 16 {
		if !fe.Equal(elpstest.FingerprintEnv(env, opts)) {
			t.Fatal("fingerprinting the same ENVIRONMENT twice disagreed; a package or binding table " +
				"is being ranged directly")
		}
	}
}
