// Copyright © 2026 The ELPS authors

// Known-bad reference walkers: the guard on the guard.
//
// The revert-proof exercise that ships with a guard like this — take out the
// fix, watch the guard go red, put it back — proves the guard works on the
// day it is written and guards nothing afterwards.  If a later change
// weakens the oracle (drops a payload kind from the walk, makes a probe
// permissive, stops encoding sharing) every test stays green and the whole
// bug class is open again.
//
// So the negative controls are permanent and committed.  Each walker below
// is a deliberately broken rebuild that reproduces ONE historical failure
// mode, in a handful of lines, entirely inside the harness — no production
// code is changed and none of these is ever registered in Walkers().  Each
// test asserts THE ORACLE DETECTS IT, and that the witness names the right
// probe site.  When someone weakens the oracle, one of these flips from
// detected to undetected and CI goes red.
//
// They double as executable documentation: each is the shortest statement of
// what the real bug was.
package elpstest_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// ---------------------------------------------------------------------------
// A reference copier, and three ways of breaking it.
// ---------------------------------------------------------------------------

// naiveCopier is a small deep copy in the shape of lisp/detach.go's walker:
// a per-header memo, plus a memo per payload kind.  Setting a payload memo
// to nil is exactly the defect the corresponding issue describes — the
// payload is then rebuilt once per HEADER, so two names for one piece of
// storage come out as two pieces of storage.
//
// It shares functions by reference, as the `copy` builtin does, so its
// contract is ClosuresRefused.
type naiveCopier struct {
	seen map[*lisp.LVal]*lisp.LVal
	// A nil memo means "de-alias this payload kind".
	maps    map[*lisp.MapData]*lisp.MapData
	bytes   map[*[]byte]*[]byte
	natives map[any]any
}

func newNaiveCopier(memoMaps, memoBytes, memoNatives bool) *naiveCopier {
	d := &naiveCopier{seen: map[*lisp.LVal]*lisp.LVal{}}
	if memoMaps {
		d.maps = map[*lisp.MapData]*lisp.MapData{}
	}
	if memoBytes {
		d.bytes = map[*[]byte]*[]byte{}
	}
	if memoNatives {
		d.natives = map[any]any{}
	}
	return d
}

func (d *naiveCopier) copy(v *lisp.LVal) *lisp.LVal {
	if v == nil {
		return nil
	}
	if cp, ok := d.seen[v]; ok {
		return cp
	}
	if v.Type == lisp.LFun {
		// Shared by reference, like the `copy` builtin.
		d.seen[v] = v
		return v
	}
	cp := new(lisp.LVal)
	*cp = *v
	d.seen[v] = cp
	switch v.Type {
	case lisp.LSortMap:
		cp.Native = d.mapData(v.Map())
	case lisp.LBytes:
		if b, ok := v.Native.(*[]byte); ok && b != nil {
			cp.Native = d.byteSlice(b)
		}
	case lisp.LNative:
		cp.Native = d.native(v.Native)
	default:
		// Every other type carries its payload in the struct copy above,
		// or in Cells below.
	}
	if len(v.Cells) > 0 {
		cells := make([]*lisp.LVal, len(v.Cells))
		for i, c := range v.Cells {
			cells[i] = d.copy(c)
		}
		cp.Cells = cells
	} else {
		cp.Cells = nil
	}
	return cp
}

func (d *naiveCopier) mapData(md *lisp.MapData) *lisp.MapData {
	if md == nil {
		return nil
	}
	if d.maps != nil {
		if cp, ok := d.maps[md]; ok {
			return cp
		}
	}
	nm := lisp.SortedMap().Map()
	if d.maps != nil {
		d.maps[md] = nm
	}
	for _, k := range md.Keys().Cells {
		val, _ := md.Get(k)
		nm.Set(d.copy(k), d.copy(val))
	}
	return nm
}

func (d *naiveCopier) byteSlice(b *[]byte) *[]byte {
	if d.bytes != nil {
		if cp, ok := d.bytes[b]; ok {
			return cp
		}
	}
	nb := append([]byte(nil), *b...)
	if d.bytes != nil {
		d.bytes[b] = &nb
	}
	return &nb
}

func (d *naiveCopier) native(payload any) any {
	cloner, ok := payload.(lisp.NativeCloner)
	if !ok {
		return payload
	}
	if d.natives != nil {
		if cp, ok := d.natives[payload]; ok {
			return cp
		}
	}
	clone := cloner.CloneNative()
	if d.natives != nil {
		d.natives[payload] = clone
	}
	return clone
}

func naiveWalker(name string, memoMaps, memoBytes, memoNatives bool) elpstest.Walker {
	return elpstest.Walker{
		Name: name,
		Kind: elpstest.WalkerCopy,
		Copy: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
			return newNaiveCopier(memoMaps, memoBytes, memoNatives).copy(v), nil
		},
		Closures: elpstest.ClosuresRefused,
		Backing:  elpstest.BackingRebuilt,
	}
}

// ---------------------------------------------------------------------------
// The graphs the controls are aimed at.
// ---------------------------------------------------------------------------

// aliasProgram is the historical shape in one program: two names for one
// sorted map, two names for one bytes value, a map that reaches itself
// through a second header, and both parked inside a list and a map so the
// aliases are nested rather than only top level.
const aliasProgram = `
(set 'a (sorted-map "k" 1))
(set 'b (quasiquote (unquote a)))
(assoc! a "self" b)
(set 'buf (to-bytes "abc"))
(set 'buf2 (quasiquote (unquote buf)))
(set 'probe (list a b buf buf2 (sorted-map "inner" a "raw" buf)))
`

// sharedCloner is a NativeCloner accumulator held by pointer: the kind of
// payload an embedder binds at load time and mutates per transaction, and
// the third payload kind of issue #576.
type sharedCloner struct{ n int }

func (c *sharedCloner) CloneNative() any { return &sharedCloner{n: c.n} }

// nativeAliasEnv binds one *sharedCloner under two headers, the shape
// `(quasiquote (unquote a))` produces for a map or a bytes value and that
// lisp cannot express for a native.
func nativeAliasEnv() (*lisp.LEnv, error) {
	env, err := elpstest.NewForkCheckEnv()
	if err != nil {
		return nil, err
	}
	a := lisp.Native(&sharedCloner{})
	b := *a // a second header over the same payload
	if rc := env.PutGlobal(lisp.Symbol("na"), a); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	if rc := env.PutGlobal(lisp.Symbol("nb"), &b); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return env, nil
}

const nativeAliasProgram = `(set 'probe (list na nb))`

// ---------------------------------------------------------------------------
// assertDetects / assertClean
// ---------------------------------------------------------------------------

// assertDetects requires the oracle to report at least one witness for the
// broken walker, and requires a witness to mention wantIn — the probe site,
// payload path or property the failure should be attributed to.  A control
// that goes undetected means the oracle has been weakened.
func assertDetects(t *testing.T, w elpstest.Walker, c elpstest.AliasCheck, wantIn string) {
	t.Helper()
	got, err := elpstest.CheckWalker(w, c)
	if err != nil {
		t.Fatalf("%s: harness error: %v", w.Name, err)
	}
	if len(got) == 0 {
		t.Fatalf("%s: the oracle reported nothing.\n"+
			"This walker is deliberately broken; going undetected means the guard has been weakened.", w.Name)
	}
	assertWitnessMentions(t, w.Name, got, wantIn)
}

func assertWitnessMentions(t *testing.T, name string, got []elpstest.Witness, wantIn string) {
	t.Helper()
	for _, wit := range got {
		if strings.Contains(wit.String(), wantIn) {
			t.Logf("%s detected, witness:\n%s", name, wit)
			return
		}
	}
	var b strings.Builder
	for _, wit := range got {
		b.WriteString("\n---\n")
		b.WriteString(wit.String())
	}
	t.Fatalf("%s: detected, but no witness mentions %q; the failure is attributed to the wrong place.%s",
		name, wantIn, b.String())
}

// assertClean requires the oracle to report nothing.  It is the positive
// control that keeps the negative ones honest: the same reference copier
// with every memo present must PASS, so the failures above are attributable
// to the missing memo and not to the copier being a poor imitation.
func assertClean(t *testing.T, w elpstest.Walker, c elpstest.AliasCheck) {
	t.Helper()
	got, err := elpstest.CheckWalker(w, c)
	if err != nil {
		t.Fatalf("%s: harness error: %v", w.Name, err)
	}
	for _, wit := range got {
		t.Errorf("%s: unexpected witness:\n%s", w.Name, wit)
	}
}

// ---------------------------------------------------------------------------
// Control 0 (positive): a complete reference copier is not flagged.
// ---------------------------------------------------------------------------

func TestReferenceCopierWithEveryMemoIsClean(t *testing.T) {
	t.Parallel()
	assertClean(t, naiveWalker("reference-copier", true, true, true),
		elpstest.AliasCheck{Program: aliasProgram})
	assertClean(t, naiveWalker("reference-copier", true, true, true),
		elpstest.AliasCheck{NewEnv: nativeAliasEnv, Program: nativeAliasProgram})
}

// ---------------------------------------------------------------------------
// Control 1: per-header memo only, so two headers over one map become two
// maps.  Issues #576 (Fork) and #585 (copy/detach).
// ---------------------------------------------------------------------------

func TestGuardDetectsDealiasedSortedMap(t *testing.T) {
	t.Parallel()
	assertDetects(t,
		naiveWalker("broken-copier/no-map-memo", false, true, true),
		elpstest.AliasCheck{Program: aliasProgram},
		"map entry")
}

// ---------------------------------------------------------------------------
// Control 2: a shared bytes payload comes apart.  Issue #576's second
// payload kind; append! grows a bytes value in place, so this is a write
// one name sees and the other does not.
// ---------------------------------------------------------------------------

func TestGuardDetectsDealiasedBytes(t *testing.T) {
	t.Parallel()
	assertDetects(t,
		naiveWalker("broken-copier/no-bytes-memo", true, false, true),
		elpstest.AliasCheck{Program: aliasProgram},
		"bytes[0]")
}

// ---------------------------------------------------------------------------
// Control 3: a pointer native comes apart — one accumulator becomes two.
// Issue #576's third payload kind.  There is no probe site inside an opaque
// payload, so this one is caught by the fingerprint's identity ordinal
// alone, which is why it is worth a control of its own: it is the property
// that survives when the guard cannot look inside.
// ---------------------------------------------------------------------------

func TestGuardDetectsDealiasedNativePayload(t *testing.T) {
	t.Parallel()
	assertDetects(t,
		naiveWalker("broken-copier/no-native-memo", true, true, false),
		elpstest.AliasCheck{NewEnv: nativeAliasEnv, Program: nativeAliasProgram},
		"same values and the same sharing")
}

// ---------------------------------------------------------------------------
// Control 4: a fork that carries the template's evaluator location.
//
// This is the location channel's failure mode, in the two shapes it has
// taken: Fork used to copyLocation the register onto every environment it
// remapped (issue #440), and a call environment used to read its captured
// environment's LIVE register instead of the definition-site snapshot (PR
// #578 review finding F1, fixed by funData.loc).  Both make a location from
// one context observable in another; this walker reproduces the observable
// half, which is what the oracle is allowed to see.
// ---------------------------------------------------------------------------

const locationProgram = `
(defun aa (n) (+ n 1))
(let ([k 2]) (set 'bb (lambda (m) (* m k))))
(let ([j 5]) (set 'dd (lambda (m) (+ m j))))
(defun cc (x) (aa (bb x)))
(set 'holder (sorted-map "fn" dd))
`

func brokenForkCarriesEvaluatorLocation(env *lisp.LEnv) (*lisp.LEnv, error) {
	loc := env.Source()
	f, err := env.Fork()
	if err != nil {
		return nil, err
	}
	if loc == nil {
		return f, nil
	}
	envs, _ := elpstest.ReachableEnvironments(f)
	for _, e := range envs {
		if err := elpstest.StampEvaluatorLocation(e, loc); err != nil {
			return nil, err
		}
	}
	return f, nil
}

func TestGuardDetectsForkCarryingAnEvaluatorLocation(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckLocations(elpstest.LocationCheck{
		Program:  locationProgram,
		Probe:    `(cc 3)`,
		Trip:     bodyEntryTrip,
		WantSite: bodyEntrySite,
		Fork:     brokenForkCarriesEvaluatorLocation,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	if len(got) == 0 {
		t.Fatal("the oracle reported nothing for a fork that carries the template's evaluator location; " +
			"the location channel has been weakened")
	}
	assertWitnessMentions(t, "broken-fork/carries-location", got, "empty evaluator location register")
}

// ---------------------------------------------------------------------------
// Control 5: a fork that shares a stateful native with every other fork.
//
// The shape the existing harness names but could not catch: a CloneNative
// that hands every fork the SAME clone.  The payload has declared that it
// must not be shared, and is shared anyway, so every transaction mutates one
// accumulator.
// ---------------------------------------------------------------------------

var oneSharedPayload = &sharedCloner{n: -1}

func brokenForkSharesStatefulNative(env *lisp.LEnv) (*lisp.LEnv, error) {
	return env.Fork(lisp.ForkWithNativeReplacer(func(payload any) (any, bool) {
		if _, ok := payload.(*sharedCloner); ok {
			return oneSharedPayload, true
		}
		return nil, false
	}))
}

func TestGuardDetectsForkSharingAStatefulNative(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		NewEnv:  nativeAliasEnv,
		Program: nativeAliasProgram + "\n(set 'counter (sorted-map \"n\" 0))",
		Tx: []string{
			`(assoc! counter "n" 1)`,
			`(assoc! counter "n" 2)`,
		},
		Fork: brokenForkSharesStatefulNative,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	if len(got) == 0 {
		t.Fatal("the oracle reported nothing for a fork that hands every fork one stateful native; " +
			"the native-sharing check has been weakened")
	}
	assertWitnessMentions(t, "broken-fork/shared-native", got, "declares NativeCloner and is shared anyway")
}

// ---------------------------------------------------------------------------
// Control 6: a stamper that writes into its source.
//
// Issue #582: the macro-expansion stamp wrote the call site onto every
// unlocated node of an expansion, and an unsealed runtime list returned by a
// macro body IS a binding, so `l` and its cells acquired the call site of
// `(m)` for the rest of the process.  Closed by making the stamp
// copy-on-write (#586).  This walker is the pre-#586 write, in three lines.
// ---------------------------------------------------------------------------

func brokenStampMutatesSource(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
	loc := &token.Location{File: "broken-stamp.lisp", Path: "broken-stamp.lisp", Line: 1, Col: 1, Pos: 0}
	var walk func(n *lisp.LVal)
	seen := map[*lisp.LVal]bool{}
	walk = func(n *lisp.LVal) {
		if n == nil || seen[n] {
			return
		}
		seen[n] = true
		n.SetSource(loc)
		for _, c := range n.Cells {
			walk(c)
		}
	}
	walk(v)
	return v, nil
}

func TestGuardDetectsStamperWritingIntoItsSource(t *testing.T) {
	t.Parallel()
	assertDetects(t,
		elpstest.Walker{
			Name:     "broken-stamp/writes-its-source",
			Kind:     elpstest.WalkerStamp,
			Copy:     brokenStampMutatesSource,
			Closures: elpstest.ClosuresRefused,
			Backing:  elpstest.BackingPreserved,
		},
		elpstest.AliasCheck{Program: `(set 'probe (list 1 2 (list 3 4)))`},
		"mutates nothing reachable outside its own output")
}

// ---------------------------------------------------------------------------
// Control 7: a truncated location sweep must announce itself.
//
// The sweep rebuilds the whole environment once per stamped environment, so
// it is capped.  The cap used to shorten the sweep SILENTLY, which made a
// truncated run and a clean run indistinguishable: with forty let-bound
// closures the sweep stamped the first twenty-four environments and never
// looked at the rest, so a fork carrying a stale location on environment
// forty-one passed while the identical leak on environment one failed.
// That is a coverage cliff at a size real programs reach — a dispatch table
// of forty handlers leaves forty-one environments — and the adversarial
// review of #599 proved it was silent.
//
// The cliff is now loud: exceeding the cap is a partial-coverage witness.
// ---------------------------------------------------------------------------

// manyScopesProgram leaves one environment per let-bound closure, plus the
// program's own, so n closures leave n+1 reachable environments.
func manyScopesProgram(n int) string {
	var b strings.Builder
	b.WriteString(locationProgram)
	b.WriteString("\n")
	for i := range n {
		fmt.Fprintf(&b, "(set 'h%d (let ([s%d (vector %d)]) (lambda () s%d)))\n", i, i, i, i)
	}
	return b.String()
}

func truncationWitnesses(ws []elpstest.Witness) []elpstest.Witness {
	var out []elpstest.Witness
	for _, w := range ws {
		if strings.Contains(w.Property, "covers every reachable environment") {
			out = append(out, w)
		}
	}
	return out
}

func TestGuardAnnouncesATruncatedLocationSweep(t *testing.T) {
	t.Parallel()
	// A cap of two against a program that leaves more forces truncation
	// cheaply; the sweep costs one environment rebuild per environment, so
	// this control does not pay for the forty-closure case to prove the
	// same branch.
	got, err := elpstest.CheckLocations(elpstest.LocationCheck{
		Program:         manyScopesProgram(6),
		Probe:           `(cc 3)`,
		Trip:            bodyEntryTrip,
		WantSite:        bodyEntrySite,
		MaxEnvironments: 2,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	tw := truncationWitnesses(got)
	if len(tw) == 0 {
		t.Fatalf("a sweep capped at 2 environments over a program that leaves more reported NO\n"+
			"partial-coverage witness. Truncation is silent again, so a leak on an environment past\n"+
			"the cap is undetectable and indistinguishable from a clean run.\nwitnesses: %v", got)
	}
	for _, w := range tw {
		if !strings.Contains(w.Detail, "MaxEnvironments") {
			t.Errorf("the partial-coverage witness does not name the field an operator would raise:\n%s", w)
		}
	}
	t.Logf("partial-coverage witness:\n%s", tw[0])
}

// The same program under a cap that covers it reports no partial coverage —
// so the signal above is attributable to truncation and not merely always
// present.
func TestGuardIsSilentWhenTheSweepIsComplete(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckLocations(elpstest.LocationCheck{
		Program:         manyScopesProgram(6),
		Probe:           `(cc 3)`,
		Trip:            bodyEntryTrip,
		WantSite:        bodyEntrySite,
		MaxEnvironments: 64,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	if tw := truncationWitnesses(got); len(tw) != 0 {
		t.Errorf("a sweep with room to cover the program still reported partial coverage:\n%s", tw[0])
	}
}

// The realistic shape from the review: forty let-bound closures exceed the
// DEFAULT cap, so an embedder running the guard over a dispatch table of
// handlers is told its sweep is partial rather than being handed a green
// result over a fraction of its environments.
func TestDefaultCapAnnouncesTruncationOnARouterShapedProgram(t *testing.T) {
	t.Parallel()
	env, err := elpstest.NewForkCheckEnv()
	if err != nil {
		t.Fatal(err)
	}
	if rc := env.LoadString("p.lisp", manyScopesProgram(40)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	envs, truncated := elpstest.ReachableEnvironments(env)
	if !truncated {
		t.Errorf("forty closures did not truncate the enumeration; the exported enumeration no longer\n" +
			"reports partial coverage, so an embedder auditing its own template would read a short\n" +
			"list as complete")
	}
	if n := len(envs); n != elpstest.DefaultMaxEnvironments {
		t.Fatalf("forty closures left %d reachable environments under the default cap of %d; "+
			"this control is no longer exercising truncation",
			n, elpstest.DefaultMaxEnvironments)
	}
}

// ---------------------------------------------------------------------------
// Control 8: a copier that rebuilds everything EXCEPT the bytes payload,
// which it hands straight through from the source.
//
// This is the negative control for the mutation-probe sweep itself — the
// centrepiece of this PR, which until now had none.  The adversarial review
// of #599 made the alias-class comparison permissive (sameIndexSet always
// true) and the entire suite stayed green, because every other control is
// caught by the fingerprint before the sweep is consulted.
//
// Bytes are the payload that isolates the sweep.  A *[]byte has no child
// headers, so sharing one changes NOTHING about the topology the
// fingerprint walks: identity ordinals are assigned per walk, and the
// contents are equal either way, so a private copy and the source's own
// buffer both render `bytes#n("abc")` at the same path.  Source and copy
// fingerprint identically whether the buffer is shared or not.  (A copier
// that shared the sorted MAP instead would be caught by the fingerprint,
// because the values inside a shared map are the source's own headers and
// the header topology changes — which is worth knowing, and is why this
// control uses bytes.)
//
// Only writing through the source and reading the copy can tell them apart.
// That is exactly the isolation arm of the sweep, so this test asserts both
// halves: the fingerprints agree, and the oracle reports the leak anyway.
//
// The bug it models is the ordinary one — a walker that rebuilds the
// containers it knows about and passes an opaque buffer straight through,
// so a "copy" is a window onto the original.
// ---------------------------------------------------------------------------

type bytesSharingCopier struct {
	seen map[*lisp.LVal]*lisp.LVal
	maps map[*lisp.MapData]*lisp.MapData
}

func (d *bytesSharingCopier) copy(v *lisp.LVal) *lisp.LVal {
	if v == nil {
		return nil
	}
	if cp, ok := d.seen[v]; ok {
		return cp
	}
	if v.Type == lisp.LFun {
		d.seen[v] = v
		return v
	}
	cp := new(lisp.LVal)
	*cp = *v
	d.seen[v] = cp
	switch v.Type {
	case lisp.LSortMap:
		cp.Native = d.mapData(v.Map())
	case lisp.LBytes:
		// THE DEFECT: the source's buffer comes across untouched.
	case lisp.LNative:
		if c, ok := v.Native.(lisp.NativeCloner); ok {
			cp.Native = c.CloneNative()
		}
	}
	if len(v.Cells) > 0 {
		cells := make([]*lisp.LVal, len(v.Cells))
		for i, c := range v.Cells {
			cells[i] = d.copy(c)
		}
		cp.Cells = cells
	} else {
		cp.Cells = nil
	}
	return cp
}

func (d *bytesSharingCopier) mapData(md *lisp.MapData) *lisp.MapData {
	if md == nil {
		return nil
	}
	if cp, ok := d.maps[md]; ok {
		return cp
	}
	nm := lisp.SortedMap().Map()
	d.maps[md] = nm
	for _, k := range md.Keys().Cells {
		val, _ := md.Get(k)
		nm.Set(d.copy(k), d.copy(val))
	}
	return nm
}

func newBytesSharingCopier() *bytesSharingCopier {
	return &bytesSharingCopier{
		seen: map[*lisp.LVal]*lisp.LVal{},
		maps: map[*lisp.MapData]*lisp.MapData{},
	}
}

func bytesSharingWalker() elpstest.Walker {
	return elpstest.Walker{
		Name: "broken-copier/shares-the-source-buffer",
		Kind: elpstest.WalkerCopy,
		Copy: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
			return newBytesSharingCopier().copy(v), nil
		},
		Closures: elpstest.ClosuresRefused,
		Backing:  elpstest.BackingRebuilt,
	}
}

func TestGuardDetectsACopyThatSharesTheSourceBuffer(t *testing.T) {
	t.Parallel()
	c := elpstest.AliasCheck{Program: aliasProgram, Repro: "a copy that shares the source's buffer"}

	// Half one: the fingerprint cannot see this defect.  If this premise
	// stops holding, the control has stopped isolating the sweep and the
	// assertion below no longer proves the sweep is doing the work.
	env, err := elpstest.NewForkCheckEnv()
	if err != nil {
		t.Fatal(err)
	}
	if rc := env.LoadString("p.lisp", aliasProgram); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	src := env.Get(lisp.Symbol("probe"))
	cp := newBytesSharingCopier().copy(src)
	opts := elpstest.FingerprintOptions{SkipCapturedEnvironments: true}
	fs := elpstest.FingerprintValue(src, opts)
	fc := elpstest.FingerprintValue(cp, opts)
	if !fs.Equal(fc) {
		t.Fatalf("premise: a copy that shares only the source's BYTES buffer must fingerprint\n"+
			"identically to its source, otherwise this control is being caught by the fingerprint\n"+
			"and proves nothing about the mutation-probe sweep:\n%s", fs.Diff(fc))
	}

	// Half two: the sweep catches it anyway.  Only a write through one and
	// a read through the other can.
	assertDetects(t, bytesSharingWalker(), c, "bytes[0]")
}
