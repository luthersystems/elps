// Copyright © 2026 The ELPS authors

package elpstest

import (
	"errors"
	"fmt"
	"reflect"
	"sort"
	"strings"
	"sync"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
)

// Transaction isolation: the guarantee the mechanisms serve.
//
// Fork exists for performance.  An embedder — substrate is the motivating
// one — loads a program ONCE into a template and runs every transaction on
// a fresh fork of it, because a fork costs a fraction of a load.  The
// product guarantee that arrangement has to keep is that NO TRANSACTION CAN
// OBSERVE OR AFFECT ANOTHER.  Aliasing, cloning, copying, location bleed
// and native sharing are all just mechanisms by which that guarantee can
// break; the other files in this guard test the mechanisms, and this one
// tests the guarantee directly.
//
// Four properties, all expressed with the same fingerprint the mechanism
// checks use:
//
//  1. TEMPLATE IMMUTABILITY UNDER LOAD.  Take N forks, run a different
//     mutating transaction on each, and the template must fingerprint
//     byte-identically to its baseline.  Run both sequentially and
//     concurrently: a data race here is the same bug wearing a different
//     hat, which is why the concurrent arm belongs in the -race gate.
//  2. FORK INDEPENDENCE.  Snapshot every fork, run one transaction, and
//     only that fork may have moved.  Swept over every fork rather than
//     demonstrated on one.
//  3. PRISTINE SUCCESSOR.  A fork taken AFTER other forks have been
//     mutated must fingerprint identically to one taken from the untouched
//     template.  This is the shape that would silently contaminate a LATER
//     customer transaction: state that leaked back into the template and
//     then forward.
//  4. NO SHARED STATEFUL NATIVE.  No native payload that declares neither
//     NativeCloner nor RuntimeBound may be reachable by pointer from two
//     forks at once.  See the census below for why this is not the same
//     check as the runtime-affinity protocol.

// TransactionCheck describes one run of the transaction-isolation oracle.
type TransactionCheck struct {
	// NewEnv builds the template.  Nil means NewForkCheckEnv.
	NewEnv func() (*lisp.LEnv, error)
	// Program is loaded into the template.
	Program string
	// Tx are the transactions, one per fork.  Each should MUTATE something
	// the template holds, or the properties pass vacuously; CheckTransactions
	// asserts that at least one of them moves its own fork.
	Tx []string
	// ExpectNoSharedNatives makes an UNDECLARED native payload reachable
	// from two forks a finding rather than a report.  Fork shares a native
	// payload by reference unless it implements NativeCloner or the
	// embedder substitutes it (lisp/fork.go), so for an embedder's own
	// values that sharing may be a deliberate choice.  For a program over
	// the standard library the expected count is zero, and the guard's own
	// test sets this.  A payload that DECLARES NativeCloner or RuntimeBound
	// and is shared anyway is always a finding: it stated that it must not
	// be.
	ExpectNoSharedNatives bool
	// Fork produces each fork.  Nil means (*lisp.LEnv).Fork.  It exists so
	// a deliberately broken reference fork can be driven through the same
	// oracle (aliasguard_broken_test.go).
	Fork func(*lisp.LEnv) (*lisp.LEnv, error)
	// Repro is attached to every witness.
	Repro string
}

// fork applies the check's fork walker, defaulting to (*lisp.LEnv).Fork.
func (c TransactionCheck) fork(env *lisp.LEnv) (*lisp.LEnv, error) {
	if c.Fork != nil {
		return c.Fork(env)
	}
	return env.Fork()
}

// RunTransactionCheck runs both the sequential and the concurrent arm and
// reports each witness.
func RunTransactionCheck(t TestingTB, c TransactionCheck) {
	t.Helper()
	got, err := CheckTransactions(c)
	if err != nil {
		t.Fatalf("transaction isolation: %v", err)
		return
	}
	for _, w := range got {
		t.Errorf("%s", w)
	}
}

// CheckTransactions runs the four properties and returns one witness per
// failure.
func CheckTransactions(c TransactionCheck) ([]Witness, error) {
	if len(c.Tx) == 0 {
		return nil, errors.New("no transactions: the properties would pass vacuously")
	}
	newEnv := c.NewEnv
	if newEnv == nil {
		newEnv = NewForkCheckEnv
	}
	build := func() (*lisp.LEnv, error) {
		env, err := newEnv()
		if err != nil {
			return nil, err
		}
		if rc := env.LoadString("program.lisp", c.Program); rc.Type == lisp.LError {
			return nil, lisp.GoError(rc)
		}
		return env, nil
	}
	tmpl, err := build()
	if err != nil {
		return nil, err
	}
	baseline := FingerprintEnv(tmpl, templateOpts)

	forks := make([]*lisp.LEnv, len(c.Tx))
	for i := range forks {
		f, err := c.fork(tmpl)
		if err != nil {
			return nil, fmt.Errorf("fork %d: %w", i, err)
		}
		forks[i] = f
	}

	var out []Witness
	// A fresh fork must be indistinguishable from its template.  This is
	// the whole fork contract in one comparison, and it subsumes the
	// alias-structure check at environment level because sharing is part of
	// the encoding.
	before := make([]*Fingerprint, len(forks))
	for i, f := range forks {
		before[i] = FingerprintEnv(f, templateOpts)
		if !baseline.Equal(before[i]) {
			out = append(out, Witness{
				Walker:   "Fork",
				Property: "a fresh fork is indistinguishable from its template",
				Detail:   fmt.Sprintf("fork %d diverges before any transaction ran\n%s", i, baseline.Diff(before[i])),
				Leak:     firstDivergentPath(baseline, before[i]),
				Repro:    c.Repro,
			})
		}
	}

	// Properties 1 and 2, swept: run transaction i on fork i, then assert
	// that the template and every OTHER fork are where they were.
	moved := false
	for i, tx := range c.Tx {
		if rc := forks[i].LoadString(fmt.Sprintf("tx%d.lisp", i), tx); rc.Type == lisp.LError {
			return nil, fmt.Errorf("transaction %d: %v", i, rc)
		}
		after := FingerprintEnv(forks[i], templateOpts)
		if !after.Equal(before[i]) {
			moved = true
		}
		before[i] = after
		if got := FingerprintEnv(tmpl, templateOpts); !baseline.Equal(got) {
			out = append(out, Witness{
				Walker:   "Fork",
				Property: "the template is unchanged by a transaction on a fork",
				Detail:   fmt.Sprintf("transaction %d moved the template\n%s", i, baseline.Diff(got)),
				Leak:     firstDivergentPath(baseline, got),
				Repro:    c.Repro,
			})
		}
		for j := range forks {
			if j == i {
				continue
			}
			if got := FingerprintEnv(forks[j], templateOpts); !before[j].Equal(got) {
				out = append(out, Witness{
					Walker:   "Fork",
					Property: "a transaction on one fork is invisible to every other fork",
					Detail:   fmt.Sprintf("transaction %d moved fork %d\n%s", i, j, before[j].Diff(got)),
					Leak:     firstDivergentPath(before[j], got),
					Repro:    c.Repro,
				})
				before[j] = got
			}
		}
	}
	if !moved {
		return nil, errors.New("no transaction changed its own fork; the isolation properties would pass vacuously")
	}

	// Property 3: a fork taken after all that must be pristine.
	successor, err := c.fork(tmpl)
	if err != nil {
		return nil, err
	}
	if got := FingerprintEnv(successor, templateOpts); !baseline.Equal(got) {
		out = append(out, Witness{
			Walker:   "Fork",
			Property: "a fork taken after other forks were mutated is pristine",
			Detail:   "state leaked back into the template and forward into a later transaction\n" + baseline.Diff(got),
			Leak:     firstDivergentPath(baseline, got),
			Repro:    c.Repro,
		})
	}

	// Property 4: no stateful native shared between two forks.
	out = append(out, sharedNativeWitnesses(c, "the template", tmpl, forks)...)

	// Property 1 again, concurrently.  Same transactions, same template,
	// forks driven in parallel: under -race this is also the data-race
	// gate, and without it it still catches a template mutation that only
	// happens under interleaving.
	conc, err := build()
	if err != nil {
		return nil, err
	}
	concBase := FingerprintEnv(conc, templateOpts)
	cforks := make([]*lisp.LEnv, len(c.Tx))
	for i := range cforks {
		f, err := c.fork(conc)
		if err != nil {
			return nil, err
		}
		cforks[i] = f
	}
	errs := make([]*lisp.LVal, len(c.Tx))
	var wg sync.WaitGroup
	for i, tx := range c.Tx {
		wg.Add(1)
		go func(i int, tx string) {
			defer wg.Done()
			errs[i] = cforks[i].LoadString(fmt.Sprintf("tx%d.lisp", i), tx)
		}(i, tx)
	}
	wg.Wait()
	for i, rc := range errs {
		if rc != nil && rc.Type == lisp.LError {
			return nil, fmt.Errorf("concurrent transaction %d: %v", i, rc)
		}
	}
	if got := FingerprintEnv(conc, templateOpts); !concBase.Equal(got) {
		out = append(out, Witness{
			Walker:   "Fork",
			Property: "the template is unchanged by concurrent transactions on its forks",
			Detail:   "run the -race gate on this arm: a template mutation under interleaving is a data race\n" + concBase.Diff(got),
			Leak:     firstDivergentPath(concBase, got),
			Repro:    c.Repro,
		})
	}
	return out, nil
}

// CheckForkTemplate holds one loaded template to the fork contract stated
// as a single comparison: a fork, and a fork of that fork, must fingerprint
// identically to the template under the template-level fingerprint — every
// value, every sharing relation, every seal bit and the per-package
// metadata tables.
//
// Because sharing is part of the encoding, this subsumes an alias-structure
// comparison; because the fingerprint carries the package metadata channel,
// it also covers the three tables Fork copies rather than shares, which
// nothing compared before.  The two-hop arm is there because a fix that
// survived one fork hop and not two has happened (issue #579).
//
// It is what RunForkCheck delegates its aliasing and isolation properties
// to, so the two harnesses share one oracle instead of carrying two.
func CheckForkTemplate(env *lisp.LEnv, opts ...lisp.ForkOption) []Witness {
	baseline := FingerprintEnv(env, templateOpts)
	var out []Witness
	fork, err := env.Fork(opts...)
	if err != nil {
		return []Witness{{Walker: "Fork", Property: "the template forks", Detail: err.Error()}}
	}
	arms := []struct {
		name string
		env  *lisp.LEnv
	}{{"a fresh fork", fork}}
	fork2, err := fork.Fork(opts...)
	if err != nil {
		out = append(out, Witness{Walker: "Fork", Property: "a fork forks", Detail: err.Error()})
	} else {
		arms = append(arms, struct {
			name string
			env  *lisp.LEnv
		}{"a fork of a fork", fork2})
	}
	for _, arm := range arms {
		got := FingerprintEnv(arm.env, templateOpts)
		if !baseline.Equal(got) {
			out = append(out, Witness{
				Walker:   "Fork",
				Property: arm.name + " is indistinguishable from the template",
				Detail:   baseline.Diff(got),
				Leak:     firstDivergentPath(baseline, got),
			})
		}
	}
	// A payload that declared a duplication protocol or a runtime affinity
	// and reached two environments anyway contradicts its own declaration.
	for _, arm := range arms {
		for _, sh := range SharedNativePayloads(env, arm.env) {
			if !sh.Cloner && !sh.Bound {
				continue
			}
			out = append(out, Witness{
				Walker:   "Fork",
				Property: "a native payload that declared how it is duplicated is not shared with " + arm.name,
				Leak:     sh.PathB,
				Detail:   sh.String(),
			})
		}
	}
	return out
}

// SharedNative is one native payload reachable from two environments at
// once.
type SharedNative struct {
	// Type is the payload's Go type.
	Type string
	// PathA and PathB are where it was reached in each environment.
	PathA, PathB string
	// Cloner and Bound report which sharing protocol, if any, the payload
	// declares.
	Cloner, Bound bool
}

func (s SharedNative) String() string {
	return fmt.Sprintf("%s reachable at %s and at %s (NativeCloner=%t RuntimeBound=%t)",
		s.Type, s.PathA, s.PathB, s.Cloner, s.Bound)
}

// SharedNativePayloads reports every native payload held by pointer that is
// reachable from BOTH a and b.
//
// This is deliberately not the runtime-affinity check (lisp/runtime_bound.go).
// That protocol is OPT-IN — a payload that never implements RuntimeBound is
// never checked — and its enforcement lives entirely behind `-tags
// elpscheck`, so no production build checks anything.  Pointer identity
// across two environments needs neither: it is observable for every payload
// type, declared or not, in every build, and it is the exact shape of the
// contamination it matters about (one transaction's stateful handle also
// being another's).
//
// It is exported so an embedder can point it at its own values.  For an
// embedder, sharing may be a deliberate choice, so this REPORTS rather than
// judges; the standard library's own expected count is zero, which is what
// the guard's test asserts.
func SharedNativePayloads(a, b *lisp.LEnv) []SharedNative {
	na := reachableNatives(a)
	nb := reachableNatives(b)
	var out []SharedNative
	for payload, pa := range na {
		pb, ok := nb[payload]
		if !ok {
			continue
		}
		_, cloner := payload.(lisp.NativeCloner)
		_, bound := payload.(lisp.RuntimeBound)
		out = append(out, SharedNative{
			Type:   fmt.Sprintf("%T", payload),
			PathA:  pa,
			PathB:  pb,
			Cloner: cloner,
			Bound:  bound,
		})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].PathA < out[j].PathA })
	return out
}

// sharedNativeWitnesses reports a native payload reachable from two
// transactions at once.
//
// A payload that DECLARED a sharing protocol and is shared anyway is always
// a finding: a NativeCloner has stated what its duplicate is, and a
// RuntimeBound has stated which Runtime it belongs to, so either one
// arriving in two forks contradicts its own declaration.  (RuntimeBound is
// enforced by the kernel, but only under `-tags elpscheck`; this reports it
// in every build.)
//
// An UNDECLARED payload is Fork's documented default — share by reference —
// so it is a finding only when the caller says it expects none.
func sharedNativeWitnesses(c TransactionCheck, aName string, a *lisp.LEnv, forks []*lisp.LEnv) []Witness {
	var out []Witness
	report := func(what string, shared []SharedNative) {
		var lines []string
		var leak string
		for _, s := range shared {
			switch {
			case s.Cloner:
				lines = append(lines, s.String()+"  [declares NativeCloner and is shared anyway]")
			case s.Bound:
				lines = append(lines, s.String()+"  [declares RuntimeBound and is shared anyway]")
			case c.ExpectNoSharedNatives:
				lines = append(lines, s.String()+"  [declares nothing; shared by Fork's default policy]")
			default:
				continue
			}
			if leak == "" {
				leak = s.PathB
			}
		}
		if len(lines) == 0 {
			return
		}
		out = append(out, Witness{
			Walker:   "Fork",
			Property: "no stateful native payload is reachable from two transactions at once",
			Detail:   what + ":\n    " + strings.Join(lines, "\n    "),
			Leak:     leak,
			Repro:    c.Repro,
		})
	}
	for i, f := range forks {
		report(fmt.Sprintf("%s and fork %d", aName, i), SharedNativePayloads(a, f))
		for j := i + 1; j < len(forks); j++ {
			report(fmt.Sprintf("fork %d and fork %d", i, j), SharedNativePayloads(f, forks[j]))
		}
	}
	return out
}

// reachableNatives maps every pointer-held native payload reachable from
// env to the first path that reached it.  A payload held by value has no
// identity to share, so it is not collected.
func reachableNatives(env *lisp.LEnv) map[any]string {
	out := map[any]string{}
	seenV := map[*lisp.LVal]bool{}
	seenE := map[*lisp.LEnv]bool{}
	var walk func(v *lisp.LVal, path string)
	var walkEnv func(e *lisp.LEnv, path string)
	walk = func(v *lisp.LVal, path string) {
		if v == nil || seenV[v] {
			return
		}
		seenV[v] = true
		switch v.Type {
		case lisp.LNative:
			if isPointerPayload(v.Native) {
				if _, dup := out[v.Native]; !dup {
					out[v.Native] = path
				}
			}
		case lisp.LFun:
			walkEnv(funraw.Env(v), path+"/env")
		case lisp.LSortMap:
			if md := v.Map(); md != nil {
				for _, k := range md.Keys().Cells {
					val, _ := md.Get(k)
					walk(val, path+"/"+k.String())
				}
			}
		default:
			// Every other type reaches a native only through its cells,
			// which the loop below walks for every type.
		}
		for i, c := range v.Cells {
			walk(c, fmt.Sprintf("%s/%d", path, i))
		}
	}
	walkEnv = func(e *lisp.LEnv, path string) {
		if e == nil || seenE[e] {
			return
		}
		seenE[e] = true
		keys, vals := sortedBindings(e)
		for _, k := range keys {
			walk(vals[k], path+"/"+k)
		}
		walkEnv(e.Parent(), path+"/parent")
	}
	roots(env, func(pkg, name string, v *lisp.LVal) {
		walk(v, pkg+":"+name)
	})
	walkEnv(env, "<env>")
	return out
}

// NativeDeclaration is one native payload TYPE reachable from an
// environment, and what it declares about being copied or shared.
type NativeDeclaration struct {
	// Type is the payload's Go type, as %T renders it.
	Type string
	// Path is the first place a payload of this type was reached.
	Path string
	// Cloner reports whether the type implements lisp.NativeCloner: it has
	// stated what its own duplicate is, so Fork, `copy` and detach all
	// duplicate it rather than sharing or refusing it.
	Cloner bool
	// Bound reports whether the type implements lisp.RuntimeBound: it has
	// declared a runtime affinity, which checked builds enforce.
	Bound bool
	// Stateless reports whether the payload's underlying type is a basic Go
	// type, which has no state to share.
	Stateless bool
}

// Declared reports whether the type has stated its sharing semantics one
// way or another.
func (d NativeDeclaration) Declared() bool { return d.Cloner || d.Bound || d.Stateless }

func (d NativeDeclaration) String() string {
	return fmt.Sprintf("%s at %s (NativeCloner=%t RuntimeBound=%t stateless=%t)",
		d.Type, d.Path, d.Cloner, d.Bound, d.Stateless)
}

// NativeDeclarations classifies every native payload type reachable from
// env.
//
// The point of the classification is that BOTH existing mechanisms miss the
// same case.  The runtime-affinity protocol is opt-in, so a payload that
// forgets to declare anything is never checked; and its enforcement is
// compiled only under `-tags elpscheck`, so no production build checks even
// the payloads that did declare.  A type that is neither a NativeCloner nor
// a RuntimeBound nor provably stateless has therefore said nothing about
// what happens when it is shared by every fork of a template — which is the
// default, and which for a stateful payload means every transaction shares
// it.
//
// Exported so an embedder can run the same census over its own loaded
// environment before shipping a phylum.
func NativeDeclarations(env *lisp.LEnv) []NativeDeclaration {
	byType := map[string]NativeDeclaration{}
	for payload, path := range reachableNatives(env) {
		key := fmt.Sprintf("%T", payload)
		if _, ok := byType[key]; ok {
			continue
		}
		_, cloner := payload.(lisp.NativeCloner)
		_, bound := payload.(lisp.RuntimeBound)
		byType[key] = NativeDeclaration{
			Type:      key,
			Path:      path,
			Cloner:    cloner,
			Bound:     bound,
			Stateless: isStatelessPayload(payload),
		}
	}
	out := make([]NativeDeclaration, 0, len(byType))
	for _, d := range byType {
		out = append(out, d)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Type < out[j].Type })
	return out
}

// isStatelessPayload reports whether a payload's underlying type is a basic
// Go type — a bool, an integer, a float, a complex or a string, possibly
// behind one pointer.  Such a payload holds no reference to anything else,
// so sharing it between two transactions shares nothing they can both
// write.  Anything else (a struct, a slice, a map, a channel, a func) may
// reach mutable state and has to declare.
func isStatelessPayload(payload any) bool {
	t := reflect.TypeOf(payload)
	if t == nil {
		return false
	}
	if t.Kind() == reflect.Pointer {
		t = t.Elem()
	}
	switch t.Kind() {
	case reflect.Bool,
		reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr,
		reflect.Float32, reflect.Float64,
		reflect.Complex64, reflect.Complex128,
		reflect.String:
		return true
	default:
		return false
	}
}
