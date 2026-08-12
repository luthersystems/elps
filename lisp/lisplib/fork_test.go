// Copyright © 2026 The ELPS authors

// Behavioral tests and benchmarks for LEnv.Fork (issue #380) over real
// parsed programs — closures, macros, labels mutual recursion, mutable
// globals.  The structural sharing contract is audited node-by-node in
// lisp/fork_test.go (package lisp); this file exercises the exported
// surface an embedder sees, including the fingerprint-based bidirectional
// isolation property and the fork-vs-full-load benchmark pair.
package lisplib_test

import (
	"fmt"
	"hash"
	"hash/fnv"
	"runtime"
	"sort"
	"testing"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

func newLoadedForkEnv(tb testing.TB) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
		tb.Fatalf("init: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); !rc.IsNil() {
		tb.Fatalf("lisplib: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); !rc.IsNil() {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}

// forkTestProgram approximates what a warm template holds after program
// load: closures over mutable and immutable state, macros, labels mutual
// recursion, and mutable globals of the shapes the fork walker copies.
const forkTestProgram = `
(set 'counter-box (vector 0))
(defun make-adder (n) (lambda (x) (+ x n)))
(set 'add2 (make-adder 2))
(defmacro with-logging (expr) (quasiquote (progn (unquote expr))))
(defun handler (m) (get-default m "k" (with-logging (add2 40))))
(labels ([even? (n) (if (= n 0) true (odd? (- n 1)))]
         [odd? (n) (if (= n 0) false (even? (- n 1)))])
  (set 'evens even?))
(set 'config (sorted-map "a" 1 "b" (vector 1 2 3)))
(set 'blob (to-bytes "mutable-bytes"))
`

func loadForkTestProgram(tb testing.TB, env *lisp.LEnv) {
	tb.Helper()
	if res := env.LoadString("template.lisp", forkTestProgram); res.Type == lisp.LError {
		tb.Fatalf("program: %v", res)
	}
}

func mustFork(tb testing.TB, env *lisp.LEnv, opts ...lisp.ForkOption) *lisp.LEnv {
	tb.Helper()
	fork, err := env.Fork(opts...)
	if err != nil {
		tb.Fatalf("fork: %v", err)
	}
	return fork
}

// TestForkLoadedProgram checks that everything interesting survives the
// remap: closures capture fork state, macros expand, labels-defined mutual
// recursion (the closure↔env cycle case) works, and the template keeps
// serving after forks are taken.
func TestForkLoadedProgram(t *testing.T) {
	env := newLoadedForkEnv(t)
	loadForkTestProgram(t, env)
	fork := mustFork(t, env)

	if r := fork.LoadString("c1.lisp", `(handler (sorted-map "k" 1))`); r.Type == lisp.LError || r.Int != 1 {
		t.Fatalf("fork handler (key present): got %v", r)
	}
	if r := fork.LoadString("c2.lisp", `(handler (sorted-map "x" 1))`); r.Type == lisp.LError || r.Int != 42 {
		t.Fatalf("fork handler (default path through closure add2): got %v", r)
	}
	if r := fork.LoadString("c3.lisp", `(funcall evens 10)`); r.Type == lisp.LError || !lisp.True(r) {
		t.Fatalf("fork labels closure: got %v", r)
	}
	if r := env.LoadString("c4.lisp", `(handler (sorted-map "k" 5))`); r.Type == lisp.LError || r.Int != 5 {
		t.Fatalf("template handler after fork: got %v", r)
	}
}

// TestForkBidirectionalIsolation drives real mutations through both sides
// and asserts neither observes the other: the isolation property stated in
// terms an embedder cares about (vector append, sorted-map assoc, bytes).
func TestForkBidirectionalIsolation(t *testing.T) {
	env := newLoadedForkEnv(t)
	loadForkTestProgram(t, env)
	fork := mustFork(t, env)

	// Template-side mutations invisible to the fork.
	if r := env.LoadString("m1.lisp", `(progn (append! counter-box 99) (assoc! config "a" 100) ())`); r.Type == lisp.LError {
		t.Fatalf("template mutate: %v", r)
	}
	if r := fork.LoadString("f1.lisp", `(length counter-box)`); r.Type == lisp.LError || r.Int != 1 {
		t.Fatalf("fork saw template vector mutation: %v", r)
	}
	if r := fork.LoadString("f2.lisp", `(get config "a")`); r.Type == lisp.LError || r.Int != 1 {
		t.Fatalf("fork saw template map mutation: %v", r)
	}

	// Fork-side mutations invisible to the template.
	if r := fork.LoadString("f3.lisp", `(append! counter-box 7)`); r.Type == lisp.LError {
		t.Fatalf("fork mutate: %v", r)
	}
	if r := env.LoadString("m2.lisp", `(length counter-box)`); r.Type == lisp.LError || r.Int != 2 {
		t.Fatalf("template saw fork mutation: %v", r)
	}
}

// TestForkIsolationFingerprint is the fingerprint edition of the isolation
// property: a deterministic hash of ALL reachable state (env tree plus
// every package symbol).  Fork-side activity must leave the template's
// fingerprint bit-identical, a fresh fork must reproduce the template
// fingerprint exactly, and the mutated fork's fingerprint must move (the
// anti-vacuity control proving the fingerprint can detect mutations at
// all).
func TestForkIsolationFingerprint(t *testing.T) {
	env := newLoadedForkEnv(t)
	loadForkTestProgram(t, env)

	fp0 := envFingerprint(env)
	if fp0 != envFingerprint(env) {
		t.Fatalf("fingerprint not deterministic")
	}

	fork := mustFork(t, env)
	if got := envFingerprint(env); got != fp0 {
		t.Fatalf("taking a fork mutated the template: fp %x != %x", got, fp0)
	}
	if got := envFingerprint(fork); got != fp0 {
		t.Fatalf("fresh fork does not reproduce template state: fp %x != %x", got, fp0)
	}

	forkBefore := envFingerprint(fork)
	if r := fork.LoadString("mut.lisp", `(progn (append! counter-box 99) (assoc! config "a" 100) (set 'newvar 1))`); r.Type == lisp.LError {
		t.Fatalf("fork mutate: %v", r)
	}
	if got := envFingerprint(fork); got == forkBefore {
		t.Fatalf("mutation control failed: fork fingerprint did not move")
	}
	if got := envFingerprint(env); got != fp0 {
		t.Fatalf("fork mutation visible in template: fp %x != %x", got, fp0)
	}

	fork2 := mustFork(t, env)
	if got := envFingerprint(fork2); got != fp0 {
		t.Fatalf("second fork not pristine after first fork's writes: fp %x != %x", got, fp0)
	}

	// Reverse direction: template mutation invisible to existing forks.
	if r := env.LoadString("mut2.lisp", `(append! counter-box 7)`); r.Type == lisp.LError {
		t.Fatalf("template mutate: %v", r)
	}
	if r := fork2.LoadString("chk.lisp", `(length counter-box)`); r.Type == lisp.LError || r.Int != 1 {
		t.Fatalf("fork2 saw template mutation: %v", r)
	}
}

// TestForkLambdaFIDContinuity: lambdas minted after the fork — on either
// side — must not collide with any FID inherited from the template (FIDs
// are "_fun<envID>"; the forked runtime continues the template's env-ID
// counter).
func TestForkLambdaFIDContinuity(t *testing.T) {
	env := newLoadedForkEnv(t)
	loadForkTestProgram(t, env)
	fork := mustFork(t, env)

	inherited := collectFIDs(fork)
	if len(inherited) == 0 {
		t.Fatalf("no inherited FIDs; test is vacuous")
	}
	for name, e := range map[string]*lisp.LEnv{"fork": fork, "template": env} {
		fun := e.LoadString("fid.lisp", `(lambda (x) (+ x 1))`)
		if fun.Type != lisp.LFun {
			t.Fatalf("%s lambda: %v", name, fun)
		}
		if fid := fun.FID(); inherited[fid] {
			t.Errorf("%s post-fork lambda FID %q collides with inherited FID", name, fid)
		}
	}
}

func collectFIDs(env *lisp.LEnv) map[string]bool {
	fids := make(map[string]bool)
	reg := env.Runtime.Registry
	for _, pname := range reg.PackageNames() {
		pkg := reg.Package(pname)
		for _, sname := range pkg.SymbolNames() {
			if v, ok := pkg.Symbol(sname); ok && v.Type == lisp.LFun {
				fids[v.FID()] = true
			}
		}
	}
	return fids
}

// ---------------------------------------------------------------------------
// Full-state fingerprint walker.
//
// Hashes structure and content — never pointer identity — of everything
// reachable from an environment: the env tree's scopes and every package
// symbol, descending through cells, closure-captured environments, sorted
// map entries, and bytes payloads.  Aliasing and cycles are preserved via
// deterministic back-references, so two environments with identical logical
// state (a template and a fresh fork) hash identically even though they
// share no mutable pointers.  Native payloads hash as an opaque type marker
// (their content lives outside the LVal universe).
// ---------------------------------------------------------------------------

func envFingerprint(env *lisp.LEnv) uint64 {
	h := fnv.New64a()
	fp := &fingerprinter{
		h:      h,
		idx:    make(map[*lisp.LVal]int, 1<<12),
		envIdx: make(map[*lisp.LEnv]int, 64),
	}
	fp.env(env)
	reg := env.Runtime.Registry
	names := append([]string(nil), reg.PackageNames()...)
	sort.Strings(names)
	for _, name := range names {
		pkg := reg.Package(name)
		fp.str("pkg:" + name)
		syms := append([]string(nil), pkg.SymbolNames()...)
		sort.Strings(syms)
		for _, s := range syms {
			fp.str("sym:" + s)
			if v, ok := pkg.Symbol(s); ok {
				fp.val(v)
			}
		}
	}
	return h.Sum64()
}

type fingerprinter struct {
	h      hash.Hash64
	idx    map[*lisp.LVal]int
	envIdx map[*lisp.LEnv]int
}

func (fp *fingerprinter) str(s string) {
	_, _ = fp.h.Write([]byte(s))
	_, _ = fp.h.Write([]byte{0})
}

func (fp *fingerprinter) val(v *lisp.LVal) {
	if v == nil {
		fp.str("<nil>")
		return
	}
	if i, ok := fp.idx[v]; ok {
		// Back-reference: preserves aliasing/cycle structure without
		// hashing pointer identity.
		fp.str(fmt.Sprintf("ref:%d", i))
		return
	}
	fp.idx[v] = len(fp.idx)
	quoted := 0
	if v.IsQuoted() {
		quoted = 1
	}
	fp.str(fmt.Sprintf("t:%d q:%d s:%s i:%d f:%g", v.Type, quoted, v.Str, v.Int, v.Float))
	switch v.Type {
	case lisp.LFun:
		// The captured environment is unexported (#382); in-repo tooling
		// reaches it through internal/funraw, and a fingerprint that
		// stopped at the function header would not see closure state —
		// exactly the state fork must not share.
		fp.str("fun:" + v.FID() + ":" + v.Package())
		fp.env(funraw.Env(v))
	case lisp.LBytes:
		fp.str("bytes")
		_, _ = fp.h.Write(v.Bytes())
	case lisp.LSortMap:
		if m := v.Map(); m != nil {
			keys := m.Keys()
			fp.str(fmt.Sprintf("map:%d", len(keys.Cells)))
			for _, k := range keys.Cells {
				kv, _ := m.Get(k)
				fp.val(k)
				fp.val(kv)
			}
		}
	case lisp.LNative:
		fp.str(fmt.Sprintf("native:%T", v.Native))
	default:
		// Every other type is fully described by the scalar header hashed
		// above plus the cells hashed below.
	}
	fp.str(fmt.Sprintf("cells:%d", len(v.Cells)))
	for _, c := range v.Cells {
		fp.val(c)
	}
}

func (fp *fingerprinter) env(e *lisp.LEnv) {
	if e == nil {
		fp.str("env:<nil>")
		return
	}
	if i, ok := fp.envIdx[e]; ok {
		fp.str(fmt.Sprintf("envref:%d", i))
		return
	}
	fp.envIdx[e] = len(fp.envIdx)
	keys := make([]string, 0, len(e.Scope))
	for k := range e.Scope {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	fp.str(fmt.Sprintf("env:%d", len(keys)))
	for _, k := range keys {
		fp.str("k:" + k)
		fp.val(e.Scope[k])
	}
	fp.env(e.Parent)
}

// ---------------------------------------------------------------------------
// Benchmarks: fork vs full load at stdlib scale.  The production-scale
// numbers (a full embedder phylum) are reported on issue #380; these two
// stay in-repo so the ratio is tracked where CI can see it.
// ---------------------------------------------------------------------------

// BenchmarkEnvConstruction compares the two ways to obtain a loaded
// environment.  The subtest split lets benchstat put them side by side
// (`benchstat -col /mode`) and interleaves them within one run, so the two
// arms see the same machine state.
func BenchmarkEnvConstruction(b *testing.B) {
	b.Run("mode=fork", func(b *testing.B) {
		env := newLoadedForkEnv(b)
		loadForkTestProgram(b, env)
		b.ReportAllocs()
		b.ResetTimer()
		for range b.N {
			fork, err := env.Fork()
			if err != nil {
				b.Fatal(err)
			}
			runtime.KeepAlive(fork)
		}
	})
	b.Run("mode=fullload", func(b *testing.B) {
		b.ReportAllocs()
		for range b.N {
			env := newLoadedForkEnv(b)
			loadForkTestProgram(b, env)
			runtime.KeepAlive(env)
		}
	})
}
