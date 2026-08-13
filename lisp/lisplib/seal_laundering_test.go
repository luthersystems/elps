// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"context"
	"fmt"
	"sort"
	"testing"
	"time"
	"unsafe"

	"github.com/luthersystems/elps/lisp"
)

// The SEAL-LAUNDERING oracle: the producer-side half of the substrate#378
// class.
//
// FuzzApplyStdlib's invariant 5 asks whether a callable WROTE a sealed
// argument in place.  That catches the mutator.  It cannot catch the
// PRODUCER, and the producer is the harder half:
//
//	A callable that returns a fresh, UNSEALED header over a sealed
//	argument's backing array writes nothing.  It passes the fingerprint
//	check.  What it has done is hand the value domain a mutable window onto
//	shared program storage, and the corrupting write happens LATER, through
//	an ordinary mutator (append 'vector, append!, stable-sort) that has no
//	way to know the storage it is extending belongs to a program literal.
//
// elps#392 is exactly this shape: libelpspath's rangePath.Get slices a
// sealed list's cells and wraps the result with lisp.SExpr/lisp.Array, both
// of which mint an unsealed header.  `(elpspath:? (limits) '(range 0 2))`
// followed by `(append 'vector … 99)` permanently rewrites the literal for
// every environment sharing the parse.
//
// The kernel's own slicing producers avoid it only because they copy the
// flag by hand -- builtinCdr, builtinRest and builtinSlice all end with
// `r.sealed = v.sealed`.  Nothing enforces that; it is a convention, and
// #392 is what happens when a producer outside lisp/builtins.go does not
// follow it.
//
// This test states the missing invariant directly, as a pointer property
// that needs no mutator to exist:
//
//	No value reachable from a callable's RESULT may be unsealed while its
//	Cells backing array lies inside the backing array of a SEALED argument.
//
// Because it is structural it fires on the PRODUCER, naming the function
// that laundered the seal, instead of on whichever mutator later wrote
// through the window.
//
// WHY A HAND-BUILT ARGUMENT POOL rather than fuzzval.  The interesting
// branches need well-typed arguments: a type-specifier symbol, an in-range
// index, a well-formed '(range a b) step.  fuzzval's random bytes reach them
// so rarely that a fuzz-driven version of this sweep passes essentially
// vacuously (measured: zero laundering reports over the whole seed corpus,
// including on elpspath:? itself).  The pool below is built to reach them,
// and TestSealLaunderingOracleRedProof pins that it does.

// launderAllowed lists callables KNOWN to launder, each with its issue.  An
// entry that stops laundering is also a failure: the allowlist must not rot
// silently once the bug is fixed.
var launderAllowed = map[string]string{
	"elpspath:?": "elps#392 -- rangePath.Get wraps a sealed list's cell slice in an unsealed list/vector",
}

const lvalPtrSize = unsafe.Sizeof((*lisp.LVal)(nil))

type sealedRegion struct {
	base uintptr
	size uintptr
	repr string
}

// sealedRegionsOf records the backing array of every sealed container
// reachable from vs.
func sealedRegionsOf(vs []*lisp.LVal) []sealedRegion {
	var out []sealedRegion
	seen := map[*lisp.LVal]bool{}
	var walk func(v *lisp.LVal, depth int)
	walk = func(v *lisp.LVal, depth int) {
		if v == nil || depth > 64 || seen[v] {
			return
		}
		seen[v] = true
		if v.IsSealed() && len(v.Cells) > 0 {
			out = append(out, sealedRegion{
				base: uintptr(unsafe.Pointer(unsafe.SliceData(v.Cells))),
				size: uintptr(cap(v.Cells)) * lvalPtrSize,
				repr: v.String(),
			})
		}
		for _, c := range v.Cells {
			walk(c, depth+1)
		}
	}
	for _, v := range vs {
		walk(v, 0)
	}
	return out
}

// findSealLaundering walks a result looking for an unsealed header whose
// backing array sits inside a sealed argument's backing array.
func findSealLaundering(result *lisp.LVal, regions []sealedRegion) (string, bool) {
	var found string
	hit := false
	seen := map[*lisp.LVal]bool{}
	var walk func(v *lisp.LVal, depth int, path string)
	walk = func(v *lisp.LVal, depth int, path string) {
		if v == nil || hit || depth > 64 || seen[v] {
			return
		}
		seen[v] = true
		if !v.IsSealed() && len(v.Cells) > 0 {
			p := uintptr(unsafe.Pointer(unsafe.SliceData(v.Cells)))
			for _, r := range regions {
				if p >= r.base && p < r.base+r.size {
					found = fmt.Sprintf(
						"unsealed %v at %s (%v) shares the backing array of sealed %s",
						v.Type, path, v, r.repr)
					hit = true
					return
				}
			}
		}
		for i, c := range v.Cells {
			walk(c, depth+1, fmt.Sprintf("%s.Cells[%d]", path, i))
		}
	}
	walk(result, 0, "result")
	return found, hit
}

// launderPool builds the argument pool for one application.  The MUTABLE
// members are rebuilt every time: without that an in-place mutator
// (append!, assoc!) can store a pool container inside itself and every
// later application in the sweep walks a cycle.
type launderPool struct {
	subject, rangeStep, nested *lisp.LVal
	lam, lam2                  *lisp.LVal
}

func newLaunderPool(env *lisp.LEnv) *launderPool {
	p := &launderPool{}
	p.subject = lisp.QExpr([]*lisp.LVal{
		lisp.Int(10), lisp.Int(20), lisp.Int(30), lisp.Int(40),
	})
	p.subject.SealAST()
	p.rangeStep = lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0), lisp.Int(2)})
	p.rangeStep.SealAST()
	p.nested = lisp.QExpr([]*lisp.LVal{
		lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}),
		lisp.QExpr([]*lisp.LVal{lisp.Int(3), lisp.Int(4)}),
	})
	p.nested.SealAST()
	p.lam = env.LoadString("pool", `(lambda (x &rest r) x)`)
	p.lam2 = env.LoadString("pool", `(lambda (a b) (< a b))`)
	return p
}

// sealBearingCount is how many leading pool entries carry sealed storage.
// Every generated tuple must include at least one of them.
const sealBearingCount = 4

// fresh returns the pool for an n-ary application.  Entries [0,
// sealBearingCount) carry the sealed subject, directly or nested inside a
// runtime container -- a producer that digs a literal out of a vector or map
// and re-wraps it is in scope too.
func (p *launderPool) fresh(n int) []*lisp.LVal {
	inVec := lisp.Array(nil, []*lisp.LVal{p.subject, lisp.Int(7)})
	inMap := lisp.SortedMap()
	inMap.MapSet("a", p.subject)
	inList := lisp.QExpr([]*lisp.LVal{p.subject, lisp.Int(7)})
	base := []*lisp.LVal{p.subject, inVec, inMap, inList, p.rangeStep}
	switch {
	case n >= 4:
		return append(base,
			lisp.Symbol("list"), lisp.Symbol("vector"), lisp.Int(0), lisp.Int(2))
	case n == 3:
		return append(base,
			lisp.Symbol("list"), lisp.Symbol("vector"), lisp.Symbol("*"),
			lisp.Int(0), lisp.Int(2), lisp.String("a"), p.lam, p.lam2)
	default:
		vec := lisp.Array(nil, []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
		m := lisp.SortedMap()
		m.MapSet("a", lisp.Int(1))
		return append(base, p.nested,
			lisp.Symbol("list"), lisp.Symbol("vector"), lisp.Symbol("bytes"),
			lisp.Symbol("string"), lisp.Symbol("*"),
			lisp.Int(0), lisp.Int(1), lisp.Int(2), lisp.Int(-1),
			lisp.String("a"), p.lam, p.lam2, m, vec, lisp.Bytes([]byte("abc")))
	}
}

func launderRequiredArity(fun *lisp.LVal) int {
	n := 0
	for _, sym := range fun.Cells[0].Cells {
		switch sym.Str {
		case lisp.VarArgSymbol, lisp.OptArgSymbol, lisp.KeyArgSymbol:
			return n
		default:
			n++
		}
	}
	return n
}

// applyForLaundering applies fun and reports laundering.  A panic or an
// LError result is not a laundering report: this oracle asks only about
// storage provenance, and refusing a badly-typed tuple is correct behaviour.
func applyForLaundering(env *lisp.LEnv, fun *lisp.LVal, args []*lisp.LVal, regions []sealedRegion) (msg string, bad bool) {
	defer func() {
		if r := recover(); r != nil {
			msg, bad = "", false
		}
	}()
	result := apply(env, fun, lisp.SExpr(args))
	if result == nil || result.Type == lisp.LError {
		return "", false
	}
	return findSealLaundering(result, regions)
}

// sweepLaundering applies every stdlib callable over tuples drawn from the
// pool and returns, per callable, the first laundering report.
func sweepLaundering(t *testing.T) (map[string]string, int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 300*time.Second)
	defer cancel()

	hits := map[string]string{}
	calls := 0
	for _, name := range callableNames(t) {
		if skipCallable(name) {
			continue
		}
		env := fuzzEnv(t, ctx)
		fun := env.GetGlobal(lisp.Symbol(name))
		if fun.Type != lisp.LFun {
			continue
		}
		pool := newLaunderPool(env)
		lo := launderRequiredArity(fun)
		if lo < 1 {
			lo = 1
		}
		if lo > 4 {
			continue
		}
		hi := min(lo+2, 4)
		for n := lo; n <= hi; n++ {
			poolLen := len(pool.fresh(n))
			idx := make([]int, n)
			for {
				sealBearing := false
				for _, i := range idx {
					if i < sealBearingCount {
						sealBearing = true
					}
				}
				if sealBearing {
					cur := pool.fresh(n)
					args := make([]*lisp.LVal, n)
					for k, i := range idx {
						args[k] = cur[i]
					}
					calls++
					if msg, bad := applyForLaundering(env, fun, args, sealedRegionsOf(cur[:sealBearingCount])); bad {
						if _, seen := hits[name]; !seen {
							hits[name] = msg
						}
					}
				}
				p := n - 1
				for p >= 0 {
					idx[p]++
					if idx[p] < poolLen {
						break
					}
					idx[p] = 0
					p--
				}
				if p < 0 {
					break
				}
			}
		}
	}
	return hits, calls
}

// TestSealLaunderingOracleRedProof pins that the oracle can see the class.
// Without it a clean sweep proves nothing.
func TestSealLaunderingOracleRedProof(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	env := fuzzEnv(t, ctx)
	pool := newLaunderPool(env)
	regions := sealedRegionsOf([]*lisp.LVal{pool.subject})

	// RED: elps#392's producer must be reported.
	fun := env.GetGlobal(lisp.Symbol("elpspath:?"))
	if fun.Type != lisp.LFun {
		t.Fatalf("elpspath:? is not a function: %v", fun.Type)
	}
	if _, bad := applyForLaundering(env, fun, []*lisp.LVal{pool.subject, pool.rangeStep}, regions); !bad {
		t.Fatal("oracle did not report elps#392's producer: a clean sweep would be meaningless")
	}

	// GREEN: the kernel producers that DO propagate the flag must not be.
	for _, g := range []struct {
		fn   string
		args []*lisp.LVal
	}{
		{"cdr", []*lisp.LVal{pool.subject}},
		{"rest", []*lisp.LVal{pool.subject}},
		{"slice", []*lisp.LVal{lisp.Symbol("list"), pool.subject, lisp.Int(0), lisp.Int(2)}},
		{"slice", []*lisp.LVal{lisp.Symbol("vector"), pool.subject, lisp.Int(0), lisp.Int(2)}},
		{"append", []*lisp.LVal{lisp.Symbol("vector"), pool.subject, lisp.Int(9)}},
	} {
		f := env.GetGlobal(lisp.Symbol(g.fn))
		if msg, bad := applyForLaundering(env, f, g.args, regions); bad {
			t.Errorf("%s must not launder (it copies the flag by hand): %s", g.fn, msg)
		}
	}
}

// TestSealLaunderingSweep is the sweep itself.
func TestSealLaunderingSweep(t *testing.T) {
	hits, calls := sweepLaundering(t)
	t.Logf("%d applications over the stdlib callable surface", calls)

	names := make([]string, 0, len(hits))
	for k := range hits {
		names = append(names, k)
	}
	sort.Strings(names)
	for _, name := range names {
		if _, ok := launderAllowed[name]; !ok {
			t.Errorf("%s launders a seal into its result: %s\n\n"+
				"It returns an unsealed value over a sealed argument's backing array. A later\n"+
				"append 'vector / append! / stable-sort on that value writes the shared program\n"+
				"literal, corrupting it for every environment sharing the parse (substrate#378).\n"+
				"Fix it the way builtinCdr/builtinRest/builtinSlice do: propagate the flag onto\n"+
				"the new header (r.sealed = v.sealed), or return a copy.",
				name, hits[name])
		}
	}
	for name, why := range launderAllowed {
		if _, ok := hits[name]; !ok {
			t.Errorf("%s no longer launders (%s) -- remove it from launderAllowed so the "+
				"oracle stays honest", name, why)
		}
	}
}
