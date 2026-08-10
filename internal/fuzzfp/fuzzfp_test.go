// Copyright © 2026 The ELPS authors

package fuzzfp_test

import (
	"fmt"
	"math"
	"regexp"
	"strings"
	"testing"
	"time"
	"unsafe"

	"github.com/luthersystems/elps/internal/fuzzfp"
	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
)

// cyclic is a Go value that points at itself. A native is an arbitrary host
// value, so nothing stops a host handing one in, and a naive reflective walk
// would recurse until the stack ran out.
type cyclic struct {
	Name string
	Self *cyclic
	Peer *cyclic
}

// unexportedFields exists to pin that the walk reads fields reflect refuses to
// hand out through Interface(). If it ever stops doing so, time.Time and
// *regexp.Regexp become opaque and this package's entire claim about LNative
// evaporates -- silently, because an opaque value still fingerprints
// consistently and so still passes every before/after comparison.
type unexportedFields struct {
	n int
	s string
	m map[string]int
}

// TestFingerprintIsDeterministic is the assertion the narrowed LNative
// exclusion rests on.
//
// The tracker (issue #318) excluded LNative contents because "formatting
// arbitrary Go values can traverse a randomised map". Go randomises map
// iteration order PER RANGE STATEMENT PER RUN, so two ranges over one map in a
// single process already disagree -- which is what makes an in-process repeat
// the strong test rather than a weak one. 2000 repetitions over maps large
// enough that a chance agreement is negligible (8! = 40320 orders for the
// smallest of them).
//
// A flaky fingerprint is worse than an honest exclusion: it would report
// perfectly correct builtins as corrupting their arguments, at a rate low
// enough to look like a real intermittent defect.
func TestFingerprintIsDeterministic(t *testing.T) {
	t.Parallel()

	loc, err := time.LoadLocation("America/New_York")
	if err != nil {
		// Not fatal: a stripped container may have no tzdata. The other cases
		// still carry the test.
		loc = time.UTC
	}
	self := &cyclic{Name: "a"}
	self.Self = self
	self.Peer = &cyclic{Name: "b", Self: self}

	shared := []byte("0123456789")

	values := map[string]any{
		"nil":            nil,
		"int":            42,
		"string":         "a\xffb",
		"bytes":          []byte{0, 1, 2},
		"empty-map":      map[string]int{},
		"small-map":      map[string]int{"a": 1},
		"map-8":          map[string]int{"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6, "g": 7, "h": 8},
		"map-nested":     map[string]map[string]int{"x": {"a": 1, "b": 2, "c": 3}, "y": {"d": 4, "e": 5, "f": 6}},
		"map-iface-key":  map[any]any{1: "a", "b": 2, 3.5: []int{1, 2}, true: nil},
		"map-struct-val": map[string]unexportedFields{"k": {n: 1, s: "x", m: map[string]int{"p": 1, "q": 2}}},
		// Pointer-bearing map VALUES. These are what pin the fresh
		// pointer-identity table per entry: with one table shared across the
		// range, an entry's "@N" index depends on how many entries were
		// rendered before it, i.e. on the very iteration order being sorted
		// away, and sorting stable strings would not save it.
		"map-slice-val": map[string][]byte{"a": {1}, "b": {2}, "c": {3}, "d": {4}, "e": {5}, "f": {6}},
		"map-ptr-val": map[string]*cyclic{
			"a": {Name: "a"}, "b": {Name: "b"}, "c": {Name: "c"}, "d": {Name: "d"},
		},
		"unexported":    unexportedFields{n: 7, s: "s", m: map[string]int{"a": 1, "b": 2, "c": 3, "d": 4}},
		"time":          time.Date(2026, 8, 10, 1, 2, 3, 4, loc),
		"duration":      3 * time.Hour,
		"regexp":        regexp.MustCompile(`^(a|b)+[0-9]{2,3}\p{L}$`),
		"regexp-simple": regexp.MustCompile(`abc`),
		"cyclic":        self,
		"shared-slices": [][]byte{shared, shared[2:5], shared},
		"func":          strings.ToUpper,
		"chan":          make(chan int, 1),
		"unsafe":        unsafe.Pointer(&shared), //nolint:gosec // G103: an embedder may hand in any Go value, including this one
		"nan":           math.NaN(),
		"negzero":       math.Copysign(0, -1),
		"deep":          deepNest(64),
		"wide-slice":    makeWide(1000),
	}

	for name, v := range values {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			want := fuzzfp.Fingerprint(v)
			for i := range 2000 {
				if got := fuzzfp.Fingerprint(v); got != want {
					t.Fatalf("fingerprint of %s is not deterministic; repetition %d differs\n want: %s\n  got: %s",
						name, i, want, got)
				}
			}
			if want == "" {
				t.Fatalf("fingerprint of %s is empty; an empty fingerprint compares equal to everything", name)
			}
		})
	}
}

// TestFingerprintSeesInsideUnexportedFields is the counterweight to the
// determinism test.
//
// "Deterministic" is trivially satisfied by a fingerprint that records
// nothing, and a walk that gave up at the first unexported field would do
// exactly that for time.Time and *regexp.Regexp -- the two native types the
// stdlib actually produces. So the fingerprint must also be SENSITIVE: two
// values that differ only in unexported state must fingerprint differently.
func TestFingerprintSeesInsideUnexportedFields(t *testing.T) {
	t.Parallel()

	pairs := []struct {
		name string
		a, b any
	}{
		{"time-instant", time.Unix(0, 0).UTC(), time.Unix(1, 0).UTC()},
		{"time-monotonic-vs-not", time.Unix(0, 0).UTC(), time.Unix(0, 0)},
		{"duration", time.Second, 2 * time.Second},
		{"regexp-pattern", regexp.MustCompile(`a`), regexp.MustCompile(`b`)},
		{"regexp-longest", regexp.MustCompile(`a|ab`), longest(regexp.MustCompile(`a|ab`))},
		{"unexported-int", unexportedFields{n: 1}, unexportedFields{n: 2}},
		{"unexported-map", unexportedFields{m: map[string]int{"a": 1}}, unexportedFields{m: map[string]int{"a": 2}}},
		{"nil-vs-empty-map", map[string]int(nil), map[string]int{}},
		{"nil-vs-empty-slice", []byte(nil), []byte{}},
		{"zero-signs", 0.0, math.Copysign(0, -1)},
		{"slice-cap", makeCap(1, 1), makeCap(1, 2)},
	}
	for _, p := range pairs {
		t.Run(p.name, func(t *testing.T) {
			t.Parallel()
			fa, fb := fuzzfp.Fingerprint(p.a), fuzzfp.Fingerprint(p.b)
			if fa == fb {
				t.Fatalf("two different values fingerprint identically, so a mutation between them"+
					" would be invisible:\n  %s", fa)
			}
		})
	}
}

// TestFingerprintIsInsensitiveToMapInsertionOrder pins the property that makes
// the sort meaningful rather than merely stabilising: a map built in a
// different order is the SAME map and must fingerprint the same. Without this,
// the fingerprint would be stable per-value but would still report a builtin
// that rebuilt an equal map as having mutated it.
func TestFingerprintIsInsensitiveToMapInsertionOrder(t *testing.T) {
	t.Parallel()
	forward := map[string]int{}
	backward := map[string]int{}
	keys := []string{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}
	for i, k := range keys {
		forward[k] = i
	}
	for i := len(keys) - 1; i >= 0; i-- {
		backward[keys[i]] = i
	}
	if a, b := fuzzfp.Fingerprint(forward), fuzzfp.Fingerprint(backward); a != b {
		t.Fatalf("insertion order reached the fingerprint:\n  %s\n  %s", a, b)
	}
}

// TestGuardDetectsNativeMutation is the (b) half of the gate: a builtin that
// reaches inside a native must be reported.
func TestGuardDetectsNativeMutation(t *testing.T) {
	t.Parallel()

	m := map[string]int{"a": 1, "b": 2, "c": 3}
	buf := []byte("abc")
	tm := time.Unix(0, 0).UTC()

	cases := []struct {
		name  string
		v     *lisp.LVal
		mutTe func()
	}{
		{"map-entry", lisp.Native(m), func() { m["a"] = 99 }},
		{"map-grow", lisp.Native(m), func() { m["z"] = 1 }},
		{"slice-element", lisp.Native(buf), func() { buf[0] = 'z' }},
		{"pointer-target", lisp.Native(&tm), func() { tm = tm.Add(time.Second) }},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			// NOT parallel: the cases share the mutable values on purpose, so
			// that each mutation is observed against the state its own Watch
			// recorded.
			g := fuzzfp.Watch(c.v)
			if msg := g.Check(); msg != "" {
				t.Fatalf("clean check reported a violation: %s", msg)
			}
			c.mutTe()
			if msg := g.Check(); msg == "" {
				t.Fatalf("mutating the Go value inside an LNative was NOT reported;"+
					" the LNative half of the guard is blind (%s)", c.name)
			}
		})
	}
}

// TestGuardDetectsNestedNativeMutation pins that the walk reaches natives that
// are not the top-level argument. A guard that only checked the argument
// itself would miss every native inside a list, a map, an array or a
// tagged-value -- which is how fuzzval hands most of them over.
func TestGuardDetectsNestedNativeMutation(t *testing.T) {
	t.Parallel()

	env := newEnv(t)
	for _, c := range []struct {
		name string
		wrap func(native *lisp.LVal) *lisp.LVal
	}{
		{"list", func(n *lisp.LVal) *lisp.LVal { return lisp.SExpr([]*lisp.LVal{lisp.Int(1), n}) }},
		{"vector", func(n *lisp.LVal) *lisp.LVal { return lisp.Vector([]*lisp.LVal{n}) }},
		{"quote", func(n *lisp.LVal) *lisp.LVal { return lisp.Quote(lisp.Quote(n)) }},
		{"tagged", func(n *lisp.LVal) *lisp.LVal { return env.TaggedValue(lisp.Symbol("user:t"), n) }},
		{"sortmap-value", func(n *lisp.LVal) *lisp.LVal {
			m := lisp.SortedMap()
			m.Map().Set(lisp.String("k"), n)
			return m
		}},
		{"deep", func(n *lisp.LVal) *lisp.LVal {
			v := n
			for range 5 {
				v = lisp.SExpr([]*lisp.LVal{lisp.Int(0), v})
			}
			return v
		}},
	} {
		t.Run(c.name, func(t *testing.T) {
			t.Parallel()
			m := map[string]int{"a": 1}
			g := fuzzfp.Watch(c.wrap(lisp.Native(m)))
			if msg := g.Check(); msg != "" {
				t.Fatalf("clean check reported a violation: %s", msg)
			}
			m["a"] = 2
			if msg := g.Check(); msg == "" {
				t.Fatalf("a native nested in a %s was not watched", c.name)
			}
		})
	}
}

// TestGuardDetectsSourceCorruption is the (c) half.
//
// Each case is a write the interpreter must never perform, and the last two
// are the ones the tracker's blanket exclusion was hiding: an in-place edit of
// the shared synthetic Location relocates every value in the process and is
// invisible to lisp.SingletonSnapshot, which compares Source by pointer.
func TestGuardDetectsSourceCorruption(t *testing.T) {
	t.Parallel()

	real1 := &token.Location{File: "a.lisp", Pos: 10, Line: 2, Col: 3}
	real2 := &token.Location{File: "b.lisp", Pos: 20, Line: 4, Col: 5}

	for _, c := range []struct {
		name string
		mut  func(v *lisp.LVal)
		make func() *lisp.LVal
	}{
		{
			name: "real replaced by real",
			make: func() *lisp.LVal { v := lisp.Int(1); v.Source = real1; return v },
			mut:  func(v *lisp.LVal) { v.Source = real2 },
		},
		{
			name: "real replaced by nil",
			make: func() *lisp.LVal { v := lisp.Int(1); v.Source = real1; return v },
			mut:  func(v *lisp.LVal) { v.Source = nil },
		},
		{
			name: "real replaced by synthetic",
			make: func() *lisp.LVal { v := lisp.Int(1); v.Source = real1; return v },
			mut:  func(v *lisp.LVal) { v.Source = &token.Location{Pos: -1} },
		},
		{
			name: "shared synthetic edited in place",
			make: func() *lisp.LVal { return lisp.Int(1) },
			mut:  func(v *lisp.LVal) { v.Source.Pos = 7 },
		},
		{
			name: "real location edited in place",
			make: func() *lisp.LVal {
				v := lisp.Int(1)
				v.Source = &token.Location{File: "c.lisp", Pos: 1, Line: 1, Col: 1}
				return v
			},
			mut: func(v *lisp.LVal) { v.Source.Line = 99 },
		},
		{
			name: "nested cell relocated",
			make: func() *lisp.LVal {
				inner := lisp.Int(1)
				inner.Source = real1
				return lisp.SExpr([]*lisp.LVal{lisp.Symbol("f"), inner})
			},
			mut: func(v *lisp.LVal) { v.Cells[1].Source = real2 },
		},
	} {
		t.Run(c.name, func(t *testing.T) {
			// NOT parallel: the "shared synthetic" case mutates a
			// process-wide Location and puts it back.
			v := c.make()
			g := fuzzfp.Watch(v)
			if msg := g.Check(); msg != "" {
				t.Fatalf("clean check reported a violation: %s", msg)
			}
			before := *v.Source
			c.mut(v)
			msg := g.Check()
			if v.Source != nil && *v.Source != before {
				*v.Source = before // restore the shared Location, if that is what was hit
			}
			if msg == "" {
				t.Fatalf("%s was not reported; the Source half of the guard is blind", c.name)
			}
		})
	}
}

// TestGuardPermitsTheMacroExpansionStamp is the false-positive half, and it is
// the reason the tracker excluded Source in the first place.
//
// stampMacroExpansion replaces a synthetic location with the macro call site
// on every node of an expansion, and it is correct to do so. A guard that
// reported it would be off within a week, so the exact transition it performs
// is permitted -- and only that one, which the test above pins in the other
// direction.
func TestGuardPermitsTheMacroExpansionStamp(t *testing.T) {
	t.Parallel()

	callSite := &token.Location{File: "caller.lisp", Pos: 100, Line: 9, Col: 1}

	// A synthetic node, exactly as every constructor in lisp/ produces.
	v := lisp.SExpr([]*lisp.LVal{lisp.Symbol("f"), lisp.Int(1)})
	if v.Source == nil || v.Source.Pos >= 0 {
		t.Fatalf("expected a synthetic Source on a constructed value, got %+v; the premise of"+
			" the Source rule (stamping only ever overwrites Pos < 0) no longer holds", v.Source)
	}
	g := fuzzfp.Watch(v)
	// What stampMacroExpansion does, with its own guard.
	var stamp func(*lisp.LVal)
	stamp = func(n *lisp.LVal) {
		if n == lisp.Nil() || n == lisp.Bool(true) || n == lisp.Bool(false) {
			return
		}
		if n.Source == nil || n.Source.Pos < 0 {
			n.Source = callSite
		}
		for _, c := range n.Cells {
			stamp(c)
		}
	}
	stamp(v)
	if msg := g.Check(); msg != "" {
		t.Fatalf("the macro-expansion stamp was reported as corruption, which would make the"+
			" guard unusable on every macro in the library: %s", msg)
	}
}

// TestGuardIsDeterministicOverGeneratedValues runs the guard over the whole
// generated corpus -- every shape internal/fuzzval can produce, from every
// seed -- and asserts that Watch/Check on an untouched value is clean.
//
// This is the measurement that decides whether the guard can be switched on at
// all. Any nondeterminism in the fingerprint, and any value shape whose Source
// legitimately moves without a builtin being involved, shows up here as a
// violation on a value nothing has touched.
func TestGuardIsDeterministicOverGeneratedValues(t *testing.T) {
	t.Parallel()
	env := newEnv(t)
	checked := 0
	for i, seed := range fuzzval.Seeds() {
		for rep := range 8 {
			gen := fuzzval.New(seed, env)
			for range 4 {
				v := gen.Value()
				g := fuzzfp.Watch(v)
				if msg := g.Check(); msg != "" {
					t.Fatalf("seed %d (repetition %d) reported a violation on a value NOTHING touched"+
						"\n%s\n--- value ---\n%s", i, rep, msg, v)
				}
				checked++
			}
		}
	}
	if checked < 1000 {
		t.Fatalf("only %d values checked; the corpus this test claims to cover is not being generated", checked)
	}
	t.Logf("clean over %d generated values", checked)
}

func newEnv(tb testing.TB) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		tb.Fatalf("load-library: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}

func deepNest(n int) any {
	var v any = 0
	for range n {
		v = []any{v}
	}
	return v
}

func makeWide(n int) any {
	m := make(map[string]int, n)
	for i := range n {
		m[fmt.Sprintf("k%04d", i)] = i
	}
	return m
}

func makeCap(l, c int) []int { return make([]int, l, c) }

func longest(re *regexp.Regexp) *regexp.Regexp {
	cp := re.Copy()
	cp.Longest()
	return cp
}

// TestGuardPermitsEveryReplacementOfASyntheticLocation is the other
// false-positive guard, and it is why the Source rule stops where it does.
//
// LEnv.errorSource stamps an error's location under the same
// `Source == nil || Source.Pos < 0` condition stampMacroExpansion uses, and it
// assigns env.Loc -- which is itself assigned from an evaluated node's Source
// and can therefore be nil. Every transition below is reachable from correct
// interpreter code, so reporting any of them would make the gate flaky rather
// than strict.
func TestGuardPermitsEveryReplacementOfASyntheticLocation(t *testing.T) {
	t.Parallel()
	for _, c := range []struct {
		name string
		to   *token.Location
	}{
		{"to a real location", &token.Location{File: "x.lisp", Pos: 3, Line: 1, Col: 4}},
		{"to nil", nil},
		{"to another synthetic location", &token.Location{File: "<native code>", Pos: -1}},
	} {
		t.Run(c.name, func(t *testing.T) {
			t.Parallel()
			v := lisp.Int(1)
			if v.Source == nil || v.Source.Pos >= 0 {
				t.Fatalf("expected a synthetic Source on a constructed value, got %+v", v.Source)
			}
			g := fuzzfp.Watch(v)
			v.Source = c.to
			if msg := g.Check(); msg != "" {
				t.Fatalf("a transition the interpreter itself performs was reported: %s", msg)
			}
		})
	}
}
