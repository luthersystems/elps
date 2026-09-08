// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"fmt"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/internal/jsonraw"
	"github.com/luthersystems/elps/internal/stdlib"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

func newTemplateTestEnv(tb testing.TB) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if got := lisp.InitializeUserEnv(env); got.Type == lisp.LError {
		tb.Fatalf("initialize environment: %v", got)
	}
	if got := stdlib.Load(env, false); got.Type == lisp.LError {
		tb.Fatalf("load standard library: %v", got)
	}
	return env
}

func loadTemplateFixture(tb testing.TB, src string) *lisp.LEnv {
	tb.Helper()
	env := newTemplateTestEnv(tb)
	if got := env.LoadString("template.lisp", src); got.Type == lisp.LError {
		tb.Fatalf("load fixture: %v", got)
	}
	return env
}

func snapshotFixture(tb testing.TB, env *lisp.LEnv) *lisp.Template {
	tb.Helper()
	tmpl, err := lisp.NewTemplate(env, templateFixturePolicy())
	if err != nil {
		tb.Fatalf("new template: %v", err)
	}
	return tmpl
}

func templateFixturePolicy() lisp.TemplateOption {
	// These fixed fixtures load only the core and standard library. Their
	// static Go functions are audited as stateless; dynamic schema validators
	// declare their mutable captures separately. An embedder accepting host
	// plugins must supply its own narrower audit, not copy this predicate.
	return lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool {
		return v.Builtin() != nil
	})
}

func forkTemplateFixture(tb testing.TB, tmpl *lisp.Template) *lisp.LEnv {
	tb.Helper()
	env, err := tmpl.NewVM()
	if err != nil {
		tb.Fatalf("template fork: %v", err)
	}
	return env
}

func assertTemplateValue(tb testing.TB, env *lisp.LEnv, src string, want *lisp.LVal) {
	tb.Helper()
	got := env.LoadString("transaction.lisp", src)
	if got.Type == lisp.LError || !lisp.True(got.Equal(want)) {
		tb.Fatalf("%s: got %v, want %v", src, got, want)
	}
}

// TestTemplateColdParity compares two independently loaded VMs with two
// forks. Expected results are pinned independently of either construction
// path, and untouched siblings are observed after every transaction.
func TestTemplateColdParity(t *testing.T) {
	list := func(ns ...int) *lisp.LVal {
		cells := make([]*lisp.LVal, len(ns))
		for i, n := range ns {
			cells[i] = lisp.Int(n)
		}
		return lisp.QExpr(cells)
	}
	for _, tc := range []struct {
		name    string
		setup   string
		observe string
		initial *lisp.LVal
		steps   []string
		want    []*lisp.LVal
	}{
		{
			name: "cdr-and-slice-slot-aliases",
			setup: `(set 'xs (list 30 10 20)) (set 'tail (cdr xs))
(set 'middle (slice 'list xs 0 2))`,
			observe: `(list (nth xs 0) (nth xs 1) (nth xs 2)
(nth tail 0) (nth tail 1) (nth middle 0) (nth middle 1))`,
			initial: list(30, 10, 20, 10, 20, 30, 10),
			steps:   []string{`(stable-sort < xs)`, `(stable-sort > tail)`},
			want:    []*lisp.LVal{list(10, 20, 30, 20, 30, 10, 20), list(10, 30, 20, 30, 20, 10, 30)},
		},
		{
			name:  "vector-rest-alias",
			setup: `(set 'xs (vector 30 10 20)) (set 'tail (rest xs))`,
			observe: `(list (nth xs 0) (nth xs 1) (nth xs 2)
(nth tail 0) (nth tail 1))`,
			initial: list(30, 10, 20, 10, 20),
			steps:   []string{`(stable-sort < xs)`, `(stable-sort > tail)`},
			want:    []*lisp.LVal{list(10, 20, 30, 20, 30), list(10, 30, 20, 30, 20)},
		},
		{
			name: "map-payload-and-cycle",
			setup: `(set 'a (sorted-map "k" 1))
(set 'alias (quasiquote (unquote a))) (assoc! a "self" alias)`,
			observe: `(list (get a "k") (get alias "k") (get (get a "self") "k"))`,
			initial: list(1, 1, 1),
			steps:   []string{`(assoc! a "k" 2)`, `(assoc! (get a "self") "k" 3)`},
			want:    []*lisp.LVal{list(2, 2, 2), list(3, 3, 3)},
		},
		{
			name:    "bytes-payload-alias",
			setup:   `(set 'a (to-bytes "ab")) (set 'alias (quasiquote (unquote a)))`,
			observe: `(list (to-string a) (to-string alias))`,
			initial: lisp.QExpr([]*lisp.LVal{lisp.String("ab"), lisp.String("ab")}),
			steps:   []string{`(append! a 99)`, `(append! alias 100)`},
			want: []*lisp.LVal{
				lisp.QExpr([]*lisp.LVal{lisp.String("abc"), lisp.String("abc")}),
				lisp.QExpr([]*lisp.LVal{lisp.String("abcd"), lisp.String("abcd")}),
			},
		},
		{
			name: "schema-capture-follows-global-map",
			setup: `(set 'allowed (sorted-map "k" 1))
(s:deftype "T" s:any (s:in allowed))`,
			observe: `(s:validate T allowed) (get allowed "k")`,
			initial: lisp.Int(1),
			steps:   []string{`(assoc! allowed "k" 2)`, `(assoc! allowed "k" 3)`},
			want:    []*lisp.LVal{lisp.Int(2), lisp.Int(3)},
		},
		{
			name: "closure-captured-state",
			setup: `(set 'state (sorted-map "n" 0))
(set 'advance (let ([box state])
  (lambda (n) (assoc! box "n" (+ (get box "n") n)) (get box "n"))))`,
			observe: `(funcall advance 0)`,
			initial: lisp.Int(0),
			steps:   []string{`(funcall advance 7)`, `(funcall advance 11)`},
			want:    []*lisp.LVal{lisp.Int(7), lisp.Int(18)},
		},
	} {
		for _, interleaved := range []bool{false, true} {
			t.Run(fmt.Sprintf("%s/interleaved=%t", tc.name, interleaved), func(t *testing.T) {
				source := loadTemplateFixture(t, tc.setup)
				tmpl := snapshotFixture(t, source)
				cold := [2]*lisp.LEnv{loadTemplateFixture(t, tc.setup), loadTemplateFixture(t, tc.setup)}
				forks := [2]*lisp.LEnv{forkTemplateFixture(t, tmpl), forkTemplateFixture(t, tmpl)}
				state := [2]*lisp.LVal{tc.initial, tc.initial}
				step := func(vm, n int) {
					t.Helper()
					transaction := tc.steps[n] + "\n" + tc.observe
					assertTemplateValue(t, cold[vm], transaction, tc.want[n])
					assertTemplateValue(t, forks[vm], transaction, tc.want[n])
					state[vm] = tc.want[n]
					assertTemplateValue(t, cold[1-vm], tc.observe, state[1-vm])
					assertTemplateValue(t, forks[1-vm], tc.observe, state[1-vm])
					assertTemplateValue(t, source, tc.observe, tc.initial)
				}
				if interleaved {
					for n := range tc.steps {
						for vm := range 2 {
							step(vm, n)
						}
					}
				} else {
					for vm := range 2 {
						for n := range tc.steps {
							step(vm, n)
						}
					}
				}
				// Caller-owned mutable aliases must not retain access to the
				// private snapshot after template publication.
				assertTemplateValue(t, source, tc.steps[0]+"\n"+tc.observe, tc.want[0])
				assertTemplateValue(t, forkTemplateFixture(t, tmpl), tc.observe, tc.initial)
				for vm := range 2 {
					assertTemplateValue(t, forks[vm], tc.observe, state[vm])
				}
			})
		}
	}
}

// Host-created views deliberately have no retained root header, so their
// relationship cannot be recovered by following a parent value.
func TestTemplateOverlappingByteViewsWithoutRoot(t *testing.T) {
	loaded := func() *lisp.LEnv {
		env := loadTemplateFixture(t, "")
		backing := []byte("abcdef")
		env.PutGlobal(lisp.Symbol("left"), lisp.Bytes(backing[:4:4]))
		env.PutGlobal(lisp.Symbol("right"), lisp.Bytes(backing[2:6:6]))
		return env
	}
	source := loaded()
	tmpl := snapshotFixture(t, source)
	cold := [2]*lisp.LEnv{loaded(), loaded()}
	forks := [2]*lisp.LEnv{forkTemplateFixture(t, tmpl), forkTemplateFixture(t, tmpl)}
	for vm, ch := range []byte{'X', 'Y'} {
		for _, env := range []*lisp.LEnv{cold[vm], forks[vm]} {
			env.Get(lisp.Symbol("left")).Bytes()[2] = ch
			assertTemplateValue(t, env, `(to-string right)`, lisp.String(string(ch)+"def"))
			env.Get(lisp.Symbol("right")).Bytes()[1] = ch + 1
			assertTemplateValue(t, env, `(to-string left)`, lisp.String("ab"+string([]byte{ch, ch + 1})))
		}
		assertTemplateValue(t, source, `(to-string right)`, lisp.String("cdef"))
	}
	assertTemplateValue(t, forks[0], `(to-string right)`, lisp.String("XYef"))
	source.Get(lisp.Symbol("right")).Bytes()[0] = 'Z'
	assertTemplateValue(t, forkTemplateFixture(t, tmpl), `(to-string left)`, lisp.String("abcd"))
	assertTemplateValue(t, forks[1], `(to-string right)`, lisp.String("YZef"))
}

func TestTemplateOverlappingCellViewsWithoutRoot(t *testing.T) {
	loaded := func() *lisp.LEnv {
		env := loadTemplateFixture(t, "")
		backing := []*lisp.LVal{lisp.Int(4), lisp.Int(3), lisp.Int(2), lisp.Int(1), lisp.Int(9), lisp.Int(8)}
		env.PutGlobal(lisp.Symbol("left"), lisp.QExpr(backing[:4:4]))
		env.PutGlobal(lisp.Symbol("right"), lisp.QExpr(backing[2:6:6]))
		return env
	}
	source := loaded()
	tmpl := snapshotFixture(t, source)
	sibling := forkTemplateFixture(t, tmpl)
	for _, env := range []*lisp.LEnv{loaded(), forkTemplateFixture(t, tmpl)} {
		assertTemplateValue(t, env, `(stable-sort < left) (first right)`, lisp.Int(3))
		assertTemplateValue(t, env, `(stable-sort > right) (nth left 2)`, lisp.Int(9))
		assertTemplateValue(t, env, `(nth left 3)`, lisp.Int(8))
	}
	assertTemplateValue(t, source, `(first left)`, lisp.Int(4))
	assertTemplateValue(t, sibling, `(first right)`, lisp.Int(2))
	assertTemplateValue(t, source, `(stable-sort < left) (first right)`, lisp.Int(3))
	assertTemplateValue(t, forkTemplateFixture(t, tmpl), `(first right)`, lisp.Int(2))
}

func TestTemplateRejectsMutableViewsOfSharedCells(t *testing.T) {
	for _, tc := range []struct {
		name    string
		setup   string
		symbol  string
		observe string
		want    *lisp.LVal
	}{
		{"literal", `(set 'literal '(3 1 2))`, "literal", `(first literal)`, lisp.Int(3)},
		{"function", `(defun identity (x) x)`, "identity", `(identity 7)`, lisp.Int(7)},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := loadTemplateFixture(t, tc.setup)
			shared, ok := env.Runtime.Package.Symbol(tc.symbol)
			if !ok || len(shared.Cells) == 0 {
				t.Fatal("fixture has no shared cell storage")
			}
			view := lisp.QExpr(shared.Cells)
			if view.IsSealed() {
				t.Fatal("fixture must expose shared storage through an unsealed header")
			}
			if got := env.PutGlobal(lisp.Symbol("mutable-view"), view); got.Type == lisp.LError {
				t.Fatal(got)
			}
			got, err := lisp.NewTemplate(env, templateFixturePolicy())
			if got != nil || err == nil || !strings.Contains(err.Error(), "overlaps shared") {
				t.Fatalf("mutable view admitted or wrong rejection: template=%v error=%v", got, err)
			}
			assertTemplateValue(t, env, tc.observe, tc.want)
		})
	}
}

func TestTemplateSchemaRejectionMatchesCold(t *testing.T) {
	const setup = `(set 'allowed (sorted-map "k" 1)) (s:deftype "T" s:any (s:in allowed))`
	tmpl := snapshotFixture(t, loadTemplateFixture(t, setup))
	for _, env := range []*lisp.LEnv{loadTemplateFixture(t, setup), forkTemplateFixture(t, tmpl)} {
		assertTemplateValue(t, env, `(assoc! allowed "k" 2) (s:validate T allowed)`, lisp.Nil())
		got := env.LoadString("invalid.lisp", `(s:validate T (sorted-map "k" 1))`)
		if got.Type != lisp.LError || !strings.Contains(got.String(), "failed-constraint") {
			t.Fatalf("stale allowed map accepted or wrong failure: %v", got)
		}
		assertTemplateValue(t, env, `(s:validate T allowed)`, lisp.Nil())
	}
}

func TestTemplateJSONMapKeyPolicyMatchesCold(t *testing.T) {
	const setup = `(set 'doc (json:load-string "{\"k\":1}" :exact-integers true))`
	source := loadTemplateFixture(t, setup)
	tmpl := snapshotFixture(t, source)
	cold := loadTemplateFixture(t, setup)
	first, sibling := forkTemplateFixture(t, tmpl), forkTemplateFixture(t, tmpl)
	for _, env := range []*lisp.LEnv{cold, first, sibling} {
		assertTemplateValue(t, env, `(get doc "k")`, lisp.Int(1))
	}
	for _, src := range []string{`(get doc 'k)`, `(assoc! doc 'k 9)`, `(dissoc! doc 'k)`} {
		want := cold.LoadString("key-policy.lisp", src)
		if want.Type != lisp.LError || !strings.Contains(want.String(), "decoded from json cannot hold key with type 'symbol") {
			t.Fatalf("cold control did not reject symbol key: %v", want)
		}
		for _, env := range []*lisp.LEnv{first, sibling} {
			got := env.LoadString("key-policy.lisp", src)
			if got.Type != lisp.LError || got.String() != want.String() {
				t.Fatalf("%s: got %v, want cold error %v", src, got, want)
			}
			assertTemplateValue(t, env, `(get doc "k")`, lisp.Int(1))
		}
	}
	for _, env := range []*lisp.LEnv{cold, first} {
		assertTemplateValue(t, env, `(assoc! doc "k" 2) (get doc "k")`, lisp.Int(2))
	}
	assertTemplateValue(t, sibling, `(get doc "k")`, lisp.Int(1))
	assertTemplateValue(t, source, `(get doc "k")`, lisp.Int(1))
	assertTemplateValue(t, source, `(assoc! doc "k" 99) (get doc "k")`, lisp.Int(99))
	assertTemplateValue(t, forkTemplateFixture(t, tmpl), `(get doc "k")`, lisp.Int(1))
}

func TestTemplateJSONBackingAliasesAcrossMapData(t *testing.T) {
	loaded := func() *lisp.LEnv {
		env := loadTemplateFixture(t, "")
		backing := map[string]any{"k": lisp.Int(1)}
		a := jsonraw.Wrap(backing)
		b := jsonraw.Wrap(backing)
		backing["self"] = b
		if a.Map() == b.Map() {
			t.Fatal("fixture must use distinct MapData wrappers")
		}
		for name, value := range map[string]*lisp.LVal{"a": a, "b": b} {
			if got := env.PutGlobal(lisp.Symbol(name), value); got.Type == lisp.LError {
				t.Fatal(got)
			}
		}
		return env
	}
	source := loaded()
	tmpl := snapshotFixture(t, source)
	sibling := forkTemplateFixture(t, tmpl)
	for _, env := range []*lisp.LEnv{loaded(), forkTemplateFixture(t, tmpl)} {
		assertTemplateValue(t, env, `(assoc! a "k" 2) (get b "k")`, lisp.Int(2))
		assertTemplateValue(t, env, `(get (get a "self") "k")`, lisp.Int(2))
		assertTemplateValue(t, env, `(assoc! (get a "self") "k" 3) (get a "k")`, lisp.Int(3))
		assertTemplateValue(t, env, `(get b "k")`, lisp.Int(3))
	}
	assertTemplateValue(t, source, `(get a "k")`, lisp.Int(1))
	assertTemplateValue(t, sibling, `(get b "k")`, lisp.Int(1))
	assertTemplateValue(t, forkTemplateFixture(t, tmpl), `(get (get a "self") "k")`, lisp.Int(1))
}

func TestTemplateConcurrentColdParity(t *testing.T) {
	const setup = `(set 'state (sorted-map "n" 0))
(set 'xs (list 3 1 2)) (set 'tail (cdr xs))
(s:deftype "T" s:any (s:in state))
(defun advance (n)
  (assoc! state "n" (+ n (get state "n")))
  (stable-sort < xs) (s:validate T state)
  (list (get state "n") (first tail)))`
	tmpl := snapshotFixture(t, loadTemplateFixture(t, setup))
	const workers, transactions = 8, 12
	var requests, expected [workers][transactions]string
	for worker := range requests {
		cold := loadTemplateFixture(t, setup)
		for n := range requests[worker] {
			requests[worker][n] = fmt.Sprintf("(advance %d)", worker+1)
			want := lisp.QExpr([]*lisp.LVal{lisp.Int((worker + 1) * (n + 1)), lisp.Int(2)})
			assertTemplateValue(t, cold, requests[worker][n], want)
			expected[worker][n] = want.String()
		}
	}
	start := make(chan struct{})
	errs := make(chan error, workers)
	var wg sync.WaitGroup
	for worker := range requests {
		wg.Go(func() {
			<-start
			env, err := tmpl.NewVM()
			if err != nil {
				errs <- fmt.Errorf("worker %d fork: %w", worker, err)
				return
			}
			for n, src := range requests[worker] {
				got := env.LoadString("transaction.lisp", src)
				if got.Type == lisp.LError || got.String() != expected[worker][n] {
					errs <- fmt.Errorf("worker %d transaction %d: got %v, want %s", worker, n, got, expected[worker][n])
					return
				}
			}
		})
	}
	close(start)
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}
	assertTemplateValue(t, forkTemplateFixture(t, tmpl), `(get state "n")`, lisp.Int(0))
}

// BenchmarkTemplateConstruction separates publication from repeated VM
// construction, comparing the same library/program set with cached cold loads.
func BenchmarkTemplateConstruction(b *testing.B) {
	for _, functions := range []int{0, 250} {
		b.Run(fmt.Sprintf("functions=%d", functions), func(b *testing.B) {
			var src strings.Builder
			src.WriteString(`(set 'xs (list 3 1 2)) (set 'tail (cdr xs))`)
			for n := range functions {
				if n%2 == 0 {
					fmt.Fprintf(&src, `
(set 'data%d (sorted-map "key" (list %d 2 1) "bytes" (to-bytes "abcdefghijklmnop")))`, n, n)
				} else {
					fmt.Fprintf(&src, `
(set 'data%d (json:load-string "{\"key\":[%d,2,1],\"text\":\"abcdefghijklmnop\"}" :exact-integers true))`, n, n)
				}
				fmt.Fprintf(&src, `
(defun fn%d (x) (+ x %d (length (get data%d "key"))))`, n, n, n)
			}
			program := src.String()
			parsed, err := lisp.ReadProgram(parser.NewReader(), "template.lisp", strings.NewReader(program))
			if err != nil {
				b.Fatalf("parse benchmark fixture: %v", err)
			}
			loadCached := func(b *testing.B) *lisp.LEnv {
				b.Helper()
				env := newTemplateTestEnv(b)
				if got := env.LoadProgram(parsed); got.Type == lisp.LError {
					b.Fatalf("load cached program: %v", got)
				}
				return env
			}
			env := loadTemplateFixture(b, program)
			tmpl := snapshotFixture(b, env)
			want := lisp.Int(2)
			assertTemplateValue(b, loadTemplateFixture(b, program), `(stable-sort < xs) (first tail)`, want)
			assertTemplateValue(b, loadCached(b), `(stable-sort < xs) (first tail)`, want)
			assertTemplateValue(b, forkTemplateFixture(b, tmpl), `(stable-sort < xs) (first tail)`, want)
			if functions != 0 {
				call := fmt.Sprintf("(fn%d 10)", functions-1)
				want := lisp.Int(10 + functions - 1 + 3)
				assertTemplateValue(b, loadCached(b), call, want)
				assertTemplateValue(b, forkTemplateFixture(b, tmpl), call, want)
			}
			b.Run("NewTemplate", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					snapshotFixture(b, env)
				}
			})
			b.Run("TemplateFork", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					forkTemplateFixture(b, tmpl)
				}
			})
			b.Run("ColdLoad", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					loadTemplateFixture(b, program)
				}
			})
			b.Run("ColdLoadCachedProgram", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					loadCached(b)
				}
			})
		})
	}
}
