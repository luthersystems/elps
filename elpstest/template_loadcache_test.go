// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"fmt"
	"strconv"
	"sync"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Issue #625 preserves the behavioral load-cache topology from PR #603:
// one published template, late cache entries shared by its VMs, and independent
// cold references. The cache owns only opaque handles; the real loader parses,
// admits and serves every entry. Counters distinguish actual hits from a cache
// that is installed but unused, and pointer checks distinguish sharing from
// harmless (but expensive) copying.
type templateTopologyCache struct {
	mu      sync.Mutex
	entries map[string]*lisp.CachedSource
	stores  map[string]int
	hits    map[string]int
}

func newTemplateTopologyCache() *templateTopologyCache {
	return &templateTopologyCache{
		entries: make(map[string]*lisp.CachedSource),
		stores:  make(map[string]int),
		hits:    make(map[string]int),
	}
}

func (c *templateTopologyCache) Load(key string) (*lisp.CachedSource, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	source, ok := c.entries[key]
	if ok {
		c.hits[source.Name()]++
	}
	return source, ok
}

func (c *templateTopologyCache) Store(key string, source *lisp.CachedSource) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.entries[key] = source
	c.stores[source.Name()]++
}

func (c *templateTopologyCache) counts(name string) (stores, hits int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.stores[name], c.hits[name]
}

const templateTopologyProgram = `
(set 'counter 0)
(set 'log (vector))
(defun bump () (set 'counter (+ counter 1)))
(load-string "(set 'tmpl-lit '(5 4 6))" :name "tmpl.lisp")
`

func templateTopologyEnv(t *testing.T, cache *templateTopologyCache) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.LoadCache = cache
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.LoadString("program.lisp", templateTopologyProgram); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	return env
}

func templateTopologyPublish(t *testing.T, env *lisp.LEnv) *lisp.Template {
	t.Helper()
	// This factory installs only the audited core builtins. No opaque host
	// closures or native payloads are introduced by the fixture.
	plan, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	if err != nil {
		t.Fatal(err)
	}
	return plan
}

func templateTopologySteps(i int) []string {
	load := `(load-string "(set 'shared-lit '(3 1 2))" :name "shared.lisp")`
	if i%2 == 1 {
		load += ` (load-string "(set 'late-lit '(9 7 8))" :name "late.lisp")`
	}
	return []string{
		load + ` (first shared-lit)`,
		fmt.Sprintf(`(dotimes (n %d) (bump)) (append! log %d) (list counter (nth log 0))`, i+1, i),
		`(set 'sort-result
  (handler-bind ((modify-literal-error (lambda (c &rest _) (list 'refused c))))
    (stable-sort < shared-lit)))
(set 'tmpl-sort-result
  (handler-bind ((modify-literal-error (lambda (c &rest _) (list 'refused c))))
    (stable-sort < tmpl-lit)))
(set 'owned (copy shared-lit))
(stable-sort < owned)`,
	}
}

// Every result is pinned independently, not merely fork == cold: two broken
// arms cannot agree on a skipped mutation, a missing cache load, or a swallowed
// modify-literal-error. Each VM's final state is rechecked after every other VM
// has run, so a concurrent-only or later-transaction leak is observable.
func TestTemplateLoadCacheTopology(t *testing.T) {
	for _, warmth := range []string{"shared", "private", "mixed"} {
		for _, schedule := range []string{"lazy", "round-robin", "concurrent"} {
			for _, hops := range []int{1, 2} {
				t.Run(fmt.Sprintf("%s/%s/hops%d", warmth, schedule, hops), func(t *testing.T) {
					runTemplateLoadCacheTopology(t, warmth, schedule, hops)
				})
			}
		}
	}
}

func runTemplateLoadCacheTopology(t *testing.T, warmth, schedule string, hops int) {
	t.Helper()
	const n = 6
	shared := newTemplateTopologyCache()
	source := templateTopologyEnv(t, shared)
	plan := templateTopologyPublish(t, source)
	if hops == 2 {
		parent, err := plan.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		plan = templateTopologyPublish(t, parent)
	}
	forks, colds := make([]*lisp.LEnv, n), make([]*lisp.LEnv, n)
	caches := make([]*templateTopologyCache, n)
	build := func(i int) {
		var err error
		forks[i], err = plan.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		if forks[i].Runtime.LoadCache != shared {
			t.Fatal("Template did not propagate the shared LoadCache")
		}
		caches[i] = shared
		if warmth == "private" || (warmth == "mixed" && i%2 == 1) {
			caches[i] = newTemplateTopologyCache()
			forks[i].Runtime.LoadCache = caches[i]
		}
		colds[i] = templateTopologyEnv(t, newTemplateTopologyCache())
	}
	step := func(i, j int) (*lisp.LVal, *lisp.LVal) {
		name := fmt.Sprintf("vm%d-step%d.lisp", i, j)
		text := templateTopologySteps(i)[j]
		return forks[i].LoadString(name, text), colds[i].LoadString(name, text)
	}
	checkResult := func(i, j int, got, cold *lisp.LVal) {
		want := []string{"3", fmt.Sprintf("'(%d %d)", i+1, i), "'(1 2 3)"}[j]
		for _, value := range []*lisp.LVal{got, cold} {
			if value.Type == lisp.LError || value.String() != want {
				t.Fatalf("VM %d step %d: got %s, want %s", i, j, value, want)
			}
		}
	}
	if schedule == "lazy" {
		for i := range n {
			build(i)
			for j := range 3 {
				got, cold := step(i, j)
				checkResult(i, j, got, cold)
			}
		}
	} else {
		for i := range n {
			build(i)
		}
		for j := range 3 {
			// Prime both late entries through VMs, never the source, before
			// concurrent hits. LoadCache need not coalesce simultaneous misses.
			first := 0
			if schedule == "concurrent" && j == 0 {
				for i := range 2 {
					got, cold := step(i, j)
					checkResult(i, j, got, cold)
				}
				first = 2
			}
			got, cold := make([]*lisp.LVal, n), make([]*lisp.LVal, n)
			var wg sync.WaitGroup
			for i := first; i < n; i++ {
				if schedule == "concurrent" {
					wg.Go(func() { got[i], cold[i] = step(i, j) })
				} else {
					got[i], cold[i] = step(i, j)
				}
			}
			wg.Wait()
			for i := first; i < n; i++ {
				checkResult(i, j, got[i], cold[i])
			}
		}
	}

	for i := range n {
		assertTemplateTopologyState(t, forks[i], i)
		assertTemplateTopologyState(t, colds[i], i)
		lit := forks[i].GetGlobal(lisp.Symbol("shared-lit"))
		if lit == colds[i].GetGlobal(lisp.Symbol("shared-lit")) {
			t.Fatal("independent cold cache shared a late-loaded literal with a fork")
		}
		if forks[i].GetGlobal(lisp.Symbol("tmpl-lit")) != source.GetGlobal(lisp.Symbol("tmpl-lit")) {
			t.Fatal("preheated immutable literal was copied instead of shared")
		}
		for k := range i {
			same := lit == forks[k].GetGlobal(lisp.Symbol("shared-lit"))
			if same != (caches[i] == caches[k]) {
				t.Fatalf("VM %d and %d late literal identity: shared=%t, same cache=%t", i, k, same, caches[i] == caches[k])
			}
			if i%2 == 1 && k%2 == 1 {
				sameLate := forks[i].GetGlobal(lisp.Symbol("late-lit")) == forks[k].GetGlobal(lisp.Symbol("late-lit"))
				if sameLate != (caches[i] == caches[k]) {
					t.Fatalf("VM %d and %d sparse late-load identity: shared=%t, same cache=%t", i, k, sameLate, caches[i] == caches[k])
				}
			}
		}
	}
	for _, name := range []string{"shared.lisp", "late.lisp"} {
		loads := make(map[*templateTopologyCache]int)
		for i, cache := range caches {
			if name == "shared.lisp" || i%2 == 1 {
				loads[cache]++
			}
		}
		for cache, count := range loads {
			stores, hits := cache.counts(name)
			if stores != 1 || hits != count-1 {
				t.Fatalf("%s: stores=%d hits=%d, want 1 and %d", name, stores, hits, count-1)
			}
		}
	}
	if stores, _ := shared.counts("tmpl.lisp"); stores != 1 {
		t.Fatalf("preheated literal stored %d times, want once", stores)
	}
	// Published source and a successor must remain pristine after all writes.
	pristine, err := plan.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	for _, env := range []*lisp.LEnv{source, pristine} {
		if got := env.LoadString("pristine.lisp", `(list counter (length log) tmpl-lit)`); got.String() != "'(0 0 '(5 4 6))" {
			t.Fatalf("source/successor changed: %s", got)
		}
		if got := env.GetGlobal(lisp.Symbol("shared-lit")); got.Type != lisp.LError {
			t.Fatalf("transaction binding leaked into source/successor: %s", got)
		}
	}
}

func assertTemplateTopologyState(t *testing.T, env *lisp.LEnv, i int) {
	t.Helper()
	want := map[string]string{
		"counter":          strconv.Itoa(i + 1),
		"shared-lit":       "'(3 1 2)",
		"tmpl-lit":         "'(5 4 6)",
		"owned":            "'(1 2 3)",
		"sort-result":      "'('refused 'modify-literal-error)",
		"tmpl-sort-result": "'('refused 'modify-literal-error)",
	}
	if i%2 == 1 {
		want["late-lit"] = "'(9 7 8)"
	} else if got := env.GetGlobal(lisp.Symbol("late-lit")); got.Type != lisp.LError {
		t.Fatalf("VM %d inherited another VM's late load: %s", i, got)
	}
	for name, value := range want {
		if got := env.GetGlobal(lisp.Symbol(name)); got.String() != value {
			t.Fatalf("VM %d %s: got %s, want %s", i, name, got, value)
		}
	}
	if got := env.LoadString("observe.lisp", `(list (length log) (nth log 0))`); got.String() != fmt.Sprintf("'(1 %d)", i) {
		t.Fatalf("VM %d log: %s", i, got)
	}
	if !env.GetGlobal(lisp.Symbol("shared-lit")).IsSealed() || !env.GetGlobal(lisp.Symbol("tmpl-lit")).IsSealed() {
		t.Fatal("a shared cached literal is not sealed")
	}
}

// Run the structural/parity oracle as well as the literal checks above. The
// source's cache is warm on forks but each cold factory starts with a new cache.
func TestTemplateLoadCacheForkCheck(t *testing.T) {
	txs := make([]string, 6)
	for i := range txs {
		steps := templateTopologySteps(i)
		txs[i] = steps[0] + "\n" + steps[1] + "\n" + steps[2]
	}
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		NewEnv: func() (*lisp.LEnv, error) {
			env, err := elpstest.NewForkCheckEnv()
			if err == nil {
				env.Runtime.LoadCache = newTemplateTopologyCache()
			}
			return env, err
		},
		TemplateOptions: []lisp.TemplateOption{lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })},
		Program:         templateTopologyProgram,
		Tx:              txs,
	})
}
