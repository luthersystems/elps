// Copyright © 2026 The ELPS authors

package synthphylum

import (
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

func newEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Stderr = io.Discard
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		t.Fatalf("LoadLibrary: %v", rc)
	}
	return env
}

func loadCorpus(t testing.TB, env *lisp.LEnv, files []File) {
	t.Helper()
	for _, f := range files {
		if v := env.LoadString(f.Name, f.Content); v.Type == lisp.LError {
			t.Fatalf("load %s: %v", f.Name, v)
		}
	}
}

func withMode(t testing.TB, mode lisp.MacroCacheMode) {
	t.Helper()
	lisp.ResetMacroCache()
	lisp.SetMacroCacheMode(mode)
	t.Cleanup(func() {
		lisp.SetMacroCacheMode(lisp.MacroCacheOff)
		lisp.ResetMacroCache()
	})
}

// requestMix returns a deterministic set of dispatch expressions as source.
func requestMix(routes int) []string {
	reqs := []string{
		`(sorted-map "f0" 1 "f2" 3)`,
		`(sorted-map "f1" 2 "f3" 4 "f5" 6)`,
		`(sorted-map)`,
	}
	var mix []string
	for i := 0; i < 12; i++ {
		route := (i * 37) % routes
		mix = append(mix, fmt.Sprintf(`(dispatch "route-%d" %s)`, route, reqs[i%len(reqs)]))
	}
	return mix
}

// TestCorpusShape pins the default corpus to the production-scale
// reference statistics (file count, line volume, form densities).
func TestCorpusShape(t *testing.T) {
	files := Generate(DefaultParams())
	if len(files) != 50 {
		t.Errorf("files: got %d want 50", len(files))
	}
	var lines, defun, defmacro, whenSites, defaultSites, getdefSites, wrapperSites int
	for _, f := range files {
		lines += strings.Count(f.Content, "\n")
		defun += strings.Count(f.Content, "(defun ")
		defmacro += strings.Count(f.Content, "(defmacro ")
		whenSites += strings.Count(f.Content, "(s-when ")
		defaultSites += strings.Count(f.Content, "(s-default ")
		getdefSites += strings.Count(f.Content, "(get-default ")
		for w := 0; w < 7; w++ {
			wrapperSites += strings.Count(f.Content, fmt.Sprintf("(def-w%d route-", w))
		}
	}
	t.Logf("shape: lines=%d defun=%d defmacro=%d wrapper-sites=%d s-when=%d s-default=%d get-default=%d",
		lines, defun, defmacro, wrapperSites, whenSites, defaultSites, getdefSites)
	if lines < 55000 || lines > 90000 {
		t.Errorf("line volume out of range: %d", lines)
	}
	if defmacro != 11 { // 4 utilities + 7 wrappers
		t.Errorf("defmacro count: got %d want 11", defmacro)
	}
	if wrapperSites != 606 {
		t.Errorf("wrapper callsites: got %d want 606", wrapperSites)
	}
	if got, want := whenSites, 1556; got != want {
		t.Errorf("s-when sites: got %d want %d", got, want)
	}
	if got, want := getdefSites, 62; got != want {
		t.Errorf("get-default sites: got %d want %d", got, want)
	}
	// Determinism: regeneration is byte-identical.
	again := Generate(DefaultParams())
	for i := range files {
		if files[i].Content != again[i].Content {
			t.Fatalf("generation not deterministic: %s", files[i].Name)
		}
	}
}

// TestCorpusModeEquivalence loads the small corpus under every cache mode
// and requires identical dispatch results.
func TestCorpusModeEquivalence(t *testing.T) {
	files := Generate(SmallParams())
	routes := 0
	for _, n := range SmallParams().WrapperUses {
		routes += n
	}
	mix := requestMix(routes)
	results := make(map[string][]string)
	for _, m := range []struct {
		name string
		mode lisp.MacroCacheMode
	}{
		{"off", lisp.MacroCacheOff},
		{"runtime", lisp.MacroCacheRuntime},
		{"shared", lisp.MacroCacheShared},
	} {
		withMode(t, m.mode)
		env := newEnv(t)
		loadCorpus(t, env, files)
		var rendered []string
		// Two passes so cached expansions are reused at least once.
		for pass := 0; pass < 2; pass++ {
			for _, call := range mix {
				v := env.LoadString("dispatch.lisp", call)
				if v.Type == lisp.LError {
					t.Fatalf("mode %s: %s: %v", m.name, call, v)
				}
				rendered = append(rendered, v.String())
			}
		}
		results[m.name] = rendered
	}
	for name, got := range results {
		for i := range got {
			if got[i] != results["off"][i] {
				t.Errorf("mode %s call %d diverged: %s vs %s", name, i, got[i], results["off"][i])
			}
		}
	}
	for _, r := range results["off"] {
		if strings.Contains(r, `"error"`) {
			t.Fatalf("dispatch errored: %s", r)
		}
	}
}

// TestCorpusHitRate loads the full production-scale corpus with the shared
// cache and reports the hit-rate distribution over a warm request mix.
func TestCorpusHitRate(t *testing.T) {
	if testing.Short() {
		t.Skip("full corpus load in -short mode")
	}
	files := Generate(DefaultParams())
	routes := 0
	for _, n := range DefaultParams().WrapperUses {
		routes += n
	}
	withMode(t, lisp.MacroCacheShared)
	lisp.EnableMacroExpansionStats(true)
	defer func() {
		lisp.EnableMacroExpansionStats(false)
		lisp.ResetMacroExpansionStats()
	}()
	env := newEnv(t)
	loadCorpus(t, env, files)
	loadStats := lisp.SnapshotMacroCacheStats()
	t.Logf("after load: %+v", loadStats)
	mix := requestMix(routes)
	const rounds = 50
	for i := 0; i < rounds; i++ {
		for _, call := range mix {
			v := env.LoadString("dispatch.lisp", call)
			if v.Type == lisp.LError {
				t.Fatalf("%s: %v", call, v)
			}
			if strings.Contains(v.String(), `"error"`) {
				t.Fatalf("dispatch errored: %s -> %s", call, v)
			}
		}
	}
	st := lisp.SnapshotMacroCacheStats()
	warmDispatched := st.Dispatched - loadStats.Dispatched
	warmHits := st.Hits - loadStats.Hits
	t.Logf("warm phase: dispatched=%d hits=%d stores=%d bypass-impure=%d bypass-unsealed=%d hit-rate=%.1f%%",
		warmDispatched, warmHits, st.Stores-loadStats.Stores,
		st.BypassImpure-loadStats.BypassImpure, st.BypassUnsealed-loadStats.BypassUnsealed,
		100*float64(warmHits)/float64(warmDispatched))
	t.Logf("cache footprint: entries=%d bytes=%d (%.0f B/entry)",
		st.SharedEntries, st.SharedBytes, float64(st.SharedBytes)/float64(st.SharedEntries))
	if warmHits == 0 {
		t.Fatal("expected cache hits on the warm request path")
	}
	var report strings.Builder
	if err := lisp.WriteMacroExpansionStats(&report); err != nil {
		t.Fatal(err)
	}
	// Log only the header lines; the full table is large.
	for _, line := range strings.SplitN(report.String(), "\n", 3)[:2] {
		t.Log(line)
	}
}

// BenchmarkDispatch measures warm dispatch cost over the full corpus for
// each cache mode.  The corpus is loaded once per mode (untimed).
func BenchmarkDispatch(b *testing.B) {
	files := Generate(DefaultParams())
	routes := 0
	for _, n := range DefaultParams().WrapperUses {
		routes += n
	}
	mix := requestMix(routes)
	for _, m := range []struct {
		name string
		mode lisp.MacroCacheMode
	}{
		{"off", lisp.MacroCacheOff},
		{"runtime", lisp.MacroCacheRuntime},
		{"shared", lisp.MacroCacheShared},
	} {
		b.Run(m.name, func(b *testing.B) {
			withMode(b, m.mode)
			env := newEnv(b)
			loadCorpus(b, env, files)
			// Pre-parse the dispatch expressions once (the callsites inside
			// route/helper bodies are what the cache targets).
			p := parser.NewReader()
			var calls []*lisp.LVal
			for _, src := range mix {
				exprs, err := p.Read("dispatch.lisp", strings.NewReader(src))
				if err != nil {
					b.Fatal(err)
				}
				calls = append(calls, exprs...)
			}
			// One warm pass, untimed.
			for _, c := range calls {
				if v := env.Eval(c); v.Type == lisp.LError {
					b.Fatalf("warmup: %v", v)
				}
			}
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				c := calls[i%len(calls)]
				if v := env.Eval(c); v.Type == lisp.LError {
					b.Fatalf("dispatch: %v", v)
				}
			}
		})
	}
}
