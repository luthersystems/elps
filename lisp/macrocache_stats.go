// Copyright © 2026 The ELPS authors

package lisp

// Instrumentation for the macro-expansion cache POC (issue #381).
//
// Two layers, both zero-cost unless enabled:
//
//   - Aggregate counters (macroCacheStats): always maintained while a cache
//     mode is active; a handful of atomic adds on macro dispatch.
//   - Per-callsite counters: enabled via EnableMacroExpansionStats or the
//     ELPS_MACRO_STATS environment variable.  Records, per callsite node,
//     the macro name and how many times the callsite was macro-dispatched —
//     the repeat rate is exactly the fraction of expansion work a
//     per-callsite cache can eliminate.  When ELPS_MACRO_STATS names a file
//     the table is re-written there periodically (and can be flushed
//     explicitly with WriteMacroExpansionStats), so harnesses that cannot
//     call Go APIs (e.g. a phylum test runner) still produce a report.

import (
	"fmt"
	"io"
	"os"
	"sort"
	"sync"
	"sync/atomic"
	"unsafe"
)

// macroCacheStatsT aggregates cache effectiveness counters.
type macroCacheStatsT struct {
	dispatched     atomic.Int64 // macro calls entering the instrumented path
	hits           atomic.Int64
	stores         atomic.Int64 // misses that populated the cache
	bypassUnsealed atomic.Int64 // unsealed callsite or debugger attached
	bypassImpure   atomic.Int64 // macro not admitted by the whitelist/prover
	unsealable     atomic.Int64 // expansion contained unsealable nodes
	evictions      atomic.Int64 // LRU evictions (shared cache with cap)
}

//elpsvet:allow process-wide instrumentation counters; scalar atomics, no LVals
var macroCacheStats macroCacheStatsT

func (s *macroCacheStatsT) reset() {
	s.dispatched.Store(0)
	s.hits.Store(0)
	s.stores.Store(0)
	s.bypassUnsealed.Store(0)
	s.bypassImpure.Store(0)
	s.unsealable.Store(0)
	s.evictions.Store(0)
}

// MacroCacheStats is a snapshot of the cache's aggregate counters.
type MacroCacheStats struct {
	Dispatched     int64 // macro dispatches through the instrumented path
	Hits           int64 // cache hits (expansion reused)
	Stores         int64 // expansions computed and cached
	BypassUnsealed int64 // bypassed: unsealed callsite / debugger attached
	BypassImpure   int64 // bypassed: macro not admitted as pure
	Unsealable     int64 // expansions refused (unsealable content)
	Evictions      int64 // LRU evictions
	SharedEntries  int64 // entries currently in the process-shared cache
	SharedBytes    int64 // approximate retained bytes of shared entries
}

// SnapshotMacroCacheStats returns the current aggregate cache counters plus
// the shared cache's entry count and approximate retained size.
func SnapshotMacroCacheStats() MacroCacheStats {
	st := MacroCacheStats{
		Dispatched:     macroCacheStats.dispatched.Load(),
		Hits:           macroCacheStats.hits.Load(),
		Stores:         macroCacheStats.stores.Load(),
		BypassUnsealed: macroCacheStats.bypassUnsealed.Load(),
		BypassImpure:   macroCacheStats.bypassImpure.Load(),
		Unsealable:     macroCacheStats.unsealable.Load(),
		Evictions:      macroCacheStats.evictions.Load(),
	}
	st.SharedEntries, st.SharedBytes = sharedMacroCacheFootprint()
	return st
}

// sharedMacroCacheFootprint walks the shared cache and returns the entry
// count and an approximation of the bytes retained by the cached expansion
// trees.  Nodes shared with the underlying program parse (argument subtrees
// spliced into expansions, singletons) are excluded when they are reachable
// from the callsite key: the approximation counts each distinct node once
// and only nodes that are NOT reachable from the callsite's own tree, since
// those are retained by the program regardless of the cache.
func sharedMacroCacheFootprint() (entries, bytes int64) {
	programNodes := make(map[*LVal]bool)
	sharedMacroCache.m.Range(func(k, _ any) bool {
		markTree(k.(*LVal), programNodes)
		return true
	})
	counted := make(map[*LVal]bool)
	sharedMacroCache.m.Range(func(_, v any) bool {
		entries++
		bytes += treeBytes(v.(*macroCacheEntry).exp, programNodes, counted)
		return true
	})
	// Table overhead: one sync.Map entry + one macroCacheEntry per callsite.
	bytes += entries * int64(unsafe.Sizeof(macroCacheEntry{})+48)
	return entries, bytes
}

// RuntimeMacroCacheFootprint reports the entry count and approximate
// retained bytes of a Runtime's private cache (MacroCacheRuntime mode).
func RuntimeMacroCacheFootprint(rt *Runtime) (entries, bytes int64) {
	programNodes := make(map[*LVal]bool)
	for k := range rt.macroCache {
		markTree(k, programNodes)
	}
	counted := make(map[*LVal]bool)
	for _, e := range rt.macroCache {
		entries++
		bytes += treeBytes(e.exp, programNodes, counted)
	}
	bytes += entries * int64(unsafe.Sizeof(macroCacheEntry{})+48)
	return entries, bytes
}

func markTree(v *LVal, seen map[*LVal]bool) {
	if v == nil || seen[v] {
		return
	}
	seen[v] = true
	for _, c := range v.Cells {
		markTree(c, seen)
	}
}

// treeBytes approximates the retained size of the expansion tree rooted at
// v, skipping nodes already retained by the program (exclude) and nodes
// counted by a previous entry (counted).
func treeBytes(v *LVal, exclude, counted map[*LVal]bool) int64 {
	if v == nil || isSingleton(v) || exclude[v] || counted[v] {
		return 0
	}
	counted[v] = true
	n := int64(unsafe.Sizeof(LVal{}))
	n += int64(len(v.Str))
	n += int64(cap(v.Cells)) * int64(unsafe.Sizeof((*LVal)(nil)))
	for _, c := range v.Cells {
		n += treeBytes(c, exclude, counted)
	}
	return n
}

// Per-callsite instrumentation ---------------------------------------------

//elpsvet:allow instrumentation state; enabled flag + counters keyed by callsite pointer, never mutates the keys
var macroExpandStats struct {
	sites    sync.Map // *LVal (callsite) -> *macroSiteStat
	dumpPath string
	events   atomic.Int64
	dumpMu   sync.Mutex
	enabled  atomic.Bool
}

type macroSiteStat struct {
	name   string // qualified macro name at first sighting
	sealed bool
	count  atomic.Int64
}

// EnableMacroExpansionStats turns per-callsite expansion counting on or off.
func EnableMacroExpansionStats(on bool) {
	macroExpandStats.enabled.Store(on)
}

// ResetMacroExpansionStats drops all per-callsite counters.
func ResetMacroExpansionStats() {
	macroExpandStats.sites.Range(func(k, _ any) bool {
		macroExpandStats.sites.Delete(k)
		return true
	})
	macroExpandStats.events.Store(0)
}

func initMacroStats() {
	if p := os.Getenv("ELPS_MACRO_STATS"); p != "" {
		macroExpandStats.dumpPath = p
		macroExpandStats.enabled.Store(true)
	}
}

// statsDumpInterval controls the periodic rewrite of ELPS_MACRO_STATS: the
// table is flushed every statsDumpInterval dispatch events, so even a
// harness that never calls WriteMacroExpansionStats leaves a recent file.
const statsDumpInterval = 1 << 17

func recordMacroExpandEvent(callsite, fun *LVal) {
	macroCacheStats.dispatched.Add(1)
	if !macroExpandStats.enabled.Load() {
		return
	}
	v, ok := macroExpandStats.sites.Load(callsite)
	if !ok {
		name := "?"
		if fun.Type == LFun {
			if fd := fun.funData(); fd != nil {
				name = fd.Package + ":" + fd.FID
			}
		}
		v, _ = macroExpandStats.sites.LoadOrStore(callsite, &macroSiteStat{
			name:   name,
			sealed: callsite.sealed,
		})
	}
	v.(*macroSiteStat).count.Add(1)
	if macroExpandStats.dumpPath != "" {
		if n := macroExpandStats.events.Add(1); n%statsDumpInterval == 0 {
			dumpMacroStatsFile()
		}
	}
}

func dumpMacroStatsFile() {
	macroExpandStats.dumpMu.Lock()
	defer macroExpandStats.dumpMu.Unlock()
	f, err := os.Create(macroExpandStats.dumpPath)
	if err != nil {
		return
	}
	_ = WriteMacroExpansionStats(f)
	_ = f.Close()
}

// FlushMacroExpansionStats writes the per-callsite table to the
// ELPS_MACRO_STATS path immediately, if one is configured.
func FlushMacroExpansionStats() {
	if macroExpandStats.dumpPath != "" {
		dumpMacroStatsFile()
	}
}

// WriteMacroExpansionStats writes a TSV report of per-callsite macro
// dispatch counts: one row per callsite (macro name, sealed flag, count)
// followed by aggregate lines.  Sorted by count descending.
func WriteMacroExpansionStats(w io.Writer) error {
	type row struct {
		name   string
		sealed bool
		count  int64
	}
	var rows []row
	var total, repeats int64
	macroExpandStats.sites.Range(func(_, v any) bool {
		s := v.(*macroSiteStat)
		c := s.count.Load()
		rows = append(rows, row{s.name, s.sealed, c})
		total += c
		if c > 1 {
			repeats += c - 1
		}
		return true
	})
	sort.Slice(rows, func(i, j int) bool { return rows[i].count > rows[j].count })
	if _, err := fmt.Fprintf(w, "# macro-expansion callsite stats: sites=%d dispatches=%d repeat-dispatches=%d\n",
		len(rows), total, repeats); err != nil {
		return err
	}
	st := SnapshotMacroCacheStats()
	if _, err := fmt.Fprintf(w, "# cache: mode=%d hits=%d stores=%d bypass-unsealed=%d bypass-impure=%d unsealable=%d evictions=%d shared-entries=%d shared-bytes=%d\n",
		GetMacroCacheMode(), st.Hits, st.Stores, st.BypassUnsealed, st.BypassImpure, st.Unsealable, st.Evictions, st.SharedEntries, st.SharedBytes); err != nil {
		return err
	}
	for _, r := range rows {
		if _, err := fmt.Fprintf(w, "%s\t%t\t%d\n", r.name, r.sealed, r.count); err != nil {
			return err
		}
	}
	return nil
}
