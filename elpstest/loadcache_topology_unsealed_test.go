// Copyright © 2026 The ELPS authors

//go:build !elpscheck

// Control (a) for the load-cache topology (loadcache_topology_test.go): a
// parse cache whose entries are NOT sealed, shared by every fork.
//
// The real hook cannot be made to produce this shape.  CachedSource is
// opaque and only lisp.newCachedSource mints one, through an admission that
// seals (or copies and seals) every entry, so the double sits one layer
// out, where an embedder's cache used to sit before Runtime.LoadCache
// existed (lisp/loadcache.go, "The seam this closes"): a Runtime.Reader
// that parses a source once and hands the SAME unsealed []*LVal to every
// load, with no LoadCache installed.  That is substrate's pre-#368 parse
// cache, and it is exactly the value-sharing the seal was introduced to
// forbid.
//
// It is excluded under `-tags elpscheck` for the reason
// aliasguard_templatefork_test.go gives: an unsealed *LVal reaching two
// Runtimes is an ownership violation, and the checker panics on it before
// any isolation property can report — elps already refuses this class in a
// checked build.  The control still earns its place in an ordinary build,
// which is what an embedder ships.
package elpstest_test

import (
	"io"
	"strings"
	"sync"
	"sync/atomic"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// unsealedAliasingReader parses each distinct source ONCE with the standard
// parser, deep-copies the parse so that the copy is unsealed (lisp/copy.go:
// a copy clears the seal by contract), and returns that same unsealed
// slice to every later load of the same source.  Copy has to run once, at
// first sight: copying per load would hand every environment private
// nodes and reproduce control (b), not this control.
//
// ONE READER PER TEMPLATE, not one per process.  The Reader travels across
// Fork (lisp/fork.go), so a reader built in NewEnv is shared by that
// template and every fork of it — the shape the control needs — and by
// nothing else.  A process-wide one was the first revision, and on this
// base it produced NO witness at all: CheckTransactions now runs its
// parity channel first (aliasguard_parity.go, commit f3538f1), whose COLD
// environments are built by the same NewEnv, so cold environment 1 sorted
// the one shared parse in place before the sweep's fork 0 ever loaded it,
// and the sweep found nothing left to move.  The counter is shared across
// readers so the "served at all" check still covers the whole run.
type unsealedAliasingReader struct {
	inner   lisp.Reader
	mu      sync.Mutex
	parses  map[string][]*lisp.LVal
	serves  *int32 // shared by every reader one test builds; see above
	sealing bool   // when set, the shared parse is re-sealed: the other arm
}

func (r *unsealedAliasingReader) Read(name string, in io.Reader) ([]*lisp.LVal, error) {
	src, err := io.ReadAll(in)
	if err != nil {
		return nil, err
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	if exprs, ok := r.parses[string(src)]; ok {
		atomic.AddInt32(r.serves, 1)
		return exprs, nil
	}
	exprs, err := r.inner.Read(name, strings.NewReader(string(src)))
	if err != nil {
		return nil, err
	}
	shared := make([]*lisp.LVal, len(exprs))
	for i, e := range exprs {
		shared[i] = e.Copy()
		if r.sealing {
			shared[i].SealAST()
		}
	}
	if r.parses == nil {
		r.parses = make(map[string][]*lisp.LVal)
	}
	r.parses[string(src)] = shared
	return shared, nil
}

// unsealedControlTx: fork 0 loads the shared source and reads it; fork 1
// loads it and sorts the literal IN PLACE.  With unsealed shared nodes the
// sort succeeds and rewrites the nodes fork 0 holds, so fork 0 moves under
// transaction 1 — the "a transaction on one fork is invisible to every
// other fork" witness.  The order matters: the observer must load first.
var unsealedControlTx = []string{
	`(load-string "(set 'shared-lit '(3 1 2))" :name "shared.lisp")` + "\n" + `(set 'seen (first shared-lit))`,
	`(load-string "(set 'shared-lit '(3 1 2))" :name "shared.lisp")` + "\n" +
		`(set 'sort-result (handler-bind ((modify-literal-error (lambda (c &rest _) (list 'refused c)))) (stable-sort < shared-lit)))`,
}

// TestLoadCacheTopology_UnsealedSharedEntryIsAWitness is control (a): one
// fork writes into the unsealed shared parse and the oracle reports it.
// Two arms over the same Reader double:
//
//   - unsealed (the control): the write lands and fork 0 moves.  BOTH of
//     CheckTransactions' channels must say so: the parity channel, where
//     fork 0's post-run state diverges from its cold load's at
//     user:shared-lit (ParityPropertyState), and the isolation sweep,
//     which reports the fork -> fork direction with "transaction 1 moved
//     fork 0" at user:shared-lit.  The sweep witness was measured on
//     commit 74e4ac8; the parity one exists since the channel does.
//   - re-sealed (the comparison): the same Reader, the same shared nodes,
//     sealed — the shape the standard parser and a legally-minted cache
//     entry have.  The sort is refused and the oracle reports nothing.
//     The only difference between the arms is the seal bit, so the seal is
//     what stands between the two outcomes.
//
// SkipConcurrentArm is set on the unsealed arm for the reason
// TransactionCheck documents: two forks writing one shared node in
// parallel is a data race by construction, and -race would report it
// against this control rather than against elps.
func TestLoadCacheTopology_UnsealedSharedEntryIsAWitness(t *testing.T) {
	t.Parallel()
	for _, sealing := range []bool{false, true} {
		name := "unsealed"
		if sealing {
			name = "resealed"
		}
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			var serves int32
			newEnv := func() (*lisp.LEnv, error) {
				return newTopologyEnv(&unsealedAliasingReader{inner: parser.NewReader(), sealing: sealing, serves: &serves}, nil)
			}
			got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
				NewEnv:            newEnv,
				Program:           `(set 'counter 0)`,
				Tx:                unsealedControlTx,
				SkipConcurrentArm: !sealing,
				Repro:             "elpstest/loadcache_topology_unsealed_test.go: one unsealed parse served to every fork",
			})
			if err != nil {
				t.Fatalf("harness error: %v", err)
			}
			if atomic.LoadInt32(&serves) == 0 {
				t.Fatalf("no Reader ever served its shared parse; the control did not share anything")
			}
			if sealing {
				for _, w := range got {
					t.Errorf("re-sealed shared parse: unexpected witness\n%s", w)
				}
				return
			}
			sweep, parity := false, false
			for _, w := range got {
				t.Logf("%s", w)
				switch {
				case w.Property == "a transaction on one fork is invisible to every other fork" &&
					strings.Contains(w.Detail, "transaction 1 moved fork 0") &&
					strings.Contains(w.Leak, "shared-lit"):
					sweep = true
				case w.Property == elpstest.ParityPropertyState &&
					strings.Contains(w.Detail, "environment 0") &&
					strings.Contains(w.Leak, "shared-lit"):
					parity = true
				}
			}
			if !sweep || !parity {
				t.Fatalf("an unsealed parse shared by every fork was written by fork 1: sweep reported fork 0 moving at shared-lit: %t; parity channel reported environment 0's state diverging at shared-lit: %t (%d witnesses)", sweep, parity, len(got))
			}
		})
	}
}
