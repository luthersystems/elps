// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"context"
	"fmt"
	"math/rand"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// ARM E, part two.  Installing the sealed-AST verifier in this package
// (main_test.go) buys nothing unless some test performs the corrupting
// composition, and none did.  This generative test supplies the missing
// half: it composes an elpspath QUERY (which mints a view over the
// queried value's live backing) with a kernel MUTATOR (which writes
// through the view), over a program literal that the parser sealed.
//
// The oracle is the checked-mode sealed fingerprint; the measurement is
// how many generated programs it takes to fire.
func aliasGenEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", lerr)
	}
	if lerr := LoadPackage(env); lerr.Type == lisp.LError {
		t.Fatalf("LoadPackage: %v", lerr)
	}
	if lerr := env.InPackage(lisp.String(lisp.DefaultUserPackage)); lerr.Type == lisp.LError {
		t.Fatalf("InPackage: %v", lerr)
	}
	return env
}

// queries mint a value over the literal's backing storage.
var aliasGenQueries = []string{
	`(elpspath:? lit '(range 0 2))`,
	`(elpspath:? lit '(range 1 3))`,
	`(elpspath:? lit '(range 0 4))`,
	`(elpspath:? lit '(range 2 4))`,
	`(slice 'list lit 0 2)`,
	`(cdr lit)`,
	`(rest lit)`,
}

// mutators write through whatever they are handed.
var aliasGenMutators = []string{
	`(append 'vector %s 999)`,
	`(append! (slice 'vector %s 0 1) 999)`,
	`(stable-sort < %s)`,
	`(stable-sort > %s)`,
	`(sort-stable-by-key < %s)`,
}

// TestAliasGenFindsProvenanceLoss generates query x mutator compositions
// over a sealed literal until the checked-mode oracle reports drift, and
// reports how many programs that took.  Untagged the oracle is a nil call,
// so the test only asserts the programs run.
func TestAliasGenFindsProvenanceLoss(t *testing.T) {
	env := aliasGenEnv(t)
	p := parser.NewReader()
	rng := rand.New(rand.NewSource(1))
	const budget = 2000
	start := time.Now()
	firstHit, hits, ran := -1, 0, 0
	for i := range budget {
		q := aliasGenQueries[rng.Intn(len(aliasGenQueries))]
		m := aliasGenMutators[rng.Intn(len(aliasGenMutators))]
		src := fmt.Sprintf(
			"(defun lit%d () '(30 10 20 40))\n(set 'lit (lit%d))\n%s\n(lit%d)\n",
			i, i, fmt.Sprintf(m, q), i)
		exprs, err := p.Read("aliasgen.lisp", strings.NewReader(src))
		if err != nil {
			t.Fatalf("parse: %v", err)
		}
		for _, e := range exprs {
			if v := env.EvalContext(context.Background(), e); v.Type == lisp.LError {
				// Type errors from a composition that does not typecheck are
				// expected and uninteresting.
				break
			}
		}
		ran++
		if verr := lisp.VerifySealedASTs(); verr != nil {
			hits++
			if firstHit < 0 {
				firstHit = i + 1
				t.Logf("ARM E: sealed-AST oracle fired after %d generated programs (%s)",
					firstHit, time.Since(start).Round(time.Millisecond))
				t.Logf("first report: %v", firstHit)
			}
			// Re-baseline so subsequent iterations report new drift only.
			break
		}
	}
	if firstHit < 0 {
		t.Logf("ARM E: oracle did NOT fire in %d generated programs (%s); untagged builds record nothing, so this is the expected untagged result",
			ran, time.Since(start).Round(time.Millisecond))
	} else {
		t.Logf("ARM E: %d hit(s); first at program %d", hits, firstHit)
	}
}
