// Copyright © 2024 The ELPS authors

package perf

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// This file gates isCallable, one of the hand-maintained mirrors of
// lisp.DefaultSpecialOps() inventoried in internal/formsync.  A special
// operator that reaches isCallable unlisted is reported as an ordinary
// function call, which inflates the caller's LocalCost (and, inside a loop,
// inflates it by LoopMultiplier per level) and adds a bogus edge to the call
// graph.
//
// WHAT AN AUDIT OF THIS MIRROR GETS WRONG, AND WHY THE EXEMPTIONS EXIST.
// Reading isCallable against the op table shows lambda, quote and quasiquote
// missing, and the obvious conclusion -- that the analyzer records a call edge
// for (lambda ...) -- is false.  scanExpr resolves all three in its own switch
// and RETURNS, several statements before isCallable is consulted, so their
// isCallable value is unreachable.  Measured on
// (defun f (xs) (map 'list (lambda (x) (+ x 1)) xs)): score 20 as the code
// stands, and 60 with those switch cases deleted -- 20 for a spurious lambda
// edge, 20 more for a spurious edge to the formal x, 20 for the real +.
//
// So the three are not drift to be tidied away by adding them to isCallable.
// Each switch case does strictly MORE than suppress a call edge, and none of
// the extra work is expressible in isCallable, which only chooses whether to
// emit an edge and cannot stop the walk or redirect it:
//
//	quote, quasiquote  stop the walk descending, so quoted data is never
//	                   costed as code
//	lambda             skip the formals and scan the BODY at the caller's
//	                   current loop depth, so an inline callback is costed as
//	                   the caller's own work
//
// Adding them to isCallable would state the same fact in a second place while
// leaving the switch load-bearing, and would invite a later reader to delete
// the switch case that is actually doing the work.  They stay out, and the
// exemptions below carry the reason.
//
// THE EXEMPTIONS ARE CHECKED, NOT PROMISED.  formsync's lesson is that
// asserting only an absence turns the stated reason into a promise nothing
// verifies: naming the rule that handles a form instead, and never checking
// it, left its gates green while the named rules had been deleted.  Every
// exemption here therefore carries a probe, and the probe pins the stronger
// behaviour the exemption claims -- deleting the switch case fails this test
// rather than silently widening the analyzer.
//
// A NEW special operator is not exempt.  Adding one without touching
// isCallable fails TestIsCallableCoversTheOpTable until someone either lists
// it or records here why it does not belong.

// callableExemption records why a special operator is absent from
// isCallable's non-callable list, together with a probe that demonstrates the
// rule handling it instead is still doing its job.
type callableExemption struct {
	// reason is why the operator does not belong in isCallable.
	reason string

	// probe is a single body form, spliced into a defun and scanned.  The
	// operator itself must never appear as a callee of that scan.
	probe string

	// absentCallees are callee names the probe must NOT produce.  They pin
	// the part of the handling rule that isCallable could not replicate.
	absentCallees []string

	// presentCallees are callee names the probe MUST produce, so a rule that
	// suppresses too much fails here instead of quietly under-reporting.
	presentCallees []string
}

// isCallableExempt lists special operators deliberately absent from
// isCallable because scanExpr resolves them earlier.  See the file comment
// for why adding them to isCallable would be the wrong fix.
var isCallableExempt = map[string]callableExemption{
	"quote": {
		reason: `scanExpr's "quote", "quasiquote" case returns first, and also stops the ` +
			`walk descending, so quoted data is never costed as code`,
		probe:         `(quote (quoted-marker 1 2))`,
		absentCallees: []string{"quoted-marker"},
	},
	"quasiquote": {
		reason: `scanExpr's "quote", "quasiquote" case returns first, and also stops the ` +
			`walk descending, so quasiquoted template data is never costed as code`,
		probe:         `(quasiquote (quoted-marker (unquote n)))`,
		absentCallees: []string{"quoted-marker", "unquote"},
	},
	"lambda": {
		reason: `scanExpr's "lambda" case returns first, and also skips the formals and ` +
			`scans the body at the caller's current loop depth, so an inline callback ` +
			`is costed as the caller's own work`,
		probe:          `(lambda (formal-marker) (body-marker formal-marker))`,
		absentCallees:  []string{"formal-marker"},
		presentCallees: []string{"body-marker"},
	},
}

// isCallableMacroExempt is the same idea for lisp.DefaultMacros().  It is
// empty: no default macro is a call edge, and there is no known reason for
// one not to be listed in isCallable.  It exists so that a future exception
// is recorded with a reason instead of being made by deleting an assertion.
var isCallableMacroExempt = map[string]callableExemption{}

// probeCallees scans a defun whose body is the given form and returns the set
// of callee names the scan recorded.
func probeCallees(t *testing.T, body string) map[string]bool {
	t.Helper()
	src := "(defun probe-subject (n) " + body + ")"
	exprs := parseSource(t, src)
	summaries := ScanFile(exprs, "drift_probe.lisp", DefaultConfig())
	require.Len(t, summaries, 1,
		"probe %q did not scan to exactly one function summary", src)
	out := map[string]bool{}
	for _, edge := range summaries[0].Calls {
		out[edge.Callee] = true
	}
	return out
}

// requireProbeHarnessWorks guards against the failure mode where every
// per-operator assertion passes because the probe records nothing at all.
func requireProbeHarnessWorks(t *testing.T) {
	t.Helper()
	require.True(t, probeCallees(t, "(ordinary-marker 1)")["ordinary-marker"],
		"the probe harness recorded no call edge for an ordinary call, so the"+
			" per-operator assertions would pass while measuring nothing")
}

// assertFormsAreNotCallEdges pins isCallable against a table of builtin
// definitions.  kind names the table for failure messages.
func assertFormsAreNotCallEdges(t *testing.T, kind string, defs []lisp.LBuiltinDef, exempt map[string]callableExemption) {
	t.Helper()
	require.NotEmpty(t, defs, "the %s table returned nothing", kind)

	seen := make(map[string]bool, len(defs))
	for _, def := range defs {
		name := def.Name()
		seen[name] = true

		ex, isExempt := exempt[name]
		if !isExempt {
			require.False(t, isCallable(name),
				"%s %q is missing from isCallable (analysis/perf/local.go), so the"+
					" perf analyzer costs it as an ordinary function call and adds a"+
					" bogus edge to the call graph; add it there, or add it to the"+
					" exemption map with a reason and a probe", kind, name)
			continue
		}

		require.NotEmpty(t, ex.reason, "exemption for %s %q must carry a reason", kind, name)
		require.NotEmpty(t, ex.probe, "exemption for %s %q must carry a probe", kind, name)

		// A live exemption means isCallable really does not list it.  Once it
		// is listed the exemption is stale and must go, or the map starts
		// describing a state of the code that no longer exists.
		require.True(t, isCallable(name),
			"%s %q is listed in the exemption map (%s) but isCallable now reports it"+
				" as non-callable too; drop the exemption", kind, name, ex.reason)

		callees := probeCallees(t, ex.probe)
		require.False(t, callees[name],
			"%s %q is exempt because %s, but probe %q recorded a call edge to it"+
				" anyway -- the rule the exemption relies on is gone",
			kind, name, ex.reason, ex.probe)
		for _, callee := range ex.absentCallees {
			require.False(t, callees[callee],
				"%s %q is exempt because %s, but probe %q recorded a call edge to %q,"+
					" which that rule is supposed to prevent",
				kind, name, ex.reason, ex.probe, callee)
		}
		for _, callee := range ex.presentCallees {
			require.True(t, callees[callee],
				"%s %q is exempt because %s, but probe %q did NOT record the call edge"+
					" to %q that rule is supposed to keep -- it now suppresses too much",
				kind, name, ex.reason, ex.probe, callee)
		}
	}

	for name, ex := range exempt {
		require.True(t, seen[name],
			"the exemption map lists %q (%s), which is no longer in the %s table;"+
				" drop the entry", name, ex.reason, kind)
	}
}

// TestIsCallableCoversTheOpTable pins isCallable against
// lisp.DefaultSpecialOps().
func TestIsCallableCoversTheOpTable(t *testing.T) {
	t.Parallel()
	requireProbeHarnessWorks(t)
	assertFormsAreNotCallEdges(t, "special operator", lisp.DefaultSpecialOps(), isCallableExempt)
}

// TestIsCallableCoversTheMacroTable pins isCallable against
// lisp.DefaultMacros(), which isCallable mirrors for the same reason and
// which drifts the same way.
func TestIsCallableCoversTheMacroTable(t *testing.T) {
	t.Parallel()
	requireProbeHarnessWorks(t)
	assertFormsAreNotCallEdges(t, "macro", lisp.DefaultMacros(), isCallableMacroExempt)
}
