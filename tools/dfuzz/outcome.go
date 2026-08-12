// Copyright © 2026 The ELPS authors

package main

import (
	"fmt"
	"regexp"
	"sort"
	"strings"
)

// Outcome is the tree-independent record of one evaluation.  Both evaluators
// (eval_stock.go, eval_sealed.go) reduce their own tree's *lisp.LVal to this
// so the comparison never touches a tree-specific type -- which is what lets
// the two interpreters, whose packages are structurally identical but are
// distinct Go types, be diffed in a single process.
//
// Every field is a STRING (or a map of them) on purpose.  The only thing a
// differential oracle may compare is what a program can OBSERVE; comparing Go
// values would compare representation, and the whole point of the sealed
// branch is that the representation changed while the observable behaviour
// must not have.
type Outcome struct {
	// Type is the LVal type name of the final value ("int", "string",
	// "sorted-map", "error", ...).
	Type string
	// Value is the printed final value.
	Value string
	// IsError reports whether the program terminated in an error condition.
	IsError bool
	// Cond is the error's condition symbol, when IsError.
	Cond string
	// Msg is the error's rendered message with source locations normalized.
	Msg string
	// InternalPanic reports whether the interpreter's recover() handler
	// converted a Go panic into this error.  A panic in one tree and not the
	// other is a finding on its own, independent of the value.
	InternalPanic bool
	// HardPanic holds a Go panic that escaped the interpreter entirely (i.e.
	// one the evaluator's own recover() did not catch).  Non-empty means the
	// tree crashed.
	HardPanic string
	// Timeout reports that the evaluation did not return inside the harness
	// watchdog.  A hang on one side only is a finding.
	Timeout bool
	// Globals is the printed final value of each probe global the generator
	// may have written (see probeGlobals).  This is the cheap side-effect
	// oracle: two trees that return the same value while leaving different
	// state behind have diverged.
	Globals map[string]string
	// Stderr is what the program wrote to the captured stderr stream
	// (debug-print and friends), normalized.
	Stderr string
	// Starved reports that this outcome was decided by WALL CLOCK rather than
	// by the program: the evaluation's context deadline fired, or the harness
	// watchdog did.  Such an outcome is not comparable -- it records how busy
	// the machine was, not what the interpreter computed -- so a pair where
	// either side is starved is retried and, if it starves again, dropped.
	//
	// This is not hypothetical.  On a loaded 4-core sandbox the first
	// unfiltered run produced 31 "findings" in 52,000 programs and every one
	// was a deadline firing at step 1 on one side only.  Reporting those as
	// semantic divergences is how a differential harness goes to noise.
	Starved bool
	// Steps is the interpreter step count.  Recorded for triage only -- it is
	// NOT compared, because the sealed branch legitimately changes the number
	// of steps some operations take.
	Steps int64
}

// probeGlobals are the symbols the side-effect oracle reads back after every
// evaluation.  The generator writes its state into these names so the oracle
// has somewhere fixed to look; reading a symbol the program never bound is
// harmless (it comes back as an unbound-symbol error, identically in both
// trees).
var probeGlobals = []string{"g0", "g1", "g2", "lst", "vec", "byt", "sm", "acc"}

// locRE matches the "name:line: " source-location prefixes elps stamps into
// error messages.  Line numbers are stable between the trees but the harness
// normalizes them anyway so a divergence report is about behaviour rather
// than about which line of a generated program raised.
var locRE = regexp.MustCompile(`(?m)\b[\w./-]+:\d+(:\d+)?:`)

// ptrRE matches the addresses elps prints for opaque values.  These are
// allocation-order dependent and differ run to run within a SINGLE tree, so
// comparing them would report noise as corruption.
var ptrRE = regexp.MustCompile(`0x[0-9a-f]{4,}`)

// gensymRE matches gensym output, which embeds a per-environment counter.
var gensymRE = regexp.MustCompile(`gensym\d+`)

// normalize erases the parts of a rendered value that are not observable
// semantics: pointer identity, gensym counters and source locations.
func normalize(s string) string {
	s = ptrRE.ReplaceAllString(s, "0xPTR")
	s = gensymRE.ReplaceAllString(s, "gensymN")
	s = locRE.ReplaceAllString(s, "LOC:")
	return s
}

// Divergence is one difference between the two trees for one program.
type Divergence struct {
	Field  string
	Stock  string
	Sealed string
}

func (d Divergence) String() string {
	return fmt.Sprintf("%s: stock=%q sealed=%q", d.Field, d.Stock, d.Sealed)
}

// Compare returns every observable difference between the two outcomes.  An
// empty result means the two trees agreed.
func Compare(stock, sealed Outcome) []Divergence {
	var out []Divergence
	add := func(field, a, b string) {
		if a != b {
			out = append(out, Divergence{Field: field, Stock: a, Sealed: b})
		}
	}
	add("panic", boolStr(stock.HardPanic != ""), boolStr(sealed.HardPanic != ""))
	if stock.HardPanic != "" || sealed.HardPanic != "" {
		add("panic.msg", firstLine(stock.HardPanic), firstLine(sealed.HardPanic))
	}
	add("timeout", boolStr(stock.Timeout), boolStr(sealed.Timeout))
	add("internal-panic", boolStr(stock.InternalPanic), boolStr(sealed.InternalPanic))
	add("is-error", boolStr(stock.IsError), boolStr(sealed.IsError))
	add("type", stock.Type, sealed.Type)
	add("value", stock.Value, sealed.Value)
	if stock.IsError && sealed.IsError {
		add("condition", stock.Cond, sealed.Cond)
		add("message", stock.Msg, sealed.Msg)
	}
	add("stderr", stock.Stderr, sealed.Stderr)

	keys := map[string]bool{}
	for k := range stock.Globals {
		keys[k] = true
	}
	for k := range sealed.Globals {
		keys[k] = true
	}
	names := make([]string, 0, len(keys))
	for k := range keys {
		names = append(names, k)
	}
	sort.Strings(names)
	for _, k := range names {
		add("global:"+k, stock.Globals[k], sealed.Globals[k])
	}
	return out
}

func boolStr(b bool) string {
	if b {
		return "true"
	}
	return "false"
}

func firstLine(s string) string {
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return s[:i]
	}
	return s
}
