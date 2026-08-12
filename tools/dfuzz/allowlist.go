// Copyright © 2026 The ELPS authors

package main

import (
	"strings"
)

// The allowlist of INTENDED divergences.
//
// The sealed branch deliberately changes some observable behaviour: that is
// what it is for.  Each rule below names one such change, says in one line why
// the new behaviour is the correct one, and cites the commit that made it
// intentional.  Anything no rule matches is a FINDING.
//
// Two design rules keep this from becoming a place where regressions hide.
//
// A rule must be NARROW.  It is keyed on the specific condition, direction or
// builtin that changed -- never on "the value differs" -- and a rule that
// would also swallow an unrelated regression is not admissible.  That is why
// Match sees the program SOURCE as well as the two outcomes: "the sealed tree
// declined to mutate a quoted literal" is a claim about which builtin ran, and
// a rule that cannot see the program cannot make it.
//
// A matched divergence is CLASSIFIED, not hidden.  The run still counts it,
// reports the count per class, and prints one exemplar per class.  Only
// unclassified divergences fail the run.  The difference matters: an allowlist
// that suppresses output turns a wrong rule into silence, and silence is
// indistinguishable from a clean run.
type allowRule struct {
	// Name identifies the rule in the report.
	Name string
	// Why is the one-line justification.
	Why string
	// Match reports whether this rule explains divergence d for program src.
	Match func(src string, d Divergence, stock, sealed Outcome) bool
}

// allowRules is the whole allowlist.  It is small, and it should stay small.
var allowRules = []allowRule{
	{
		Name: "error-instead-of-panic",
		Why:  "paths that panicked now return an ordinary error or a value (95bbd0c, 0442c52, #355); a panic on the left and none on the right is the intended direction, and the value difference that follows is part of the same change.",
		Match: func(src string, d Divergence, stock, sealed Outcome) bool {
			// Strictly directional: it never explains a NEW panic on the
			// sealed side, which must always be a finding.
			if sealed.InternalPanic || sealed.HardPanic != "" {
				return false
			}
			return stock.InternalPanic || stock.HardPanic != ""
		},
	},
	{
		Name: "stable-sort/literal-not-mutated",
		Why:  "stable-sort sorted a quoted literal IN PLACE on origin/main, editing the parse tree; the copy-on-write guards (4c7649a) stop it, so a program that observes the literal after sorting sees the original on the sealed side. Sealed is correct.",
		Match: func(src string, d Divergence, stock, sealed Outcome) bool {
			// Requires the program to actually call stable-sort: this rule is
			// about one builtin, not about values differing.
			if !strings.Contains(src, "stable-sort") {
				return false
			}
			// Never explains a difference in WHETHER or HOW the program
			// failed: the two trees must agree exactly on that, and only the
			// computed values may differ.  Note this is not the same as
			// "neither tree errored" -- a program that sorts a literal and
			// then fails on an unrelated expression still shows the intended
			// difference in the globals it set, and both trees still report
			// the identical error.
			if stock.IsError != sealed.IsError ||
				stock.Cond != sealed.Cond ||
				stock.Msg != sealed.Msg ||
				stock.InternalPanic != sealed.InternalPanic ||
				(stock.HardPanic != "") != (sealed.HardPanic != "") {
				return false
			}
			return d.Field == "value" || strings.HasPrefix(d.Field, "global:")
		},
	},
}

// Classification is the verdict on one program's divergences.
type Classification struct {
	// Intended maps rule name to the divergences it explained.
	Intended map[string][]Divergence
	// Findings are the divergences no rule explained.
	Findings []Divergence
}

// Classify splits a program's divergences into intended and findings.
func Classify(src string, ds []Divergence, stock, sealed Outcome, enabled map[string]bool) Classification {
	c := Classification{Intended: map[string][]Divergence{}}
	for _, d := range ds {
		matched := ""
		for _, r := range allowRules {
			if !enabled[r.Name] {
				continue
			}
			if r.Match(src, d, stock, sealed) {
				matched = r.Name
				break
			}
		}
		if matched == "" {
			c.Findings = append(c.Findings, d)
			continue
		}
		c.Intended[matched] = append(c.Intended[matched], d)
	}
	return c
}

// defaultAllow is the set of rules on by default.
//
// Both are on: each names a change the branch made deliberately, and the run
// reports the count and an exemplar for every class it applies, so turning
// them on trades away no visibility -- only the exit status.
func defaultAllow() map[string]bool {
	return map[string]bool{
		"error-instead-of-panic":          true,
		"stable-sort/literal-not-mutated": true,
	}
}
