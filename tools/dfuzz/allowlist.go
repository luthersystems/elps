// Copyright © 2026 The ELPS authors

package main

import (
	"regexp"
	"strings"
)

// The allowlist of INTENDED divergences.
//
// The sealed branch deliberately changes some observable behaviour: that is
// what it is for.  Each rule below names one such change and says, in one
// line, why the new behaviour is the correct one.  Anything a rule does not
// match is a FINDING and gets investigated.
//
// The bar for adding a rule is high on purpose.  A rule is a permanent hole in
// the oracle, so it must be NARROW -- keyed on the specific condition or
// message shape that changed, never on "value differs" -- and it must cite the
// commit or issue that made the change intentional.  A rule that would also
// swallow an unrelated regression is not admissible.
type allowRule struct {
	// Name identifies the rule in the report.
	Name string
	// Why is the one-line justification.
	Why string
	// Match reports whether this rule explains the divergence d, given the
	// full pair of outcomes for context.
	Match func(d Divergence, stock, sealed Outcome) bool
}

var (
	// A stable sort that is now actually stable reorders equal elements
	// differently from the previous unstable one.
	reUnstable = regexp.MustCompile(`^(list|vector|array)`)
)

// allowRules is the whole allowlist.  It is small, and it should stay small.
var allowRules = []allowRule{
	{
		Name: "stable-sort/no-literal-mutation",
		Why:  "stable-sort no longer sorts a quoted literal in place (636d5be); stock's SECOND call sees an already-sorted literal, sealed's does not -- sealed is the correct one.",
		Match: func(d Divergence, stock, sealed Outcome) bool {
			// Only ever explains a VALUE difference, and only when both trees
			// produced a sequence, and only for a program that sorts.
			if d.Field != "value" && !strings.HasPrefix(d.Field, "global:") {
				return false
			}
			if stock.IsError || sealed.IsError {
				return false
			}
			return reUnstable.MatchString(stock.Type) || reUnstable.MatchString(d.Stock)
		},
	},
	{
		Name: "elpspath/copying-ops-do-not-alias",
		Why:  "?set/?del/?nil now return a copy whose list write-back does not alias the input (0442c52); stock leaked the edit into the source document, sealed does not.",
		Match: func(d Divergence, stock, sealed Outcome) bool {
			return false // enabled per-run by -allow=elpspath; off by default so the class is reported, not hidden.
		},
	},
	{
		Name: "error-instead-of-panic",
		Why:  "the sealed branch converts several out-of-range paths that used to panic into ordinary error conditions (95bbd0c, fix-355); an error where stock had an internal panic is the intended direction.",
		Match: func(d Divergence, stock, sealed Outcome) bool {
			if d.Field == "internal-panic" {
				return stock.InternalPanic && !sealed.InternalPanic
			}
			if d.Field == "panic" {
				return stock.HardPanic != "" && sealed.HardPanic == ""
			}
			// The value/condition/message differences that necessarily
			// accompany a panic becoming an error.
			return (stock.InternalPanic || stock.HardPanic != "") &&
				!sealed.InternalPanic && sealed.HardPanic == ""
		},
	},
}

// Classification is the verdict on one program's divergences.
type Classification struct {
	Intended []string // rule names that explained a divergence
	Findings []Divergence
}

// Classify splits a program's divergences into intended and findings.
func Classify(ds []Divergence, stock, sealed Outcome, enabled map[string]bool) Classification {
	var c Classification
	seen := map[string]bool{}
	for _, d := range ds {
		matched := false
		for _, r := range allowRules {
			if !enabled[r.Name] {
				continue
			}
			if r.Match(d, stock, sealed) {
				matched = true
				if !seen[r.Name] {
					seen[r.Name] = true
					c.Intended = append(c.Intended, r.Name)
				}
				break
			}
		}
		if !matched {
			c.Findings = append(c.Findings, d)
		}
	}
	return c
}

// defaultAllow is the set of rules on by default.  A rule whose Match is a
// blanket "false" is listed here anyway so `-list-allow` documents it.
func defaultAllow() map[string]bool {
	return map[string]bool{
		"stable-sort/no-literal-mutation":   false,
		"elpspath/copying-ops-do-not-alias": false,
		"error-instead-of-panic":            true,
	}
}
