// Copyright © 2024 The ELPS authors

package perf

import (
	"strings"

	"github.com/luthersystems/elps/lisp"
)

type suppression struct {
	all   bool
	rules map[RuleID]bool
}

func parseSuppression(node *lisp.LVal, prefix string) suppression {
	var out suppression
	if node == nil || node.Meta == nil {
		return out
	}
	if prefix == "" {
		prefix = "elps-analyze-disable"
	}
	for _, tok := range node.Meta.LeadingComments {
		if tok == nil {
			continue
		}
		text := strings.TrimSpace(tok.Text)
		text = strings.TrimLeft(text, ";")
		text = strings.TrimSpace(text)
		if text == prefix {
			out.all = true
			out.rules = nil
			return out
		}
		ruleText, ok := strings.CutPrefix(text, prefix+":")
		if !ok {
			continue
		}
		if out.rules == nil {
			out.rules = make(map[RuleID]bool)
		}
		for _, part := range strings.Split(ruleText, ",") {
			rule := parseSuppressedRule(part)
			if rule == "" {
				continue
			}
			out.rules[rule] = true
		}
	}
	return out
}

func parseSuppressedRule(part string) RuleID {
	rule := RuleID(strings.TrimSpace(part))
	switch rule {
	case PERF001, PERF002, PERF003, PERF004, UNKNOWN001:
		return rule
	default:
		return ""
	}
}

func applySuppression(summary *FunctionSummary, s suppression) {
	if summary == nil {
		return
	}
	summary.SuppressAll = s.all
	summary.SuppressedRules = s.rules
}

func suppressesRule(summary *FunctionSummary, rule RuleID) bool {
	if summary == nil {
		return false
	}
	if summary.SuppressAll {
		return true
	}
	return summary.SuppressedRules[rule]
}
