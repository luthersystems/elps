// Copyright © 2026 The ELPS authors

// Package lambdalist shares lambda-list validation between runtime and lint.
package lambdalist

import (
	"fmt"
	"strings"
)

// InvalidName reports why s cannot NAME a formal argument, or "" when it can.
// A keyword and the constants true and false are refused by LEnv.Put at bind
// time, so a function declaring one could never be called; saying so where the
// function is written puts the diagnostic on the definition a reader can fix.
// Control markers (&optional, &key, &rest) are a matter of position rather
// than of the name alone and are checked by Validate.
func InvalidName(s string) string {
	switch {
	case strings.HasPrefix(s, ":"):
		return "function formal argument list contains a keyword: " + s
	case s == "true" || s == "false":
		return "function formal argument list contains the constant " + s
	}
	return ""
}

// Validate checks n symbol names supplied by name without copying the list.
// It returns the offending index and the runtime diagnostic, or (-1, "").
// Callers check that the list contains only symbols before calling Validate.
func Validate(n int, name func(int) string) (int, string) {
	invalid := func(i int, control string) (int, string) {
		return i, "function formal argument list contains a control symbol at an invalid location: " + control
	}
	// A marker introduces a group of parameter names, and a group with no
	// names in it declares nothing. (a &optional &rest b) reads as if it
	// declared an optional argument and does not: the marker was accepted
	// because a later marker followed it, so the mistake surfaced -- if at
	// all -- as a call that would not take the argument the definition seems
	// to offer. &rest has always required exactly one name after it.
	emptyGroup := func(i int, control string) (int, string) {
		return i, "function formal argument list: " + control + " must be followed by at least one parameter name"
	}
	group := func(i int, control string) (int, string, bool) {
		if i == n-1 || strings.HasPrefix(name(i+1), "&") {
			i, message := emptyGroup(i, control)
			return i, message, false
		}
		return 0, "", true
	}
	optional := false
	for i := range n {
		s := name(i)
		switch s {
		case "&optional":
			if j, message, ok := group(i, s); !ok {
				return j, message
			}
			if optional {
				return invalid(i, s)
			}
			optional = true
		case "&key":
			if j, message, ok := group(i, s); !ok {
				return j, message
			}
			for j := i + 1; j < n; j++ {
				if strings.HasPrefix(name(j), "&") {
					return invalid(j, s)
				}
			}
		case "&rest":
			if i != n-2 {
				return invalid(i, s)
			}
			if strings.HasPrefix(name(i+1), "&") {
				// The binder reports the would-be rest name in this case.
				return invalid(i+1, name(i+1))
			}
		default:
			if strings.HasPrefix(s, "&") {
				return i, fmt.Sprintf("function formal argument list contains invalid control symbol ``%s''", s)
			}
			if message := InvalidName(s); message != "" {
				return i, message
			}
		}
	}
	// Small lists need no map allocation. Bound the quadratic scan so large
	// generated lambda lists still take linear time.
	var seen map[string]bool
	if n > 16 {
		seen = make(map[string]bool, n)
	}
	for i := range n {
		s := name(i)
		if strings.HasPrefix(s, "&") {
			continue
		}
		duplicate := false
		if seen != nil {
			duplicate = seen[s]
			seen[s] = true
		} else {
			for j := range i {
				if name(j) == s {
					duplicate = true
					break
				}
			}
		}
		if duplicate {
			return i, "duplicate formal argument name: " + s
		}
	}
	return -1, ""
}
