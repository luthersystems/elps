// Copyright © 2026 The ELPS authors

package lambdalist

import (
	"strings"
	"testing"
)

func validate(names ...string) (int, string) {
	return Validate(len(names), func(i int) string { return names[i] })
}

// A marker introduces a group of parameter names. A group with no names in it
// is not a shorter spelling of anything -- (lambda (a &optional &rest b)) reads
// as if it declared an optional and does not -- so it is a mistake at the
// definition, where a reader can fix it.
func TestValidateRejectsEmptyMarkerGroups(t *testing.T) {
	for _, tc := range []struct {
		name  string
		names []string
		index int
		want  string
	}{
		{"optional-last", []string{"a", "&optional"}, 1, "function formal argument list: &optional must be followed by at least one parameter name"},
		{"optional-then-rest", []string{"a", "&optional", "&rest", "b"}, 1, "function formal argument list: &optional must be followed by at least one parameter name"},
		{"optional-then-key", []string{"a", "&optional", "&key", "b"}, 1, "function formal argument list: &optional must be followed by at least one parameter name"},
		{"optional-only", []string{"&optional"}, 0, "function formal argument list: &optional must be followed by at least one parameter name"},
		{"optional-then-optional", []string{"&optional", "&optional", "a"}, 0, "function formal argument list: &optional must be followed by at least one parameter name"},
		{"key-last", []string{"a", "&key"}, 1, "function formal argument list: &key must be followed by at least one parameter name"},
		{"key-only", []string{"&key"}, 0, "function formal argument list: &key must be followed by at least one parameter name"},
		{"key-then-rest", []string{"&key", "&rest", "b"}, 0, "function formal argument list: &key must be followed by at least one parameter name"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			i, message := validate(tc.names...)
			if message != tc.want {
				t.Errorf("Validate(%v) message = %q, want %q", tc.names, message, tc.want)
			}
			if i != tc.index {
				t.Errorf("Validate(%v) index = %d, want %d", tc.names, i, tc.index)
			}
		})
	}
}

// A group with names in it, in every marker combination the language accepts,
// stays accepted.
func TestValidateAcceptsNonEmptyMarkerGroups(t *testing.T) {
	for _, names := range [][]string{
		{},
		{"a"},
		{"a", "&optional", "b"},
		{"a", "&optional", "b", "c"},
		{"&optional", "a", "&rest", "r"},
		{"a", "&optional", "b", "&rest", "r"},
		{"a", "&key", "k"},
		{"&key", "a", "b"},
		{"a", "&rest", "r"},
		{"&rest", "r"},
	} {
		if i, message := validate(names...); message != "" {
			t.Errorf("Validate(%v) rejected at %d: %s", names, i, message)
		}
	}
}

// The positional rules a marker group's contents do not affect keep reporting
// what they reported: a repeated marker, a marker after &rest, a bogus marker.
func TestValidateKeepsPositionalDiagnostics(t *testing.T) {
	for _, tc := range []struct {
		names []string
		want  string
	}{
		{[]string{"a", "&optional", "b", "&optional", "c"}, "control symbol at an invalid location"},
		{[]string{"&rest", "a", "&optional", "b"}, "control symbol at an invalid location"},
		{[]string{"&rest"}, "control symbol at an invalid location"},
		{[]string{"&rest", "a", "b"}, "control symbol at an invalid location"},
		{[]string{"&bogus", "a"}, "invalid control symbol"},
		{[]string{"a", "a"}, "duplicate formal argument name"},
		{[]string{":x"}, "contains a keyword"},
	} {
		if _, message := validate(tc.names...); !strings.Contains(message, tc.want) {
			t.Errorf("Validate(%v) message = %q, want it to contain %q", tc.names, message, tc.want)
		}
	}
}
