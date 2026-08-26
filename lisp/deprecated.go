// Copyright © 2026 The ELPS authors

package lisp

import "strings"

// DeprecationNotice reports whether a docstring marks its symbol as
// deprecated and returns the notice text.
//
// The convention mirrors Go's: a docstring paragraph beginning with
// "Deprecated:" (or "DEPRECATED:") marks the symbol deprecated, and the
// rest of that paragraph tells callers what to use instead. Paragraphs
// are separated by blank lines. Leading and trailing whitespace on each
// line is ignored, so tab-indented builtin docstrings written as Go raw
// string literals work unchanged.
//
// The returned notice is the marker paragraph's text with the marker
// removed and its lines joined by single spaces. A marker paragraph with
// no text after the marker returns ("", true).
func DeprecationNotice(doc string) (string, bool) {
	lines := strings.Split(doc, "\n")
	paraStart := true
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			paraStart = true
			continue
		}
		if !paraStart {
			continue
		}
		paraStart = false
		rest, ok := strings.CutPrefix(trimmed, "Deprecated:")
		if !ok {
			rest, ok = strings.CutPrefix(trimmed, "DEPRECATED:")
		}
		if !ok {
			continue
		}
		parts := []string{strings.TrimSpace(rest)}
		for _, cont := range lines[i+1:] {
			cont = strings.TrimSpace(cont)
			if cont == "" {
				break
			}
			parts = append(parts, cont)
		}
		return strings.TrimSpace(strings.Join(parts, " ")), true
	}
	return "", false
}
