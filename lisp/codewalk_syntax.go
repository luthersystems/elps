// Copyright © 2026 The ELPS authors

package lisp

import (
	"slices"

	"github.com/luthersystems/elps/internal/codewalk/hook"
)

func init() {
	w := CodeWalker{}
	hook.SyntaxOp = w.syntaxOperator
}

// syntaxOperator classifies raw heads, including quoted heads, without
// applying runtime resolution or the resolver's narrower source policy.
// Syntax walks call it on every list, so it rejects ordinary call names by
// length and first byte before comparing any strings.
func (*CodeWalker) syntaxOperator(head *LVal) string {
	if head == nil || head.Type != LSymbol {
		return ""
	}
	name := head.Str
	if len(name) > len(syntaxLangPrefix) && name[:len(syntaxLangPrefix)] == syntaxLangPrefix {
		if e := syntaxLookup(name[len(syntaxLangPrefix):]); e != nil && !e.bareOnly {
			return e.name
		}
		return ""
	}
	if e := syntaxLookup(name); e != nil {
		return e.name
	}
	return ""
}

const syntaxLangPrefix = DefaultLangPackage + ":"

type syntaxEntry struct {
	name     string
	next     uint8 // 1-based index of the next entry in the bucket, or 0
	bareOnly bool  // template-hole markers have no lisp: spelling
}

var (
	syntaxEntries []syntaxEntry
	// syntaxBuckets[len][first byte] is the 1-based index of the first entry.
	syntaxBuckets [][256]uint8
	// Template-hole markers are recognized only in their bare spelling.
	syntaxHoleMarkers = []string{"unquote", "unquote-splicing"}
)

func syntaxLookup(name string) *syntaxEntry {
	if len(name) == 0 || len(name) >= len(syntaxBuckets) {
		return nil
	}
	for i := syntaxBuckets[len(name)][name[0]]; i != 0; i = syntaxEntries[i-1].next {
		if e := &syntaxEntries[i-1]; e.name == name {
			return e
		}
	}
	return nil
}

// The syntax operators are the structural special forms of formKinds plus
// the template grammar, which is not a special operator in code position.
func init() {
	names := make([]string, 0, len(formKinds)+len(syntaxHoleMarkers))
	for name := range formKinds {
		names = append(names, name)
	}
	names = append(names, syntaxHoleMarkers...)
	maxLength := 0
	for _, name := range names {
		maxLength = max(maxLength, len(name))
	}
	if len(names) >= 255 {
		panic("lisp: too many syntax operators for the bucket table")
	}
	syntaxBuckets = make([][256]uint8, maxLength+1)
	for _, name := range names {
		b := &syntaxBuckets[len(name)][name[0]]
		bare := slices.Contains(syntaxHoleMarkers, name)
		syntaxEntries = append(syntaxEntries, syntaxEntry{name: name, next: *b, bareOnly: bare})
		*b = uint8(len(syntaxEntries))
	}
}
