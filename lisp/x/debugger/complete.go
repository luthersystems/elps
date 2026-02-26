// Copyright © 2018 The ELPS authors

package debugger

import (
	"sort"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// CompletionCandidate represents a single auto-complete suggestion.
type CompletionCandidate struct {
	Label  string // symbol name (possibly qualified with package prefix)
	Type   string // "variable", "function", "keyword", "module"
	Detail string // e.g. package name or "<builtin>"
}

// CompleteInContext returns completion candidates for the given prefix
// in the context of env. It collects candidates from function locals,
// current package symbols, qualified package symbols, package names,
// exported symbols from used packages, and keywords.
//
// An empty prefix returns no candidates (matches REPL behavior).
func CompleteInContext(env *lisp.LEnv, prefix string) []CompletionCandidate {
	if prefix == "" || env == nil {
		return nil
	}

	seen := make(map[string]bool)
	var candidates []CompletionCandidate

	add := func(label, typ, detail string) {
		if seen[label] {
			return
		}
		seen[label] = true
		candidates = append(candidates, CompletionCandidate{
			Label:  label,
			Type:   typ,
			Detail: detail,
		})
	}

	// 1. Function locals — walk up env chain, stopping before root.
	locals := InspectFunctionLocals(env)
	for _, b := range locals {
		if strings.HasPrefix(b.Name, prefix) {
			add(b.Name, lvalCompletionType(b.Value), "local")
		}
	}

	// 2. Current package symbols.
	if pkg := env.Runtime.Package; pkg != nil {
		for name, val := range pkg.Symbols {
			if strings.HasPrefix(name, prefix) {
				add(name, lvalCompletionType(val), pkg.Name)
			}
		}
	}

	// 3. Qualified package completion and package name completion.
	if env.Runtime.Registry != nil {
		for pkgName, pkg := range env.Runtime.Registry.Packages {
			qualPrefix := pkgName + ":"
			if strings.HasPrefix(prefix, qualPrefix) {
				// Prefix contains "pkg:" — complete within that package's exports.
				symPrefix := prefix[len(qualPrefix):]
				for _, ext := range pkg.Externals {
					if strings.HasPrefix(ext, symPrefix) {
						name := qualPrefix + ext
						typ := "function"
						if val, ok := pkg.Symbols[ext]; ok {
							typ = lvalCompletionType(val)
						}
						add(name, typ, pkgName)
					}
				}
			} else if strings.HasPrefix(qualPrefix, prefix) {
				// Prefix matches start of package name — suggest "pkg:".
				add(qualPrefix, "module", pkgName)
			}

			// 4. Unqualified exported symbols from all packages.
			for _, ext := range pkg.Externals {
				if strings.HasPrefix(ext, prefix) {
					typ := "function"
					if val, ok := pkg.Symbols[ext]; ok {
						typ = lvalCompletionType(val)
					}
					add(ext, typ, pkgName)
				}
			}
		}
	}

	// 5. Keyword completion — when prefix starts with ":".
	if strings.HasPrefix(prefix, ":") {
		collectKeywords(env, prefix, add)
	}

	sort.Slice(candidates, func(i, j int) bool {
		return candidates[i].Label < candidates[j].Label
	})
	return candidates
}

// ExtractPrefix extracts the completion prefix from text at the given
// column position. Column is 1-based (DAP convention). The prefix is
// the word being typed, walking backward from the cursor to whitespace
// or an open paren.
func ExtractPrefix(text string, column int) string {
	// Convert 1-based column to 0-based index.
	pos := column - 1
	if pos < 0 {
		pos = 0
	}
	if pos > len(text) {
		pos = len(text)
	}

	start := pos
	for start > 0 {
		ch := text[start-1]
		if ch == ' ' || ch == '\t' || ch == '(' || ch == '\n' {
			break
		}
		start--
	}
	return text[start:pos]
}

// lvalCompletionType maps an LVal to a completion type string.
func lvalCompletionType(v *lisp.LVal) string {
	if v == nil {
		return "variable"
	}
	if v.Type == lisp.LFun {
		return "function"
	}
	return "variable"
}

// collectKeywords scans package symbol tables for keyword-like symbols
// (names starting with ":") and adds them as candidates.
func collectKeywords(env *lisp.LEnv, prefix string, add func(string, string, string)) {
	if env.Runtime.Registry == nil {
		return
	}
	for _, pkg := range env.Runtime.Registry.Packages {
		for name := range pkg.Symbols {
			if strings.HasPrefix(name, prefix) && strings.HasPrefix(name, ":") {
				add(name, "keyword", pkg.Name)
			}
		}
	}
}
