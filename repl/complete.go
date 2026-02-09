// Copyright © 2018 The ELPS authors

package repl

import (
	"sort"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// symbolCompleter implements readline.AutoCompleter by enumerating symbols
// from the current ELPS environment.
type symbolCompleter struct {
	env *lisp.LEnv
}

func (c *symbolCompleter) Do(line []rune, pos int) ([][]rune, int) {
	// Extract the word being typed (backwards from cursor to whitespace or open paren).
	start := pos
	for start > 0 {
		ch := line[start-1]
		if ch == ' ' || ch == '\t' || ch == '(' || ch == '\n' {
			break
		}
		start--
	}
	prefix := string(line[start:pos])
	if prefix == "" {
		return nil, 0
	}

	candidates := c.collectSymbols(prefix)
	if len(candidates) == 0 {
		return nil, 0
	}

	// Build completions: each entry is the suffix to append.
	result := make([][]rune, 0, len(candidates))
	for _, sym := range candidates {
		suffix := sym[len(prefix):]
		result = append(result, []rune(suffix))
	}
	return result, len(prefix)
}

func (c *symbolCompleter) collectSymbols(prefix string) []string {
	seen := make(map[string]bool)
	var result []string

	// Symbols from the current package.
	if pkg := c.env.Runtime.Package; pkg != nil {
		for name := range pkg.Symbols {
			if strings.HasPrefix(name, prefix) && !seen[name] {
				seen[name] = true
				result = append(result, name)
			}
		}
	}

	// Exported symbols from all packages (as qualified names).
	for pkgName, pkg := range c.env.Runtime.Registry.Packages {
		// Check for qualified prefix like "string:jo".
		qualPrefix := pkgName + ":"
		if strings.HasPrefix(prefix, qualPrefix) {
			// Complete within this package.
			symPrefix := prefix[len(qualPrefix):]
			for _, ext := range pkg.Externals {
				if strings.HasPrefix(ext, symPrefix) {
					name := qualPrefix + ext
					if !seen[name] {
						seen[name] = true
						result = append(result, name)
					}
				}
			}
		} else if strings.HasPrefix(qualPrefix, prefix) {
			// Complete the package name itself.
			if !seen[qualPrefix] {
				seen[qualPrefix] = true
				result = append(result, qualPrefix)
			}
		}

		// Unqualified exported symbols from used packages.
		for _, ext := range pkg.Externals {
			if strings.HasPrefix(ext, prefix) && !seen[ext] {
				seen[ext] = true
				result = append(result, ext)
			}
		}
	}

	sort.Strings(result)
	return result
}
