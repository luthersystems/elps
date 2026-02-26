// Copyright © 2018 The ELPS authors

package debugrepl

import (
	"sort"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
)

// debugCommands lists all debug command names for tab completion.
var debugCommands = []string{
	"backtrace",
	"break",
	"breakpoints",
	"continue",
	"delete",
	"help",
	"locals",
	"next",
	"out",
	"print",
	"quit",
	"step",
	"where",
}

// debugCompleter implements readline.AutoCompleter for the debug REPL.
// It merges debug command names with symbol completions from the paused
// environment.
type debugCompleter struct {
	env    *lisp.LEnv
	engine *debugger.Engine
}

func (c *debugCompleter) Do(line []rune, pos int) ([][]rune, int) {
	// Extract prefix (word being typed).
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

	// Determine if we're completing the first word (debug command) or
	// a subsequent word (symbols/expressions).
	beforePrefix := strings.TrimSpace(string(line[:start]))
	firstWord := beforePrefix == ""

	var candidates []string
	seen := make(map[string]bool)

	// Debug commands only for first word.
	if firstWord {
		for _, cmd := range debugCommands {
			if strings.HasPrefix(cmd, prefix) && !seen[cmd] {
				seen[cmd] = true
				candidates = append(candidates, cmd)
			}
		}
	}

	// Symbol completions from the debugger context.
	env, _ := c.engine.PausedState()
	if env == nil {
		env = c.env
	}
	for _, cand := range debugger.CompleteInContext(env, prefix) {
		if !seen[cand.Label] {
			seen[cand.Label] = true
			candidates = append(candidates, cand.Label)
		}
	}

	sort.Strings(candidates)

	result := make([][]rune, 0, len(candidates))
	for _, sym := range candidates {
		suffix := sym[len(prefix):]
		result = append(result, []rune(suffix))
	}
	return result, len(prefix)
}
