// Copyright © 2024 The ELPS authors

package formatter

import "strings"

// IndentStyle determines how arguments in an s-expression are indented.
type IndentStyle int

const (
	// IndentAlign indents subsequent lines to align with the first argument.
	IndentAlign IndentStyle = iota
	// IndentBody indents all subforms at bracket column + indent size.
	IndentBody
	// IndentSpecial indents N header args aligned, rest at bracket + indent size.
	IndentSpecial
)

// IndentRule specifies the indentation behavior for a particular form.
type IndentRule struct {
	Style      IndentStyle
	HeaderArgs int // for IndentSpecial: args before the "body"
}

// Config holds formatting configuration.
type Config struct {
	IndentSize    int                    // spaces per indent level (default: 2)
	MaxBlankLines int                    // max consecutive blank lines (default: 1)
	Rules         map[string]*IndentRule // form name -> rule
}

// DefaultConfig returns the default formatting configuration.
func DefaultConfig() *Config {
	return &Config{
		IndentSize:    2,
		MaxBlankLines: 1,
		Rules:         DefaultRules(),
	}
}

// DefaultRules returns the default indent rules table.
func DefaultRules() map[string]*IndentRule {
	return map[string]*IndentRule{
		// 2 header args + body
		"defun":    {Style: IndentSpecial, HeaderArgs: 2},
		"defmacro": {Style: IndentSpecial, HeaderArgs: 2},
		"deftype":  {Style: IndentSpecial, HeaderArgs: 2},

		// 1 header arg + body
		"lambda":       {Style: IndentSpecial, HeaderArgs: 1},
		"let":          {Style: IndentSpecial, HeaderArgs: 1},
		"let*":         {Style: IndentSpecial, HeaderArgs: 1},
		"flet":         {Style: IndentSpecial, HeaderArgs: 1},
		"labels":       {Style: IndentSpecial, HeaderArgs: 1},
		"macrolet":     {Style: IndentSpecial, HeaderArgs: 1},
		"handler-bind": {Style: IndentSpecial, HeaderArgs: 1},
		"dotimes":      {Style: IndentSpecial, HeaderArgs: 1},
		"if":           {Style: IndentSpecial, HeaderArgs: 1},
		"do":           {Style: IndentSpecial, HeaderArgs: 1},
		"unless":       {Style: IndentSpecial, HeaderArgs: 1},
		"when":         {Style: IndentSpecial, HeaderArgs: 1},

		// testing forms
		"test":             {Style: IndentSpecial, HeaderArgs: 1},
		"benchmark-simple": {Style: IndentSpecial, HeaderArgs: 1},
		"test-let":         {Style: IndentSpecial, HeaderArgs: 2},
		"test-let*":        {Style: IndentSpecial, HeaderArgs: 2},
		"benchmark":        {Style: IndentSpecial, HeaderArgs: 2},

		// threading — align all forms with first arg
		"thread-first": {Style: IndentAlign},
		"thread-last":  {Style: IndentAlign},

		// all body
		"progn":         {Style: IndentBody},
		"ignore-errors": {Style: IndentBody},

		// quasiquote forms
		"quasiquote":       {Style: IndentBody},
		"unquote":          {Style: IndentBody},
		"unquote-splicing": {Style: IndentBody},
	}
}

// RuleFor returns the indent rule for the given form name.
// If no specific rule exists, returns the default first-arg alignment rule.
// Forms starting with "def" get defun-style indent (2 header args + body),
// following the common Lisp convention for definition forms.
func (c *Config) RuleFor(name string) *IndentRule {
	if r, ok := c.Rules[name]; ok {
		return r
	}
	// Common Lisp convention: forms starting with "def" get defun-style indent.
	// This handles user-defined macros like def-app-route, def-case-verification, etc.
	if strings.HasPrefix(name, "def") {
		return &IndentRule{Style: IndentSpecial, HeaderArgs: 2}
	}
	return &IndentRule{Style: IndentAlign}
}
