package profiler

import (
	"regexp"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// FunLabeler provides an alternative name for a function label in the trace.
type FunLabeler func(runtime *lisp.Runtime, fun *lisp.LVal) string

// WithELPSDocLabeler labels spans using elps doc magic strings.
func WithELPSDocLabeler() Option {
	return WithFunLabeler(elpsDocFunLabeler)
}

// WithFunLabeler sets the labeler for tracing spans.
func WithFunLabeler(funLabeler FunLabeler) Option {
	return func(p *profiler) {
		p.funLabeler = funLabeler
	}
}

// ELPSDocLabel is a magic string used to extract function labels.
const ELPSDocLabel = `@trace\s*{([^}]+)}`

var (
	elpsDocLabelRegExp = regexp.MustCompile(ELPSDocLabel)
	sanitizeRegExp     = regexp.MustCompile(`[\s_]+`)
	validLabelRegExp   = regexp.MustCompile(`[[:graph:]]*`)
)

func sanitizeLabel(userLabel string) string {
	if userLabel == "" {
		return ""
	}

	// Replace spaces with underscores
	userLabel = sanitizeRegExp.ReplaceAllString(userLabel, "_")

	// Find the first valid label match
	matches := validLabelRegExp.FindStringSubmatch(userLabel)
	if len(matches) > 0 {
		return matches[0]
	}

	return ""
}

func extractLabel(docStr string) string {
	if docStr == "" {
		return ""
	}

	matches := elpsDocLabelRegExp.FindAllStringSubmatch(docStr, -1)
	label := ""
	for _, match := range matches {
		if len(match) > 1 {
			label = match[1]
			break
		}
	}

	return strings.TrimSpace(label)
}

func cleanLabel(docStr string) string {
	return sanitizeLabel(extractLabel(docStr))
}

func elpsDocFunLabeler(runtime *lisp.Runtime, fun *lisp.LVal) string {
	return extractLabel(fun.Docstring())
}
