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

// defaultFunName constructs a pretty canonical name using the function name.
// The first return is the package name, and the second is the canonical name.
func defaultFunName(runtime *lisp.Runtime, fun *lisp.LVal) (string, string) {
	if fun.Type != lisp.LFun {
		return "", ""
	}
	funData := fun.FunData()
	if funData == nil {
		return "", ""
	}
	name := ""
	if env := fun.Env(); env != nil {
		name = env.GetFunName(fun)
	}
	if name == "" {
		name = getFunNameFromFID(runtime, funData.FID)
	}
	return funData.Package, name
}

// ELPSDocLabel is a magic string used to extract function labels.
const ELPSDocLabel = `@trace\s*{([^}]+)}`

var elpsDocLabelRegExp = regexp.MustCompile(ELPSDocLabel)
var sanitizeRegExp = regexp.MustCompile(`[\W_]+`)

var validLabelRegExp = regexp.MustCompile(`[[:alpha:]][\w_]*`)

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

func docLabel(docStr string) string {
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

func elpsDocFunLabeler(runtime *lisp.Runtime, fun *lisp.LVal) string {
	docStr := fun.Docstring()
	if docStr == "" {
		return ""
	}
	label := docLabel(docStr)
	if label == "" {
		return ""
	}
	return sanitizeLabel(label)
}
