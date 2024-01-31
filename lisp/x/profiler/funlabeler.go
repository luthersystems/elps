package profiler

import (
	"fmt"
	"regexp"
	"strings"
	"unicode"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
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
func defaultFunName(runtime *lisp.Runtime, fun *lisp.LVal) string {
	if fun.Type != lisp.LFun {
		return ""
	}
	funData := fun.FunData()
	if funData == nil {
		return ""
	}
	return fmt.Sprintf("%s:%s", funData.Package, getFunNameFromFID(runtime, funData.FID))
}

// ELPSDocLabel is a magic string used to extract function labels.
const ELPSDocLabel = `@trace{([^}]+)}`

var elpsDocLabelRegExp = regexp.MustCompile(ELPSDocLabel)

var sanitizeRegExp = regexp.MustCompile(`[\W_]+`)

func sanitizeLabel(userLabel string) string {
	userLabel = strings.TrimSpace(userLabel)
	if userLabel == "" {
		return ""
	}

	userLabel = sanitizeRegExp.ReplaceAllString(userLabel, "_")
	// Ensure the label doesn't start with a digit or special character
	for len(userLabel) > 0 && !unicode.IsLetter(rune(userLabel[0])) {
		userLabel = userLabel[1:]
	}

	// Eliminate duplicate underscores
	parts := strings.Split(userLabel, "_")
	var nonEmptyParts []string
	for _, part := range parts {
		if part != "" {
			nonEmptyParts = append(nonEmptyParts, part)
		}
	}
	return strings.Join(nonEmptyParts, "_")
}

func elpsDocFunLabeler(runtime *lisp.Runtime, fun *lisp.LVal) string {
	docStr := libhelp.FunDocstring(fun)
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

	return sanitizeLabel(label)
}
