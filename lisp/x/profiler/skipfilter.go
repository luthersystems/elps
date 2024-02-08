package profiler

import (
	"regexp"

	"github.com/luthersystems/elps/lisp"
)

type SkipFilter func(fun *lisp.LVal) bool

func defaultSkipFilter(fun *lisp.LVal) bool {
	switch fun.Type {
	case lisp.LFun, lisp.LSymbol, lisp.LSExpr:
		return false
	case lisp.LInt, lisp.LString, lisp.LFloat, lisp.LBytes, lisp.LError, lisp.LArray, lisp.LQuote, lisp.LNative, lisp.LQSymbol, lisp.LSortMap:
		return true
	default:
		return true
	}
}

// WithELPSDocFilter filters to only include spans for functions with elps
// docs that denote tracing.
func WithELPSDocFilter() Option {
	return WithSkipFilter(elpsDocSkipFilter)
}

// WithSkipFilter sets the filter for tracing spans.
func WithSkipFilter(skipFilter SkipFilter) Option {
	return func(p *profiler) {
		p.skipFilter = skipFilter
	}
}

// ELPSDocTrace is a magic string used to enable tracing in a profiler
// configured WithELPSDocFilter. All functions with an elps doc comment that
// contains this string will be traced.
const ELPSDocTrace = "@trace"

var elpsDocTraceRegExp = regexp.MustCompile(ELPSDocTrace)

func elpsDocSkipFilter(fun *lisp.LVal) bool {
	docStr := fun.Docstring()
	if docStr == "" {
		return true
	}
	// do not skip elps docs that include trace constant
	return !elpsDocTraceRegExp.MatchString(docStr)
}
