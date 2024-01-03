package profiler

import (
	"fmt"
	"regexp"

	"github.com/luthersystems/elps/lisp"
)

// profiler is a minimal lisp.Profiler
type profiler struct {
	runtime    *lisp.Runtime
	enabled    bool
	skipFilter SkipFilter
}

var _ lisp.Profiler = &profiler{}

func (p *profiler) IsEnabled() bool {
	return p.enabled
}

type Option func(*profiler)

func (p *profiler) applyConfigs(opts ...Option) {
	for _, opt := range opts {
		opt(p)
	}
}

func (p *profiler) Enable() error {
	if p.enabled {
		return fmt.Errorf("profiler already enabled")
	}
	p.enabled = true
	return nil
}

func (p *profiler) Start(fun *lisp.LVal) func() {
	return func() {}
}

// skipTrace is a helper function to decide whether to skip tracing.
func (p *profiler) skipTrace(v *lisp.LVal) bool {
	return !p.enabled || defaultSkipFilter(v) || p.skipFilter != nil && p.skipFilter(v)
}

var builtinRegex = regexp.MustCompile("\\<(?:builtin|special)-[a-z]+ \\`\\`(.*)\\'\\'\\>")

// Gets a canonical version of the function name suitable for human viewing.
func getFunNameFromFID(rt *lisp.Runtime, in string) string {
	// Most of the time we can just look this up in FunNames
	if name, ok := rt.Package.FunNames[in]; ok {
		return name
	}
	// but sometimes something doesn't match - so we'll try to regexp it out
	if !builtinRegex.MatchString(in) {
		return in
	}
	return builtinRegex.FindStringSubmatch(in)[1]
}

// prettyFunName constructs a pretty canonical name.
func prettyFunName(runtime *lisp.Runtime, fun *lisp.LVal) string {
	if fun.Type != lisp.LFun {
		return ""
	}
	funData := fun.FunData()
	if funData == nil {
		return ""
	}
	return fmt.Sprintf("%s:%s", funData.Package, getFunNameFromFID(runtime, funData.FID))
}

func getSource(function *lisp.LVal) (string, int) {
	if function.Source != nil {
		return function.Source.File, function.Source.Line
	}

	source := "no-source"
	line := 0
	if cell := function.Cells[0]; cell != nil && cell.Source != nil {
		source = cell.Source.File
		line = cell.Source.Line
	}
	return source, line
}
