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
	funLabeler FunLabeler
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

// defaultFunName constructs a pretty canonical name using the function name.
func defaultFunName(runtime *lisp.Runtime, fun *lisp.LVal) string {
	if fun.Type != lisp.LFun {
		return ""
	}
	funData := fun.FunData()
	if funData == nil {
		return ""
	}
	name := ""
	if env := fun.Env(); env != nil {
		name = env.GetFunName(fun)
	}
	if name == "" {
		name = getFunNameFromFID(runtime, funData.FID)
	}
	return name
}

// prettyFunName returns a pretty name and original name for a fun. If there is
// no pretty name, then the pretty name is the original name. The pretty name
// includes the package prefix, while the original name does not.
func (p *profiler) prettyFunName(fun *lisp.LVal) (string, string) {
	origLabel := defaultFunName(p.runtime, fun)
	if origLabel == "" {
		return "", ""
	}
	prettyLabel := origLabel
	if p.funLabeler != nil {
		prettyLabel = p.funLabeler(p.runtime, fun)
	}
	if prettyLabel == "" {
		prettyLabel = origLabel
	}

	return prettyLabel, origLabel
}

// skipTrace is a helper function to decide whether to skip tracing.
func (p *profiler) skipTrace(v *lisp.LVal) bool {
	return !p.enabled || defaultSkipFilter(v) || p.skipFilter != nil && p.skipFilter(v)
}

var builtinRegex = regexp.MustCompile("\\<(?:builtin|special)-[a-z]+ \\`\\`(.*)\\'\\'\\>")

// Gets a canonical version of the function name suitable for human viewing.
func getFunNameFromFID(rt *lisp.Runtime, in string) string {
	// but sometimes something doesn't match - so we'll try to regexp it out
	if !builtinRegex.MatchString(in) {
		return in
	}
	return builtinRegex.FindStringSubmatch(in)[1]
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
