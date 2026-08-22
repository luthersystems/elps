package profiler

import (
	"errors"
	"regexp"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"go.opentelemetry.io/otel/trace"
)

// profiler is a minimal lisp.Profiler
type profiler struct {
	runtime    *lisp.Runtime
	enabled    bool
	skipFilter SkipFilter
	funLabeler FunLabeler
	// otelTracer is consumed only by the OpenTelemetry annotator. It lives
	// here because Option is func(*profiler), so it is the only place a
	// shared option can write to. Set via WithTracer or WithTracerProvider;
	// nil means fall back to the global TracerProvider.
	otelTracer trace.Tracer
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
		return errors.New("profiler already enabled")
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
	name := ""
	if env := funraw.Env(fun); env != nil {
		name = env.GetFunName(fun)
	}
	if name == "" {
		name = getFunNameFromFID(runtime, fun.FID())
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

// getSourceLoc returns a copy of fun's best source location.  lisp.LVal
// exposes locations by value only (issue #362), so the returned pointer is a
// private copy.  A function with no recorded location (a builtin) reports
// the synthetic "<native code>" location, matching what such functions
// historically carried, so callgrind emitters keep their fl= attribution.
func getSourceLoc(fun *lisp.LVal) *token.Location {
	if len(fun.Cells) > 0 {
		if loc, ok := fun.Cells[0].Source(); ok {
			return &loc
		}
	}
	loc, _ := fun.Source()
	return &loc
}
