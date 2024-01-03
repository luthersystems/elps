package profiler

import (
	"context"
	"errors"

	"github.com/luthersystems/elps/lisp"
	"go.opencensus.io/trace"
)

type ocAnnotator struct {
	profiler
	currentContext context.Context
	currentSpan    *trace.Span
}

var _ lisp.Profiler = &ocAnnotator{}

func NewOpenCensusAnnotator(runtime *lisp.Runtime, parentContext context.Context, opts ...Option) *ocAnnotator {
	p := &ocAnnotator{
		profiler: profiler{
			runtime: runtime,
		},
		currentContext: parentContext,
	}
	p.profiler.applyConfigs(opts...)
	return p
}

func (p *ocAnnotator) Enable() error {
	p.runtime.Profiler = p
	if p.currentContext == nil {
		return errors.New("we can only append spans to a context that is linked to opencensus")
	}
	return p.profiler.Enable()
}

func (p *ocAnnotator) Complete() error {
	if p.currentSpan != nil {
		p.currentSpan.End()
	}
	return nil
}

func (p *ocAnnotator) Start(fun *lisp.LVal) func() {
	if p.skipTrace(fun) {
		return func() {}
	}
	oldContext := p.currentContext
	p.currentContext, p.currentSpan = trace.StartSpan(p.currentContext, prettyFunName(p.runtime, fun))
	return func() {
		file, line := getSource(fun)
		p.currentSpan.Annotate([]trace.Attribute{
			trace.StringAttribute("file", file),
			trace.Int64Attribute("line", int64(line)),
		}, "source")
		p.currentSpan.End()
		// And pop the current context back
		p.currentContext = oldContext
		p.currentSpan = trace.FromContext(p.currentContext)
	}
}
