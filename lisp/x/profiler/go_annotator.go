package profiler

import (
	"context"
	"runtime/pprof"

	"github.com/luthersystems/elps/lisp"
)

// This profiler type appends tags to pprof output if pprof is enabled.
// I've taken the decision to not start pprof for the user as I suspect that would be more
// annoying than useful in the majority of contexts where this could be used.
// Unfortunately, due to the decision of the writers of pprof to fix the sampling rate at 100Hz
// this is now less useful than might be expected.
type pprofAnnotator struct {
	profiler
	currentContext context.Context
}

var _ lisp.Profiler = &pprofAnnotator{}

func NewPprofAnnotator(runtime *lisp.Runtime, parentContext context.Context, opts ...Option) *pprofAnnotator {
	p := &pprofAnnotator{
		profiler: profiler{
			runtime: runtime,
		},
		currentContext: parentContext,
	}
	p.profiler.applyConfigs(opts...)
	return p
}

func (p *pprofAnnotator) Enable() error {
	p.runtime.Profiler = p
	if p.currentContext == nil {
		p.currentContext = context.Background()
	}
	return p.profiler.Enable()
}

func (p *pprofAnnotator) Complete() error {
	pprof.SetGoroutineLabels(context.Background())
	return nil
}

func (p *pprofAnnotator) Start(fun *lisp.LVal) func() {
	if p.skipTrace(fun) {
		return func() {}
	}
	// We're keeping the context on a stack here rather than using the pprof.Do function for the simple
	// reason that I would have had to make more changes to the lisp.EvalSExpr function to accommodate that,
	// and doing so would have a negative effect on users not profiling - it would either be an extra stack entry
	// if we always ran inside a context, or a whole conditional code path and the added complication that brings
	// if we did it that way.
	oldContext := p.currentContext
	prettyLabel, _ := p.prettyFunName(fun)
	p.currentContext = pprof.WithLabels(p.currentContext, pprof.Labels("function", prettyLabel))
	// apply the selected labels to the current goroutine (NB this will propagate if the code branches further down...
	pprof.SetGoroutineLabels(p.currentContext)

	return func() {
		p.currentContext = oldContext
	}
}
