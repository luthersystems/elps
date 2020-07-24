package profiler

import (
	"context"
	"errors"
	"fmt"
	"github.com/golang-collections/collections/stack"
	"github.com/luthersystems/elps/lisp"
	"runtime/pprof"
	"time"
)

// This profiler type appends tags to pprof output if pprof is enabled.
// I've taken the decision to not start pprof for the user as I suspect that would be more
// annoying than useful in the majority of contexts where this could be used.
// Unfortunately, due to the decision of the writers of pprof to fix the sampling rate at 100Hz
// this is now less useful than might be expected.
type pprofAnnotator struct {
	runtime        *lisp.Runtime
	enabled        bool
	startTime      time.Time
	currentContext context.Context
	contexts       *stack.Stack
}

func NewPprofAnnotator(runtime *lisp.Runtime, parentContext context.Context) lisp.Profiler {
	return &pprofAnnotator{
		runtime:        runtime,
		currentContext: parentContext,
		contexts:       stack.New(),
	}
}

func (p *pprofAnnotator) IsEnabled() bool {
	return p.enabled
}

func (p *pprofAnnotator) Enable() error {
	p.runtime.Profiler = p
	p.enabled = true
	if p.currentContext == nil {
		p.currentContext = context.Background()
	}
	return nil
}

func (p *pprofAnnotator) SetFile(filename string) error {
	return errors.New("No need to set a file for this profiler type")
}

func (p *pprofAnnotator) Complete() error {
	pprof.SetGoroutineLabels(context.Background())
	return nil
}

func (p *pprofAnnotator) Start(function *lisp.LVal) {
	if !p.enabled {
		return
	}
	switch function.Type {
	case lisp.LInt, lisp.LString, lisp.LFloat, lisp.LBytes, lisp.LError, lisp.LArray, lisp.LQuote, lisp.LNative, lisp.LQSymbol, lisp.LSortMap:
		// We don't need to profile these types. We could, but we're not that LISP :D
		return
	case lisp.LFun, lisp.LSymbol, lisp.LSExpr:
		// We're keeping the context on a stack here rather than using the pprof.Do function for the simple
		// reason that I would have had to make more changes to the lisp.EvalSExpr function to accommodate that,
		// and doing so would have a negative effect on users not profiling - it would either be an extra stack entry
		// if we always ran inside a context, or a whole conditional code path and the added complication that brings
		// if we did it that way.
		fName := fmt.Sprintf("%s:%s", function.FunData().Package, GetFunNameFromFID(p.runtime, function.FunData().FID))
		labels := pprof.Labels("function", fName)
		p.contexts.Push(p.currentContext)
		p.currentContext = pprof.WithLabels(p.currentContext, labels)
		// apply the selected labels to the current goroutine (NB this will propagate if the code branches further down...
		pprof.SetGoroutineLabels(p.currentContext)
	default:
		panic(fmt.Sprintf("Missing type %d", function.Type))
	}

}

func (p *pprofAnnotator) End(function *lisp.LVal) {
	if !p.enabled {
		return
	}
	switch function.Type {
	case lisp.LInt, lisp.LString, lisp.LFloat, lisp.LBytes, lisp.LError, lisp.LArray, lisp.LQuote, lisp.LNative, lisp.LQSymbol, lisp.LSortMap:
		// We don't need to profile these types. We could, but we're not that LISP :D
		return
	case lisp.LFun, lisp.LSymbol, lisp.LSExpr:
		// And pop the current context back
		p.currentContext = p.contexts.Pop().(context.Context)
	default:
		panic(fmt.Sprintf("Missing type %d", function.Type))

	}

}
