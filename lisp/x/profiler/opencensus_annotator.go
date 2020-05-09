package profiler

import (
	"context"
	"errors"
	"fmt"
	"github.com/golang-collections/collections/stack"
	"github.com/luthersystems/elps/lisp"
	"go.opencensus.io/trace"
	"time"
)

type ocAnnotator struct {
	runtime        *lisp.Runtime
	enabled        bool
	startTime      time.Time
	currentContext context.Context
	currentSpan    *trace.Span
	contexts       *stack.Stack
}

func NewOpenCensusAnnotator(runtime *lisp.Runtime, parentContext context.Context) lisp.Profiler {
	return &ocAnnotator{
		runtime:        runtime,
		currentContext: parentContext,
		contexts:       stack.New(),
	}
}

func (p *ocAnnotator) IsEnabled() bool {
	return p.enabled
}

func (p *ocAnnotator) EnableWithContext(ctx context.Context) error {
	p.runtime.Profiler = p
	p.enabled = true
	if ctx == nil {
		return errors.New("Set a context to use this function")
	}
	p.currentContext = ctx
	return nil
}

func (p *ocAnnotator) Enable() error {
	p.runtime.Profiler = p
	p.enabled = true
	if p.currentContext == nil {
		return errors.New("We can only append spans to a context that is linked to opencensus")
	}
	return nil
}

func (p *ocAnnotator) SetFile(filename string) error {
	return errors.New("No need to set a file for this profiler type")
}

func (p *ocAnnotator) Complete() error {
	if p.currentSpan != nil {
		p.currentSpan.End()
	}
	return nil
}

func (p *ocAnnotator) Start(function *lisp.LVal) {
	if !p.enabled {
		return
	}
	switch function.Type {
	case lisp.LInt, lisp.LString, lisp.LFloat, lisp.LBytes, lisp.LError, lisp.LArray, lisp.LQuote, lisp.LNative, lisp.LQSymbol, lisp.LSortMap:
		// We don't need to profile these types. We could, but we're not that LISP :D
		return
	case lisp.LFun, lisp.LSymbol, lisp.LSExpr:
		fName := fmt.Sprintf("%s:%s", function.FunData().Package, getFunNameFromFID(p.runtime, function.FunData().FID))
		p.contexts.Push(p.currentContext)
		p.currentContext, p.currentSpan = trace.StartSpan(p.currentContext, fName)
	default:
		panic(fmt.Sprintf("Missing type %d", function.Type))
	}

}

func (p *ocAnnotator) End(function *lisp.LVal) {
	if !p.enabled {
		return
	}
	switch function.Type {
	case lisp.LInt, lisp.LString, lisp.LFloat, lisp.LBytes, lisp.LError, lisp.LArray, lisp.LQuote, lisp.LNative, lisp.LQSymbol, lisp.LSortMap:
		// We don't need to profile these types. We could, but we're not that LISP :D
		return
	case lisp.LFun, lisp.LSymbol, lisp.LSExpr:
		file, line := p.getSource(function)
		p.currentSpan.Annotate([]trace.Attribute{
			trace.StringAttribute("file", file),
			trace.Int64Attribute("line", int64(line)),
		}, "source")
		p.currentSpan.End()
		// And pop the current context back
		p.currentContext = p.contexts.Pop().(context.Context)
		p.currentSpan = trace.FromContext(p.currentContext)
	default:
		panic(fmt.Sprintf("Missing type %d", function.Type))

	}
}

func (p *ocAnnotator) getSource(function *lisp.LVal) (source string, line int) {
	if function.Source == nil {
		if cell := function.Cells[0]; cell != nil && cell.Source != nil {
			source = cell.Source.File
			line = cell.Source.Line
		} else {
			source = "no-source"
		}
		return
	}
	source = function.Source.File
	line = function.Source.Line
	return
}
