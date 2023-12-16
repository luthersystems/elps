package profiler

import (
	"context"
	"errors"
	"fmt"

	"github.com/luthersystems/elps/lisp"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
	"go.opentelemetry.io/otel/trace"
)

const (
	// ContextOpenTelemetryTracerKey looks up a parent tracer name from a context key.
	ContextOpenTelemetryTracerKey = "otelParentTracer"
)

type otelAnnotator struct {
	runtime        *lisp.Runtime
	enabled        bool
	currentContext context.Context
	currentSpan    trace.Span
}

func NewOpenTelemetryAnnotator(runtime *lisp.Runtime, parentContext context.Context) lisp.Profiler {
	return &otelAnnotator{
		runtime:        runtime,
		currentContext: parentContext,
	}
}

func (p *otelAnnotator) IsEnabled() bool {
	return p.enabled
}

func (p *otelAnnotator) EnableWithContext(ctx context.Context) error {
	p.runtime.Profiler = p
	p.enabled = true
	if ctx == nil {
		return errors.New("set a context to use this function")
	}
	p.currentContext = ctx
	return nil
}

func (p *otelAnnotator) Enable() error {
	p.runtime.Profiler = p
	p.enabled = true
	if p.currentContext == nil {
		return errors.New("we can only append spans to a context that is linked to opentelemetry")
	}
	return nil
}

func (p *otelAnnotator) SetFile(filename string) error {
	return errors.New("no need to set a file for this profiler type")
}

func (p *otelAnnotator) Complete() error {
	if p.currentSpan != nil {
		p.currentSpan.End()
	}
	return nil
}

func contextTracer(ctx context.Context) trace.Tracer {
	tracerName, ok := ctx.Value(ContextOpenTelemetryTracerKey).(string)
	if !ok {
		tracerName = "elps"
	}
	return otel.GetTracerProvider().Tracer(tracerName)
}

func (p *otelAnnotator) Start(function *lisp.LVal) func() {
	if !p.enabled {
		return func() {}
	}
	oldContext := p.currentContext
	switch function.Type {
	case lisp.LInt, lisp.LString, lisp.LFloat, lisp.LBytes, lisp.LError, lisp.LArray, lisp.LQuote, lisp.LNative, lisp.LQSymbol, lisp.LSortMap:
		// We don't need to profile these types. We could, but we're not that LISP :D
		return func() {}
	case lisp.LFun, lisp.LSymbol, lisp.LSExpr:
		fName := fmt.Sprintf("%s:%s", function.FunData().Package, getFunNameFromFID(p.runtime, function.FunData().FID))
		p.currentContext, p.currentSpan = contextTracer(p.currentContext).Start(p.currentContext, fName)
	default:
		panic(fmt.Sprintf("missing type %d", function.Type))
	}
	return func() {
		file, line := p.getSource(function)
		p.currentSpan.AddEvent("source", trace.WithAttributes(attribute.Key("file").String(file), attribute.Key("line").Int64(int64(line))))
		p.currentSpan.End()
		p.currentContext = oldContext
		p.currentSpan = trace.SpanFromContext(p.currentContext)
	}
}

func (p *otelAnnotator) getSource(function *lisp.LVal) (source string, line int) {
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
