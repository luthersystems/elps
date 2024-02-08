package profiler

import (
	"context"
	"errors"

	"github.com/luthersystems/elps/lisp"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
	"go.opentelemetry.io/otel/trace"
)

const (
	// ContextOpenTelemetryTracerKey looks up a parent tracer name from a context key.
	ContextOpenTelemetryTracerKey = "otelParentTracer"
)

var _ lisp.Profiler = &otelAnnotator{}

type otelAnnotator struct {
	profiler
	currentContext context.Context
	currentSpan    trace.Span
}

func NewOpenTelemetryAnnotator(runtime *lisp.Runtime, parentContext context.Context, opts ...Option) *otelAnnotator {
	p := &otelAnnotator{
		profiler: profiler{
			runtime: runtime,
		},
		currentContext: parentContext,
	}
	p.profiler.applyConfigs(opts...)
	return p
}

func (p *otelAnnotator) Enable() error {
	p.runtime.Profiler = p
	if p.currentContext == nil {
		return errors.New("we can only append spans to a context that is linked to opentelemetry")
	}
	return p.profiler.Enable()
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

func (p *otelAnnotator) Start(fun *lisp.LVal) func() {
	if p.skipTrace(fun) {
		return func() {}
	}
	oldContext := p.currentContext
	prettyLabel, _ := p.prettyFunName(fun)
	p.currentContext, p.currentSpan = contextTracer(p.currentContext).Start(p.currentContext, prettyLabel)
	return func() {
		file, line := getSource(fun)
		p.currentSpan.AddEvent("source", trace.WithAttributes(attribute.Key("file").String(file), attribute.Key("line").Int64(int64(line))))
		p.currentSpan.End()
		// And pop the current context back
		p.currentContext = oldContext
		p.currentSpan = trace.SpanFromContext(p.currentContext)
	}
}
