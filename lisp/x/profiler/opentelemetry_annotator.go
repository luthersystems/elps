package profiler

import (
	"context"
	"errors"

	"github.com/luthersystems/elps/lisp"
	"go.opentelemetry.io/otel/attribute"
	semconv "go.opentelemetry.io/otel/semconv/v1.21.0"
	"go.opentelemetry.io/otel/trace"
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
	// The tracer is resolved after the options are applied: reading the
	// global TracerProvider before them would capture whichever provider
	// happened to be installed at construction, which is exactly what
	// WithTracer and WithTracerProvider exist to avoid.
	p.applyConfigs(opts...)
	p.otelTracer = p.resolveOtelTracer()
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

func (p *otelAnnotator) Start(fun *lisp.LVal) func() {
	if p.skipTrace(fun) {
		return func() {}
	}
	oldContext := p.currentContext
	prettyLabel, funName := p.prettyFunName(fun)
	p.currentContext, p.currentSpan = p.otelTracer.Start(p.currentContext, prettyLabel)
	p.addCodeAttributes(fun, funName)
	return func() {
		p.currentSpan.End()
		// And pop the current context back
		p.currentContext = oldContext
		p.currentSpan = trace.SpanFromContext(p.currentContext)
	}
}

func (p *otelAnnotator) addCodeAttributes(fun *lisp.LVal, funName string) {
	loc := getSourceLoc(fun)
	attrs := []attribute.KeyValue{
		semconv.CodeNamespace(fun.Package()),
		semconv.CodeFunction(funName),
	}
	if loc != nil {
		attrs = append(attrs,
			semconv.CodeColumn(loc.Col),
			semconv.CodeFilepath(loc.File),
			semconv.CodeLineNumber(loc.Line),
		)
	}
	p.currentSpan.SetAttributes(attrs...)
}
