package profiler

import (
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/trace"
)

// tracerName is the instrumentation name reported for spans created by the
// OpenTelemetry annotator.
const tracerName = "elps"

// WithTracer sets the tracer used to create spans, instead of resolving one
// from the global TracerProvider.
//
// Embedders that run more than one runtime in a single process should set
// this or WithTracerProvider. otel.SetTracerProvider installs a *process
// global*, so annotators left to resolve it all emit into whichever provider
// was installed most recently -- spans from one runtime surface under another
// runtime's provider, attached to the wrong trace.
//
// A nil tracer is ignored, leaving the global fallback in place.
func WithTracer(tracer trace.Tracer) Option {
	return func(p *profiler) {
		if tracer != nil {
			p.otelTracer = tracer
		}
	}
}

// WithTracerProvider sets the TracerProvider used to create spans, instead of
// the global one. See WithTracer for why an embedder would want this.
//
// A nil provider is ignored, leaving the global fallback in place.
func WithTracerProvider(tp trace.TracerProvider) Option {
	return func(p *profiler) {
		if tp != nil {
			p.otelTracer = tp.Tracer(tracerName)
		}
	}
}

// resolveOtelTracer returns the configured tracer, falling back to the global
// TracerProvider for embedders that set neither option.
func (p *profiler) resolveOtelTracer() trace.Tracer {
	if p.otelTracer != nil {
		return p.otelTracer
	}
	return otel.GetTracerProvider().Tracer(tracerName)
}
