package profiler_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/sdk/trace"
	"go.opentelemetry.io/otel/sdk/trace/tracetest"
	oteltrace "go.opentelemetry.io/otel/trace"
	"go.opentelemetry.io/otel/trace/noop"
)

// newExportingProvider returns a provider that samples everything and syncs
// spans into the returned exporter.
func newExportingProvider(t *testing.T) (*trace.TracerProvider, *tracetest.InMemoryExporter) {
	t.Helper()
	exporter := tracetest.NewInMemoryExporter()
	tp := trace.NewTracerProvider(
		trace.WithSyncer(exporter),
		trace.WithSampler(trace.AlwaysSample()),
	)
	t.Cleanup(func() {
		assert.NoError(t, tp.Shutdown(context.Background()), "TracerProvider shutdown")
	})
	return tp, exporter
}

// setGlobalProvider installs tp as the process-global provider and restores
// the previous one afterwards.
//
// These tests are deliberately not parallel: otel.SetTracerProvider is
// process-global, which is the very hazard the options under test exist to
// let embedders avoid.
func setGlobalProvider(t *testing.T, tp oteltrace.TracerProvider) {
	t.Helper()
	prev := otel.GetTracerProvider()
	otel.SetTracerProvider(tp)
	t.Cleanup(func() { otel.SetTracerProvider(prev) })
}

// runTraced evaluates the shared test lisp source under an annotator built
// with opts, and returns once the profile is complete.
func runTraced(t *testing.T, opts ...profiler.Option) {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	ppa := profiler.NewOpenTelemetryAnnotator(env.Runtime, context.Background(), opts...)
	require.NoError(t, ppa.Enable())
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	lerr := env.Eval(env.LoadString("test.lisp", testLisp))
	require.NotEqual(t, lisp.LError, lerr.Type, lerr.Str)
	require.NoError(t, ppa.Complete())
}

// TestWithTracerProviderBypassesGlobal asserts that an annotator configured
// with an explicit provider emits into that provider and not into the global
// one, even when the global is installed and exporting.
func TestWithTracerProviderBypassesGlobal(t *testing.T) {
	globalTP, globalExporter := newExportingProvider(t)
	setGlobalProvider(t, globalTP)
	injectedTP, injectedExporter := newExportingProvider(t)

	runTraced(t, profiler.WithELPSDocFilter(), profiler.WithTracerProvider(injectedTP))

	assert.NotEmpty(t, injectedExporter.GetSpans(),
		"spans must be emitted into the injected provider")
	assert.Empty(t, globalExporter.GetSpans(),
		"an injected provider must not leak spans into the global one")
}

// TestWithTracerBypassesGlobal covers the same guarantee for a directly
// supplied tracer.
func TestWithTracerBypassesGlobal(t *testing.T) {
	globalTP, globalExporter := newExportingProvider(t)
	setGlobalProvider(t, globalTP)
	injectedTP, injectedExporter := newExportingProvider(t)

	runTraced(t, profiler.WithELPSDocFilter(),
		profiler.WithTracer(injectedTP.Tracer("embedder")))

	assert.NotEmpty(t, injectedExporter.GetSpans(),
		"spans must be emitted into the injected tracer")
	assert.Empty(t, globalExporter.GetSpans(),
		"an injected tracer must not leak spans into the global provider")
}

// TestAnnotatorsAreIsolated is the multi-runtime case this option exists for.
//
// Two runtimes in one process, each with its own provider, must keep their
// spans separate. Without injection both resolve the global provider, so the
// one installed most recently captures both runtimes' spans -- and those
// spans land under the wrong trace root, not merely in the wrong exporter.
func TestAnnotatorsAreIsolated(t *testing.T) {
	// A third provider is installed globally and must stay empty: it
	// stands in for a provider some other component set up.
	globalTP, globalExporter := newExportingProvider(t)
	setGlobalProvider(t, globalTP)

	firstTP, firstExporter := newExportingProvider(t)
	secondTP, secondExporter := newExportingProvider(t)

	// Construct and run interleaved, so that a construction-time read of
	// the global provider cannot accidentally produce the right answer.
	runTraced(t, profiler.WithELPSDocFilter(), profiler.WithTracerProvider(firstTP))
	runTraced(t, profiler.WithELPSDocFilter(), profiler.WithTracerProvider(secondTP))

	first, second := firstExporter.GetSpans(), secondExporter.GetSpans()
	require.NotEmpty(t, first, "first runtime emitted no spans")
	require.NotEmpty(t, second, "second runtime emitted no spans")
	assert.Len(t, second, len(first),
		"identical sources must produce the same number of spans in each provider")
	assert.Empty(t, globalExporter.GetSpans(),
		"neither runtime may emit into the global provider")

	// Every span in each provider must belong to a single trace, and the
	// two providers must not share it: a shared trace ID is the signature
	// of one runtime's spans being rooted in the other's trace.
	firstTraces, secondTraces := traceIDs(first), traceIDs(second)
	for id := range firstTraces {
		assert.NotContains(t, secondTraces, id,
			"the two runtimes must not share a trace")
	}
}

// traceIDs returns the set of trace IDs present in spans.
func traceIDs(spans tracetest.SpanStubs) map[oteltrace.TraceID]struct{} {
	out := make(map[oteltrace.TraceID]struct{}, len(spans))
	for _, s := range spans {
		out[s.SpanContext.TraceID()] = struct{}{}
	}
	return out
}

// TestDefaultsToGlobalTracerProvider pins the pre-existing behaviour for
// embedders that pass neither option: the global provider is still used, and
// is read at construction.
func TestDefaultsToGlobalTracerProvider(t *testing.T) {
	globalTP, globalExporter := newExportingProvider(t)
	setGlobalProvider(t, globalTP)

	runTraced(t, profiler.WithELPSDocFilter())

	assert.NotEmpty(t, globalExporter.GetSpans(),
		"without an injected tracer the global provider must still be used")
}

// TestNilTracerOptionsFallBackToGlobal asserts the options ignore nil rather
// than installing a nil tracer, which would panic on the first span.
func TestNilTracerOptionsFallBackToGlobal(t *testing.T) {
	globalTP, globalExporter := newExportingProvider(t)
	setGlobalProvider(t, globalTP)

	require.NotPanics(t, func() {
		runTraced(t, profiler.WithELPSDocFilter(),
			profiler.WithTracer(nil), profiler.WithTracerProvider(nil))
	})
	assert.NotEmpty(t, globalExporter.GetSpans(),
		"nil options must leave the global fallback in place")
}

// TestLastTracerOptionWins pins the ordering semantics shared by every option
// in this package: later options overwrite earlier ones.
func TestLastTracerOptionWins(t *testing.T) {
	firstTP, firstExporter := newExportingProvider(t)
	secondTP, secondExporter := newExportingProvider(t)
	setGlobalProvider(t, noop.NewTracerProvider())

	runTraced(t, profiler.WithELPSDocFilter(),
		profiler.WithTracerProvider(firstTP), profiler.WithTracerProvider(secondTP))

	assert.Empty(t, firstExporter.GetSpans(), "the overridden provider must be unused")
	assert.NotEmpty(t, secondExporter.GetSpans(), "the last option must win")
}
