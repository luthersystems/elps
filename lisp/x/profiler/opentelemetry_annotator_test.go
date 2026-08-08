package profiler_test

import (
	"context"
	_ "embed"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/sdk/trace"
	"go.opentelemetry.io/otel/sdk/trace/tracetest"
)

func TestNewOpenTelemetryAnnotator(t *testing.T) {
	exporter := tracetest.NewInMemoryExporter()

	tp := trace.NewTracerProvider(
		trace.WithSyncer(exporter),
		trace.WithSampler(trace.AlwaysSample()),
	)
	t.Cleanup(func() {
		err := tp.Shutdown(context.Background())
		assert.NoError(t, err, "TracerProvider shutdown")
	})
	otel.SetTracerProvider(tp)

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	ppa := profiler.NewOpenTelemetryAnnotator(env.Runtime, context.Background())
	require.NoError(t, ppa.Enable())
	lerr := lisp.InitializeUserEnv(env)
	require.NoError(t, lisp.GoError(lerr))
	testsrc := env.LoadString("test.lisp", testLisp)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type, lerr.Str)
	require.NoError(t, ppa.Complete())

	spans := exporter.GetSpans()
	assert.GreaterOrEqual(t, len(spans), 3, "Expected at least three spans")
}

func TestNewOpenTelemetryAnnotatorSkip(t *testing.T) {
	exporter := tracetest.NewInMemoryExporter()

	tp := trace.NewTracerProvider(
		trace.WithSyncer(exporter),
		trace.WithSampler(trace.AlwaysSample()),
	)
	t.Cleanup(func() {
		err := tp.Shutdown(context.Background())
		assert.NoError(t, err, "TracerProvider shutdown")
	})
	otel.SetTracerProvider(tp)

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	ppa := profiler.NewOpenTelemetryAnnotator(env.Runtime, context.Background(),
		profiler.WithELPSDocFilter(),
		profiler.WithELPSDocLabeler())
	require.NoError(t, ppa.Enable())
	lerr := lisp.InitializeUserEnv(env)
	require.NoError(t, lisp.GoError(lerr))
	testsrc := env.LoadString("test.lisp", testLisp)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type, lerr.Str)
	require.NoError(t, ppa.Complete())

	spans := exporter.GetSpans()
	assert.Len(t, spans, 7, "Expected selective spans")
	assert.Equal(t, "Add-It", spans[0].Name, "Expected custom label")
	assert.Equal(t, "Add-It-Again", spans[3].Name, "Expected custom label")
	assert.Equal(t, "lambda", spans[4].Name, "Expected custom label")
}
