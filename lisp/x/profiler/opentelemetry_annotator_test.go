package profiler_test

import (
	"context"
	_ "embed"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
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
	assert.NoError(t, ppa.Enable())
	lerr := lisp.InitializeUserEnv(env)
	assert.NoError(t, lisp.GoError(lerr))
	testsrc := env.LoadString("test.lisp", testLisp)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type, lerr.Str)
	assert.NoError(t, ppa.Complete())

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
	assert.NoError(t, ppa.Enable())
	lerr := lisp.InitializeUserEnv(env)
	assert.NoError(t, lisp.GoError(lerr))
	testsrc := env.LoadString("test.lisp", testLisp)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type, lerr.Str)
	assert.NoError(t, ppa.Complete())

	spans := exporter.GetSpans()
	assert.Equal(t, 7, len(spans), "Expected selective spans")
	assert.Equal(t, "Add_It", spans[0].Name, "Expected custom label")
	assert.Equal(t, "Add_It_Again", spans[3].Name, "Expected custom label")
	assert.Equal(t, "lambda", spans[4].Name, "Expected custom label")
}
