package profiler_test

import (
	"context"
	"log"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/exporters/stdout/stdouttrace"
	sdktrace "go.opentelemetry.io/otel/sdk/trace"
)

func TestNewOpenTelemetryAnnotator(t *testing.T) {
	// Configure a stdout exporter to log the tracing data.
	exporter, err := stdouttrace.New(stdouttrace.WithPrettyPrint())
	if err != nil {
		log.Fatalf("failed to initialize stdout export pipeline: %v", err)
	}

	tp := sdktrace.NewTracerProvider(
		// Use the custom exporter
		sdktrace.WithBatcher(exporter),
		// Always sample
		sdktrace.WithSampler(sdktrace.AlwaysSample()),
	)
	// Register the tracer provider using global API.
	otel.SetTracerProvider(tp)

	// Ensure we're using the OpenTelemetry Annotator.
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	ppa := profiler.NewOpenTelemetryAnnotator(env.Runtime, context.Background())
	assert.NoError(t, ppa.Enable())
	lerr := lisp.InitializeUserEnv(env)
	if lisp.GoError(lerr) != nil {
		t.Fatal(lisp.GoError(lerr))
	}
	testsrc := env.LoadString("test.lisp", `
(defun print-it
	('x)
	(debug-print x)
)
(defun add-it
	('x 'y)
	(+ x y)
)
(defun recurse-it
	('x)
	(if
		(< x 4)
		(recurse-it (- x 1))
		(add-it x 3)
	)
)
(print-it "Hello")
(print-it (add-it (add-it 3 (recurse-it 5)) 8))`)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type)
	// Mark the profile as complete and dump the rest of the profile
	assert.NoError(t, ppa.Complete())

	err = tp.Shutdown(context.Background())
	if err != nil {
		t.Errorf("Error during TracerProvider shutdown: %v", err)
	}
}
