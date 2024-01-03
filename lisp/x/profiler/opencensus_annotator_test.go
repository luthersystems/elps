package profiler_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"go.opencensus.io/trace"
)

type inMemoryExporter struct {
	spans []*trace.SpanData
}

func (e *inMemoryExporter) ExportSpan(sd *trace.SpanData) {
	e.spans = append(e.spans, sd)
}

func TestNewOpenCensusAnnotator(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	trace.ApplyConfig(trace.Config{DefaultSampler: trace.AlwaysSample()})
	exporter := &inMemoryExporter{}
	trace.RegisterExporter(exporter)
	t.Cleanup(func() {
		trace.UnregisterExporter(exporter)
	})

	ppa := profiler.NewOpenCensusAnnotator(env.Runtime, context.Background())
	assert.NoError(t, ppa.Enable())
	lerr := lisp.InitializeUserEnv(env)
	assert.NoError(t, lisp.GoError(lerr))
	testsrc := env.LoadString("test.lisp", testLisp)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type)
	assert.NoError(t, ppa.Complete())

	assert.GreaterOrEqual(t, len(exporter.spans), 1, "Expected at least one span")
}
