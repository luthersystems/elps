package profiler_test

import (
	"context"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"go.opencensus.io/trace"
	"log"
	"testing"
)

func TestNewOpenCensusAnnotator(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	// Let's sample at 100% for the purposes of this test...
	trace.ApplyConfig(trace.Config{DefaultSampler: trace.AlwaysSample()})
	trace.RegisterExporter(new(customExporter))
	ppa := profiler.NewOpenCensusAnnotator(env.Runtime, context.Background())
	assert.NoError(t, ppa.Enable())
	lerr := lisp.InitializeUserEnv(env)
	if lisp.GoError(lerr) != nil {
		t.Fatal(lisp.GoError(lerr))
	}
	var testsrc *lisp.LVal
	testsrc = env.LoadString("test.lisp", `
(defun printIt 
	('x)
	(debug-print x)
)
(defun addIt
	('x 'y)
	(+ x y)
)
(defun recurseIt
	('x)
	(if 
		(< x 4)
		(recurseIt (- x 1))
		(addIt x 3)
	)
)
(printIt "Hello")
(printIt (addIt (addIt 3 (recurseIt 5)) 8))`)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type)
	// Mark the profile as complete and dump the rest of the profile
	ppa.Complete()
}

// a simple exporter that prints to the screen - in the real world, you'd go to one of the
// myriad exporters supported by opencensus https://opencensus.io/exporters/supported-exporters/go/
type customExporter struct{}

func (cse *customExporter) ExportSpan(sd *trace.SpanData) {
	log.Printf("Name: %s\n\tTraceID: %x\n\tSpanID: %x\n\tParentSpanID: %x\n\tStartTime: %s\n\tEndTime: %s\n\tAnnotations: %+v\n",
		sd.Name, sd.TraceID, sd.SpanID, sd.ParentSpanID, sd.StartTime, sd.EndTime, sd.Annotations)
}
