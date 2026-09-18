// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"
	"testing"
)

// equal? must be bounded by the limit the runtime was configured with, not by
// the million-deep default: Equal passes a nil runtime, so only
// EqualWithRuntime reads WithMaxValueDepth, and the builtin is the caller that
// has a runtime to read it from.
func TestEqualBuiltinHonoursRuntimeDepthLimit(t *testing.T) {
	env := initSafetyTestEnv(t)
	if rc := WithMaxValueDepth(runtimeDepthLimit)(env); rc.Type == LError {
		t.Fatalf("configuring the limit failed: %s", rc)
	}
	a, b := depthTestValue(overRuntimeLimit), depthTestValue(overRuntimeLimit)

	got := env.Eval(SExpr([]*LVal{Symbol("equal?"), Quote(a), Quote(b)}))
	if got.Type != LError || !strings.Contains(got.String(), "maximum: 2048") {
		t.Fatalf("equal? walked past the configured limit: %s", got)
	}

	// Under the limit the comparison still answers, so the error above is the
	// budget and not a refusal to compare.
	shallow := depthTestValue(underRuntimeLimit)
	got = env.Eval(SExpr([]*LVal{Symbol("equal?"), Quote(shallow), Quote(depthTestValue(underRuntimeLimit))}))
	if got.Type == LError || !True(got) {
		t.Fatalf("equal? rejected a value under the configured limit: %s", got)
	}

	// The exported runtime-less form keeps the default, which is what callers
	// with no runtime in reach depend on.
	if got := a.Equal(b); got.Type == LError || !True(got) {
		t.Fatalf("Equal must keep the MaxValueDepth default: %s", got)
	}
}
