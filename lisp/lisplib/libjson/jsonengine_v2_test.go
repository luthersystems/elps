// Copyright © 2026 The ELPS authors

//go:build goexperiment.jsonv2

package libjson

import "testing"

// TestJSONEngine fails a test run built on Go 1.27's JSON v2 engine and says
// why, so the libjson failures that follow on that engine are not read as
// regressions.  libjson's native encoding still depends on the engine, and
// elps is built and tested on the v1 engine (GOEXPERIMENT=nojsonv2), as
// substrate's chaincode is (elps#928).  See "JSON ENGINE" at the top of the Makefile.
func TestJSONEngine(t *testing.T) {
	t.Fatal("built on the JSON v2 engine: run the tests with GOEXPERIMENT=nojsonv2 (make test sets it)")
}
