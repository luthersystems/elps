// Copyright © 2026 The ELPS authors

package main

import (
	"path/filepath"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestCallListAnalyzer(t *testing.T) {
	analysistest.Run(t, filepath.Join(analysistest.TestData(), "calllist"), callListAnalyzer, lispPkgPath)
}
