package libelpslang_test

import (
	_ "embed"
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestPackage(t *testing.T) {
	r := &elpstest.Runner{}
	r.RunTestFile(t, "libelps_test.lisp")
}

func BenchmarkPackage(b *testing.B) {
	r := &elpstest.Runner{}
	r.RunBenchmarkFile(b, "libelps_test.lisp")
}
