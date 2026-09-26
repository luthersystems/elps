// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
)

// BenchmarkCorpus runs whole lisp test files per iteration -- building a
// Runner environment (included deliberately: about 2k of each row's allocs,
// measured on its own by BenchmarkEnvConstructionFull), loading the file, and
// running every test in it -- which is the shape of an elps project's own test
// run.  The files are chosen because their tests share one environment; files
// that need a fresh environment per test (e.g. libjson_test.lisp) do not fit
// this loop, and when_family_test.lisp is left out because one pass takes
// seconds.
//
// The files are frozen copies under testdata/corpus, not the live stdlib test
// files: the benchmark gate compares allocs/op at a 5% threshold, and adding a
// test case to a live file would move its row past that without any change to
// the evaluator.  Refresh the copies deliberately, in a PR of their own.
func BenchmarkCorpus(b *testing.B) {
	files := []string{
		"testdata/corpus/libstring_test.lisp",
		"testdata/corpus/libelpspath_test.lisp",
		"testdata/corpus/libschema_test.lisp",
	}
	for _, f := range files {
		src, err := os.ReadFile(f) //#nosec G304 -- fixed benchmark corpus paths
		if err != nil {
			b.Fatal(err)
		}
		b.Run(filepath.Base(f), func(b *testing.B) {
			r := &elpstest.Runner{}
			defer r.Close()
			b.ReportAllocs()
			for b.Loop() {
				env, err := r.NewEnv(b)
				if err != nil {
					b.Fatal(err)
				}
				if err := lisp.GoError(env.LoadString(filepath.Base(f), string(src))); err != nil {
					b.Fatal(err)
				}
				suite := libtesting.EnvTestSuite(env)
				for i := range suite.Len() {
					if err := lisp.GoError(env.Eval(lisp.SExpr([]*lisp.LVal{suite.Test(i).Fun}))); err != nil {
						b.Fatal(err)
					}
				}
			}
		})
	}
}
