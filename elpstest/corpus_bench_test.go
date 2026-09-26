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

// BenchmarkCorpus runs whole lisp test files per iteration -- a Runner
// environment, loading the file, and every test in it -- which is the shape of
// an elps project's own test run.  The files are chosen because their tests
// share one environment; files that need a fresh environment per test (e.g.
// libjson_test.lisp) do not fit this loop, and when_family_test.lisp is left
// out because one pass takes seconds.
func BenchmarkCorpus(b *testing.B) {
	files := []string{
		"../lisp/lisplib/libstring/libstring_test.lisp",
		"../lisp/lisplib/libelpspath/libelpspath_test.lisp",
		"../lisp/lisplib/libschema/libschema_test.lisp",
	}
	for _, f := range files {
		src, err := os.ReadFile(f) //#nosec G304 -- fixed in-repo test fixture paths
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
