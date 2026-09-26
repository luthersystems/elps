// Copyright © 2026 The ELPS authors

package elpstest

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const helperData = "testdata/helpers"

// One helper file serves two different test files in its directory.
func TestTestHelpersSharedAcrossFiles(t *testing.T) {
	r := &Runner{}
	for _, f := range []string{"a_test.lisp", "b_test.lisp"} {
		t.Run(f, func(t *testing.T) {
			r.RunTestFile(t, filepath.Join(helperData, "shared", f))
		})
	}
}

func TestTestHelpersOrderDeterministic(t *testing.T) {
	files, err := TestHelperFiles(filepath.Join(helperData, "order"))
	require.NoError(t, err)
	assert.Equal(t, []string{
		filepath.Join(helperData, "order", "a_testhelpers.lisp"),
		filepath.Join(helperData, "order", "b_testhelpers.lisp"),
	}, files)
	(&Runner{}).RunTestFile(t, filepath.Join(helperData, "order", "o_test.lisp"))
}

func TestTestHelpersNone(t *testing.T) {
	files, err := TestHelperFiles(filepath.Join(helperData, "none"))
	require.NoError(t, err)
	assert.Empty(t, files)
	files, err = TestHelperFiles(filepath.Join(helperData, "does-not-exist"))
	require.NoError(t, err)
	assert.Empty(t, files)
	(&Runner{}).RunTestFile(t, filepath.Join(helperData, "none", "plain_test.lisp"))
}

func TestTestHelpersErrorNamesHelper(t *testing.T) {
	env, err := (&Runner{}).NewEnv(t)
	require.NoError(t, err)
	err = LoadTestHelpers(env, filepath.Join(helperData, "broken", "x_test.lisp"))
	require.Error(t, err)
	assert.Contains(t, err.Error(), filepath.Join(helperData, "broken", "bad_testhelpers.lisp"))
	assert.Contains(t, err.Error(), "boom")
}

// A helper file is neither a test file nor part of a plain (production) load.
func TestTestHelpersNotLoadedOutsideTests(t *testing.T) {
	matched, err := filepath.Match("*_test.lisp", "common_testhelpers.lisp")
	require.NoError(t, err)
	assert.False(t, matched)

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	src, err := os.ReadFile(filepath.Join(helperData, "shared", "a_test.lisp"))
	require.NoError(t, err)
	require.NoError(t, lisp.GoError(env.Load("a_test.lisp", strings.NewReader(string(src)))))
	v := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("double"), lisp.Int(2)}))
	assert.Equal(t, lisp.LError, v.Type, "double must be unbound without the test runner")
}
