// Copyright © 2024 The ELPS authors

package lisplib_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNewDocEnv(t *testing.T) {
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	require.NotNil(t, env)

	// Should have all stdlib packages loaded.
	expectedPkgs := []string{
		"lisp", "user", "time", "help", "golang", "math",
		"string", "base64", "json", "regexp", "testing", "s",
	}
	for _, name := range expectedPkgs {
		assert.NotNilf(t, env.Runtime.Registry.Packages[name],
			"NewDocEnv should include package %q", name)
	}

	// Should be in the user package.
	assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)
}

func TestNewDocEnv_CanLookupSymbols(t *testing.T) {
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	// Look up a stdlib function (not a core builtin) to verify LoadLibrary ran.
	v := env.Get(lisp.Symbol("math:sin"))
	require.Equal(t, lisp.LFun, v.Type, "should resolve 'math:sin' as a function")
	assert.NotEmpty(t, v.Docstring(), "math:sin should have a docstring")

	// Also verify a core builtin works.
	v2 := env.Get(lisp.Symbol("map"))
	assert.Equal(t, lisp.LFun, v2.Type, "should resolve 'map' as a function")
}
