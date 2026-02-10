// Copyright © 2024 The ELPS authors

package analysis

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func loadedRegistry() *lisp.PackageRegistry {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	lisp.InitializeUserEnv(env)
	lisplib.LoadLibrary(env)
	return env.Runtime.Registry
}

func TestExtractPackageExports_NilRegistry(t *testing.T) {
	result := ExtractPackageExports(nil)
	assert.Nil(t, result)
}

func TestExtractPackageExports_HasPackages(t *testing.T) {
	reg := loadedRegistry()
	exports := ExtractPackageExports(reg)
	require.NotNil(t, exports)

	// The testing package should have exports (assert-equal, test, etc.)
	testingPkg, ok := exports["testing"]
	assert.True(t, ok, "testing package should have exports")
	assert.NotEmpty(t, testingPkg)

	// Check that common testing symbols are present
	names := make(map[string]bool)
	for _, sym := range testingPkg {
		names[sym.Name] = true
	}
	assert.True(t, names["assert-equal"], "testing package should export assert-equal")
}

func TestExtractPackageExports_SymbolKinds(t *testing.T) {
	reg := loadedRegistry()
	exports := ExtractPackageExports(reg)
	require.NotNil(t, exports)

	// Find any function export and verify it has a signature
	found := false
	for _, pkgSyms := range exports {
		for _, sym := range pkgSyms {
			if sym.Kind == SymFunction || sym.Kind == SymMacro {
				found = true
				// Functions/macros should have signatures
				assert.NotNil(t, sym.Signature, "callable %s should have a signature", sym.Name)
				break
			}
		}
		if found {
			break
		}
	}
	assert.True(t, found, "should find at least one callable export")
}

func TestExtractPackageExports_PackageField(t *testing.T) {
	reg := loadedRegistry()
	exports := ExtractPackageExports(reg)
	require.NotNil(t, exports)

	for pkgName, syms := range exports {
		for _, sym := range syms {
			assert.Equal(t, pkgName, sym.Package,
				"symbol %s should have package %s", sym.Name, pkgName)
		}
	}
}
