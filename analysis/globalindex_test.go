// Copyright © 2026 The ELPS authors

package analysis

import (
	"fmt"
	"slices"
	"strings"
	"sync"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

func indexTestGlobals() []ExternalSymbol {
	return []ExternalSymbol{
		{Name: "f", Package: "alpha", Kind: SymFunction},
		{Name: "f", Package: "beta", Kind: SymFunction},
		{Name: "top", Kind: SymVariable},
	}
}

// TestGlobalIndexOnlyForItsGlobals pins that Analyze reads an index only
// for the slice it was built from. A Config whose ExtraGlobals were
// replaced after the index was made must not resolve the old globals.
func TestGlobalIndexOnlyForItsGlobals(t *testing.T) {
	globals := indexTestGlobals()
	src := "(in-package 'gamma)\n(defun g () (alpha:f) (f) (top))\n"
	exprs, err := rdparser.New(token.NewScanner("t.lisp", strings.NewReader(src))).ParseProgram()
	require.NoError(t, err)

	same := Analyze(exprs, &Config{ExtraGlobals: globals, GlobalIndex: NewGlobalIndex(globals)})
	assert.NotNil(t, same.RootScope.globals, "the index of these globals must be used")
	assert.NotNil(t, same.RootScope.LookupInPackage("f", "alpha"))

	other := slices.Clone(globals)
	stale := Analyze(exprs, &Config{ExtraGlobals: other[:1], GlobalIndex: NewGlobalIndex(globals)})
	assert.Nil(t, stale.RootScope.globals, "an index of other globals must be ignored")
	assert.Nil(t, stale.RootScope.LookupInPackage("f", "beta"),
		"a global missing from ExtraGlobals must not resolve through a stale index")
}

// TestGlobalIndexSymbolsArePerFile pins that two analyses sharing an index
// get their own Symbol for a global, so a reference count written by one
// is not seen by the other, and that each lookup in one file returns the
// same Symbol. Run with -race, it also shows an index can be shared by
// analyses on several goroutines.
func TestGlobalIndexSymbolsArePerFile(t *testing.T) {
	globals := indexTestGlobals()
	cfg := &Config{ExtraGlobals: globals, GlobalIndex: NewGlobalIndex(globals)}
	src := "(in-package 'gamma)\n(defun g () (alpha:f) (beta:f) (f))\n"
	exprs, err := rdparser.New(token.NewScanner("t.lisp", strings.NewReader(src))).ParseProgram()
	require.NoError(t, err)

	const n = 8
	results := make([]*Result, n)
	var wg sync.WaitGroup
	for i := range n {
		wg.Go(func() {
			results[i] = Analyze(exprs, cfg)
			// Read the scope from many goroutines while the others still
			// analyze.
			results[i].RootScope.LookupAllLocal("f")
		})
	}
	wg.Wait()
	first := results[0].RootScope.LookupInPackage("f", "alpha")
	require.NotNil(t, first)
	assert.Same(t, first, results[0].RootScope.LookupInPackage("f", "alpha"), "one Symbol per global per file")
	for _, res := range results[1:] {
		sym := res.RootScope.LookupInPackage("f", "alpha")
		assert.NotSame(t, first, sym, "each file has its own Symbol for a global")
		assert.Equal(t, first.References, sym.References)
	}
}

// TestEachPackageSymbolListsTheGlobals pins that EachPackageSymbol lists
// what a range over PackageSymbols listed without an index: the LSP's
// completion lists a package's definitions this way.
func TestEachPackageSymbolListsTheGlobals(t *testing.T) {
	globals := append(indexTestGlobals(),
		ExternalSymbol{Name: "h", Package: "alpha", Kind: SymFunction},
		ExternalSymbol{Name: "f", Package: "alpha", Kind: SymMacro})
	src := "(in-package 'alpha)\n(defun h () (f))\n(defun own () 1)\n"
	exprs, err := rdparser.New(token.NewScanner("t.lisp", strings.NewReader(src))).ParseProgram()
	require.NoError(t, err)

	list := func(cfg *Config) map[string]string {
		out := map[string]string{}
		Analyze(exprs, cfg).RootScope.EachPackageSymbol("alpha", func(key string, sym *Symbol) bool {
			out[key] = fmt.Sprintf("%v external=%v", sym.Kind, sym.External)
			return true
		})
		return out
	}
	want := list(&Config{ExtraGlobals: globals})
	got := list(&Config{ExtraGlobals: globals, GlobalIndex: NewGlobalIndex(globals)})
	assert.Equal(t, want, got)
	assert.Len(t, got, 3, "alpha:f, alpha:h and alpha:own")
}
