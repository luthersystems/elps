// Copyright © 2026 The ELPS authors

// Call-site aliasing regression tests for elps#431.
//
// stampMacroExpansion writes callSite onto every node of a macro expansion
// that has no real position of its own, by POINTER.  macroCall used to pass
// env.loc -- which eval sets to the location of the node it is evaluating, i.e.
// a *token.Location owned by a node in the CALLER'S parse tree.  One expansion
// of a macro with N synthesized nodes therefore left N+1 nodes, in two trees
// with unrelated lifetimes, holding one mutable object; writing through any of
// them moved the position the others report.
//
// Third instance of one shape: elps#362 (a process-wide native Location handed
// to every constructed value) and elps#426 (a prefix form and its operand
// sharing the scanner's per-token object) are the other two.
//
// STATUS -- read this before treating these as a caught bug.  Both tests below
// are red on the parent commit and green after, so they are regression tests
// for the ALIASING.  The CORRUPTION the aliasing enables is LATENT: nothing in
// the tree writes through a *token.Location it does not own, except the five
// sites in parser/rdparser/parser.go -- which operate on nodes the reader has
// just built, and since elps#426 on Locations those nodes own -- and
// lisp/env.go's error constructors, which take env.loc.Copy() since elps#421.
// lsp/, lint/, analysis/, analysis/perf/, mcpserver/, minifier/, formatter/,
// lisp/x/debugger/ and lisp/x/profiler/ read a Location and format it into
// their own Position/Range/Span value types; none writes through one.
//
// So the write in step 3 of the first test is one the TEST performs: it
// demonstrates the mechanism, it does not record an observed failure.  What
// these pin is the ownership property, which does not depend on today's
// population of writers -- the point, since that population is not fixed:
// elps#370 was a walk writing positions into a tree it did not own, and
// elps#426 was a parser helper writing through a pointer two nodes held.
//
// NOT COVERED HERE, deliberately: LEnv.Lambda's `source: env.loc` puts the same
// caller-owned Location on the LFun a `defun` expansion builds.  elps#421's
// neighbourhood audit found that, weighed it ("a copy here is a new allocation
// on a path that has none, per lambda"), and left it reported and subsumed by
// the LVal.source immutability work.  It is still there; these tests use macros
// whose expansions contain no lambda so they measure the stamp and not that.

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// callSiteEnv builds an environment with a real reader, so the caller's nodes
// carry the parser's own Locations rather than constructed ones.
func callSiteEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	return env
}

// expandOnce drives a macro exactly as eval does -- env.loc set to the calling
// node's own location -- and returns the expansion with MacroCall's
// LMarkMacExpand wrapper removed.
func expandOnce(t testing.TB, env *lisp.LEnv, name string, callNode *lisp.LVal, args ...*lisp.LVal) *lisp.LVal {
	t.Helper()
	fun := env.GetGlobal(lisp.Symbol(name))
	require.Equal(t, lisp.LFun, fun.Type, "%s is not a macro: %v", name, fun)

	if callNode != nil {
		lisp.SetEnvLocForTest(env, lisp.SourceRefForTest(callNode))
	} else {
		lisp.SetEnvLocForTest(env, nil)
	}
	res := env.MacroCall(fun, lisp.SExpr(args))
	require.NotEqual(t, lisp.LError, res.Type, "%v", res)
	if res.Type == lisp.LMarkMacExpand {
		require.Len(t, res.Cells, 1)
		res = res.Cells[0]
	}
	return res
}

// collectSources returns every distinct non-nil Source pointer in v's tree.
func collectSources(v *lisp.LVal) map[*token.Location]bool {
	out := map[*token.Location]bool{}
	seen := map[*lisp.LVal]bool{}
	var walk func(*lisp.LVal)
	walk = func(v *lisp.LVal) {
		if v == nil || seen[v] {
			return
		}
		seen[v] = true
		if loc := lisp.SourceRefForTest(v); loc != nil {
			out[loc] = true
		}
		for _, c := range v.Cells {
			walk(c)
		}
	}
	walk(v)
	return out
}

// parseOne reads a single form and returns it.
func parseOne(t testing.TB, env *lisp.LEnv, name, src string) *lisp.LVal {
	t.Helper()
	exprs, err := env.Runtime.Reader.Read(name, strings.NewReader(src))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	require.NotNil(t, lisp.SourceRefForTest(exprs[0]))
	return exprs[0]
}

// TestMacroExpansionDoesNotAliasCallerLocation is the regression arm.
//
// `defconst` is a builtin macro that builds its whole expansion from
// lisp.Symbol/lisp.SExpr/lisp.Quote, so every node arrives carrying the
// synthetic "<native code>" position the stamp exists to replace: the maximal
// case, and one with no lambda in it.
//
// Pre-fix every one of those nodes held the *same pointer* as the caller's
// `(defconst ...)` node, and a write through the expansion moved the caller's
// node with it.
func TestMacroExpansionDoesNotAliasCallerLocation(t *testing.T) {
	t.Parallel()
	env := callSiteEnv(t)

	callNode := parseOne(t, env, "caller.lisp", "(defconst answer 42)\n")
	expansion := expandOnce(t, env, "lisp:defconst", callNode,
		lisp.Symbol("answer"), lisp.Int(42))

	// 1. No node of the expansion holds the caller's object.
	for loc := range collectSources(expansion) {
		assert.NotSame(t, lisp.SourceRefForTest(callNode), loc,
			"a macro expansion node shares the caller's *token.Location (#431)")
	}

	// 2. It still reports the call site, by value.
	require.NotNil(t, lisp.SourceRefForTest(expansion))
	assert.Equal(t, *lisp.SourceRefForTest(callNode), *lisp.SourceRefForTest(expansion),
		"the expansion must still report the call site's position")

	// 3. The failure the aliasing enables: an in-place write through the
	//    expansion moving a position the caller's parse tree records.  That
	//    tree outlives the expansion -- a function body IS the parse tree it
	//    was defined from, re-entered on every call -- and every error
	//    message, stack frame and LSP range is computed from those positions.
	before := lisp.SourceRefForTest(callNode).String()
	lisp.SourceRefForTest(expansion).Line = 99
	lisp.SourceRefForTest(expansion).Col = 42
	assert.Equal(t, before, lisp.SourceRefForTest(callNode).String(),
		"a write through the expansion moved the caller's recorded position (#431)")
}

// TestMacroExpansionCallSiteIsPrivate states the ownership property on its own
// terms, with no write at all: whatever Location the expansion is stamped with
// must be an object no node of the caller's parse tree holds.  It covers the
// ordinary user-macro shape -- quasiquote splicing an argument into a
// synthesized frame -- rather than the all-synthetic builtin above.  Also red
// on the parent commit.
func TestMacroExpansionCallSiteIsPrivate(t *testing.T) {
	t.Parallel()
	env := callSiteEnv(t)

	require.NotEqual(t, lisp.LError,
		env.LoadString("prelude.lisp",
			"(defmacro wrap (x) (quasiquote (progn (unquote x))))").Type)

	callNode := parseOne(t, env, "caller.lisp", "(wrap 1)\n")
	callerLocs := collectSources(callNode)
	require.NotEmpty(t, callerLocs)

	// The argument is a value the test constructed, so no node of the caller's
	// tree is spliced into the expansion; an unquoted caller node legitimately
	// carries its own Location along with it, and that is not this property.
	expansion := expandOnce(t, env, "wrap", callNode, lisp.Int(1))

	for loc := range collectSources(expansion) {
		assert.False(t, callerLocs[loc],
			"the expansion is stamped with a Location the caller's parse tree also holds (#431)")
	}
}

// TestMacroCallSiteCopyPreservesNil pins the nil-preserving half of the copy.
// A macro expanded with no recorded position must stamp nothing --
// stampMacroExpansion returns early on a nil callSite -- so materialising nil
// into a zero Location would relabel every synthesized node from
// "<native code>" to ":0:0".
func TestMacroCallSiteCopyPreservesNil(t *testing.T) {
	t.Parallel()
	env := callSiteEnv(t)

	expansion := expandOnce(t, env, "lisp:defconst", nil,
		lisp.Symbol("answer"), lisp.Int(42))

	// The node records NO location, which is how "<native code>" is spelled
	// since issue #362 deleted the shared singleton: Source() reports ok=false
	// and synthesizes the native location by value.  The property under test
	// is unchanged -- a nil call site must not relabel synthesized nodes from
	// "<native code>" to ":0:0" -- only the representation of "no position" is.
	assert.Nil(t, lisp.SourceRefForTest(expansion),
		"a nil call site stamped a Location onto a synthesized node")
	loc, ok := expansion.Source()
	assert.False(t, ok, "a nil call site must leave synthesized nodes with no RECORDED position")
	assert.Equal(t, token.NativeFile, loc.File,
		"a nil call site must leave synthesized nodes on <native code>, not ':0:0'")
	assert.Negative(t, loc.Pos,
		"a nil call site must leave the synthetic Pos < 0 marker in place")
}
