// Copyright © 2026 The ELPS authors

// Regression tests for elps#446 -- LVal.Copy sharing its *token.Location.
//
// LVal.Copy is documented as "creates a deep copy of the receiver", and for
// Cells it is one: copyCells allocates a fresh *LVal per node.  Source was
// not.  `*cp = *v` carried the *token.Location across, so the copy and the
// original pointed at ONE mutable Location object, at every depth, and a
// write through either moved the other.
//
// The caller that makes this matter is lisp.TextLoader.  It exists to hand
// each evaluation a PRIVATE tree -- it is the entry point an embedder is
// pointed at for a reusable parse cache, and macro_stamp_shared_ast_test.go
// contrasts it with the Load* entry points, which do not copy.  For Cells the
// privacy was real; for positions it was not, so every evaluation reported
// its positions through the retained cache's objects.
//
// LIVE OR LATENT: latent, and checked rather than assumed -- see the
// neighbourhood audit on the PR.  No non-test code writes THROUGH a borrowed
// *token.Location: parser/rdparser's five write sites operate on Locations
// the nodes they have just built own (#426/#442), lisp/env.go's three error
// constructors take env.Loc.Copy() (#421), stampMacroExpansion ASSIGNS
// v.Source rather than writing through it, and every position consumer
// (lsp/, lint/, analysis/, mcpserver/, minifier/, formatter/, debugger/,
// profiler/) reads a Location and formats it into its own value type.  So the
// writes these tests perform are writes the TESTS perform: they demonstrate
// the mechanism, they do not record an observed corruption.  What is a real
// observed defect is the SHARING itself, which the tests below assert
// directly and which failed on 5ef6106.
//
// Tests marked GUARD pass before the fix as well as after.  They pin
// behaviour the fix must not break; they are not catches.

package lisp_test

import (
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// readOne parses src and returns its single top-level expression.
func readOne(t *testing.T, src string) *lisp.LVal {
	t.Helper()
	exprs, err := parser.NewReader().Read("copy-source.lisp", strings.NewReader(src))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	return exprs[0]
}

// TestCopyDoesNotAliasSourceLocation is the reproduction from issue #446.
// CATCH: both assertions failed on 5ef6106.
func TestCopyDoesNotAliasSourceLocation(t *testing.T) {
	orig := readOne(t, "(+ 1 2)")
	cp := orig.Copy()

	require.NotNil(t, orig.Source)
	assert.NotSame(t, orig.Source, cp.Source,
		"a copy shares the original's *token.Location (#446)")

	cp.Source.Line = 99
	assert.Equal(t, 1, orig.Source.Line,
		"a write through the copy moved the original's recorded position (#446)")
}

// TestCopyDoesNotAliasSourceAtDepth pins that the separation reaches every
// node copyCells reaches, not just the root.  The issue's probe counted the
// root and all three cells of (+ 1 2) on one object.
// CATCH: failed on 5ef6106 at every depth.
func TestCopyDoesNotAliasSourceAtDepth(t *testing.T) {
	orig := readOne(t, "(defun f (x) (let ([y (+ x 1)]) (* y y)))")
	cp := orig.Copy()

	origNodes := flatten(orig)
	cpNodes := flatten(cp)
	require.Len(t, cpNodes, len(origNodes), "copy has a different shape")
	require.Greater(t, len(origNodes), 10, "test program is too small to be interesting")

	shared := 0
	for i := range origNodes {
		if origNodes[i].Source != nil && origNodes[i].Source == cpNodes[i].Source {
			shared++
		}
	}
	assert.Zero(t, shared,
		"%d of %d copied nodes share the original's *token.Location (#446)",
		shared, len(origNodes))
}

// TestTextLoaderEvaluationsGetPrivatePositions is the reason #446 matters.
// TextLoader retains one parse tree and hands each evaluation expr.Copy();
// the copy is supposed to be private.  A builtin observes the Location the
// evaluation actually carries.
//
// CATCH: on 5ef6106 both evaluations handed the builtin the RETAINED tree's
// own *token.Location -- the same pointer, twice, and the cache's.
func TestTextLoaderEvaluationsGetPrivatePositions(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)

	var seen []*token.Location
	env.AddBuiltins(true, elpsutil.Function("probe-loc", lisp.Formals("x"),
		func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			seen = append(seen, args.Cells[0].Source)
			return lisp.Nil()
		}))

	cr := &capturingReader{inner: parser.NewReader()}
	load, err := lisp.TextLoader(cr, "cache.lisp", strings.NewReader("(probe-loc 'sym)"))
	require.NoError(t, err)
	require.Len(t, cr.exprs, 1)

	retained := cr.exprs[0]
	require.Len(t, retained.Cells, 2)
	cached := retained.Cells[1].Source
	require.NotNil(t, cached)
	cachedLine := cached.Line

	for i := range 2 {
		require.NotEqual(t, lisp.LError, load(env).Type, "evaluation %d failed", i)
	}
	require.Len(t, seen, 2)

	assert.NotSame(t, cached, seen[0],
		"the first evaluation reports its position through the retained cache's object (#446)")
	assert.NotSame(t, cached, seen[1],
		"the second evaluation reports its position through the retained cache's object (#446)")
	assert.NotSame(t, seen[0], seen[1],
		"two evaluations of the cached tree share one *token.Location (#446)")

	// The corruption the sharing enables.  The write is this test's, not
	// production code's -- see the file header.
	seen[0].Line = 99
	assert.Equal(t, cachedLine, cached.Line,
		"a write through one evaluation moved the position the cache will report forever (#446)")
	assert.Equal(t, cachedLine, seen[1].Line,
		"a write through one evaluation moved a sibling evaluation's position (#446)")
}

// TestCopyPreservesSourcePosition pins that separating the objects does not
// change what they say.
// GUARD: passes before the fix.
func TestCopyPreservesSourcePosition(t *testing.T) {
	orig := readOne(t, "(+ 1 2)")
	cp := orig.Copy()

	origNodes := flatten(orig)
	cpNodes := flatten(cp)
	require.Len(t, cpNodes, len(origNodes))
	for i := range origNodes {
		if origNodes[i].Source == nil {
			assert.Nil(t, cpNodes[i].Source, "node %d: nil Source became non-nil", i)
			continue
		}
		require.NotNil(t, cpNodes[i].Source, "node %d: Source was dropped", i)
		assert.Equal(t, *origNodes[i].Source, *cpNodes[i].Source,
			"node %d: copied position differs", i)
	}
}

// TestCopyPreservesNilSource pins that a nil Source stays nil.  A nil Source
// means "no position recorded" throughout the tree and is distinct from a
// zero one; token.Location.Copy is nil-preserving for that reason (#421).
// GUARD: passes before the fix.
func TestCopyPreservesNilSource(t *testing.T) {
	v := lisp.SExpr([]*lisp.LVal{lisp.Symbol("a")})
	v.Source = nil
	v.Cells[0].Source = nil
	cp := v.Copy()
	assert.Nil(t, cp.Source)
	require.Len(t, cp.Cells, 1)
	assert.Nil(t, cp.Cells[0].Source)
}

// TestCopyKeepsSharingTheNativeSingleton pins the ONE Location that is
// deliberately shared and must stay shared: lisp.nativeSource's process-wide
// singleton, stamped on every natively-constructed LVal.
//
// Copying it would buy nothing -- it has a single owner, the process; it is
// read-only by contract; SingletonSnapshot watches its bit pattern; and the
// parser never leaves it in an AST (#362/#421).  It would cost a heap
// allocation on the interpreter's hottest path, which is the +18.6% sec/op,
// +20.2% allocs/op that #362's own comment records and rejects.  Separating
// two owners is what the copy is for, and here there is only one.
//
// GUARD: passes before the fix, and pins that the fix did not "improve" it.
func TestCopyKeepsSharingTheNativeSingleton(t *testing.T) {
	a, b := lisp.Int(1), lisp.Int(2)
	require.NotNil(t, a.Source)
	require.Same(t, a.Source, b.Source, "premise: natively-constructed values share one Location")

	cp := a.Copy()
	assert.Same(t, a.Source, cp.Source,
		"copying a native value allocated a private Location on a hot path (#362)")
	assert.Equal(t, token.NativeFile, cp.Source.File)
}

// TestParseTreeCopyIsFullyPrivate closes the gap the native-singleton
// exception could otherwise leave in TextLoader's guarantee.  The exception is
// only harmless for a parse cache if a parse tree never contains that pointer,
// which is a property #362/#421 states and parser/rdparser's
// TestParserDoesNotAliasSharedNativeLocation guards at the reader end.  This
// asserts the consequence at the end that consumes it: for a tree that came
// from the reader, "no node shares" has no exceptions at all.
// CATCH: failed on 5ef6106 (every node shared).
func TestParseTreeCopyIsFullyPrivate(t *testing.T) {
	const src = `(defmacro m (x) (quasiquote (+ 1 (unquote x))))
(defun f (a &optional b) (let ([c (m a)]) (list c b '(1 2 3) #^0)))
(f 1 2)`
	exprs, err := parser.NewReader().Read("private.lisp", strings.NewReader(src))
	require.NoError(t, err)
	require.NotEmpty(t, exprs)

	native, shared, total := 0, 0, 0
	for _, expr := range exprs {
		origNodes := flatten(expr)
		cpNodes := flatten(expr.Copy())
		require.Len(t, cpNodes, len(origNodes))
		for i := range origNodes {
			total++
			if origNodes[i].Source == nil {
				continue
			}
			if origNodes[i].Source.File == token.NativeFile {
				native++
			}
			if origNodes[i].Source == cpNodes[i].Source {
				shared++
			}
		}
	}
	require.Greater(t, total, 30, "test program is too small to be interesting")
	assert.Zero(t, native,
		"the reader put the shared native Location into a parse tree, so the "+
			"exception in LVal.Copy is no longer safe for a parse cache (#362)")
	assert.Zero(t, shared,
		"%d of %d copied parse-tree nodes share the original's *token.Location (#446)",
		shared, total)
}

// TestCopyDoesNotAliasSourceOfArrayNode pins that the reference types get the
// same treatment for their OWN Source.  LArray shares its Cells backing and
// LSortMap shares its value pointers, both deliberately, so the nodes reached
// THROUGH them keep sharing; the node Copy() was called on must not.
// CATCH for the array/sortmap root: failed on 5ef6106.
func TestCopyDoesNotAliasSourceOfReferenceTypes(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)

	for _, src := range []string{`(vector 1 2 3)`, `(sorted-map 'a 1)`} {
		t.Run(src, func(t *testing.T) {
			v := env.LoadString("ref.lisp", src)
			require.NotEqual(t, lisp.LError, v.Type, "%v", v)
			loc := token.Location{File: "ref.lisp", Pos: 3, Line: 1, Col: 4}
			v.Source = &loc

			cp := v.Copy()
			require.NotEqual(t, lisp.LError, cp.Type, "%v", cp)
			assert.NotSame(t, v.Source, cp.Source,
				"a copied %s shares the original's *token.Location (#446)", v.Type)
			assert.Equal(t, *v.Source, *cp.Source)
		})
	}
}

// flatten returns v and every node reachable through Cells, in a fixed
// pre-order, so two trees of the same shape line up index for index.
func flatten(v *lisp.LVal) []*lisp.LVal {
	if v == nil {
		return nil
	}
	out := []*lisp.LVal{v}
	for _, c := range v.Cells {
		out = append(out, flatten(c)...)
	}
	return out
}

// capturingReader records the expressions it hands back, so a test can hold
// the tree TextLoader retains.
type capturingReader struct {
	inner lisp.Reader
	exprs []*lisp.LVal
}

func (c *capturingReader) Read(name string, r io.Reader) ([]*lisp.LVal, error) {
	v, err := c.inner.Read(name, r)
	c.exprs = append(c.exprs, v...)
	return v, err
}
