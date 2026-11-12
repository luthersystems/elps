// Copyright © 2026 The ELPS authors

package lisp

import (
	"go/ast"
	"go/parser"
	gotoken "go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"unsafe"

	"github.com/luthersystems/elps/internal/fmtmeta"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// These tests pin the tooling record's semantics (see auxKind).  The record
// used to be two pointer fields of LVal; it moved off the struct to take
// LVal down a size class (luthersystems/elps#944), and off token.Location
// to keep locations at 80 bytes (luthersystems/elps#947).  Each test names
// the behavior of the two fields that the record must keep.

// TestLValSizeClass pins the point of both moves: a third pointer in LVal
// puts every value in the 112-byte class, and a record slot on
// token.Location put every location in the 96-byte class.  The carriers
// are pinned too: metaAux must stay in the size class of the metadata it
// replaced plus the location pointer, or a format-preserving parse pays
// more bytes per node than it did with the slot.
func TestLValSizeClass(t *testing.T) {
	assert.Equal(t, uintptr(96), unsafe.Sizeof(LVal{}))
	assert.Equal(t, uintptr(80), unsafe.Sizeof(token.Location{}))
	assert.Equal(t, uintptr(24), unsafe.Sizeof(nodeAux{}))
	assert.LessOrEqual(t, unsafe.Sizeof(metaAux{}), uintptr(128))
}

func TestNodeAuxAbsentInProduction(t *testing.T) {
	v := Int(1)
	assert.Nil(t, v.fmtMeta())
	assert.Nil(t, v.macroExp())
	loc := &token.Location{File: "a.lisp", Pos: 3}
	v.SetSource(loc)
	assert.Same(t, loc, v.loc(), "SetSource stores the pointer it is given")
	assert.Equal(t, locWord(loc), v.source, "a node without a record holds the location itself")
	assert.False(t, v.hasAux())
}

// A header copy copied the two pointers, so it shared the record.
func TestNodeAuxHeaderCopyShares(t *testing.T) {
	v := Symbol("x")
	v.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	m := v.ensureFmtMeta()
	m.OriginalText = "x"
	cp := Quote(v) // a header copy
	assert.Same(t, m, cp.fmtMeta())

	w := Symbol("y")
	ext := &fmtmeta.Meta{OriginalText: "y"}
	w.setFmtMeta(ext)
	assert.Same(t, ext, Quote(w).fmtMeta())
}

// A node that borrowed another node's Location did not get its record.
func TestNodeAuxBorrowedLocationDoesNotShare(t *testing.T) {
	v := Symbol("x")
	v.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	v.setMacroExp(&macroExpansionInfo{ID: 7})
	w := Symbol("y")
	w.setPlainLoc(v.loc())
	assert.Same(t, v.loc(), w.loc())
	assert.Nil(t, w.macroExp())
	_, ok := w.MacroExpansion()
	assert.False(t, ok)
}

// A write to one header's field did not reach a header copy.
func TestNodeAuxWriteDoesNotReachHeaderCopy(t *testing.T) {
	v := Symbol("x")
	v.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	first := &macroExpansionInfo{ID: 1}
	v.setMacroExp(first)
	cp := Quote(v) // a header copy
	second := &macroExpansionInfo{ID: 2}
	cp.setMacroExp(second)
	assert.Same(t, v.macroExp(), first)
	assert.Same(t, cp.macroExp(), second)
	cp.setMacroExp(nil)
	assert.Nil(t, cp.macroExp())
	assert.Same(t, v.macroExp(), first)
	assert.Same(t, v.loc(), cp.loc(), "dropping the record keeps the location")

	// The same for a location change on a node whose metadata lives in
	// its carrier.
	a := Symbol("a")
	locA := &token.Location{File: "a.lisp", Pos: 1}
	a.SetSource(locA)
	m := a.ensureFmtMeta()
	b := Quote(a) // a header copy
	locB := &token.Location{File: "a.lisp", Pos: 2}
	b.SetSource(locB)
	assert.Same(t, locA, a.loc())
	assert.Same(t, locB, b.loc())
	assert.Same(t, m, a.fmtMeta())
	assert.Same(t, m, b.fmtMeta())
}

// The two halves of the record are independent, as the two fields were.
func TestNodeAuxHalvesAreIndependent(t *testing.T) {
	v := Symbol("x")
	m := &fmtmeta.Meta{OriginalText: "x"}
	info := &macroExpansionInfo{ID: 1}
	v.setFmtMeta(m)
	v.setMacroExp(info)
	assert.Same(t, m, v.fmtMeta())
	assert.Same(t, v.macroExp(), info)
	v.setFmtMeta(nil)
	assert.Nil(t, v.fmtMeta())
	assert.Same(t, v.macroExp(), info)
	v.setMacroExp(nil)
	assert.False(t, v.hasAux())
	assert.Nil(t, v.source)

	// ensureFmtMeta keeps a macro-expansion record.
	w := Symbol("y")
	w.setMacroExp(info)
	got := w.ensureFmtMeta()
	assert.Same(t, got, w.fmtMeta())
	assert.Same(t, w.macroExp(), info)
}

// A node without a location could carry either field.  It still reports no
// location, although its source word is now the record's carrier.
func TestNodeAuxWithoutLocation(t *testing.T) {
	v := Symbol("x")
	v.setMacroExp(&macroExpansionInfo{ID: 1})
	require.NotNil(t, v.macroExp())
	assert.Nil(t, v.loc())
	_, ok := v.Source()
	assert.False(t, ok)
	assert.True(t, needsStamp(v))

	loc := &token.Location{File: "a.lisp", Pos: 3}
	v.SetSource(loc)
	got, ok := v.Source()
	require.True(t, ok)
	assert.Equal(t, *loc, got)
	require.NotNil(t, v.macroExp(), "SetSource keeps the record")

	v.SetSource(nil)
	_, ok = v.Source()
	assert.False(t, ok)
	require.NotNil(t, v.macroExp(), "clearing the location keeps the record")

	w := Symbol("y")
	w.ensureFmtMeta()
	assert.Nil(t, w.loc())
	_, ok = w.Source()
	assert.False(t, ok)
}

// The format-preserving parser hands a node a Location, stamps the node,
// and then writes end positions through the pointer it kept.  The record
// never writes the Location, so SetSource keeps the given pointer even for
// a node with a debugger record (#946 had to copy the Location then, to
// keep its slot write away from Locations other goroutines read).
func TestNodeAuxParserKeepsItsLocationPointer(t *testing.T) {
	v := Symbol("x")
	loc := &token.Location{File: "a.lisp", Pos: 3}
	v.SetSource(loc)
	v.ensureFmtMeta()
	assert.Same(t, loc, v.loc())
	loc.EndPos = 9
	got, _ := v.Source()
	assert.Equal(t, 9, got.EndPos)

	// The parser's Quote path: a header copy gets a Location of its own
	// and still shares the operand's metadata.
	outer := Quote(v) // a header copy
	loc2 := &token.Location{File: "a.lisp", Pos: 2}
	outer.SetSource(loc2)
	assert.Same(t, loc2, outer.loc())
	assert.Same(t, v.fmtMeta(), outer.fmtMeta())
	got, _ = v.Source()
	assert.Equal(t, 3, got.Pos, "the operand keeps its own location")

	w := Symbol("y")
	w.setMacroExp(&macroExpansionInfo{ID: 1})
	shared := &token.Location{File: "a.lisp", Pos: 3}
	before := *shared
	w.SetSource(shared)
	assert.Same(t, shared, w.loc())
	assert.Equal(t, before, *shared, "the Location was written")
}

// ensureFmtMeta puts the metadata in the node's carrier: one allocation per
// stamped node, as the metadata alone cost before #944.  A second object
// per node made the format-preserving parse allocate 7 to 8% more objects
// (BenchmarkMutationChecksScaling in lint).
func TestNodeAuxEnsureFmtMetaIsOneAllocation(t *testing.T) {
	loc := &token.Location{File: "a.lisp", Pos: 3}
	nodes := make([]LVal, 101)
	i := 0
	allocs := testing.AllocsPerRun(100, func() {
		v := &nodes[i]
		i++
		v.setPlainLoc(loc)
		v.ensureFmtMeta().OriginalText = "x"
	})
	assert.Equal(t, 1, int(allocs))
}

// Copy and detach kept a private copy of the formatter metadata and dropped
// the macro-expansion record.
func TestNodeAuxCopyAndDetach(t *testing.T) {
	v := Symbol("x")
	v.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	m := v.ensureFmtMeta()
	m.OriginalText = "x"
	v.setMacroExp(&macroExpansionInfo{ID: 1})

	cp := v.Copy()
	require.NotNil(t, cp.fmtMeta())
	assert.NotSame(t, m, cp.fmtMeta())
	assert.Equal(t, "x", cp.fmtMeta().OriginalText)
	assert.Nil(t, cp.macroExp())
	assert.NotSame(t, v.loc(), cp.loc())
	assert.Equal(t, *v.loc(), *cp.loc())

	plain := Symbol("y")
	plain.SetSource(&token.Location{File: "a.lisp", Pos: 3})
	plain.setMacroExp(&macroExpansionInfo{ID: 2})
	pc := plain.Copy()
	assert.False(t, pc.hasAux())
	got, ok := pc.Source()
	require.True(t, ok)
	assert.Equal(t, 3, got.Pos)

	bare := Symbol("z")
	bare.setMacroExp(&macroExpansionInfo{ID: 3})
	bc := bare.Copy()
	assert.False(t, bc.hasAux())
	assert.Nil(t, bc.source, "a carrier is not a location and is not copied")
}

// MacroExpansion hands out copies of the call site and the definition
// site, never the Locations the record points to.
func TestNodeAuxMacroExpansionCopiesSites(t *testing.T) {
	macro := Symbol("m")
	defSite := &token.Location{File: "def.lisp", Pos: 7, Line: 2, Col: 1}
	macro.SetSource(defSite)
	macro.ensureFmtMeta()
	require.Same(t, defSite, macro.loc())

	callSite := &token.Location{File: "call.lisp", Pos: 3, Line: 1, Col: 4}
	v := Symbol("x")
	v.setMacroExp(&macroExpansionInfo{ID: 1, macroExpansionContext: &macroExpansionContext{
		Name: "user:m", CallSite: callSite, DefSite: macro.loc(),
	}})
	m, ok := v.MacroExpansion()
	require.True(t, ok)
	assert.Equal(t, *defSite, *m.DefSite)
	assert.Equal(t, *callSite, *m.CallSite)
	assert.NotSame(t, defSite, m.DefSite)
	assert.NotSame(t, callSite, m.CallSite)

	// No site: no Location.
	w := Symbol("y")
	w.setMacroExp(&macroExpansionInfo{ID: 2, macroExpansionContext: &macroExpansionContext{Name: "user:b"}})
	m, ok = w.MacroExpansion()
	require.True(t, ok)
	assert.Nil(t, m.CallSite)
	assert.Nil(t, m.DefSite)
}

// TestNodeAuxSourceWritesStayInNodeAux keeps the source word and auxKind
// written together.  loc and the record accessors convert source by
// auxKind; a write of one without the other would make them read a
// Location as a carrier or the reverse, which is memory corruption and not
// a wrong answer.  So outside nodeaux.go no production file assigns either
// field, and a composite literal may set source only to locWord(...) and
// never sets auxKind (its zero, auxNone, is what locWord means).
func TestNodeAuxSourceWritesStayInNodeAux(t *testing.T) {
	files, err := filepath.Glob("*.go")
	require.NoError(t, err)
	fset := gotoken.NewFileSet()
	checked := 0
	for _, name := range files {
		if strings.HasSuffix(name, "_test.go") || name == "nodeaux.go" {
			continue
		}
		src, err := os.ReadFile(name) //nolint:gosec // G304: a file of this package, listed by Glob above
		require.NoError(t, err)
		f, err := parser.ParseFile(fset, name, src, 0)
		require.NoError(t, err)
		checked++
		ast.Inspect(f, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.AssignStmt:
				for _, lhs := range n.Lhs {
					if sel, ok := lhs.(*ast.SelectorExpr); ok && (sel.Sel.Name == "source" || sel.Sel.Name == "auxKind") {
						t.Errorf("%s: assigns .%s; use setPlainLoc, setLoc or another setter in nodeaux.go", fset.Position(sel.Pos()), sel.Sel.Name)
					}
				}
			case *ast.IncDecStmt:
				if sel, ok := n.X.(*ast.SelectorExpr); ok && sel.Sel.Name == "auxKind" {
					t.Errorf("%s: changes .auxKind outside nodeaux.go", fset.Position(sel.Pos()))
				}
			case *ast.KeyValueExpr:
				key, ok := n.Key.(*ast.Ident)
				if !ok {
					return true
				}
				switch key.Name {
				case "auxKind":
					t.Errorf("%s: a composite literal sets auxKind outside nodeaux.go", fset.Position(key.Pos()))
				case "source":
					var fn *ast.Ident
					if call, ok := n.Value.(*ast.CallExpr); ok {
						fn, _ = call.Fun.(*ast.Ident)
					}
					if fn == nil || fn.Name != "locWord" {
						t.Errorf("%s: a composite literal sets source to something other than locWord(...)", fset.Position(key.Pos()))
					}
				}
			}
			return true
		})
	}
	require.Greater(t, checked, 50, "the scan found too few files to mean anything")
}
