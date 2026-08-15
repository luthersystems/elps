// Copyright © 2026 The ELPS authors

// Shared-parse-tree regression tests for elps#370.
//
// stampMacroExpansion walks a macro's expansion and replaces every SYNTHETIC
// source location -- nil, or Pos < 0, i.e. lisp.nativeSource's "<native code>"
// -- with the macro's call site, attaching a MacroExpansionInfo as well when a
// debugger is attached.  Both are writes to *LVal fields.
//
// The walk is meant to reach only nodes the macro CREATED.  It reached parser
// output too, because the reader emitted two nodes with synthetic locations of
// its own: the "lisp:function" head behind #' and, in the nil-source edge
// case, the "lisp:expr" head behind #^.  Macro arguments are not evaluated, so
// a form containing #' arrives at the macro's parameters as the CALLER'S OWN
// parse-tree nodes and is spliced into the expansion -- and the stamp then
// wrote into the caller's parse tree.
//
// That tree is not private to a single evaluation:
//
//   - LEnv.load evaluates the reader's expressions directly.  It does not
//     copy (lisp.TextLoader does; the Load* entry points do not).
//   - A function body IS the parse tree it was defined from, re-entered on
//     every call.
//   - A *Package -- LFun bodies included -- is shared by pointer across the
//     per-request environments an embedder derives from one registry.  That is
//     the same sharing elps#397 turned into "fatal error: concurrent map read
//     and map write".
//
// So two environments expanding the same macro call wrote to one *LVal.Source
// word with no synchronisation between them.  Before the fix `go test -race`
// reported it at macro.go:276 (the read) and macro.go:277 (the write).
//
// The fix is in the reader: parser/rdparser gives the synthesized #'/#^ heads
// the real location of the prefix token they stand for, which empties the set
// of shared nodes the stamp can reach.  parser/rdparser's
// TestParserEmitsNoSyntheticSourceLocations pins that end; these tests pin the
// consequence at the end that was corrupted.

package lisp_test

import (
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// dormantDebugger is the cheapest thing that satisfies lisp.Debugger.  Only
// its presence matters here: macroCall builds a MacroExpansionContext whenever
// Runtime.Debugger is non-nil, and that context is what stampMacroExpansion
// turns into a MacroExpansionInfo on every node it claims.  It is never asked
// to do anything, so every hook is dormant.
type dormantDebugger struct{}

func (dormantDebugger) IsEnabled() bool                                { return false }
func (dormantDebugger) OnEval(*lisp.LEnv, *lisp.LVal) bool             { return false }
func (dormantDebugger) OnFunEntry(*lisp.LEnv, *lisp.LVal, *lisp.LEnv)  {}
func (dormantDebugger) OnFunReturn(*lisp.LEnv, *lisp.LVal, *lisp.LVal) {}
func (dormantDebugger) AfterFunCall(*lisp.LEnv) bool                   { return false }
func (dormantDebugger) OnError(*lisp.LEnv, *lisp.LVal) bool            { return false }
func (dormantDebugger) WaitIfPaused(*lisp.LEnv, *lisp.LVal) lisp.DebugAction {
	return lisp.DebugContinue
}

// stampPrelude defines the smallest macro that reproduces the defect: one that
// returns its argument unchanged, so the expansion IS the caller's node.
//
// Nothing about the macro is exotic.  Any macro that splices an argument into
// its expansion -- which is what quasiquote/unquote is for, and therefore what
// nearly every macro does -- puts the caller's nodes in the stamp's path.
const stampPrelude = `
(defmacro ident (x) x)
(defun target () 1)`

// sharedFormSrc is a call to that macro whose argument contains a #' function
// reference: the form whose head the reader used to leave on "<native code>".
const sharedFormSrc = `(ident #'target)`

func newStampEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnvRuntime(&lisp.Runtime{
		Registry: lisp.NewRegistry(),
		Stack:    &lisp.CallStack{},
		Reader:   parser.NewReader(),
	})
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.NotEqual(t, lisp.LError, lisplib.LoadLibrary(env).Type)
	require.NotEqual(t, lisp.LError, env.InPackage(lisp.String(lisp.DefaultUserPackage)).Type)
	require.NotEqual(t, lisp.LError, env.LoadString("prelude.lisp", stampPrelude).Type)
	return env
}

func parseOneForm(t testing.TB, name, src string) *lisp.LVal {
	t.Helper()
	exprs, err := rdparser.NewReader().Read(name, strings.NewReader(src))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	return exprs[0]
}

// findSymbol returns the first node named name in v's tree.
func findSymbol(v *lisp.LVal, name string) *lisp.LVal {
	if v.Type == lisp.LSymbol && v.Str == name {
		return v
	}
	for _, c := range v.Cells {
		if got := findSymbol(c, name); got != nil {
			return got
		}
	}
	return nil
}

// snapshotSources records the Source pointer of every node in v, keyed by the
// node pointer, so a later comparison names the node that moved.
func snapshotSources(v *lisp.LVal, out map[*lisp.LVal]*struct {
	src  string
	name string
},
) {
	if v == nil {
		return
	}
	loc := "<nil>"
	if v.Source != nil {
		loc = v.Source.String()
	}
	out[v] = &struct {
		src  string
		name string
	}{loc, v.Type.String() + " " + v.Str}
	for _, c := range v.Cells {
		snapshotSources(c, out)
	}
}

// TestMacroExpansionDoesNotRestampCallerParseTree is the deterministic arm.
//
// Evaluating a form must not move the positions recorded in it.  Those
// positions are what every error message, every stack frame, and every LSP
// range is computed from, and the parse tree outlives the evaluation.
//
// Pre-fix this failed on the (lisp:function target) head: "<native code>"
// became "shared.lisp:1:8", the location of the `ident` call, which is neither
// where the reference is nor where anything the interpreter synthesised is.
func TestMacroExpansionDoesNotRestampCallerParseTree(t *testing.T) {
	env := newStampEnv(t)
	form := parseOneForm(t, "shared.lisp", sharedFormSrc)

	type entry = struct{ src, name string }
	before := map[*lisp.LVal]*entry{}
	snapshotSources(form, before)

	res := env.Eval(form)
	require.NotEqual(t, lisp.LError, res.Type, "%v", res)

	after := map[*lisp.LVal]*entry{}
	snapshotSources(form, after)

	require.Len(t, after, len(before))
	for node, was := range before {
		now, ok := after[node]
		require.True(t, ok)
		assert.Equal(t, was.src, now.src,
			"evaluation moved the source location of %s in the caller's parse tree", was.name)
	}
}

// TestMacroExpansionDoesNotStampMacroExpansionInfo is the other half of the
// same write.  With a debugger attached the stamp also allocates a
// MacroExpansionInfo onto each node it claims, which is a second field written
// into the shared tree -- and one that makes the node report itself to the
// debugger as macro-generated when the user wrote it by hand.
func TestMacroExpansionDoesNotStampMacroExpansionInfo(t *testing.T) {
	env := newStampEnv(t)
	env.Runtime.Debugger = dormantDebugger{}
	form := parseOneForm(t, "shared.lisp", sharedFormSrc)
	head := findSymbol(form, "lisp:function")
	require.NotNil(t, head, "the #' head should be in the parse tree")

	res := env.Eval(form)
	require.NotEqual(t, lisp.LError, res.Type, "%v", res)

	assert.Nil(t, head.MacroExpansion,
		"macro expansion attached MacroExpansionInfo to a node the reader produced")
}

// TestMacroExpansionSharedParseTreeIsRaceFree is the concurrency arm, and the
// one that reproduces the defect as reported.  It is meaningful only under
// `go test -race`; without it the two goroutines simply both write the same
// word and the test passes either way.
//
// Two INDEPENDENT environments -- separate runtimes, separate registries,
// separate stacks -- evaluate the same parsed forms.  Nothing is shared
// between them except the AST, which is exactly the embedder shape the issue
// describes: a parse cache handing one tree to per-request environments.
//
// Each form is used once.  The write settles (a node that has acquired a real
// location is skipped by every later stamp), so re-using one form would race
// only on the first pass and then go quiet; a fresh form per iteration keeps
// presenting the first write, which is the racing one.
func TestMacroExpansionSharedParseTreeIsRaceFree(t *testing.T) {
	const forms = 200

	envs := []*lisp.LEnv{newStampEnv(t), newStampEnv(t)}
	shared := make([]*lisp.LVal, forms)
	for i := range shared {
		shared[i] = parseOneForm(t, "shared.lisp", sharedFormSrc)
	}

	start := make(chan struct{})
	var wg sync.WaitGroup
	for _, env := range envs {
		wg.Add(1)
		go func(env *lisp.LEnv) {
			defer wg.Done()
			<-start
			for _, form := range shared {
				env.Eval(form)
			}
		}(env)
	}
	close(start)
	wg.Wait()

	// The tree the two of them walked must still say what the reader said.
	for i, form := range shared {
		head := findSymbol(form, "lisp:function")
		require.NotNil(t, head)
		require.NotNil(t, head.Source)
		assert.GreaterOrEqualf(t, head.Source.Pos, 0,
			"form %d: the #' head lost its real source location", i)
		assert.Equalf(t, "shared.lisp", head.Source.File,
			"form %d: the #' head was relocated", i)
	}
}
