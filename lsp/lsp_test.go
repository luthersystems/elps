// Copyright © 2024 The ELPS authors

package lsp

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// testServer creates a server with a standard library registry for testing.
func testServer() *Server {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	if !rc.IsNil() {
		panic("failed to initialize env: " + rc.Str)
	}
	rc = lisplib.LoadLibrary(env)
	if !rc.IsNil() {
		panic("failed to load library: " + rc.Str)
	}
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if !rc.IsNil() {
		panic("failed to set package: " + rc.Str)
	}

	return New(WithEnv(env))
}

// openDoc opens a document in the test server and returns it.
func openDoc(s *Server, uri, content string) *Document {
	return s.docs.Open(uri, 1, content)
}

// setTestAnalysisCfg sets the analysis config on a test server and marks the
// workspace index as done so ensureWorkspaceIndex won't overwrite it.
func setTestAnalysisCfg(s *Server, cfg *analysis.Config) {
	s.indexOnce.Do(func() {}) // prevent ensureWorkspaceIndex from running
	s.analysisCfg = cfg
}

// mockContext returns a minimal glsp.Context for testing.
func mockContext() *glsp.Context {
	return &glsp.Context{
		Notify: func(method string, params any) {},
	}
}

// capturingContext returns a context that captures published diagnostics.
func capturingContext() (*glsp.Context, *[]*protocol.PublishDiagnosticsParams) {
	var captured []*protocol.PublishDiagnosticsParams
	ctx := &glsp.Context{
		Notify: func(method string, params any) {
			if method == protocol.ServerTextDocumentPublishDiagnostics {
				captured = append(captured, params.(*protocol.PublishDiagnosticsParams))
			}
		},
	}
	return ctx, &captured
}

// completionLabels extracts labels from a completion result.
func completionLabels(t *testing.T, result any) []string {
	t.Helper()
	require.NotNil(t, result, "completion result should not be nil")
	items, ok := result.([]protocol.CompletionItem)
	require.True(t, ok, "completion result should be []CompletionItem, got %T", result)
	labels := make([]string, len(items))
	for i, item := range items {
		labels[i] = item.Label
	}
	return labels
}

// --- Position conversion tests ---

func TestPositionConversion(t *testing.T) {
	t.Run("1-based to 0-based", func(t *testing.T) {
		pos := elpsToLSPPosition(&token.Location{File: "test.lisp", Line: 1, Col: 1})
		assert.Equal(t, protocol.UInteger(0), pos.Line)
		assert.Equal(t, protocol.UInteger(0), pos.Character)
	})
	t.Run("multi-digit", func(t *testing.T) {
		pos := elpsToLSPPosition(&token.Location{File: "test.lisp", Line: 5, Col: 10})
		assert.Equal(t, protocol.UInteger(4), pos.Line)
		assert.Equal(t, protocol.UInteger(9), pos.Character)
	})
	t.Run("zero values clamp", func(t *testing.T) {
		pos := elpsToLSPPosition(&token.Location{File: "test.lisp", Line: 0, Col: 0})
		assert.Equal(t, protocol.UInteger(0), pos.Line)
		assert.Equal(t, protocol.UInteger(0), pos.Character)
	})
}

func TestPositionConversionWithEnd(t *testing.T) {
	loc := &token.Location{
		File:    "test.lisp",
		Line:    3,
		Col:     5,
		EndLine: 3,
		EndCol:  10,
	}
	r := elpsToLSPRange(loc, 5)
	assert.Equal(t, protocol.UInteger(2), r.Start.Line)
	assert.Equal(t, protocol.UInteger(4), r.Start.Character)
	assert.Equal(t, protocol.UInteger(2), r.End.Line)
	assert.Equal(t, protocol.UInteger(9), r.End.Character)
}

func TestPositionRangeWithoutEnd(t *testing.T) {
	loc := &token.Location{File: "test.lisp", Line: 1, Col: 1}
	r := elpsToLSPRange(loc, 5)
	assert.Equal(t, protocol.UInteger(0), r.Start.Line)
	assert.Equal(t, protocol.UInteger(0), r.Start.Character)
	assert.Equal(t, protocol.UInteger(0), r.End.Line)
	assert.Equal(t, protocol.UInteger(5), r.End.Character)
}

// --- Word at position tests ---

func TestWordAtPosition(t *testing.T) {
	content := "(defun my-func (x y)\n  (+ x y))"
	t.Run("middle of word", func(t *testing.T) {
		assert.Equal(t, "defun", wordAtPosition(content, 0, 1))
		assert.Equal(t, "my-func", wordAtPosition(content, 0, 7))
	})
	t.Run("single char symbol", func(t *testing.T) {
		assert.Equal(t, "+", wordAtPosition(content, 1, 3))
		assert.Equal(t, "x", wordAtPosition(content, 1, 5))
	})
	t.Run("on paren", func(t *testing.T) {
		assert.Equal(t, "", wordAtPosition(content, 0, 0))
	})
	t.Run("end of line", func(t *testing.T) {
		// Cursor at end of "(my-" should find "my-"
		partial := "(my-add"
		assert.Equal(t, "my-add", wordAtPosition(partial, 0, 7))
	})
	t.Run("empty content", func(t *testing.T) {
		assert.Equal(t, "", wordAtPosition("", 0, 0))
	})
	t.Run("out of bounds", func(t *testing.T) {
		assert.Equal(t, "", wordAtPosition("hello", -1, 0))
		assert.Equal(t, "", wordAtPosition("hello", 5, 0))
	})
	t.Run("special chars", func(t *testing.T) {
		assert.Equal(t, "*global*", wordAtPosition("*global*", 0, 0))
		assert.Equal(t, "set!", wordAtPosition("(set! x 1)", 0, 1))
	})
}

// --- Document store tests ---

func TestDocumentStore(t *testing.T) {
	t.Run("Open", func(t *testing.T) {
		store := NewDocumentStore()
		doc := store.Open("file:///test.lisp", 1, "(+ 1 2)")
		require.NotNil(t, doc)
		assert.Equal(t, "(+ 1 2)", doc.Content)
		assert.NotNil(t, doc.ast)
	})
	t.Run("Get", func(t *testing.T) {
		store := NewDocumentStore()
		store.Open("file:///test.lisp", 1, "(+ 1 2)")
		got := store.Get("file:///test.lisp")
		require.NotNil(t, got)
		assert.Equal(t, "(+ 1 2)", got.Content)
		assert.Nil(t, store.Get("file:///nonexistent.lisp"))
	})
	t.Run("Change", func(t *testing.T) {
		store := NewDocumentStore()
		store.Open("file:///test.lisp", 1, "(+ 1 2)")
		changed := store.Change("file:///test.lisp", 2, "(+ 3 4)")
		assert.Equal(t, "(+ 3 4)", changed.Content)
		assert.Equal(t, int32(2), changed.Version)
		assert.Nil(t, changed.analysis, "analysis cache should be cleared on change")
	})
	t.Run("Close", func(t *testing.T) {
		store := NewDocumentStore()
		store.Open("file:///test.lisp", 1, "(+ 1 2)")
		store.Close("file:///test.lisp")
		assert.Nil(t, store.Get("file:///test.lisp"))
	})
}

func TestDocumentParse(t *testing.T) {
	store := NewDocumentStore()
	doc := store.Open("file:///test.lisp", 1, "(defun add (x y) (+ x y))")
	require.NotNil(t, doc)
	assert.Empty(t, doc.parseErrors)
	assert.Len(t, doc.ast, 1)
}

func TestDocumentParseError(t *testing.T) {
	store := NewDocumentStore()
	doc := store.Open("file:///test.lisp", 1, "(defun add (x y")
	require.NotNil(t, doc)
	require.Len(t, doc.parseErrors, 1)
	var ev *lisp.ErrorVal
	require.ErrorAs(t, doc.parseErrors[0], &ev)
	assert.Equal(t, lisp.CondUnmatchedSyntax, ev.Condition())
}

func TestDocumentFaultTolerantParse(t *testing.T) {
	store := NewDocumentStore()
	// Two valid expressions followed by an incomplete one.
	doc := store.Open("file:///test.lisp", 1, "(defun a () 1)\n(defun b () 2)\n(incomplete")
	require.NotNil(t, doc)
	require.Len(t, doc.parseErrors, 1, "should record one parse error")
	assert.Len(t, doc.ast, 2, "should recover the two valid expressions")
	assert.Equal(t, "(defun a () 1)", doc.ast[0].String())
	assert.Equal(t, "(defun b () 2)", doc.ast[1].String())
}

// --- Diagnostics tests ---

func TestDiagnosticsOnOpen_ValidCode(t *testing.T) {
	s := testServer()
	ctx, captured := capturingContext()

	err := s.textDocumentDidOpen(ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{
			URI:        "file:///test.lisp",
			LanguageID: "elps",
			Version:    1,
			Text:       "(defun add (x y) (+ x y))",
		},
	})
	require.NoError(t, err)
	require.Len(t, *captured, 1)
	pub := (*captured)[0]
	assert.Equal(t, "file:///test.lisp", pub.URI)
	// Valid code should produce no error diagnostics from the parser.
	for _, d := range pub.Diagnostics {
		if d.Source != nil && *d.Source == "elps" {
			t.Errorf("unexpected parse diagnostic for valid code: %s", d.Message)
		}
	}
}

func TestDiagnosticsOnParseError(t *testing.T) {
	s := testServer()
	ctx, captured := capturingContext()

	err := s.textDocumentDidOpen(ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{
			URI:        "file:///test.lisp",
			LanguageID: "elps",
			Version:    1,
			Text:       "(defun broken (x y",
		},
	})
	require.NoError(t, err)
	require.Len(t, *captured, 1)
	pub := (*captured)[0]
	require.NotEmpty(t, pub.Diagnostics, "parse error should produce diagnostics")
	assert.Equal(t, protocol.DiagnosticSeverityError, *pub.Diagnostics[0].Severity)
}

func TestDiagnosticsParseErrorPosition(t *testing.T) {
	s := testServer()
	ctx, captured := capturingContext()

	// Open document with an unclosed paren — parse error should have a real position.
	err := s.textDocumentDidOpen(ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{
			URI:     "file:///test.lisp",
			Version: 1,
			Text:    "(defun broken (x y",
		},
	})
	require.NoError(t, err)
	require.Len(t, *captured, 1)
	pub := (*captured)[0]
	require.NotEmpty(t, pub.Diagnostics)

	// Find the parse error diagnostic.
	var parseDiag *protocol.Diagnostic
	for i, d := range pub.Diagnostics {
		if d.Source != nil && *d.Source == "elps" {
			parseDiag = &pub.Diagnostics[i]
			break
		}
	}
	require.NotNil(t, parseDiag, "should have a parse error diagnostic")
	// The range should NOT be at 0:0 — the parser error should have a real position.
	assert.True(t,
		parseDiag.Range.Start.Line > 0 || parseDiag.Range.Start.Character > 0,
		"parse error diagnostic should have a non-zero position, got %v", parseDiag.Range)
}

func TestDiagnosticsOnClose_Cleared(t *testing.T) {
	s := testServer()
	openCtx, _ := capturingContext()

	// Open a file with an error to generate diagnostics.
	err := s.textDocumentDidOpen(openCtx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{
			URI:     "file:///test.lisp",
			Version: 1,
			Text:    "(defun broken",
		},
	})
	require.NoError(t, err)

	// Close should clear diagnostics.
	closeCtx, closeCaptured := capturingContext()
	s.captureNotify(closeCtx)
	err = s.textDocumentDidClose(closeCtx, &protocol.DidCloseTextDocumentParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
	})
	require.NoError(t, err)
	require.Len(t, *closeCaptured, 1)
	assert.Empty(t, (*closeCaptured)[0].Diagnostics, "close should clear diagnostics")
	assert.Nil(t, s.docs.Get("file:///test.lisp"), "document should be removed from store")
}

func TestDiagnosticsOnSave_Immediate(t *testing.T) {
	s := testServer()
	ctx, captured := capturingContext()

	// Open a valid document.
	err := s.textDocumentDidOpen(ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{
			URI:     "file:///test.lisp",
			Version: 1,
			Text:    "(+ 1 2)",
		},
	})
	require.NoError(t, err)

	// Save should publish diagnostics immediately.
	before := len(*captured)
	err = s.textDocumentDidSave(ctx, &protocol.DidSaveTextDocumentParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
	})
	require.NoError(t, err)
	assert.Greater(t, len(*captured), before, "save should trigger immediate diagnostics publish")
}

func TestDocumentFaultTolerantRecoveryBeyondError(t *testing.T) {
	store := NewDocumentStore()
	// Mismatched bracket in middle, valid code AFTER the error is recovered.
	doc := store.Open("file:///test.lisp", 1, "(defun a () 1)\n(broken]\n(defun b () 2)")
	require.NotNil(t, doc)
	require.Len(t, doc.parseErrors, 1, "should record one parse error")
	assert.Len(t, doc.ast, 2, "should recover both valid expressions around the error")
	assert.Equal(t, "(defun a () 1)", doc.ast[0].String())
	assert.Equal(t, "(defun b () 2)", doc.ast[1].String())
}

func TestDiagnosticsMultipleParseErrors(t *testing.T) {
	s := testServer()
	ctx, captured := capturingContext()

	// Two mismatched bracket errors with valid code between.
	err := s.textDocumentDidOpen(ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{
			URI:     "file:///test.lisp",
			Version: 1,
			Text:    "(err1]\n(+ 1 2)\n(err2]",
		},
	})
	require.NoError(t, err)
	require.Len(t, *captured, 1)
	pub := (*captured)[0]

	// Collect parse error diagnostics.
	var parseDiags []protocol.Diagnostic
	for _, d := range pub.Diagnostics {
		if d.Source != nil && *d.Source == "elps" {
			parseDiags = append(parseDiags, d)
		}
	}
	require.Len(t, parseDiags, 2, "should publish one diagnostic per parse error")
	// The two errors are on different lines; verify distinct positions.
	assert.NotEqual(t, parseDiags[0].Range.Start.Line, parseDiags[1].Range.Start.Line,
		"two errors on different lines should have different diagnostic positions")
}

// --- Hover tests ---

func TestHoverOnDefun(t *testing.T) {
	s := testServer()
	content := `(defun greet (name)
  "Say hello to someone."
  (concat "Hello, " name))`
	openDoc(s, "file:///test.lisp", content)

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 7}, // on "greet"
		},
	})
	require.NoError(t, err)
	require.NotNil(t, hover, "hover on user-defined function should not be nil")
	mc, ok := hover.Contents.(protocol.MarkupContent)
	require.True(t, ok, "hover contents should be MarkupContent")
	assert.Contains(t, mc.Value, "greet")
	assert.Contains(t, mc.Value, "function")
}

func TestHoverOnBuiltin(t *testing.T) {
	s := testServer()
	content := "(map (lambda (x) (+ x 1)) '(1 2 3))"
	openDoc(s, "file:///test.lisp", content)

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 1}, // on "map"
		},
	})
	require.NoError(t, err)
	require.NotNil(t, hover, "hover on builtin should not be nil")
	mc, ok := hover.Contents.(protocol.MarkupContent)
	require.True(t, ok, "hover contents should be MarkupContent")
	assert.Contains(t, mc.Value, "map")
}

func TestHoverOnEmpty(t *testing.T) {
	s := testServer()
	openDoc(s, "file:///test.lisp", "(+ 1 2)")

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 0}, // on '('
		},
	})
	require.NoError(t, err)
	assert.Nil(t, hover, "hover on non-symbol should be nil")
}

// --- Definition tests ---

func TestDefinition(t *testing.T) {
	s := testServer()
	content := `(defun add (x y) (+ x y))
(add 1 2)`
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentDefinition(mockContext(), &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 1, Character: 1}, // on "add" call
		},
	})
	require.NoError(t, err)
	require.NotNil(t, result, "definition of user function should not be nil")
	loc, ok := result.(protocol.Location)
	require.True(t, ok, "definition result should be Location, got %T", result)
	assert.Equal(t, "file:///test.lisp", loc.URI)
	assert.Equal(t, protocol.UInteger(0), loc.Range.Start.Line, "definition should point to line 0")
}

func TestDefinitionBuiltinReturnsNil(t *testing.T) {
	s := testServer()
	openDoc(s, "file:///test.lisp", "(map identity '(1 2 3))")

	result, err := s.textDocumentDefinition(mockContext(), &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 1}, // on "map"
		},
	})
	require.NoError(t, err)
	// Builtins have no navigable source; nil is the correct response.
	assert.Nil(t, result, "definition of builtin should be nil")
}

// --- References tests ---

func TestReferences(t *testing.T) {
	s := testServer()
	content := `(defun double (x) (* x 2))
(double 5)
(double 10)`
	openDoc(s, "file:///test.lisp", content)

	locs, err := s.textDocumentReferences(mockContext(), &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 7}, // on "double" def
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: true},
	})
	require.NoError(t, err)
	require.NotNil(t, locs, "references should not be nil")
	// Expect: 1 declaration + 2 call sites = 3.
	assert.Len(t, locs, 3, "should have declaration + 2 call sites")

	// Verify locations span the expected lines.
	lines := map[protocol.UInteger]bool{}
	for _, loc := range locs {
		lines[loc.Range.Start.Line] = true
	}
	assert.True(t, lines[0], "should include definition on line 0")
	assert.True(t, lines[1], "should include call on line 1")
	assert.True(t, lines[2], "should include call on line 2")
}

func TestReferencesExcludeDeclaration(t *testing.T) {
	s := testServer()
	content := `(defun f () nil)
(f)
(f)`
	openDoc(s, "file:///test.lisp", content)

	locs, err := s.textDocumentReferences(mockContext(), &protocol.ReferenceParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 7}, // on "f" def
		},
		Context: protocol.ReferenceContext{IncludeDeclaration: false},
	})
	require.NoError(t, err)
	require.NotNil(t, locs)
	// Without declaration, should only have 2 call sites.
	assert.Len(t, locs, 2, "should have only call sites without declaration")
}

// --- Document symbols tests ---

func TestDocumentSymbols(t *testing.T) {
	s := testServer()
	content := `(defun foo () "Doc for foo" nil)
(defun bar (x) x)
(set *global* 42)`
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentDocumentSymbol(mockContext(), &protocol.DocumentSymbolParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
	})
	require.NoError(t, err)
	require.NotNil(t, result, "document symbols should not be nil")
	symbols, ok := result.([]protocol.DocumentSymbol)
	require.True(t, ok, "result should be []DocumentSymbol, got %T", result)

	names := make([]string, len(symbols))
	for i, sym := range symbols {
		names[i] = sym.Name
	}
	assert.Contains(t, names, "foo")
	assert.Contains(t, names, "bar")

	// Verify symbol kinds.
	for _, sym := range symbols {
		if sym.Name == "foo" || sym.Name == "bar" {
			assert.Equal(t, protocol.SymbolKindFunction, sym.Kind, "defun should be Function kind")
		}
	}
}

// --- Completion tests ---

func TestCompletion(t *testing.T) {
	s := testServer()
	content := `(defun my-add (x y) (+ x y))
(defun my-sub (x y) (- x y))
(my-`
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 2, Character: 4}, // after "(my-"
		},
	})
	require.NoError(t, err)
	labels := completionLabels(t, result)
	assert.Contains(t, labels, "my-add")
	assert.Contains(t, labels, "my-sub")
}

func TestCompletionPackageQualified(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"math": {
				{Name: "abs", Kind: analysis.SymFunction},
				{Name: "floor", Kind: analysis.SymFunction},
				{Name: "ceil", Kind: analysis.SymFunction},
			},
		},
	})

	content := "(math:"
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 6}, // after "math:"
		},
	})
	require.NoError(t, err)
	labels := completionLabels(t, result)
	assert.Contains(t, labels, "math:abs")
	assert.Contains(t, labels, "math:floor")
	assert.Contains(t, labels, "math:ceil")
	assert.Len(t, labels, 3, "should return exactly the 3 math exports")
}

// --- Rename tests ---

func TestRename(t *testing.T) {
	s := testServer()
	content := `(defun add (x y) (+ x y))
(add 1 2)
(add 3 4)`
	openDoc(s, "file:///test.lisp", content)

	edit, err := s.textDocumentRename(mockContext(), &protocol.RenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 7}, // on "add" def
		},
		NewName: "sum",
	})
	require.NoError(t, err)
	require.NotNil(t, edit, "rename should return a workspace edit")
	require.NotNil(t, edit.Changes, "workspace edit should have changes")

	edits := edit.Changes["file:///test.lisp"]
	require.Len(t, edits, 3, "should rename at definition + 2 call sites")
	for _, e := range edits {
		assert.Equal(t, "sum", e.NewText)
	}

	// Verify the edit positions cover the expected lines.
	lines := map[protocol.UInteger]bool{}
	for _, e := range edits {
		lines[e.Range.Start.Line] = true
	}
	assert.True(t, lines[0], "should include definition on line 0")
	assert.True(t, lines[1], "should include call on line 1")
	assert.True(t, lines[2], "should include call on line 2")
}

func TestRenameBuiltinRejected(t *testing.T) {
	s := testServer()
	content := "(map (lambda (x) x) '(1 2 3))"
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentPrepareRename(mockContext(), &protocol.PrepareRenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 1}, // on "map"
		},
	})
	require.NoError(t, err, "prepareRename should not error (returns nil per LSP spec)")
	assert.Nil(t, result, "renaming a builtin should return nil")
}

func TestPrepareRenameUserFunction(t *testing.T) {
	s := testServer()
	content := "(defun my-func () nil)"
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentPrepareRename(mockContext(), &protocol.PrepareRenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 7}, // on "my-func"
		},
	})
	require.NoError(t, err)
	require.NotNil(t, result, "prepare rename for user function should succeed")
	rp, ok := result.(*protocol.RangeWithPlaceholder)
	require.True(t, ok, "result should be RangeWithPlaceholder")
	assert.Equal(t, "my-func", rp.Placeholder)
}

// --- Unit tests for helpers ---

func TestFormatSignature(t *testing.T) {
	t.Run("required params", func(t *testing.T) {
		sig := &analysis.Signature{
			Params: []lisp.ParamInfo{
				{Name: "x", Kind: lisp.ParamRequired},
				{Name: "y", Kind: lisp.ParamRequired},
			},
		}
		assert.Equal(t, "(x y)", formatSignature(sig))
	})
	t.Run("optional param", func(t *testing.T) {
		sig := &analysis.Signature{
			Params: []lisp.ParamInfo{
				{Name: "x", Kind: lisp.ParamRequired},
				{Name: "y", Kind: lisp.ParamOptional},
			},
		}
		assert.Equal(t, "(x &optional y)", formatSignature(sig))
	})
	t.Run("rest param", func(t *testing.T) {
		sig := &analysis.Signature{
			Params: []lisp.ParamInfo{
				{Name: "items", Kind: lisp.ParamRest},
			},
		}
		assert.Equal(t, "(&rest items)", formatSignature(sig))
	})
	t.Run("key param", func(t *testing.T) {
		sig := &analysis.Signature{
			Params: []lisp.ParamInfo{
				{Name: "x", Kind: lisp.ParamRequired},
				{Name: "verbose", Kind: lisp.ParamKey},
			},
		}
		assert.Equal(t, "(x &key verbose)", formatSignature(sig))
	})
	t.Run("nil signature", func(t *testing.T) {
		assert.Equal(t, "()", formatSignature(nil))
	})
	t.Run("empty params", func(t *testing.T) {
		sig := &analysis.Signature{Params: nil}
		assert.Equal(t, "()", formatSignature(sig))
	})
}

func TestMapSymbolKind(t *testing.T) {
	assert.Equal(t, protocol.SymbolKindFunction, mapSymbolKind(analysis.SymFunction))
	assert.Equal(t, protocol.SymbolKindFunction, mapSymbolKind(analysis.SymMacro))
	assert.Equal(t, protocol.SymbolKindVariable, mapSymbolKind(analysis.SymVariable))
	assert.Equal(t, protocol.SymbolKindVariable, mapSymbolKind(analysis.SymParameter))
	assert.Equal(t, protocol.SymbolKindClass, mapSymbolKind(analysis.SymType))
	assert.Equal(t, protocol.SymbolKindOperator, mapSymbolKind(analysis.SymSpecialOp))
	assert.Equal(t, protocol.SymbolKindFunction, mapSymbolKind(analysis.SymBuiltin))
}

func TestMapCompletionItemKind(t *testing.T) {
	assert.Equal(t, protocol.CompletionItemKindFunction, mapCompletionItemKind(analysis.SymFunction))
	assert.Equal(t, protocol.CompletionItemKindFunction, mapCompletionItemKind(analysis.SymBuiltin))
	assert.Equal(t, protocol.CompletionItemKindKeyword, mapCompletionItemKind(analysis.SymMacro))
	assert.Equal(t, protocol.CompletionItemKindVariable, mapCompletionItemKind(analysis.SymVariable))
	assert.Equal(t, protocol.CompletionItemKindVariable, mapCompletionItemKind(analysis.SymParameter))
	assert.Equal(t, protocol.CompletionItemKindKeyword, mapCompletionItemKind(analysis.SymSpecialOp))
	assert.Equal(t, protocol.CompletionItemKindClass, mapCompletionItemKind(analysis.SymType))
}

func TestURIConversion(t *testing.T) {
	assert.Equal(t, "/path/to/file.lisp", uriToPath("file:///path/to/file.lisp"))
	assert.Equal(t, "file:///path/to/file.lisp", pathToURI("/path/to/file.lisp"))
	// Non-URI input returned unchanged.
	assert.Equal(t, "relative/path", uriToPath("relative/path"))
	assert.Equal(t, "relative/path", pathToURI("relative/path"))

	// Percent-encoded spaces.
	assert.Equal(t, "/path/to/my file.lisp", uriToPath("file:///path/to/my%20file.lisp"))
	// Percent-encoded parentheses.
	assert.Equal(t, "/path/to/(test).lisp", uriToPath("file:///path/to/%28test%29.lisp"))
	// Round-trip: path with spaces.
	spacePath := "/path/to/my file.lisp"
	assert.Equal(t, spacePath, uriToPath(pathToURI(spacePath)))
	// Round-trip: path with parentheses.
	parenPath := "/path/to/(test).lisp"
	assert.Equal(t, parenPath, uriToPath(pathToURI(parenPath)))
}

func TestSplitPackageQualified(t *testing.T) {
	pkg, partial, ok := splitPackageQualified("math:abs")
	assert.True(t, ok)
	assert.Equal(t, "math", pkg)
	assert.Equal(t, "abs", partial)

	pkg, partial, ok = splitPackageQualified("math:")
	assert.True(t, ok)
	assert.Equal(t, "math", pkg)
	assert.Equal(t, "", partial)

	_, _, ok = splitPackageQualified(":keyword")
	assert.False(t, ok)

	_, _, ok = splitPackageQualified("nocolon")
	assert.False(t, ok)
}

// --- Formatting tests ---

func TestFormatting(t *testing.T) {
	s := testServer()
	// Badly formatted: missing indentation.
	content := "(defun add (x y)\n(+ x y))"
	openDoc(s, "file:///test.lisp", content)

	edits, err := s.textDocumentFormatting(mockContext(), &protocol.DocumentFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
		Options:      protocol.FormattingOptions{},
	})
	require.NoError(t, err)
	require.NotNil(t, edits, "badly formatted code should produce edits")
	assert.Len(t, edits, 1, "should return a single whole-document edit")
	assert.Contains(t, edits[0].NewText, "  (+", "formatted output should indent body")
}

func TestFormattingAlreadyFormatted(t *testing.T) {
	s := testServer()
	// Already formatted (indented correctly with trailing newline).
	content := "(defun add (x y)\n  (+ x y))\n"
	openDoc(s, "file:///test.lisp", content)

	edits, err := s.textDocumentFormatting(mockContext(), &protocol.DocumentFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
		Options:      protocol.FormattingOptions{},
	})
	require.NoError(t, err)
	assert.Nil(t, edits, "already formatted code should produce no edits")
}

func TestFormattingParseError(t *testing.T) {
	s := testServer()
	// Incomplete code — can't format.
	content := "(defun broken (x y"
	openDoc(s, "file:///test.lisp", content)

	edits, err := s.textDocumentFormatting(mockContext(), &protocol.DocumentFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
		Options:      protocol.FormattingOptions{},
	})
	require.NoError(t, err, "parse error should not produce an RPC error")
	assert.Nil(t, edits, "parse error should produce nil edits")
}

func TestFormattingUnknownDocument(t *testing.T) {
	s := testServer()

	edits, err := s.textDocumentFormatting(mockContext(), &protocol.DocumentFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///unknown.lisp"},
		Options:      protocol.FormattingOptions{},
	})
	require.NoError(t, err)
	assert.Nil(t, edits, "unknown document should produce nil edits")
}

func TestFormattingTabSize(t *testing.T) {
	s := testServer()
	content := "(defun add (x y)\n(+ x y))"
	openDoc(s, "file:///test.lisp", content)

	edits, err := s.textDocumentFormatting(mockContext(), &protocol.DocumentFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
		Options:      protocol.FormattingOptions{"tabSize": float64(4)},
	})
	require.NoError(t, err)
	require.NotNil(t, edits)
	assert.Contains(t, edits[0].NewText, "    (+", "should use 4-space indent from tabSize")
}

// --- Signature help tests ---

func TestSignatureHelp(t *testing.T) {
	s := testServer()
	content := `(defun greet (name greeting)
  "Greet someone."
  (concat greeting " " name))

(greet "world" "hello")`
	openDoc(s, "file:///test.lisp", content)

	// Cursor after "(greet " — should show signature for greet, active param 0.
	result, err := s.textDocumentSignatureHelp(mockContext(), &protocol.SignatureHelpParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 4, Character: 7}, // after "(greet "
		},
	})
	require.NoError(t, err)
	require.NotNil(t, result, "signature help should not be nil for user function")
	require.Len(t, result.Signatures, 1)
	sig := result.Signatures[0]
	assert.Contains(t, sig.Label, "greet")
	assert.Contains(t, sig.Label, "name")
	assert.Contains(t, sig.Label, "greeting")
	assert.Len(t, sig.Parameters, 2)
}

func TestSignatureHelpBuiltin(t *testing.T) {
	s := testServer()
	content := `(map identity '(1 2 3))`
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentSignatureHelp(mockContext(), &protocol.SignatureHelpParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 5}, // after "(map "
		},
	})
	require.NoError(t, err)
	if result != nil {
		require.Len(t, result.Signatures, 1)
		assert.Contains(t, result.Signatures[0].Label, "map")
	}
}

func TestSignatureHelpOutside(t *testing.T) {
	s := testServer()
	content := `(defun add (x y) (+ x y))
; some comment`
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentSignatureHelp(mockContext(), &protocol.SignatureHelpParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 1, Character: 0}, // on comment line
		},
	})
	require.NoError(t, err)
	assert.Nil(t, result, "signature help outside a call should be nil")
}

func TestEnclosingCall(t *testing.T) {
	s := testServer()
	content := "(defun add (x y) (+ x y))\n(add 1 2)"
	openDoc(s, "file:///test.lisp", content)

	doc := s.docs.Get("file:///test.lisp")
	require.NotNil(t, doc)

	// Cursor inside "(add 1 2)" after "add ".
	name, argIdx := enclosingCall(doc.ast, 2, 6)
	assert.Equal(t, "add", name)
	assert.Equal(t, 0, argIdx, "first argument position")
}

func TestSignatureHelpUnknownDocument(t *testing.T) {
	s := testServer()

	result, err := s.textDocumentSignatureHelp(mockContext(), &protocol.SignatureHelpParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///unknown.lisp"},
			Position:     protocol.Position{Line: 0, Character: 5},
		},
	})
	require.NoError(t, err)
	assert.Nil(t, result)
}

func TestExitHandler(t *testing.T) {
	s := testServer()
	var exitCode int
	var exitCalled bool
	s.exitFn = func(code int) {
		exitCode = code
		exitCalled = true
	}

	err := s.exit(mockContext())
	require.NoError(t, err)
	assert.True(t, exitCalled, "exit handler should call exitFn")
	assert.Equal(t, 0, exitCode, "exit should call with code 0")
}

func TestInitializeLifecycle(t *testing.T) {
	s := testServer()

	rootURI := "file:///workspace"
	result, err := s.initialize(mockContext(), &protocol.InitializeParams{
		RootURI: &rootURI,
	})
	require.NoError(t, err)
	require.NotNil(t, result)

	initResult, ok := result.(protocol.InitializeResult)
	require.True(t, ok)
	assert.NotNil(t, initResult.ServerInfo)
	assert.Equal(t, serverName, initResult.ServerInfo.Name)
	assert.Equal(t, "/workspace", s.rootPath)
}

func TestLocContainsCol(t *testing.T) {
	loc := &token.Location{Line: 1, Col: 5}
	assert.True(t, locContainsCol(loc, "defun", 5))
	assert.True(t, locContainsCol(loc, "defun", 9))
	assert.False(t, locContainsCol(loc, "defun", 10))
	assert.False(t, locContainsCol(loc, "defun", 4))
	// Zero col means untracked — never matches.
	assert.False(t, locContainsCol(&token.Location{Line: 1, Col: 0}, "x", 1))
}

// --- Additional tests from gap analysis ---

func TestDiagnosticsIncludeLintWarnings(t *testing.T) {
	s := testServer()
	ctx, captured := capturingContext()

	// (if true) has wrong arity — the linter should flag it.
	err := s.textDocumentDidOpen(ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{
			URI:     "file:///test.lisp",
			Version: 1,
			Text:    "(if true)",
		},
	})
	require.NoError(t, err)
	require.Len(t, *captured, 1)
	pub := (*captured)[0]

	// Should have at least one lint diagnostic.
	var foundLint bool
	for _, d := range pub.Diagnostics {
		if d.Source != nil && *d.Source == "elps-lint" {
			foundLint = true
		}
	}
	assert.True(t, foundLint, "diagnostics should include lint warnings")
}

func TestHoverOnSpecialOperator(t *testing.T) {
	s := testServer()
	content := "(if true 1 2)"
	openDoc(s, "file:///test.lisp", content)

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 1}, // on "if"
		},
	})
	require.NoError(t, err)
	if hover != nil {
		mc, ok := hover.Contents.(protocol.MarkupContent)
		require.True(t, ok)
		assert.Contains(t, mc.Value, "if")
	}
	// hover may be nil if analysis doesn't track "if" as a symbol — both are valid.
}

func TestHoverOnSetVariable(t *testing.T) {
	s := testServer()
	content := "(set 'my-var 42)\nmy-var"
	openDoc(s, "file:///test.lisp", content)

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 1, Character: 0}, // on "my-var" usage
		},
	})
	require.NoError(t, err)
	if hover != nil {
		mc, ok := hover.Contents.(protocol.MarkupContent)
		require.True(t, ok)
		assert.Contains(t, mc.Value, "my-var")
		assert.Contains(t, mc.Value, "variable")
	}
}

func TestCompletionEmptyPrefix(t *testing.T) {
	s := testServer()
	content := "(defun aaa () 1)\n("
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 1, Character: 1}, // after "("
		},
	})
	require.NoError(t, err)
	require.NotNil(t, result, "completion with empty prefix should return all visible symbols")
	items, ok := result.([]protocol.CompletionItem)
	require.True(t, ok)
	// With no prefix filter, should return many symbols (builtins + user-defined).
	assert.Greater(t, len(items), 5, "should return many completions for empty prefix")

	// User-defined "aaa" should be among them.
	labels := make([]string, len(items))
	for i, item := range items {
		labels[i] = item.Label
	}
	assert.Contains(t, labels, "aaa")
}

func TestHoverOnQualifiedSymbol(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"string": {
				{
					Name:      "join",
					Kind:      analysis.SymFunction,
					Package:   "string",
					DocString: "Join a list of strings with a separator.",
					Signature: &analysis.Signature{
						Params: []lisp.ParamInfo{
							{Name: "sep", Kind: lisp.ParamRequired},
							{Name: "lst", Kind: lisp.ParamRequired},
						},
					},
				},
			},
		},
	})

	content := "(string:join \",\" '(\"a\" \"b\" \"c\"))"
	openDoc(s, "file:///test.lisp", content)

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 2}, // on "string:join"
		},
	})
	require.NoError(t, err)
	require.NotNil(t, hover, "hover on qualified symbol should not be nil")
	mc, ok := hover.Contents.(protocol.MarkupContent)
	require.True(t, ok, "hover contents should be MarkupContent")
	assert.Contains(t, mc.Value, "join")
	assert.Contains(t, mc.Value, "function")
	assert.Contains(t, mc.Value, "Join a list of strings with a separator.")
}

func TestHoverOnQualifiedSymbolUnknown(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"string": {
				{Name: "join", Kind: analysis.SymFunction, Package: "string"},
			},
		},
	})

	content := "(string:nonexistent 1)"
	openDoc(s, "file:///test.lisp", content)

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 2},
		},
	})
	require.NoError(t, err)
	assert.Nil(t, hover, "hover on unknown qualified symbol should be nil")
}

func TestCompletionPackageQualifiedDocString(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"math": {
				{
					Name:      "abs",
					Kind:      analysis.SymFunction,
					Package:   "math",
					DocString: "Return the absolute value.",
				},
			},
		},
	})

	content := "(math:"
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 6},
		},
	})
	require.NoError(t, err)
	items, ok := result.([]protocol.CompletionItem)
	require.True(t, ok)
	require.Len(t, items, 1)
	assert.Equal(t, "math:abs", items[0].Label)
	require.NotNil(t, items[0].Documentation, "completion item should have documentation")
	mc, ok := items[0].Documentation.(*protocol.MarkupContent)
	require.True(t, ok, "documentation should be MarkupContent")
	assert.Equal(t, "Return the absolute value.", mc.Value)
}

func TestCompletionPackageQualifiedNoDocString(t *testing.T) {
	s := testServer()
	setTestAnalysisCfg(s, &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"math": {
				{Name: "abs", Kind: analysis.SymFunction, Package: "math"},
			},
		},
	})

	content := "(math:"
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 6},
		},
	})
	require.NoError(t, err)
	items, ok := result.([]protocol.CompletionItem)
	require.True(t, ok)
	require.Len(t, items, 1)
	assert.Equal(t, "math:abs", items[0].Label)
	assert.Nil(t, items[0].Documentation,
		"completion item without docstring should have nil documentation")
}

func TestHoverOnUnknownPackageQualifiedSymbol(t *testing.T) {
	s := testServer()
	// Package "nosuchpkg" doesn't exist in exports at all.
	setTestAnalysisCfg(s, &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{},
	})

	content := "(nosuchpkg:something 1)"
	openDoc(s, "file:///test.lisp", content)

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 2},
		},
	})
	require.NoError(t, err)
	assert.Nil(t, hover, "hover on unknown package should be nil")
}

func TestDeduplicateExports(t *testing.T) {
	t.Run("no duplicates", func(t *testing.T) {
		syms := []analysis.ExternalSymbol{
			{Name: "foo", Kind: analysis.SymFunction},
			{Name: "bar", Kind: analysis.SymFunction},
		}
		result := deduplicateExports(syms)
		require.Len(t, result, 2)
		names := make(map[string]bool)
		for _, sym := range result {
			names[sym.Name] = true
		}
		assert.True(t, names["foo"], "foo should survive deduplication")
		assert.True(t, names["bar"], "bar should survive deduplication")
	})

	t.Run("duplicates removed", func(t *testing.T) {
		syms := []analysis.ExternalSymbol{
			{Name: "foo", Kind: analysis.SymFunction, DocString: "workspace"},
			{Name: "bar", Kind: analysis.SymFunction, DocString: "bar-doc"},
			{Name: "foo", Kind: analysis.SymFunction, DocString: "registry"},
		}
		result := deduplicateExports(syms)
		require.Len(t, result, 2)
		resultMap := make(map[string]analysis.ExternalSymbol)
		for _, sym := range result {
			resultMap[sym.Name] = sym
		}
		assert.Equal(t, "registry", resultMap["foo"].DocString,
			"registry entry should take precedence over workspace")
		assert.Equal(t, "bar-doc", resultMap["bar"].DocString,
			"non-duplicate entry should be preserved")
	})

	t.Run("empty input", func(t *testing.T) {
		result := deduplicateExports(nil)
		assert.Empty(t, result)
	})
}

func TestDocumentStoreAll(t *testing.T) {
	store := NewDocumentStore()
	store.Open("file:///a.lisp", 1, "(+ 1 2)")
	store.Open("file:///b.lisp", 1, "(+ 3 4)")

	all := store.All()
	require.Len(t, all, 2, "All() should return all open documents")

	docsByURI := make(map[string]*Document)
	for _, doc := range all {
		docsByURI[doc.URI] = doc
	}
	require.Contains(t, docsByURI, "file:///a.lisp")
	require.Contains(t, docsByURI, "file:///b.lisp")
	assert.Equal(t, "(+ 1 2)", docsByURI["file:///a.lisp"].Content)
	assert.Equal(t, "(+ 3 4)", docsByURI["file:///b.lisp"].Content)

	// After closing one document, All() should reflect the change.
	store.Close("file:///a.lisp")
	all = store.All()
	require.Len(t, all, 1, "All() should reflect closed documents")
	assert.Equal(t, "file:///b.lisp", all[0].URI)
}

func TestReanalyzeOpenDocuments(t *testing.T) {
	s := testServer()
	// Set up notification capturing.
	var captured []*protocol.PublishDiagnosticsParams
	s.notify = func(method string, params any) {
		if method == protocol.ServerTextDocumentPublishDiagnostics {
			captured = append(captured, params.(*protocol.PublishDiagnosticsParams))
		}
	}

	// Start with an empty workspace config (no package exports).
	// This simulates the state before the workspace index has run.
	setTestAnalysisCfg(s, &analysis.Config{})

	// Open a document that uses a package that isn't in the config yet.
	content := `(use-package 'fakepkg)
(fake-func 1 1)`
	doc := openDoc(s, "file:///test.lisp", content)

	// First analysis: no exports for "fakepkg", so fake-func is unresolvable.
	s.analyzeAndPublish(doc)
	require.Len(t, captured, 1, "initial analysis should publish one diagnostic set")
	initialDiags := captured[0].Diagnostics

	// Verify we actually get a diagnostic about the unresolvable symbol.
	require.NotEmpty(t, initialDiags, "initial analysis should produce diagnostics for unresolvable package")
	hasUnresolved := false
	for _, d := range initialDiags {
		if strings.Contains(d.Message, "fake-func") || strings.Contains(d.Message, "fakepkg") {
			hasUnresolved = true
			break
		}
	}
	require.True(t, hasUnresolved,
		"initial diagnostics should mention fake-func or fakepkg, got: %v", diagMessages(initialDiags))

	// Simulate workspace index completing with fakepkg exports.
	s.analysisCfg = &analysis.Config{
		PackageExports: map[string][]analysis.ExternalSymbol{
			"fakepkg": {
				{Name: "fake-func", Kind: analysis.SymFunction, Package: "fakepkg"},
			},
		},
	}

	// Re-analyze should publish new diagnostics for all open docs.
	s.reanalyzeOpenDocuments()
	require.Greater(t, len(captured), 1,
		"reanalyze should publish new diagnostics after workspace index completes")

	// The fake-func diagnostic should be gone now that fakepkg is registered.
	reanalyzedDiags := captured[len(captured)-1].Diagnostics
	for _, d := range reanalyzedDiags {
		assert.NotContains(t, d.Message, "fake-func",
			"re-analysis should not flag fake-func after fakepkg is registered")
	}
}

// diagMessages extracts messages from diagnostics for test failure output.
func diagMessages(diags []protocol.Diagnostic) []string {
	msgs := make([]string, len(diags))
	for i, d := range diags {
		msgs[i] = d.Message
	}
	return msgs
}

func TestDefinitionOnUndefinedSymbol(t *testing.T) {
	s := testServer()
	content := "(nonexistent 1 2 3)"
	openDoc(s, "file:///test.lisp", content)

	result, err := s.textDocumentDefinition(mockContext(), &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 1}, // on "nonexistent"
		},
	})
	require.NoError(t, err)
	// Undefined symbol should return nil, not error.
	assert.Nil(t, result, "definition of undefined symbol should be nil")
}

func TestDocumentSymbolsEmptyFile(t *testing.T) {
	s := testServer()
	openDoc(s, "file:///test.lisp", "(+ 1 2)\n(- 3 4)")

	result, err := s.textDocumentDocumentSymbol(mockContext(), &protocol.DocumentSymbolParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
	})
	require.NoError(t, err)
	// A file with no defun/set should return empty or nil symbols.
	if result != nil {
		symbols, ok := result.([]protocol.DocumentSymbol)
		if ok {
			assert.Empty(t, symbols, "file with no definitions should have no document symbols")
		}
	}
}

func TestRenameVariable(t *testing.T) {
	s := testServer()
	content := `(set 'counter 0)
(set! 'counter (+ counter 1))
counter`
	openDoc(s, "file:///test.lisp", content)

	// Try to rename "counter".
	result, err := s.textDocumentPrepareRename(mockContext(), &protocol.PrepareRenameParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 2, Character: 0}, // on "counter" reference
		},
	})
	require.NoError(t, err)
	if result != nil {
		rp, ok := result.(*protocol.RangeWithPlaceholder)
		require.True(t, ok)
		assert.Equal(t, "counter", rp.Placeholder)
	}
}

func TestMultipleDocuments(t *testing.T) {
	s := testServer()
	openDoc(s, "file:///a.lisp", "(defun foo () 1)")
	openDoc(s, "file:///b.lisp", "(defun bar () 2)")

	// Hover on foo in document A.
	hoverA, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///a.lisp"},
			Position:     protocol.Position{Line: 0, Character: 7},
		},
	})
	require.NoError(t, err)
	require.NotNil(t, hoverA)
	mc, ok := hoverA.Contents.(protocol.MarkupContent)
	require.True(t, ok)
	assert.Contains(t, mc.Value, "foo")

	// Hover on bar in document B.
	hoverB, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///b.lisp"},
			Position:     protocol.Position{Line: 0, Character: 7},
		},
	})
	require.NoError(t, err)
	require.NotNil(t, hoverB)
	mc, ok = hoverB.Contents.(protocol.MarkupContent)
	require.True(t, ok)
	assert.Contains(t, mc.Value, "bar")

	// Close A, B should still work.
	s.docs.Close("file:///a.lisp")
	assert.Nil(t, s.docs.Get("file:///a.lisp"))
	assert.NotNil(t, s.docs.Get("file:///b.lisp"))
}

func TestParseErrorRange(t *testing.T) {
	t.Run("ErrorVal with source", func(t *testing.T) {
		errVal := &lisp.ErrorVal{
			Source: &token.Location{File: "test.lisp", Line: 3, Col: 5},
		}
		r := parseErrorRange(errVal)
		assert.Equal(t, protocol.UInteger(2), r.Start.Line)
		assert.Equal(t, protocol.UInteger(4), r.Start.Character)
		// Range should be at least 1 char wide.
		assert.Equal(t, protocol.UInteger(2), r.End.Line)
		assert.Equal(t, protocol.UInteger(5), r.End.Character)
	})
	t.Run("LocationError with source", func(t *testing.T) {
		locErr := &token.LocationError{
			Err:    fmt.Errorf("unexpected token"),
			Source: &token.Location{File: "test.lisp", Line: 1, Col: 10},
		}
		r := parseErrorRange(locErr)
		assert.Equal(t, protocol.UInteger(0), r.Start.Line)
		assert.Equal(t, protocol.UInteger(9), r.Start.Character)
	})
	t.Run("plain error", func(t *testing.T) {
		r := parseErrorRange(fmt.Errorf("some error"))
		assert.Equal(t, protocol.UInteger(0), r.Start.Line)
		assert.Equal(t, protocol.UInteger(0), r.Start.Character)
	})
}

func TestConvertLintDiagnostic(t *testing.T) {
	from := lint.Diagnostic{
		Pos:      lint.Position{File: "test.lisp", Line: 3, Col: 5},
		Message:  "test message",
		Analyzer: "test-check",
		Severity: lint.SeverityWarning,
	}
	d := convertLintDiagnostic(from)
	assert.Equal(t, "test message", d.Message)
	assert.Equal(t, protocol.DiagnosticSeverityWarning, *d.Severity)
	assert.Equal(t, protocol.UInteger(2), d.Range.Start.Line)
	assert.Equal(t, protocol.UInteger(4), d.Range.Start.Character)
}

func TestConvertLintDiagnosticWithEndPos(t *testing.T) {
	from := lint.Diagnostic{
		Pos:      lint.Position{File: "test.lisp", Line: 3, Col: 5},
		EndPos:   lint.Position{File: "test.lisp", Line: 3, Col: 10},
		Message:  "test message",
		Analyzer: "test-check",
		Severity: lint.SeverityWarning,
	}
	d := convertLintDiagnostic(from)
	assert.Equal(t, protocol.UInteger(2), d.Range.Start.Line)
	assert.Equal(t, protocol.UInteger(4), d.Range.Start.Character)
	assert.Equal(t, protocol.UInteger(2), d.Range.End.Line)
	assert.Equal(t, protocol.UInteger(9), d.Range.End.Character)
}

func TestConvertLintDiagnosticZeroEndPos(t *testing.T) {
	// When EndPos is zero, End should equal Start (zero-width).
	from := lint.Diagnostic{
		Pos:      lint.Position{File: "test.lisp", Line: 3, Col: 5},
		Message:  "test message",
		Analyzer: "test-check",
	}
	d := convertLintDiagnostic(from)
	assert.Equal(t, d.Range.Start, d.Range.End, "zero EndPos should produce zero-width range")
}

// --- Issue reproduction tests (#169, #170, #171, #172, #173) ---

// testServerWithWorkspaceIndex creates a server with a standard library
// registry AND ensures the workspace index is built (so PackageExports are
// available). This simulates the state after a successful initialized
// notification.
func testServerWithWorkspaceIndex(t *testing.T) *Server {
	t.Helper()
	s := testServer()
	s.ensureWorkspaceIndex()

	// Guard: fail fast if index build silently panicked.
	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()
	require.NotNil(t, cfg, "workspace index should be built")
	require.NotEmpty(t, cfg.PackageExports, "workspace index should have package exports")
	return s
}

// assertHoverContains checks that hover returns non-nil Markdown content
// containing all expected substrings.
func assertHoverContains(t *testing.T, hover *protocol.Hover, expected ...string) {
	t.Helper()
	require.NotNil(t, hover, "hover should not be null")
	content, ok := hover.Contents.(protocol.MarkupContent)
	require.True(t, ok, "hover contents should be MarkupContent, got %T", hover.Contents)
	assert.Equal(t, protocol.MarkupKindMarkdown, content.Kind, "hover should be Markdown")
	for _, s := range expected {
		assert.Contains(t, content.Value, s, "hover content should contain %q", s)
	}
}

// TestHoverOnStdlibQualifiedSymbol reproduces issue #169: hover on
// package-qualified stdlib symbols like "string:join" returns null.
func TestHoverOnStdlibQualifiedSymbol(t *testing.T) {
	s := testServerWithWorkspaceIndex(t)
	doc := openDoc(s, "file:///test.lisp", `(string:join "," '("a" "b" "c"))`)
	s.ensureAnalysis(doc)

	// Hover at column 1 — inside "string:join".
	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 1},
		},
	})
	require.NoError(t, err)
	assertHoverContains(t, hover, "join", "function")
}

// TestHoverOnStdlibQualifiedSymbol_LazyIndex verifies that hover on a
// qualified symbol triggers lazy workspace index building when the
// initialized notification hasn't fired yet (issue #173 fallback).
func TestHoverOnStdlibQualifiedSymbol_LazyIndex(t *testing.T) {
	s := testServer()

	// Verify the workspace index has NOT been built yet.
	s.analysisCfgMu.RLock()
	require.Nil(t, s.analysisCfg, "analysisCfg should be nil before lazy init")
	s.analysisCfgMu.RUnlock()

	doc := openDoc(s, "file:///test.lisp", `(string:join "," '("a" "b" "c"))`)
	s.ensureAnalysis(doc)

	// After ensureAnalysis, the lazy init should have built the index.
	s.analysisCfgMu.RLock()
	require.NotNil(t, s.analysisCfg, "analysisCfg should be built by lazy init")
	s.analysisCfgMu.RUnlock()

	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 1},
		},
	})
	require.NoError(t, err)
	// Lazy-built index should produce equivalent results to eagerly-built index.
	assertHoverContains(t, hover, "join", "function")
}

// TestCompletionForPackageQualifiedPrefix reproduces issue #170: completion
// at "string:" prefix returns null instead of listing package exports.
func TestCompletionForPackageQualifiedPrefix(t *testing.T) {
	s := testServerWithWorkspaceIndex(t)
	doc := openDoc(s, "file:///test.lisp", "(string:")
	s.ensureAnalysis(doc)

	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 8},
		},
	})
	require.NoError(t, err)
	labels := completionLabels(t, result)
	require.Contains(t, labels, "string:join", "completion should include string:join (#170)")

	// Verify ALL completion items have Kind metadata set.
	items := result.([]protocol.CompletionItem)
	var foundJoin bool
	for _, item := range items {
		require.NotNil(t, item.Kind, "completion item %q should have a Kind", item.Label)
		if item.Label == "string:join" {
			foundJoin = true
			assert.Equal(t, protocol.CompletionItemKindFunction, *item.Kind,
				"string:join should be a Function, not %v", *item.Kind)
		}
	}
	require.True(t, foundJoin, "string:join should be present in completion items")
}

// TestCompletionForPackageQualifiedPrefix_LazyIndex verifies that
// completion triggers lazy workspace index building when the initialized
// notification hasn't fired yet (issue #173 fallback).
func TestCompletionForPackageQualifiedPrefix_LazyIndex(t *testing.T) {
	s := testServer()

	// Verify the workspace index has NOT been built yet.
	s.analysisCfgMu.RLock()
	require.Nil(t, s.analysisCfg, "analysisCfg should be nil before lazy init")
	s.analysisCfgMu.RUnlock()

	doc := openDoc(s, "file:///test.lisp", "(string:")
	s.ensureAnalysis(doc)

	// After ensureAnalysis, the lazy init should have built the index.
	s.analysisCfgMu.RLock()
	require.NotNil(t, s.analysisCfg, "analysisCfg should be built by lazy init")
	s.analysisCfgMu.RUnlock()

	result, err := s.textDocumentCompletion(mockContext(), &protocol.CompletionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 0, Character: 8},
		},
	})
	require.NoError(t, err)
	labels := completionLabels(t, result)
	require.Contains(t, labels, "string:join",
		"completion should include string:join even without prior workspace index (#173 fallback)")
}

// TestHoverOnUsePackageImportedSymbol reproduces issue #171: hover on a
// symbol imported via use-package returns null.
func TestHoverOnUsePackageImportedSymbol(t *testing.T) {
	s := testServerWithWorkspaceIndex(t)
	doc := openDoc(s, "file:///test.lisp", "(use-package 'string)\n(join \",\" '(\"a\" \"b\"))")
	s.ensureAnalysis(doc)

	// Hover on "join" at line 1, col 1 — imported via use-package.
	hover, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
			Position:     protocol.Position{Line: 1, Character: 1},
		},
	})
	require.NoError(t, err)
	assertHoverContains(t, hover, "join", "function")

	// Negative control: a symbol from a package NOT imported via use-package
	// should NOT produce hover (unless it's qualified).
	doc2 := openDoc(s, "file:///test2.lisp", "(concat \"a\" \"b\")")
	s.ensureAnalysis(doc2)
	hover2, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test2.lisp"},
			Position:     protocol.Position{Line: 0, Character: 1},
		},
	})
	require.NoError(t, err)
	// "concat" without use-package 'string should resolve as a builtin or not at all,
	// but should NOT resolve as a string package import.
	if hover2 != nil {
		content, ok := hover2.Contents.(protocol.MarkupContent)
		if ok && strings.Contains(content.Value, "concat") {
			// If it resolves, it should NOT claim to be from the string package import.
			// It should resolve as a builtin (which is fine).
			assert.NotContains(t, content.Value, "Defined in",
				"unimported symbol should not show as defined from an external source")
		}
	}
}

// TestFormattingReturnsEdits reproduces issue #172: formatting returns null
// instead of text edits for badly-formatted code.
func TestFormattingReturnsEdits(t *testing.T) {
	s := testServer()
	input := "(defun  add  (x  y)\n(+  x  y))"
	openDoc(s, "file:///test.lisp", input)

	edits, err := s.textDocumentFormatting(mockContext(), &protocol.DocumentFormattingParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.lisp"},
		Options: map[string]any{
			"tabSize":      float64(2),
			"insertSpaces": true,
		},
	})
	require.NoError(t, err)
	require.NotNil(t, edits, "formatting should return edits, not null (#172)")
	require.Len(t, edits, 1, "should return a single whole-document edit")

	// Verify the edit range covers the whole document.
	assert.Equal(t, protocol.Position{Line: 0, Character: 0}, edits[0].Range.Start,
		"edit should start at beginning of document")
	assert.Equal(t, protocol.UInteger(1), edits[0].Range.End.Line,
		"edit end line should cover the 2-line input")

	// Verify formatting improved indentation.
	assert.Contains(t, edits[0].NewText, "  (+", "formatted text should be properly indented")
	// Verify the formatted output differs from the input.
	assert.NotEqual(t, input, edits[0].NewText, "formatted output should differ from input")
}

// TestWorkspaceIndexWithEmptyRootPath verifies that the workspace index
// still extracts registry exports even when rootPath is empty.
func TestWorkspaceIndexWithEmptyRootPath(t *testing.T) {
	s := testServer()
	// rootPath is "" by default — simulates no rootUri in initialize.
	s.ensureWorkspaceIndex()

	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()

	require.NotNil(t, cfg, "analysisCfg should be set even with empty rootPath")
	require.NotNil(t, cfg.PackageExports, "PackageExports should be set from registry")
	strExports := cfg.PackageExports["string"]
	require.NotEmpty(t, strExports, "string package exports should be populated from registry")

	// Verify the "join" export exists with correct metadata.
	var joinSym *analysis.ExternalSymbol
	for i := range strExports {
		if strExports[i].Name == "join" {
			joinSym = &strExports[i]
			break
		}
	}
	require.NotNil(t, joinSym, "string package should include 'join' export")
	assert.Equal(t, analysis.SymFunction, joinSym.Kind, "join should be a function")
	assert.NotEmpty(t, joinSym.DocString, "join should have a docstring from registry")
}
