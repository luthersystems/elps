// Copyright © 2024 The ELPS authors

package lsp

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// jsonRPCRequest builds a JSON-RPC 2.0 request.
func jsonRPCRequest(id int, method string, params any) []byte {
	msg := map[string]any{
		"jsonrpc": "2.0",
		"id":      id,
		"method":  method,
		"params":  params,
	}
	b, _ := json.Marshal(msg)
	return b
}

// jsonRPCNotification builds a JSON-RPC 2.0 notification (no id).
func jsonRPCNotification(method string, params any) []byte {
	msg := map[string]any{
		"jsonrpc": "2.0",
		"method":  method,
		"params":  params,
	}
	b, _ := json.Marshal(msg)
	return b
}

// lspMessage wraps JSON content with the LSP Content-Length header.
func lspMessage(content []byte) []byte {
	return fmt.Appendf(nil, "Content-Length: %d\r\n\r\n%s", len(content), content)
}

// readLSPMessage reads a single LSP message from a buffered reader.
// Returns the parsed JSON as a map.
func readLSPMessage(t *testing.T, r *bufio.Reader) map[string]any {
	t.Helper()

	// Read headers until blank line.
	var contentLength int
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			t.Fatalf("failed to read LSP header: %v", err)
		}
		line = strings.TrimRight(line, "\r\n")
		if line == "" {
			break
		}
		if val, ok := strings.CutPrefix(line, "Content-Length: "); ok {
			n, err := strconv.Atoi(val)
			require.NoError(t, err, "parsing Content-Length")
			contentLength = n
		}
	}
	require.Greater(t, contentLength, 0, "Content-Length must be positive")

	// Read content body.
	body := make([]byte, contentLength)
	_, err := io.ReadFull(r, body)
	require.NoError(t, err, "reading message body")

	var msg map[string]any
	require.NoError(t, json.Unmarshal(body, &msg), "parsing JSON body")
	return msg
}

// readResponse reads LSP messages until a response with the given id appears.
// Returns the response and any notifications received along the way.
func readResponse(t *testing.T, r *bufio.Reader, id int) (map[string]any, []map[string]any) {
	t.Helper()
	var notifications []map[string]any
	deadline := time.After(10 * time.Second)
	for {
		select {
		case <-deadline:
			t.Fatalf("timeout waiting for response id=%d", id)
		default:
		}
		msg := readLSPMessage(t, r)
		// If this message has the expected id, it's our response.
		if msgID, ok := msg["id"]; ok {
			var msgIDFloat float64
			switch v := msgID.(type) {
			case float64:
				msgIDFloat = v
			case json.Number:
				f, _ := v.Float64()
				msgIDFloat = f
			}
			if int(msgIDFloat) == id {
				return msg, notifications
			}
		}
		// Otherwise it's a notification (no id, or different id).
		notifications = append(notifications, msg)
	}
}

// e2eServer starts an LSP server on a random TCP port and returns the
// connection and a cleanup function.
func e2eServer(t *testing.T) (net.Conn, func()) {
	t.Helper()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "init env: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil(), "load lib: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "set pkg: %v", rc)

	srv := New(WithEnv(env))

	// Find a free port.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	require.NoError(t, err)
	addr := listener.Addr().String()
	_ = listener.Close()

	// Start the server in the background.
	done := make(chan error, 1)
	go func() {
		done <- srv.RunTCP(addr)
	}()

	// Give server a moment to start listening, then connect.
	var conn net.Conn
	for range 50 {
		conn, err = net.Dial("tcp", addr)
		if err == nil {
			break
		}
		time.Sleep(20 * time.Millisecond)
	}
	require.NoError(t, err, "failed to connect to LSP server at %s", addr)

	cleanup := func() {
		_ = conn.Close()
	}

	return conn, cleanup
}

// send writes an LSP message to the connection.
func send(t *testing.T, conn net.Conn, data []byte) {
	t.Helper()
	_, err := conn.Write(lspMessage(data))
	require.NoError(t, err, "writing LSP message")
}

func TestE2E_FullLifecycle(t *testing.T) {
	conn, cleanup := e2eServer(t)
	defer cleanup()

	reader := bufio.NewReader(conn)

	testURI := "file:///tmp/e2e-test/test.lisp"
	testContent := `(defun add (a b)
  "Add two numbers."
  (+ a b))

(defun multiply (x y)
  "Multiply two numbers."
  (* x y))

(set 'result (add 1 2))
`

	// --- Step 1: Initialize ---
	send(t, conn, jsonRPCRequest(1, "initialize", map[string]any{
		"capabilities": map[string]any{},
		"rootUri":      "file:///tmp/e2e-test",
	}))

	resp, _ := readResponse(t, reader, 1)
	result := resp["result"].(map[string]any)
	caps := result["capabilities"].(map[string]any)

	// Verify key capabilities.
	assert.NotNil(t, caps["hoverProvider"], "should have hover")
	assert.NotNil(t, caps["definitionProvider"], "should have definition")
	assert.NotNil(t, caps["completionProvider"], "should have completion")
	assert.NotNil(t, caps["referencesProvider"], "should have references")
	assert.NotNil(t, caps["documentSymbolProvider"], "should have document symbols")
	assert.NotNil(t, caps["renameProvider"], "should have rename")

	serverInfo := result["serverInfo"].(map[string]any)
	assert.Equal(t, "elps-lsp", serverInfo["name"])

	// --- Step 2: Initialized ---
	send(t, conn, jsonRPCNotification("initialized", map[string]any{}))

	// --- Step 3: Open document ---
	send(t, conn, jsonRPCNotification("textDocument/didOpen", map[string]any{
		"textDocument": map[string]any{
			"uri":        testURI,
			"languageId": "elps",
			"version":    1,
			"text":       testContent,
		},
	}))

	// Read the diagnostics notification that didOpen triggers.
	// Give a brief pause for the server to process.
	time.Sleep(200 * time.Millisecond)

	// --- Step 4: Hover on "add" at line 0, char 7 ---
	send(t, conn, jsonRPCRequest(2, "textDocument/hover", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 0, "character": 7},
	}))

	hoverResp, _ := readResponse(t, reader, 2)
	require.NotNil(t, hoverResp["result"], "hover should return a result")
	hoverResult := hoverResp["result"].(map[string]any)
	hoverContents := hoverResult["contents"].(map[string]any)
	hoverValue := hoverContents["value"].(string)
	assert.Contains(t, hoverValue, "add", "hover should mention function name")
	assert.Contains(t, hoverValue, "function", "hover should show kind")
	assert.Contains(t, hoverValue, "Add two numbers", "hover should show docstring")

	// --- Step 5: Go to Definition on "add" call at line 8, char 15 ---
	send(t, conn, jsonRPCRequest(3, "textDocument/definition", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 8, "character": 15},
	}))

	defResp, _ := readResponse(t, reader, 3)
	require.NotNil(t, defResp["result"], "definition should return a result")
	defResult := defResp["result"].(map[string]any)
	defRange := defResult["range"].(map[string]any)
	defStart := defRange["start"].(map[string]any)
	assert.Equal(t, float64(0), defStart["line"], "definition should point to line 0")

	// --- Step 6: Document Symbols ---
	send(t, conn, jsonRPCRequest(4, "textDocument/documentSymbol", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
	}))

	symResp, _ := readResponse(t, reader, 4)
	require.NotNil(t, symResp["result"], "document symbols should return a result")
	syms := symResp["result"].([]any)
	require.GreaterOrEqual(t, len(syms), 2, "should have at least add and multiply")

	var symNames []string
	for _, s := range syms {
		sym := s.(map[string]any)
		symNames = append(symNames, sym["name"].(string))
	}
	assert.Contains(t, symNames, "add")
	assert.Contains(t, symNames, "multiply")

	// --- Step 7: Completion after "(ad" ---
	send(t, conn, jsonRPCRequest(5, "textDocument/completion", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 8, "character": 15},
	}))

	compResp, _ := readResponse(t, reader, 5)
	require.NotNil(t, compResp["result"], "completion should return a result")
	compItems := compResp["result"].([]any)
	var compLabels []string
	for _, item := range compItems {
		ci := item.(map[string]any)
		compLabels = append(compLabels, ci["label"].(string))
	}
	assert.Contains(t, compLabels, "add", "completion should include 'add'")

	// --- Step 8: References for "add" ---
	send(t, conn, jsonRPCRequest(6, "textDocument/references", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 0, "character": 7},
		"context":      map[string]any{"includeDeclaration": true},
	}))

	refsResp, _ := readResponse(t, reader, 6)
	require.NotNil(t, refsResp["result"], "references should return a result")
	refs := refsResp["result"].([]any)
	assert.GreaterOrEqual(t, len(refs), 2, "should find definition + at least one call site")

	// --- Step 9: Prepare Rename on "add" ---
	send(t, conn, jsonRPCRequest(7, "textDocument/prepareRename", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 0, "character": 7},
	}))

	prepResp, _ := readResponse(t, reader, 7)
	require.NotNil(t, prepResp["result"], "prepare rename should succeed for user function")
	prepResult := prepResp["result"].(map[string]any)
	assert.Equal(t, "add", prepResult["placeholder"], "placeholder should be the symbol name")

	// --- Step 10: Rename "add" to "sum" ---
	send(t, conn, jsonRPCRequest(8, "textDocument/rename", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 0, "character": 7},
		"newName":      "sum",
	}))

	renameResp, _ := readResponse(t, reader, 8)
	require.NotNil(t, renameResp["result"], "rename should return a workspace edit")
	renameResult := renameResp["result"].(map[string]any)
	changes := renameResult["changes"].(map[string]any)
	fileEdits := changes[testURI].([]any)
	assert.GreaterOrEqual(t, len(fileEdits), 2, "should rename at definition + call site(s)")
	for _, edit := range fileEdits {
		e := edit.(map[string]any)
		assert.Equal(t, "sum", e["newText"], "all edits should use the new name")
	}

	// --- Step 11: Change document (introduce error) ---
	send(t, conn, jsonRPCNotification("textDocument/didChange", map[string]any{
		"textDocument": map[string]any{"uri": testURI, "version": 2},
		"contentChanges": []any{
			map[string]any{"text": "(defun broken-func (x\n"},
		},
	}))

	// Wait for debounce to fire and diagnostics to arrive.
	time.Sleep(500 * time.Millisecond)

	// --- Step 12: Hover on a builtin should work ---
	send(t, conn, jsonRPCNotification("textDocument/didChange", map[string]any{
		"textDocument": map[string]any{"uri": testURI, "version": 3},
		"contentChanges": []any{
			map[string]any{"text": "(map identity '(1 2 3))\n"},
		},
	}))

	// Wait for debounce.
	time.Sleep(500 * time.Millisecond)

	send(t, conn, jsonRPCRequest(9, "textDocument/hover", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 0, "character": 1},
	}))

	builtinHoverResp, _ := readResponse(t, reader, 9)
	require.NotNil(t, builtinHoverResp["result"], "hover on builtin should return a result")
	builtinResult := builtinHoverResp["result"].(map[string]any)
	builtinContents := builtinResult["contents"].(map[string]any)
	builtinValue := builtinContents["value"].(string)
	assert.Contains(t, builtinValue, "map")

	// --- Step 13: Close document ---
	send(t, conn, jsonRPCNotification("textDocument/didClose", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
	}))

	// --- Step 14: Shutdown ---
	send(t, conn, jsonRPCRequest(99, "shutdown", nil))

	shutdownResp, _ := readResponse(t, reader, 99)
	// Shutdown should return null result.
	assert.Nil(t, shutdownResp["error"], "shutdown should not error")

	// --- Step 15: Exit ---
	send(t, conn, jsonRPCNotification("exit", nil))
}

func TestE2E_DiagnosticsPublishedOnOpen(t *testing.T) {
	conn, cleanup := e2eServer(t)
	defer cleanup()

	reader := bufio.NewReader(conn)
	testURI := "file:///tmp/e2e-diag/test.lisp"

	// Initialize.
	send(t, conn, jsonRPCRequest(1, "initialize", map[string]any{
		"capabilities": map[string]any{},
	}))
	readResponse(t, reader, 1)
	send(t, conn, jsonRPCNotification("initialized", map[string]any{}))

	// Open document with a parse error.
	send(t, conn, jsonRPCNotification("textDocument/didOpen", map[string]any{
		"textDocument": map[string]any{
			"uri":        testURI,
			"languageId": "elps",
			"version":    1,
			"text":       "(defun broken (x",
		},
	}))

	// Read the diagnostics notification.
	// We need to read messages until we see a publishDiagnostics notification.
	var diagParams map[string]any
	deadline := time.After(5 * time.Second)
	for {
		select {
		case <-deadline:
			t.Fatal("timeout waiting for diagnostics notification")
		default:
		}
		msg := readLSPMessage(t, reader)
		if method, ok := msg["method"].(string); ok && method == "textDocument/publishDiagnostics" {
			diagParams = msg["params"].(map[string]any)
			break
		}
	}

	require.NotNil(t, diagParams)
	assert.Equal(t, testURI, diagParams["uri"])
	diags := diagParams["diagnostics"].([]any)
	require.NotEmpty(t, diags, "parse error should produce diagnostics")

	// Verify at least one diagnostic is an error.
	var foundError bool
	for _, d := range diags {
		diag := d.(map[string]any)
		if sev, ok := diag["severity"].(float64); ok && sev == 1 { // 1 = Error
			foundError = true
		}
	}
	assert.True(t, foundError, "should have at least one error diagnostic")

	// Cleanup.
	send(t, conn, jsonRPCRequest(99, "shutdown", nil))
	readResponse(t, reader, 99)
	send(t, conn, jsonRPCNotification("exit", nil))
}

func TestE2E_LintDiagnostics(t *testing.T) {
	conn, cleanup := e2eServer(t)
	defer cleanup()

	reader := bufio.NewReader(conn)
	testURI := "file:///tmp/e2e-lint/test.lisp"

	// Initialize.
	send(t, conn, jsonRPCRequest(1, "initialize", map[string]any{
		"capabilities": map[string]any{},
	}))
	readResponse(t, reader, 1)
	send(t, conn, jsonRPCNotification("initialized", map[string]any{}))

	// Open a document with a lint issue (if with 1 arg is bad arity).
	send(t, conn, jsonRPCNotification("textDocument/didOpen", map[string]any{
		"textDocument": map[string]any{
			"uri":        testURI,
			"languageId": "elps",
			"version":    1,
			"text":       "(if true)",
		},
	}))

	// Read diagnostics — expect lint warnings.
	var diagParams map[string]any
	deadline := time.After(5 * time.Second)
	for {
		select {
		case <-deadline:
			t.Fatal("timeout waiting for diagnostics notification")
		default:
		}
		msg := readLSPMessage(t, reader)
		if method, ok := msg["method"].(string); ok && method == "textDocument/publishDiagnostics" {
			diagParams = msg["params"].(map[string]any)
			break
		}
	}

	require.NotNil(t, diagParams)
	diags := diagParams["diagnostics"].([]any)
	require.NotEmpty(t, diags, "lint issues should produce diagnostics")

	// At least one diagnostic should be from the linter.
	var foundLint bool
	for _, d := range diags {
		diag := d.(map[string]any)
		if source, ok := diag["source"].(string); ok && source == "elps-lint" {
			foundLint = true
		}
	}
	assert.True(t, foundLint, "should have at least one lint diagnostic")

	// Cleanup.
	send(t, conn, jsonRPCRequest(99, "shutdown", nil))
	readResponse(t, reader, 99)
	send(t, conn, jsonRPCNotification("exit", nil))
}

func TestE2E_HoverOnWhitespace(t *testing.T) {
	conn, cleanup := e2eServer(t)
	defer cleanup()

	reader := bufio.NewReader(conn)
	testURI := "file:///tmp/e2e-hover/test.lisp"

	// Initialize.
	send(t, conn, jsonRPCRequest(1, "initialize", map[string]any{
		"capabilities": map[string]any{},
	}))
	readResponse(t, reader, 1)
	send(t, conn, jsonRPCNotification("initialized", map[string]any{}))

	// Open simple document.
	send(t, conn, jsonRPCNotification("textDocument/didOpen", map[string]any{
		"textDocument": map[string]any{
			"uri":        testURI,
			"languageId": "elps",
			"version":    1,
			"text":       "(+ 1 2)\n\n(- 3 4)",
		},
	}))

	// Give time for diagnostics to arrive.
	time.Sleep(200 * time.Millisecond)

	// Hover on empty line (line 1, char 0) should return null.
	send(t, conn, jsonRPCRequest(2, "textDocument/hover", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 1, "character": 0},
	}))

	resp, _ := readResponse(t, reader, 2)
	assert.Nil(t, resp["result"], "hover on whitespace should return null")

	// Cleanup.
	send(t, conn, jsonRPCRequest(99, "shutdown", nil))
	readResponse(t, reader, 99)
	send(t, conn, jsonRPCNotification("exit", nil))
}

func TestE2E_DefinitionOnUndefinedSymbol(t *testing.T) {
	conn, cleanup := e2eServer(t)
	defer cleanup()

	reader := bufio.NewReader(conn)
	testURI := "file:///tmp/e2e-undef/test.lisp"

	// Initialize.
	send(t, conn, jsonRPCRequest(1, "initialize", map[string]any{
		"capabilities": map[string]any{},
	}))
	readResponse(t, reader, 1)
	send(t, conn, jsonRPCNotification("initialized", map[string]any{}))

	// Open document with a call to an undefined function.
	send(t, conn, jsonRPCNotification("textDocument/didOpen", map[string]any{
		"textDocument": map[string]any{
			"uri":        testURI,
			"languageId": "elps",
			"version":    1,
			"text":       "(nonexistent-func 1 2 3)",
		},
	}))

	time.Sleep(200 * time.Millisecond)

	// Definition on the undefined symbol — should return null gracefully.
	send(t, conn, jsonRPCRequest(2, "textDocument/definition", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 0, "character": 1},
	}))

	resp, _ := readResponse(t, reader, 2)
	// For an undefined symbol, definition should return null (not an error).
	assert.Nil(t, resp["error"], "definition on undefined symbol should not error")

	// Cleanup.
	send(t, conn, jsonRPCRequest(99, "shutdown", nil))
	readResponse(t, reader, 99)
	send(t, conn, jsonRPCNotification("exit", nil))
}

func TestE2E_EmptyDocument(t *testing.T) {
	conn, cleanup := e2eServer(t)
	defer cleanup()

	reader := bufio.NewReader(conn)
	testURI := "file:///tmp/e2e-empty/test.lisp"

	// Initialize.
	send(t, conn, jsonRPCRequest(1, "initialize", map[string]any{
		"capabilities": map[string]any{},
	}))
	readResponse(t, reader, 1)
	send(t, conn, jsonRPCNotification("initialized", map[string]any{}))

	// Open empty document.
	send(t, conn, jsonRPCNotification("textDocument/didOpen", map[string]any{
		"textDocument": map[string]any{
			"uri":        testURI,
			"languageId": "elps",
			"version":    1,
			"text":       "",
		},
	}))

	time.Sleep(200 * time.Millisecond)

	// Hover should return null.
	send(t, conn, jsonRPCRequest(2, "textDocument/hover", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 0, "character": 0},
	}))
	hoverResp, _ := readResponse(t, reader, 2)
	assert.Nil(t, hoverResp["result"], "hover on empty doc should return null")

	// Completion should return empty or null.
	send(t, conn, jsonRPCRequest(3, "textDocument/completion", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 0, "character": 0},
	}))
	compResp, _ := readResponse(t, reader, 3)
	// Completion may return null or an empty list — both are valid.
	assert.Nil(t, compResp["error"], "completion on empty doc should not error")

	// Document symbols should return empty.
	send(t, conn, jsonRPCRequest(4, "textDocument/documentSymbol", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
	}))
	symResp, _ := readResponse(t, reader, 4)
	assert.Nil(t, symResp["error"], "document symbols on empty doc should not error")

	// Cleanup.
	send(t, conn, jsonRPCRequest(99, "shutdown", nil))
	readResponse(t, reader, 99)
	send(t, conn, jsonRPCNotification("exit", nil))
}

func TestE2E_SignatureHelp(t *testing.T) {
	conn, cleanup := e2eServer(t)
	defer cleanup()

	reader := bufio.NewReader(conn)
	testURI := "file:///tmp/e2e-sigh/test.lisp"

	// Initialize.
	send(t, conn, jsonRPCRequest(1, "initialize", map[string]any{
		"capabilities": map[string]any{},
	}))

	resp, _ := readResponse(t, reader, 1)
	result := resp["result"].(map[string]any)
	caps := result["capabilities"].(map[string]any)
	assert.NotNil(t, caps["signatureHelpProvider"], "should have signatureHelp capability")

	send(t, conn, jsonRPCNotification("initialized", map[string]any{}))

	// Open document with a user-defined function and a call.
	send(t, conn, jsonRPCNotification("textDocument/didOpen", map[string]any{
		"textDocument": map[string]any{
			"uri":        testURI,
			"languageId": "elps",
			"version":    1,
			"text":       "(defun greet (name greeting)\n  \"Greet someone.\"\n  (concat greeting \" \" name))\n\n(greet \"world\" \"hello\")",
		},
	}))

	time.Sleep(200 * time.Millisecond)

	// Request signature help inside the (greet ...) call.
	send(t, conn, jsonRPCRequest(2, "textDocument/signatureHelp", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 4, "character": 7},
	}))

	sigResp, _ := readResponse(t, reader, 2)
	require.NotNil(t, sigResp["result"], "signature help should return a result")
	sigResult := sigResp["result"].(map[string]any)
	sigs := sigResult["signatures"].([]any)
	require.Len(t, sigs, 1)
	sig := sigs[0].(map[string]any)
	sigLabel := sig["label"].(string)
	assert.Contains(t, sigLabel, "greet")
	assert.Contains(t, sigLabel, "name")

	// Cleanup.
	send(t, conn, jsonRPCRequest(99, "shutdown", nil))
	readResponse(t, reader, 99)
	send(t, conn, jsonRPCNotification("exit", nil))
}

func TestE2E_Formatting(t *testing.T) {
	conn, cleanup := e2eServer(t)
	defer cleanup()

	reader := bufio.NewReader(conn)
	testURI := "file:///tmp/e2e-fmt/test.lisp"

	// Initialize.
	send(t, conn, jsonRPCRequest(1, "initialize", map[string]any{
		"capabilities": map[string]any{},
	}))
	readResponse(t, reader, 1)
	send(t, conn, jsonRPCNotification("initialized", map[string]any{}))

	// Open badly formatted document.
	send(t, conn, jsonRPCNotification("textDocument/didOpen", map[string]any{
		"textDocument": map[string]any{
			"uri":        testURI,
			"languageId": "elps",
			"version":    1,
			"text":       "(defun add (x y)\n(+ x y))",
		},
	}))

	time.Sleep(200 * time.Millisecond)

	// Request formatting.
	send(t, conn, jsonRPCRequest(2, "textDocument/formatting", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"options":      map[string]any{"tabSize": 2, "insertSpaces": true},
	}))

	resp, _ := readResponse(t, reader, 2)
	require.NotNil(t, resp["result"], "formatting should return edits")
	edits := resp["result"].([]any)
	require.Len(t, edits, 1, "should return a single whole-document edit")
	edit := edits[0].(map[string]any)
	newText := edit["newText"].(string)
	assert.Contains(t, newText, "  (+", "formatted text should be indented")

	// Cleanup.
	send(t, conn, jsonRPCRequest(99, "shutdown", nil))
	readResponse(t, reader, 99)
	send(t, conn, jsonRPCNotification("exit", nil))
}

func TestE2E_RenameBuiltinRejected(t *testing.T) {
	conn, cleanup := e2eServer(t)
	defer cleanup()

	reader := bufio.NewReader(conn)
	testURI := "file:///tmp/e2e-rename/test.lisp"

	// Initialize.
	send(t, conn, jsonRPCRequest(1, "initialize", map[string]any{
		"capabilities": map[string]any{},
	}))
	readResponse(t, reader, 1)
	send(t, conn, jsonRPCNotification("initialized", map[string]any{}))

	// Open document with a builtin.
	send(t, conn, jsonRPCNotification("textDocument/didOpen", map[string]any{
		"textDocument": map[string]any{
			"uri":        testURI,
			"languageId": "elps",
			"version":    1,
			"text":       "(map identity '(1 2 3))",
		},
	}))

	time.Sleep(200 * time.Millisecond)

	// prepareRename on "map" should return null (not renameable, per LSP spec).
	send(t, conn, jsonRPCRequest(2, "textDocument/prepareRename", map[string]any{
		"textDocument": map[string]any{"uri": testURI},
		"position":     map[string]any{"line": 0, "character": 1},
	}))

	resp, _ := readResponse(t, reader, 2)
	assert.Nil(t, resp["error"], "prepareRename on builtin should not error")
	assert.Nil(t, resp["result"], "prepareRename on builtin should return null")

	// Cleanup.
	send(t, conn, jsonRPCRequest(99, "shutdown", nil))
	readResponse(t, reader, 99)
	send(t, conn, jsonRPCNotification("exit", nil))
}
