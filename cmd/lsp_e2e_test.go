// Copyright © 2026 The ELPS authors

package cmd

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// This file drives the built elps binary's "lsp --stdio" like an editor
// does, against a workspace on disk.
//
// Why it exists: the tests in package lsp build the server in process,
// with the options each test picks and most of them with no workspace on
// disk. luthersystems/elps#952 got past all of them: "elps lsp" started the
// server with a registry and no env, so it never expanded a workspace
// macro, and no test started the server the way the command does. This
// test runs the command itself, so a change to how the command builds the
// server, or to anything the server reads from disk, is tested as a user
// gets it.

// lspStdioClient is a minimal LSP client over the stdio of an elps
// process. It answers each request the server sends with a null result,
// as a client with no extra capabilities does.
type lspStdioClient struct {
	t     *testing.T
	cmd   *exec.Cmd
	stdin io.WriteCloser

	writeMu sync.Mutex
	nextID  int

	mu    sync.Mutex
	cond  *sync.Cond
	resps map[int]json.RawMessage
	// diags holds each publishDiagnostics in the order received.
	diags []lspPublished
	done  bool

	// exited receives the result of the one cmd.Wait.
	exited chan error
}

type lspPublished struct {
	URI         string            `json:"uri"`
	Diagnostics []json.RawMessage `json:"diagnostics"`
	// Null reports that the diagnostics field was JSON null.
	Null bool `json:"-"`
}

func startLSPStdio(t *testing.T) *lspStdioClient {
	t.Helper()
	bin := buildTestBinary(t)
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	cmd := exec.CommandContext(ctx, bin, "lsp", "--stdio") //nolint:gosec // test executes freshly built local binary
	stdin, err := cmd.StdinPipe()
	require.NoError(t, err)
	stdout, err := cmd.StdoutPipe()
	require.NoError(t, err)
	cmd.Stderr = io.Discard
	require.NoError(t, cmd.Start())

	c := &lspStdioClient{t: t, cmd: cmd, stdin: stdin, resps: map[int]json.RawMessage{}, exited: make(chan error, 1)}
	c.cond = sync.NewCond(&c.mu)
	readDone := make(chan struct{})
	go func() {
		c.readLoop(bufio.NewReader(stdout))
		close(readDone)
	}()
	// Wait closes stdout, so it runs only after the reader saw its end.
	go func() {
		<-readDone
		c.exited <- cmd.Wait()
	}()
	t.Cleanup(func() {
		_ = stdin.Close()
		cancel()
	})
	return c
}

func (c *lspStdioClient) readLoop(r *bufio.Reader) {
	defer func() {
		c.mu.Lock()
		c.done = true
		c.cond.Broadcast()
		c.mu.Unlock()
	}()
	for {
		length := -1
		for {
			line, err := r.ReadString('\n')
			if err != nil {
				return
			}
			line = strings.TrimRight(line, "\r\n")
			if line == "" {
				break
			}
			if v, ok := strings.CutPrefix(line, "Content-Length: "); ok {
				if length, err = strconv.Atoi(v); err != nil {
					return
				}
			}
		}
		if length < 0 {
			return
		}
		body := make([]byte, length)
		if _, err := io.ReadFull(r, body); err != nil {
			return
		}
		var msg struct {
			ID     *json.RawMessage `json:"id"`
			Method string           `json:"method"`
			Params json.RawMessage  `json:"params"`
			Result json.RawMessage  `json:"result"`
			Error  json.RawMessage  `json:"error"`
		}
		if json.Unmarshal(body, &msg) != nil {
			continue
		}
		switch {
		case msg.ID != nil && msg.Method != "":
			// A request from the server, such as client/registerCapability.
			// An error here is a closed pipe: the test is over.
			_ = c.write(map[string]any{"jsonrpc": "2.0", "id": msg.ID, "result": nil})
		case msg.ID != nil:
			id, err := strconv.Atoi(string(*msg.ID))
			if err != nil {
				continue
			}
			res := msg.Result
			if len(msg.Error) > 0 && string(msg.Error) != "null" {
				res = json.RawMessage(fmt.Sprintf(`{"error":%s}`, msg.Error))
			}
			c.mu.Lock()
			c.resps[id] = res
			c.cond.Broadcast()
			c.mu.Unlock()
		case msg.Method == "textDocument/publishDiagnostics":
			var p lspPublished
			var raw struct {
				Diagnostics json.RawMessage `json:"diagnostics"`
			}
			if json.Unmarshal(msg.Params, &p) != nil || json.Unmarshal(msg.Params, &raw) != nil {
				continue
			}
			p.Null = string(raw.Diagnostics) == "null"
			c.mu.Lock()
			c.diags = append(c.diags, p)
			c.cond.Broadcast()
			c.mu.Unlock()
		}
	}
}

// write sends one message. It returns the error, not fails the test,
// because the reader goroutine answers server requests with it, and a
// test may fail only from its own goroutine.
func (c *lspStdioClient) write(msg map[string]any) error {
	body, err := json.Marshal(msg)
	if err != nil {
		return err
	}
	c.writeMu.Lock()
	defer c.writeMu.Unlock()
	_, err = fmt.Fprintf(c.stdin, "Content-Length: %d\r\n\r\n%s", len(body), body)
	return err
}

func (c *lspStdioClient) notify(method string, params any) {
	c.t.Helper()
	require.NoError(c.t, c.write(map[string]any{"jsonrpc": "2.0", "method": method, "params": params}))
}

// request sends a request and decodes its result into out (may be nil).
func (c *lspStdioClient) request(method string, params, out any) {
	c.t.Helper()
	c.writeMu.Lock()
	c.nextID++
	id := c.nextID
	c.writeMu.Unlock()
	require.NoError(c.t, c.write(map[string]any{"jsonrpc": "2.0", "id": id, "method": method, "params": params}))

	deadline := time.Now().Add(30 * time.Second)
	c.mu.Lock()
	for {
		if res, ok := c.resps[id]; ok {
			delete(c.resps, id)
			c.mu.Unlock()
			require.NotContains(c.t, string(res), `"error"`, "%s failed", method)
			if out != nil {
				require.NoError(c.t, json.Unmarshal(res, out), "%s result: %s", method, res)
			}
			return
		}
		if c.done || time.Now().After(deadline) {
			c.mu.Unlock()
			c.t.Fatalf("no response to %s", method)
		}
		c.waitLocked(deadline)
	}
}

// waitLocked waits on the cond until a message arrives or deadline. The
// cond has no timeout, so a timer wakes it.
func (c *lspStdioClient) waitLocked(deadline time.Time) {
	timer := time.AfterFunc(time.Until(deadline), func() {
		c.mu.Lock()
		c.cond.Broadcast()
		c.mu.Unlock()
	})
	c.cond.Wait()
	timer.Stop()
}

// mark returns a position in the diagnostics stream; diagnosticsAfter
// waits for a publication after it.
func (c *lspStdioClient) mark() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return len(c.diags)
}

// diagnosticsAfter returns the messages of the first publication for uri
// after mark, and whether its diagnostics were JSON null.
func (c *lspStdioClient) diagnosticsAfter(mark int, uri string) ([]string, bool) {
	c.t.Helper()
	deadline := time.Now().Add(30 * time.Second)
	c.mu.Lock()
	defer c.mu.Unlock()
	for {
		for _, p := range c.diags[mark:] {
			if p.URI != uri {
				continue
			}
			msgs := []string{}
			for _, d := range p.Diagnostics {
				var v struct {
					Message string `json:"message"`
				}
				require.NoError(c.t, json.Unmarshal(d, &v))
				msgs = append(msgs, v.Message)
			}
			return msgs, p.Null
		}
		if c.done || time.Now().After(deadline) {
			c.t.Fatalf("no diagnostics for %s", uri)
		}
		c.waitLocked(deadline)
	}
}

type lspLocation struct {
	URI   string `json:"uri"`
	Range struct {
		Start struct {
			Line      int `json:"line"`
			Character int `json:"character"`
		} `json:"start"`
	} `json:"range"`
}

func (l lspLocation) String() string {
	return fmt.Sprintf("%s:%d:%d", filepath.Base(uriToTestPath(l.URI)), l.Range.Start.Line, l.Range.Start.Character)
}

func uriToTestPath(uri string) string { return strings.TrimPrefix(uri, "file://") }

func TestLSPCommand_StdioWorkspace(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test in short mode")
	}
	root := t.TempDir()
	// The workspace of elps#952: a macro in one file defines alpha, and
	// another file calls the macro and then alpha.
	macros := "(defmacro make-fn (name) (list 'defun 'alpha '() 1))\n"
	app := "(make-fn ignored)\n" +
		"(alpha)\n" +
		"(beta)\n" +
		"(defun gamma (x) \"Gamma doc.\" (+ x 1))\n" +
		"(gamma 2)\n"
	// other.lisp is never opened: requests on it read it from disk.
	other := "(gamma 3)\n"
	files := map[string]string{"macros.lisp": macros, "app.lisp": app, "other.lisp": other}
	for name, src := range files {
		require.NoError(t, os.WriteFile(filepath.Join(root, name), []byte(src), 0o600))
	}
	uri := func(name string) string { return "file://" + filepath.Join(root, name) }
	pos := func(name string, line, char int) map[string]any {
		return map[string]any{
			"textDocument": map[string]any{"uri": uri(name)},
			"position":     map[string]any{"line": line, "character": char},
		}
	}
	open := func(c *lspStdioClient, name string) {
		c.notify("textDocument/didOpen", map[string]any{"textDocument": map[string]any{
			"uri": uri(name), "languageId": "lisp", "version": 1, "text": files[name],
		}})
	}

	c := startLSPStdio(t)
	var initResult struct {
		Capabilities map[string]any `json:"capabilities"`
	}
	c.request("initialize", map[string]any{
		"processId": nil, "rootUri": "file://" + root, "capabilities": map[string]any{},
	}, &initResult)
	require.Contains(t, initResult.Capabilities, "referencesProvider")
	c.notify("initialized", map[string]any{})

	// Open: the macro call defines alpha, so only beta is undefined.
	// "ignored" is an argument the macro drops; with the expansion it is
	// not a reference at all.
	m := c.mark()
	open(c, "app.lisp")
	msgs, _ := c.diagnosticsAfter(m, uri("app.lisp"))
	assert.Equal(t, []string{"undefined symbol: beta"}, msgs,
		"elps#952: the server must expand the workspace macro")

	// Completion offers the function the macro defines.
	var completion []struct {
		Label string `json:"label"`
	}
	c.request("textDocument/completion", pos("app.lisp", 1, 2), &completion)
	var labels []string
	for _, it := range completion {
		labels = append(labels, it.Label)
	}
	assert.Contains(t, labels, "alpha")

	// Hover and definition of alpha lead to the macro.
	var hover struct {
		Contents struct {
			Value string `json:"value"`
		} `json:"contents"`
	}
	c.request("textDocument/hover", pos("app.lisp", 1, 2), &hover)
	assert.Contains(t, hover.Contents.Value, "alpha")
	assert.Contains(t, hover.Contents.Value, "macros.lisp")
	var def lspLocation
	c.request("textDocument/definition", pos("app.lisp", 1, 2), &def)
	assert.Equal(t, uri("macros.lisp"), def.URI)

	// References of gamma from the open file and from the unopened one
	// find the same three places.
	want := []string{"app.lisp:3:7", "app.lisp:4:1", "other.lisp:0:1"}
	refs := func(name string, line, char int) []string {
		var locs []lspLocation
		p := pos(name, line, char)
		p["context"] = map[string]any{"includeDeclaration": true}
		c.request("textDocument/references", p, &locs)
		out := make([]string, 0, len(locs))
		for _, l := range locs {
			out = append(out, l.String())
		}
		return out
	}
	assert.ElementsMatch(t, want, refs("app.lisp", 3, 8))
	assert.ElementsMatch(t, want, refs("other.lisp", 0, 2),
		"references from a file the editor has not opened")

	// Hover on a file the editor has not opened.
	hover.Contents.Value = ""
	c.request("textDocument/hover", pos("other.lisp", 0, 2), &hover)
	assert.Contains(t, hover.Contents.Value, "Gamma doc.")

	// Rename gamma edits both files.
	var edit struct {
		Changes map[string][]struct {
			NewText string `json:"newText"`
		} `json:"changes"`
	}
	p := pos("app.lisp", 3, 8)
	p["newName"] = "delta"
	c.request("textDocument/rename", p, &edit)
	assert.Len(t, edit.Changes[uri("app.lisp")], 2)
	assert.Len(t, edit.Changes[uri("other.lisp")], 1)

	// Formatting returns an edit that indents the body.
	files["fmt.lisp"] = "(defun zeta (x)\n(+ x 1))\n"
	open(c, "fmt.lisp")
	var fmtEdits []struct {
		NewText string `json:"newText"`
	}
	c.request("textDocument/formatting", map[string]any{
		"textDocument": map[string]any{"uri": uri("fmt.lisp")},
		"options":      map[string]any{"tabSize": 2, "insertSpaces": true},
	}, &fmtEdits)
	require.Len(t, fmtEdits, 1)
	assert.Equal(t, "(defun zeta (x)\n  (+ x 1))\n", fmtEdits[0].NewText)

	// Save a change to the macro so it defines beta too: app.lisp is
	// analyzed again and has no diagnostics. They are sent as an empty
	// array, not null, so a client clears the old ones.
	newMacros := "(defmacro make-fn (name)\n  (quasiquote (progn (defun alpha () 1) (defun beta () 2))))\n"
	require.NoError(t, os.WriteFile(filepath.Join(root, "macros.lisp"), []byte(newMacros), 0o600))
	open(c, "macros.lisp")
	c.notify("textDocument/didChange", map[string]any{
		"textDocument":   map[string]any{"uri": uri("macros.lisp"), "version": 2},
		"contentChanges": []any{map[string]any{"text": newMacros}},
	})
	m = c.mark()
	c.notify("textDocument/didSave", map[string]any{"textDocument": map[string]any{"uri": uri("macros.lisp")}})
	msgs, null := c.diagnosticsAfter(m, uri("app.lisp"))
	assert.Empty(t, msgs, "the saved macro defines beta")
	assert.False(t, null, "diagnostics must be an array, not null")

	// Shutdown and exit end the process with status 0.
	c.request("shutdown", nil, nil)
	c.notify("exit", nil)
	select {
	case err := <-c.exited:
		require.NoError(t, err, "the server exits with status 0")
	case <-time.After(10 * time.Second):
		t.Fatal("the server did not exit")
	}
}

// TestLSPCommand_StdioMacroDefinitionsAcrossFiles runs the workspace of
// luthersystems/elps#953 through elps lsp over stdio, as an editor does. A
// call of a workspace macro in app.lisp defines foo, and other.lisp calls
// foo. The scan of the text cannot see foo, so the index adds what the
// macro calls of each file define once the macros load, and a save of
// either file keeps that current.
func TestLSPCommand_StdioMacroDefinitionsAcrossFiles(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test in short mode")
	}
	root := t.TempDir()
	files := map[string]string{
		"macros.lisp": "(defmacro defhelper (name) (quasiquote (defun (unquote name) () 1)))\n",
		"app.lisp":    "(defhelper foo)\n(foo)\n",
		"other.lisp":  "(foo)\n",
	}
	for name, src := range files {
		require.NoError(t, os.WriteFile(filepath.Join(root, name), []byte(src), 0o600))
	}
	uri := func(name string) string { return "file://" + filepath.Join(root, name) }

	c := startLSPStdio(t)
	c.request("initialize", map[string]any{
		"processId": nil, "rootUri": "file://" + root, "capabilities": map[string]any{},
	}, nil)
	c.notify("initialized", map[string]any{})
	for _, name := range []string{"app.lisp", "other.lisp"} {
		c.notify("textDocument/didOpen", map[string]any{"textDocument": map[string]any{
			"uri": uri(name), "languageId": "lisp", "version": 1, "text": files[name],
		}})
	}
	// save writes a new text of name to disk and saves it, as an editor
	// does, and returns the next diagnostics of other.lisp.
	version := 1
	save := func(name, text string) []string {
		t.Helper()
		require.NoError(t, os.WriteFile(filepath.Join(root, name), []byte(text), 0o600))
		version++
		c.notify("textDocument/didOpen", map[string]any{"textDocument": map[string]any{
			"uri": uri(name), "languageId": "lisp", "version": version, "text": text,
		}})
		m := c.mark()
		c.notify("textDocument/didSave", map[string]any{"textDocument": map[string]any{"uri": uri(name)}})
		msgs, _ := c.diagnosticsAfter(m, uri("other.lisp"))
		return msgs
	}

	// The first analysis of other.lisp waits for the index build.
	m := c.mark()
	var hover struct {
		Contents struct {
			Value string `json:"value"`
		} `json:"contents"`
	}
	pos := map[string]any{
		"textDocument": map[string]any{"uri": uri("other.lisp")},
		"position":     map[string]any{"line": 0, "character": 2},
	}
	c.request("textDocument/hover", pos, &hover)
	msgs, _ := c.diagnosticsAfter(m, uri("other.lisp"))
	assert.Empty(t, msgs, "elps#953: foo, which a macro call in app.lisp defines, resolves in other.lisp")
	assert.Contains(t, hover.Contents.Value, "foo")

	// Definition of foo in other.lisp leads to the call that defines it.
	var def lspLocation
	c.request("textDocument/definition", pos, &def)
	assert.Equal(t, "app.lisp:0:11", def.String())

	// A save of the macro that stops it defining its argument: foo is
	// undefined again. The save loads the macro again, so the calls of
	// every file are expanded again.
	assert.Equal(t, []string{"undefined symbol: foo"},
		save("macros.lisp", "(defmacro defhelper (name) '(defun fixed () 1))\n"))
	assert.Empty(t, save("macros.lisp", files["macros.lisp"]), "the macro defines foo again")

	// A save of app.lisp that defines bar in place of foo.
	assert.Equal(t, []string{"undefined symbol: foo"}, save("app.lisp", "(defhelper bar)\n(bar)\n"))
	assert.Empty(t, save("app.lisp", files["app.lisp"]))
	// A save that removes the call: app.lisp keeps no macro call, so the
	// tables lose foo all the same.
	assert.Equal(t, []string{"undefined symbol: foo"}, save("app.lisp", "(foo)\n"))
}
