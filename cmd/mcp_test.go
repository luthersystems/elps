package cmd

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/mcpserver"
	"github.com/modelcontextprotocol/go-sdk/mcp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMCPCommand_StdioRoundTrip(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test in short mode")
	}
	wd, err := os.Getwd()
	require.NoError(t, err)
	root := filepath.Dir(wd)
	bin := filepath.Join(t.TempDir(), "elps")

	build := exec.Command("go", "build", "-o", bin, ".") //nolint:gosec // test builds local binary in temp dir
	build.Dir = root
	output, err := build.CombinedOutput()
	require.NoError(t, err, string(output))

	client := mcp.NewClient(&mcp.Implementation{Name: "test-client", Version: "v1.0.0"}, nil)
	session, err := client.Connect(context.Background(), &mcp.CommandTransport{
		Command: exec.Command(bin, "mcp"), //nolint:gosec // test executes freshly built local binary
	}, nil)
	require.NoError(t, err)
	defer closeMCPClientSession(t, session)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{Name: "describe_server"})
	require.NoError(t, err)
	require.False(t, res.IsError)

	var got struct {
		Name     string `json:"name"`
		ReadOnly bool   `json:"read_only"`
	}
	data, err := json.Marshal(res.StructuredContent)
	require.NoError(t, err)
	require.NoError(t, json.Unmarshal(data, &got))
	assert.Equal(t, "elps-mcp", got.Name)
	assert.True(t, got.ReadOnly)
}

func TestMCPCommand_StdioRoundTripWithConfigFile(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test in short mode")
	}
	wd, err := os.Getwd()
	require.NoError(t, err)
	root := filepath.Dir(wd)
	bin := filepath.Join(t.TempDir(), "elps")

	build := exec.Command("go", "build", "-o", bin, ".") //nolint:gosec // test builds local binary in temp dir
	build.Dir = root
	output, err := build.CombinedOutput()
	require.NoError(t, err, string(output))

	home := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(home, ".elps.yaml"), []byte("color: never\n"), 0o600))

	client := mcp.NewClient(&mcp.Implementation{Name: "test-client", Version: "v1.0.0"}, nil)
	cmd := exec.Command(bin, "mcp") //nolint:gosec // test executes freshly built local binary
	cmd.Env = append(os.Environ(), "HOME="+home)
	session, err := client.Connect(context.Background(), &mcp.CommandTransport{Command: cmd}, nil)
	require.NoError(t, err)
	defer closeMCPClientSession(t, session)

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{Name: "describe_server"})
	require.NoError(t, err)
	require.False(t, res.IsError)
}

func TestMCPCommand_ProvidesStdlibQualifiedSymbolsByDefault(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test in short mode")
	}
	wd, err := os.Getwd()
	require.NoError(t, err)
	root := filepath.Dir(wd)
	bin := filepath.Join(t.TempDir(), "elps")

	build := exec.Command("go", "build", "-o", bin, ".") //nolint:gosec // test builds local binary in temp dir
	build.Dir = root
	output, err := build.CombinedOutput()
	require.NoError(t, err, string(output))

	client := mcp.NewClient(&mcp.Implementation{Name: "test-client", Version: "v1.0.0"}, nil)
	session, err := client.Connect(context.Background(), &mcp.CommandTransport{
		Command: exec.Command(bin, "mcp"), //nolint:gosec // test executes freshly built local binary
	}, nil)
	require.NoError(t, err)
	defer closeMCPClientSession(t, session)

	content := `(string:join "," items)`
	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{
		Name: "hover",
		Arguments: map[string]any{
			"path":      filepath.Join(t.TempDir(), "stdlib.lisp"),
			"line":      0,
			"character": strings.Index(content, "join"),
			"content":   content,
		},
	})
	require.NoError(t, err)
	require.False(t, res.IsError)

	var got struct {
		Found      bool   `json:"found"`
		SymbolName string `json:"symbol_name"`
	}
	data, err := json.Marshal(res.StructuredContent)
	require.NoError(t, err)
	require.NoError(t, json.Unmarshal(data, &got))
	assert.True(t, got.Found)
	assert.Equal(t, "join", got.SymbolName)
}

func withTestDocEnv(fn func() (*lisp.LEnv, error)) Option {
	return func(c *cmdConfig) { c.newDocEnv = fn }
}

func withTestStdioRunner(fn func(context.Context, *mcpserver.Server) error) Option {
	return func(c *cmdConfig) { c.runStdio = fn }
}

func TestMCPCommand_ReturnsBootstrapErrors(t *testing.T) {
	cmd := MCPCommand(
		withTestDocEnv(func() (*lisp.LEnv, error) {
			return nil, errors.New("boom")
		}),
		withTestStdioRunner(func(context.Context, *mcpserver.Server) error {
			return nil
		}),
	)
	cmd.SetArgs(nil)
	err := cmd.Execute()
	require.Error(t, err)
	assert.Contains(t, err.Error(), "mcp server bootstrap error")
	assert.Contains(t, err.Error(), "boom")
}

func TestMCPCommand_ReturnsServerErrors(t *testing.T) {
	cmd := MCPCommand(
		withTestStdioRunner(func(context.Context, *mcpserver.Server) error {
			return errors.New("stdio failed")
		}),
	)
	cmd.SetArgs(nil)
	err := cmd.Execute()
	require.Error(t, err)
	assert.Contains(t, err.Error(), "mcp server error")
	assert.Contains(t, err.Error(), "stdio failed")
}

func closeMCPClientSession(t *testing.T, session *mcp.ClientSession) {
	t.Helper()
	require.NoError(t, session.Close())
}
