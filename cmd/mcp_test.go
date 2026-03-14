package cmd

import (
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/modelcontextprotocol/go-sdk/mcp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMCPCommand_StdioRoundTrip(t *testing.T) {
	wd, err := os.Getwd()
	require.NoError(t, err)
	root := filepath.Dir(wd)
	bin := filepath.Join(t.TempDir(), "elps")

	build := exec.Command("go", "build", "-o", bin, ".")
	build.Dir = root
	output, err := build.CombinedOutput()
	require.NoError(t, err, string(output))

	client := mcp.NewClient(&mcp.Implementation{Name: "test-client", Version: "v1.0.0"}, nil)
	session, err := client.Connect(context.Background(), &mcp.CommandTransport{
		Command: exec.Command(bin, "mcp"),
	}, nil)
	require.NoError(t, err)
	defer session.Close()

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
	wd, err := os.Getwd()
	require.NoError(t, err)
	root := filepath.Dir(wd)
	bin := filepath.Join(t.TempDir(), "elps")

	build := exec.Command("go", "build", "-o", bin, ".")
	build.Dir = root
	output, err := build.CombinedOutput()
	require.NoError(t, err, string(output))

	home := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(home, ".elps.yaml"), []byte("color: never\n"), 0o644))

	client := mcp.NewClient(&mcp.Implementation{Name: "test-client", Version: "v1.0.0"}, nil)
	cmd := exec.Command(bin, "mcp")
	cmd.Env = append(os.Environ(), "HOME="+home)
	session, err := client.Connect(context.Background(), &mcp.CommandTransport{Command: cmd}, nil)
	require.NoError(t, err)
	defer session.Close()

	res, err := session.CallTool(context.Background(), &mcp.CallToolParams{Name: "describe_server"})
	require.NoError(t, err)
	require.False(t, res.IsError)
}

func TestMCPCommand_ProvidesStdlibQualifiedSymbolsByDefault(t *testing.T) {
	wd, err := os.Getwd()
	require.NoError(t, err)
	root := filepath.Dir(wd)
	bin := filepath.Join(t.TempDir(), "elps")

	build := exec.Command("go", "build", "-o", bin, ".")
	build.Dir = root
	output, err := build.CombinedOutput()
	require.NoError(t, err, string(output))

	client := mcp.NewClient(&mcp.Implementation{Name: "test-client", Version: "v1.0.0"}, nil)
	session, err := client.Connect(context.Background(), &mcp.CommandTransport{
		Command: exec.Command(bin, "mcp"),
	}, nil)
	require.NoError(t, err)
	defer session.Close()

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
