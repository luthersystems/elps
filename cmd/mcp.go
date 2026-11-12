package cmd

import (
	"context"
	"fmt"

	"github.com/luthersystems/elps/mcpserver"
	"github.com/spf13/cobra"
)

// MCPCommand creates the "mcp" cobra command with optional embedder
// configuration.
func MCPCommand(opts ...Option) *cobra.Command {
	var cfg cmdConfig
	for _, opt := range opts {
		opt(&cfg)
	}

	runStdio := cfg.runStdio
	if runStdio == nil {
		runStdio = func(ctx context.Context, srv *mcpserver.Server) error {
			return srv.RunStdio(ctx)
		}
	}

	cmd := &cobra.Command{
		Use:   "mcp",
		Short: "Start the ELPS MCP server over stdio",
		Long: `Start an MCP server that exposes read-only ELPS language and analysis tools.

This server is intended for agent/tooling clients over stdio. It does not replace
the ELPS LSP server or the existing ELPS CLI commands.`,
		Args: cobra.NoArgs,
		RunE: func(_ *cobra.Command, _ []string) error {
			// An env, not only its registry, so the server expands
			// workspace macros (see cmdConfig.analysisEnv).
			setup, err := cfg.analysisEnv()
			if err != nil {
				return fmt.Errorf("mcp server bootstrap error: %w", err)
			}
			serverOpts := []mcpserver.Option{mcpserver.WithRegistry(setup.registry)}
			if setup.env != nil {
				serverOpts = append(serverOpts, mcpserver.WithEnv(setup.env))
			}
			srv := mcpserver.New(serverOpts...)
			if err := runStdio(context.Background(), srv); err != nil {
				return fmt.Errorf("mcp server error: %w", err)
			}
			return nil
		},
	}

	return cmd
}

func init() {
	rootCmd.AddCommand(MCPCommand())
}
