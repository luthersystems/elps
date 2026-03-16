package cmd

import (
	"context"
	"fmt"

	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/mcpserver"
	"github.com/spf13/cobra"
)

var (
	newMCPDocEnv = lisplib.NewDocEnv
	runMCPStdio  = func(ctx context.Context, srv *mcpserver.Server) error {
		return srv.RunStdio(ctx)
	}
)

// MCPCommand creates the "mcp" cobra command with optional embedder
// configuration.
func MCPCommand(opts ...Option) *cobra.Command {
	var cfg cmdConfig
	for _, opt := range opts {
		opt(&cfg)
	}

	cmd := &cobra.Command{
		Use:   "mcp",
		Short: "Start the ELPS MCP server over stdio",
		Long: `Start an MCP server that exposes read-only ELPS language and analysis tools.

		This server is intended for agent/tooling clients over stdio. It does not replace
the ELPS LSP server or the existing ELPS CLI commands.`,
		Args: cobra.NoArgs,
		RunE: func(_ *cobra.Command, _ []string) error {
			var serverOpts []mcpserver.Option
			if cfg.env != nil {
				serverOpts = append(serverOpts, mcpserver.WithEnv(cfg.env))
			}
			if reg := cfg.resolveRegistry(); reg != nil {
				serverOpts = append(serverOpts, mcpserver.WithRegistry(reg))
			} else {
				env, err := newMCPDocEnv()
				if err != nil {
					return fmt.Errorf("mcp server bootstrap error: %w", err)
				}
				serverOpts = append(serverOpts, mcpserver.WithRegistry(env.Runtime.Registry))
			}
			srv := mcpserver.New(serverOpts...)
			if err := runMCPStdio(context.Background(), srv); err != nil {
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
