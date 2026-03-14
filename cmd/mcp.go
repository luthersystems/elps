package cmd

import (
	"context"
	"fmt"
	"os"

	"github.com/luthersystems/elps/lisp/lisplib"
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

	cmd := &cobra.Command{
		Use:   "mcp",
		Short: "Start the ELPS MCP server over stdio",
		Long: `Start an MCP server that exposes read-only ELPS language and analysis tools.

This server is intended for agent/tooling clients over stdio. It does not replace
the ELPS LSP server or the existing ELPS CLI commands.`,
		Args: cobra.NoArgs,
		Run: func(_ *cobra.Command, _ []string) {
			var serverOpts []mcpserver.Option
			if cfg.env != nil {
				serverOpts = append(serverOpts, mcpserver.WithEnv(cfg.env))
			}
			if reg := cfg.resolveRegistry(); reg != nil {
				serverOpts = append(serverOpts, mcpserver.WithRegistry(reg))
			} else {
				env, err := lisplib.NewDocEnv()
				if err != nil {
					fmt.Fprintf(os.Stderr, "mcp server bootstrap error: %v\n", err)
					os.Exit(1)
				}
				serverOpts = append(serverOpts, mcpserver.WithRegistry(env.Runtime.Registry))
			}
			srv := mcpserver.New(serverOpts...)
			if err := srv.RunStdio(context.Background()); err != nil {
				fmt.Fprintf(os.Stderr, "mcp server error: %v\n", err)
				os.Exit(1)
			}
		},
	}

	return cmd
}

func init() {
	rootCmd.AddCommand(MCPCommand())
}
