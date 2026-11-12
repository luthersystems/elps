// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"log"

	"github.com/luthersystems/elps/lsp"
	"github.com/spf13/cobra"
)

// LSPCommand creates the "lsp" cobra command with optional embedder
// configuration. Embedders can pass WithRegistry or WithEnv to inject
// Go-registered symbols for semantic analysis. Only an env lets the
// server expand workspace macros; without either option the command
// builds a stdlib env for that (see cmdConfig.analysisEnv).
func LSPCommand(opts ...Option) *cobra.Command {
	var cfg cmdConfig
	for _, o := range opts {
		o(&cfg)
	}

	var (
		stdio bool
		port  int
	)

	cmd := &cobra.Command{
		Use:   "lsp [flags]",
		Short: "Start the ELPS Language Server Protocol server",
		Long: `Start an LSP server for ELPS source files.

The language server provides real-time IDE features including diagnostics,
hover documentation, go-to-definition, find references, completion,
document symbols, and rename support.

Transport modes:
  --stdio      Use stdin/stdout for LSP communication (default)
  --port N     Listen for an LSP client on TCP port N

Examples:
  elps lsp                           Start with stdio transport
  elps lsp --stdio                   Same as above (explicit)
  elps lsp --port 7998               Start with TCP on port 7998

Editor configuration (VS Code):
  Install a generic LSP client extension and configure it to run
  "elps lsp --stdio" for .lisp files.`,
		Args: cobra.NoArgs,
		RunE: func(_ *cobra.Command, _ []string) error {
			setup, err := cfg.analysisEnv()
			if err != nil {
				return fmt.Errorf("lsp server bootstrap error: %w", err)
			}
			serverOpts := []lsp.Option{lsp.WithRegistry(setup.registry)}
			if setup.env != nil {
				serverOpts = append(serverOpts, lsp.WithEnv(setup.env))
			}
			srv := lsp.New(serverOpts...)

			if !stdio && port > 0 {
				addr := fmt.Sprintf("localhost:%d", port)
				log.Printf("ELPS LSP server listening on %s", addr)
				if err := srv.RunTCP(addr); err != nil {
					return fmt.Errorf("lsp server error: %w", err)
				}
			} else {
				if err := srv.RunStdio(); err != nil {
					return fmt.Errorf("lsp server error: %w", err)
				}
			}
			return nil
		},
	}

	cmd.Flags().BoolVar(&stdio, "stdio", false,
		"Use stdin/stdout for LSP communication (default behavior)")
	cmd.Flags().IntVar(&port, "port", 0,
		"TCP port for LSP server (use instead of --stdio)")

	return cmd
}

func init() {
	rootCmd.AddCommand(LSPCommand())
}
