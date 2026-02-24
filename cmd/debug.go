// Copyright © 2018 The ELPS authors

package cmd

import (
	"fmt"
	"log"
	"net"
	"os"
	"path/filepath"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/lisp/x/debugger/dapserver"
	"github.com/luthersystems/elps/parser"
	"github.com/spf13/cobra"
)

var (
	debugPort        int
	debugStdio       bool
	debugStopOnEntry bool
	debugRootDir     string
)

var debugCmd = &cobra.Command{
	Use:   "debug [flags] file.lisp",
	Short: "Run a file under the DAP debugger",
	Long: `Start a DAP (Debug Adapter Protocol) debug server for an ELPS source file.

The debug server allows editors (VS Code, Neovim, Helix, etc.) to set
breakpoints, step through code, and inspect variables using the standard
Debug Adapter Protocol.

Transport modes:
  --port N     Listen for a DAP client on TCP port N (default: 4711)
  --stdio      Use stdin/stdout for DAP communication (for editors that
               launch the debug adapter as a child process)

The --stop-on-entry flag pauses execution before the first expression,
giving the editor time to set breakpoints.

Examples:
  elps debug myfile.lisp                     Debug with TCP on port 4711
  elps debug --port 9229 myfile.lisp         Debug with TCP on port 9229
  elps debug --stdio myfile.lisp             Debug with stdio transport
  elps debug --stop-on-entry myfile.lisp     Pause at first expression`,
	Args: cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		rootDir := debugRootDir
		if rootDir == "" {
			wd, err := os.Getwd()
			if err != nil {
				fmt.Fprintf(os.Stderr, "cannot determine working directory: %v\n", err)
				os.Exit(1)
			}
			rootDir = wd
		}
		rootDir, err := filepath.Abs(rootDir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cannot resolve root directory: %v\n", err)
			os.Exit(1)
		}

		file := args[0]
		relFile, ferr := toRelativePath(rootDir, file)
		if ferr != nil {
			fmt.Fprintf(os.Stderr, "%v\n", ferr)
			os.Exit(1)
		}

		// Create the debugger engine.
		dbg := debugger.New(
			debugger.WithStopOnEntry(debugStopOnEntry),
		)
		dbg.Enable()

		// Create the DAP server.
		srv := dapserver.New(dbg)

		// Wire the engine's event callback to send DAP events.
		// This runs on the eval goroutine, so it must not block.
		// The DAP server's handler will pick up stopped events
		// directly from the engine.

		// Set up the ELPS environment.
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS(rootDir)}
		env.Runtime.Debugger = dbg

		rc := lisp.InitializeUserEnv(env)
		if !rc.IsNil() {
			fmt.Fprintln(os.Stderr, rc)
			os.Exit(1)
		}
		rc = lisplib.LoadLibrary(env)
		if !rc.IsNil() {
			fmt.Fprintln(os.Stderr, rc)
			os.Exit(1)
		}
		rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
		if !rc.IsNil() {
			fmt.Fprintln(os.Stderr, rc)
			os.Exit(1)
		}

		// Start the eval goroutine. After evaluation finishes, notify
		// the DAP server so it can send ExitedEvent + TerminatedEvent
		// while ServeConn is still running.
		evalDone := make(chan *lisp.LVal, 1)
		go func() {
			res := env.LoadFile(relFile)
			exitCode := 0
			if res.Type == lisp.LError {
				exitCode = 1
			}
			dbg.NotifyExit(exitCode)
			evalDone <- res
		}()

		// Serve DAP.
		if debugStdio {
			log.Println("DAP debugger: using stdio transport")
			if err := srv.ServeStdio(os.Stdin, os.Stdout); err != nil {
				fmt.Fprintf(os.Stderr, "dap server error: %v\n", err)
			}
		} else {
			addr := fmt.Sprintf("localhost:%d", debugPort)
			ln, err := net.Listen("tcp", addr)
			if err != nil {
				fmt.Fprintf(os.Stderr, "cannot listen on %s: %v\n", addr, err)
				os.Exit(1)
			}
			defer ln.Close() //nolint:errcheck
			log.Printf("DAP debugger listening on %s", addr)
			log.Println("Waiting for DAP client to connect...")

			conn, err := ln.Accept()
			if err != nil {
				fmt.Fprintf(os.Stderr, "accept error: %v\n", err)
				os.Exit(1)
			}
			if err := srv.ServeConn(conn); err != nil {
				fmt.Fprintf(os.Stderr, "dap server error: %v\n", err)
			}
		}

		// Wait for eval to finish and report any errors.
		res := <-evalDone
		if res.Type == lisp.LError {
			renderLispError(res, file)
			os.Exit(1)
		}
	},
}

func init() {
	rootCmd.AddCommand(debugCmd)

	debugCmd.Flags().IntVar(&debugPort, "port", 4711,
		"TCP port for DAP server (default: 4711)")
	debugCmd.Flags().BoolVar(&debugStdio, "stdio", false,
		"Use stdin/stdout for DAP communication")
	debugCmd.Flags().BoolVar(&debugStopOnEntry, "stop-on-entry", false,
		"Pause execution before the first expression")
	debugCmd.Flags().StringVar(&debugRootDir, "root-dir", "",
		"Root directory for file access confinement (default: working directory)")
}
