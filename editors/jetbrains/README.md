# ELPS for JetBrains IDEs

Language server (LSP) and debugger (DAP) setup for IntelliJ-based IDEs. Both
use the [LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij) plugin,
which includes LSP and DAP client support, and need the `elps` binary on your
`$PATH` (`go install github.com/luthersystems/elps@latest`).

Install **LSP4IJ** from the JetBrains Marketplace first.

## Language Server (LSP)

1. Go to **Settings > Languages & Frameworks > Language Servers** and add a
   new server.
2. Set **Command** to `elps lsp --stdio`.
3. Under **Mappings > File name patterns**, add `*.lisp` (and `*.elps` if you
   use it).

## Debugger (DAP)

`elps debug` takes the file to debug as a command-line argument, so the most
reliable setup is to start the DAP server yourself and attach to it.

### Attach Configuration (recommended)

1. Start a DAP server, e.g. in the IDE terminal:
   `elps debug --stop-on-entry myfile.lisp` (listens on `localhost:4711`), or
   start one from your Go application.
2. **Run > Edit Configurations > + > DAP**
3. Set:
   - **Request**: attach
   - **Host**: localhost
   - **Port**: 4711
4. Run the configuration.

### Launch Configuration

1. Go to **Settings > Languages & Frameworks > LSP4IJ > DAP** and add a new
   DAP server:

   | Field      | Value                                              |
   |------------|----------------------------------------------------|
   | Name       | ELPS Debug                                         |
   | Command    | `elps debug --stdio --stop-on-entry path/to/file.lisp` |
   | File types | `*.lisp`, `*.elps`                                 |

   The file path is part of the command because `elps debug` does not read
   it from the launch request. It must lie under the working directory (or
   pass `--root-dir DIR`).
2. **Run > Edit Configurations > + > DAP**
3. Set:
   - **DAP Server**: ELPS Debug
   - **Request**: launch
   - **Program**: `$FilePath$`
   - **Stop on Entry**: true

## Alternative: External Tool

If LSP4IJ is not available, you can run ELPS as an External Tool:

1. **Settings > Tools > External Tools > +**
2. Set **Program** to `elps` and **Arguments** to `run $FilePath$` (output
   only), or `debug --repl $FilePath$` for the interactive CLI debugger in the
   tool window.

See `elps doc --debug-guide` for the launch/attach fields the adapter accepts.
