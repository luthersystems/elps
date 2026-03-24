# ELPS — VS Code Extension

Full-featured VS Code extension for the [ELPS](https://github.com/luthersystems/elps) Lisp interpreter, providing syntax highlighting, language server integration, and debugging.

## Prerequisites

Install the `elps` binary using the Go toolchain:

```bash
go install github.com/luthersystems/elps@latest
```

This installs to `$GOPATH/bin` (typically `~/go/bin`). The extension auto-discovers the binary in common Go install locations. Alternatively, build from source:

```bash
git clone https://github.com/luthersystems/elps.git
cd elps && make
```

## Install

From the `editors/vscode/` directory:

```bash
npm install
npm run package
code --install-extension elps-0.2.0.vsix
```

## Features

### Syntax Highlighting

TextMate grammar for `.lisp` files with support for:

- Comments, strings, raw strings (`"""..."""`)
- Numbers (integer, float, hex `#xFF`, octal `#o77`)
- Keywords (`:keyword`), booleans (`true`, `false`), nil (`()`)
- Definition forms (`defun`, `defmacro`, `deftype`, `defconst`, `lambda`, `let`)
- Special operators (`if`, `cond`, `set`, `handler-bind`, `thread-first`, etc.)
- Built-in functions (`car`, `map`, `concat`, `sorted-map`, etc.)
- Qualified symbols (`pkg:name`)
- Quote/function-quote/expr-shorthand prefixes (`'`, `#'`, `#^`)
- Parameter lists with `&optional`, `&rest`, `&key`

### Language Server (LSP)

Automatically starts `elps lsp --stdio` and provides:

- **Diagnostics** — real-time parse errors and lint warnings
- **Hover** — documentation and type information
- **Completion** — symbol completion (triggers on `(` and `:`)
- **Signature Help** — function parameter hints
- **Go to Definition** — jump to symbol definitions
- **Find References** — find all usages across workspace
- **Rename** — rename symbols across files
- **Document Symbols** — outline view of functions, macros, variables
- **Workspace Symbols** — search symbols across all `.lisp` files
- **Semantic Tokens** — precise syntax highlighting from the language server
- **Call Hierarchy** — incoming and outgoing call chains
- **Inlay Hints** — parameter name hints for function calls
- **Code Actions** — quick fixes for diagnostics
- **Folding Ranges** — fold s-expressions and comment blocks
- **Selection Range** — expand/contract selection by AST node
- **Document Highlighting** — highlight symbol occurrences
- **Formatting** — format documents via the language server

### Debugger (DAP)

Launch or attach to the ELPS debug adapter:

- Line breakpoints
- Conditional breakpoints (Lisp expressions)
- Hit count breakpoints (`>N`, `>=N`, `==N`, `%N`)
- Log points (message templates with `{expr}` interpolation)
- Function breakpoints
- Exception breakpoints
- Step into / over / out
- Variable inspection (local and package scopes)
- Watch expressions
- Debug console evaluation
- Pause / continue

## Settings

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `elps.path` | string | `"elps"` | Path to the `elps` binary |
| `elps.lsp.enable` | boolean | `true` | Enable the ELPS language server |
| `elps.lsp.trace.server` | string | `"off"` | Trace LSP communication (`off`, `messages`, `verbose`) |

## Debug Configuration

Create a `.vscode/launch.json` (or use the auto-generated snippet):

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "elps",
      "request": "launch",
      "name": "Debug ELPS",
      "program": "${file}",
      "stopOnEntry": true
    },
    {
      "type": "elps",
      "request": "attach",
      "name": "Attach to ELPS",
      "host": "localhost",
      "port": 4711
    }
  ]
}
```

### Debug Launch Attributes

| Attribute | Type | Default | Description |
|-----------|------|---------|-------------|
| `program` | string | `${file}` | Path to the `.lisp` file to debug |
| `stopOnEntry` | boolean | `true` | Pause before the first expression |
| `rootDir` | string | `${workspaceFolder}` | Source root for file resolution |
| `elpsPath` | string | `""` | Override `elps.path` setting for this session |

### Debug Attach Attributes

| Attribute | Type | Default | Description |
|-----------|------|---------|-------------|
| `host` | string | `"localhost"` | Host of the running DAP server |
| `port` | number | `4711` | Port of the running DAP server |

## Troubleshooting

**Extension doesn't activate / "ELPS" not in language list**: VS Code must trust the workspace. If the workspace is in "Restricted Mode", click "Trust" in the banner or run `Workspaces: Manage Workspace Trust` from the command palette.

**Language server fails to start**: VS Code on macOS doesn't inherit your shell PATH. If `elps` is installed in a non-standard location (e.g., `~/go/bin`), set the full path in settings:

```json
{
  "elps.path": "/Users/you/go/bin/elps"
}
```

Check the "ELPS Language Server" output channel (`View > Output > ELPS Language Server`) for details.

**Syntax highlighting looks wrong**: If another extension claims `.lisp` files, set `"files.associations": { "*.lisp": "elps" }` in your workspace settings.

**Debug adapter fails**: Verify `elps debug --stdio --help` works from your terminal.
