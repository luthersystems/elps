<p align="center">
  <img src="https://raw.githubusercontent.com/luthersystems/elps/main/editors/vscode/images/logo.png" alt="ELPS — Embedded Lisp Interpreter" width="128">
</p>

<p align="center">
  <strong>ELPS — Syntax highlighting, language server, and debugger for VS Code</strong>
</p>

<p align="center">
  <a href="https://github.com/luthersystems/elps">ELPS</a> •
  <a href="https://luthersystems.com">Luther Systems</a> •
  <a href="https://insideout.luthersystems.com">InsideOut</a> •
  <a href="https://insideout.luthersystems.com/discord"><img src="https://img.shields.io/badge/Discord-Join%20Us-5865F2?logo=discord&logoColor=white" alt="Discord"></a>
</p>

---

Full-featured VS Code extension for [ELPS](https://github.com/luthersystems/elps), an embedded Lisp interpreter implemented in Go. Provides syntax highlighting, language server integration, and debugging.

> **Requires the `elps` binary.** The extension provides editor features but needs the `elps` command-line tool installed separately. See [Prerequisites](#prerequisites) below.

## Prerequisites

Install the `elps` binary using the Go toolchain (requires [Go 1.21+](https://go.dev/dl/)):

```bash
go install github.com/luthersystems/elps@latest
```

This installs to `$GOPATH/bin` (typically `~/go/bin`). The extension auto-discovers the binary in common Go install locations (`~/go/bin`, `$GOPATH/bin`, `/usr/local/bin`, `/opt/homebrew/bin`).

If the binary isn't found automatically, set the path in VS Code settings: `elps.path`.

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

Automatically starts the ELPS language server and provides:

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
- **Code Actions** — quick fixes for diagnostics (including `; nolint:` suppression)
- **Folding Ranges** — fold s-expressions and comment blocks
- **Selection Range** — expand/contract selection by AST node
- **Document Highlighting** — highlight symbol occurrences
- **Formatting** — format documents (`Shift+Alt+F`)

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

Create a `.vscode/launch.json` (or use the auto-generated snippet from `Cmd+Shift+P` > "Debug: Add Configuration"):

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

**Language server fails to start**: The extension auto-discovers `elps` in common Go install locations. If it's installed elsewhere, set the full path:

```json
{
  "elps.path": "/path/to/elps"
}
```

Check the "ELPS Language Server" output channel (`View > Output > ELPS Language Server`) for details.

**Syntax highlighting looks wrong**: If another extension claims `.lisp` files, set `"files.associations": { "*.lisp": "elps" }` in your workspace settings.

**Debug adapter fails**: Verify `elps debug --help` works from your terminal.

## Development

To build and install from source:

```bash
git clone https://github.com/luthersystems/elps.git
cd elps/editors/vscode
npm install
npm run package
code --install-extension elps-*.vsix
```

Run grammar tests:

```bash
npm test
```

## Links

- [ELPS Repository](https://github.com/luthersystems/elps)
- [Luther Systems](https://luthersystems.com)
- [InsideOut Platform](https://insideout.luthersystems.com)

### Community & Support

- [Discord](https://insideout.luthersystems.com/discord)
- [General Inquiry Call](https://insideout.luthersystems.com/general-call)
- [Tech Call](https://insideout.luthersystems.com/tech-call)
