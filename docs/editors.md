# Editor Setup

Per-editor setup for the ELPS language server (`elps lsp`) and debugger
(`elps debug`) lives next to each editor's integration under
[`editors/`](../editors/):

| Editor | LSP | DAP | Setup |
|--------|-----|-----|-------|
| VS Code | Yes (extension) | Yes (extension) | [editors/vscode/README.md](../editors/vscode/README.md) |
| Neovim | `vim.lsp` | nvim-dap | [editors/neovim/README.md](../editors/neovim/README.md) |
| Emacs | eglot, lsp-mode | dap-mode | [editors/emacs/README.md](../editors/emacs/README.md) |
| Helix | Yes | Attach only (experimental) | [editors/helix/README.md](../editors/helix/README.md) |
| JetBrains IDEs | LSP4IJ | LSP4IJ | [editors/jetbrains/README.md](../editors/jetbrains/README.md) |
| Claude Code | Plugin | — | [below](#claude-code) |

Any other LSP client can run `elps lsp --stdio` for `*.lisp` files (stdio is
the default transport; `elps lsp --port N` listens on TCP instead). Any other
DAP client can spawn `elps debug --stdio file.lisp`, or attach over TCP to
`elps debug file.lisp` (port 4711 by default). Note that `elps debug` takes
the file on the command line.

For features and configuration, see `elps doc --lsp-guide`
([lsp-guide.md](lsp-guide.md)) and `elps doc --debug-guide`
([debugging-guide.md](debugging-guide.md)).

## Claude Code

Claude Code supports LSP plugins that provide inline diagnostics when reading
or editing files.

### Prerequisites

- `elps` binary on `$PATH` (or an embedder like `shirotester`)
- Claude Code installed

### 1. Create the plugin directory structure

```
.claude/plugins/elps-lsp/
  .claude-plugin/
    plugin.json        # Plugin manifest (required)
  .lsp.json            # LSP server config
```

### 2. Plugin manifest

Create `.claude/plugins/elps-lsp/.claude-plugin/plugin.json`:

```json
{
  "name": "elps-lsp",
  "description": "ELPS Lisp language server",
  "version": "1.0.0",
  "author": {
    "name": "Luther Systems"
  }
}
```

### 3. LSP config

Create `.claude/plugins/elps-lsp/.lsp.json`:

```json
{
  "elps": {
    "command": "elps",
    "args": ["lsp"],
    "extensionToLanguage": {
      ".lisp": "lisp"
    }
  }
}
```

For embedders that register Go builtins (e.g., substrate's `shirotester`):

```json
{
  "elps": {
    "command": "shirotester",
    "args": ["lsp"],
    "extensionToLanguage": {
      ".lisp": "lisp"
    }
  }
}
```

### 4. Create a local marketplace

Create `.claude/plugins/.claude-plugin/marketplace.json`:

```json
{
  "name": "my-plugins",
  "owner": {
    "name": "My Org"
  },
  "plugins": [
    {
      "name": "elps-lsp",
      "source": "./elps-lsp",
      "description": "ELPS Lisp language server"
    }
  ]
}
```

### 5. Install the plugin

```bash
claude plugin marketplace add ./.claude/plugins
claude plugin install elps-lsp@my-plugins
```

### 6. Restart Claude Code

The LSP starts on demand when Claude reads or edits `.lisp` files. Diagnostics
appear inline in tool results.

## Embedder Variant

When ELPS is embedded in a Go application (e.g., `shirotester`), the embedder
can expose an LSP command that boots the full runtime. This ensures
Go-registered builtins are resolved correctly for hover, completion, and
diagnostics.

Replace `elps` with the embedder binary in any editor configuration:

```bash
shirotester lsp --stdio
```

The embedder wires the LSP server with `lsp.WithEnv(env)` or
`lsp.WithRegistry(reg)` to inject its packages.
