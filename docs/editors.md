# Editor Setup

This guide covers setting up the ELPS LSP server in various editors and tools.

For LSP features and capabilities, see `elps doc --lsp-guide` or [lsp-guide.md](lsp-guide.md).

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

## VS Code

Use any generic LSP client extension (e.g., [vscode-languageclient](https://github.com/ArtifactDB/vscode-lsp-sample)).

Add to `.vscode/settings.json`:

```json
{
  "elps.server.command": "elps",
  "elps.server.args": ["lsp", "--stdio"]
}
```

Or configure a generic LSP client extension to run `elps lsp --stdio` for
files matching `*.lisp`.

## Neovim (nvim-lspconfig)

```lua
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'lisp',
  callback = function()
    vim.lsp.start({
      name = 'elps',
      cmd = { 'elps', 'lsp', '--stdio' },
      root_dir = vim.fs.dirname(vim.fs.find({ '.git' }, { upward = true })[1]),
    })
  end,
})
```

## Emacs (eglot)

```elisp
(add-to-list 'eglot-server-programs
             '(lisp-mode . ("elps" "lsp" "--stdio")))
```

Then `M-x eglot` in a `.lisp` buffer.

## Embedder Variant

When ELPS is embedded in a Go application (e.g., `shirotester`), the embedder
can expose an LSP command that boots the full runtime. This ensures
Go-registered builtins are resolved correctly for hover, completion, and
diagnostics.

Replace `elps` with the embedder binary in any of the configurations above:

```bash
shirotester lsp --stdio
```

The embedder wires the LSP server with `lsp.WithEnv(env)` or
`lsp.WithRegistry(reg)` to inject its packages.

## Available Features

| Feature            | Description                                        |
|--------------------|----------------------------------------------------|
| Diagnostics        | Parse errors and lint warnings as you type          |
| Hover              | Function signatures, docstrings, source locations   |
| Go to Definition   | Jump to defun/defmacro/set definition sites         |
| Find References    | Find all uses of a symbol across the file           |
| Document Symbols   | Outline of top-level definitions                    |
| Completion         | Scope-aware symbol + package-qualified completion   |
| Rename             | Rename a symbol at definition and all references    |
| Formatting         | Source code formatting via `textDocument/formatting` |
| Signature Help     | Parameter hints for function calls                  |
