# Language Server Protocol Guide

ELPS includes a built-in LSP server that provides real-time IDE features for
ELPS source files. It supports any editor with LSP client capabilities.

| Feature              | Description                                        |
|----------------------|----------------------------------------------------|
| Diagnostics          | Parse errors and lint warnings as you type          |
| Hover                | Function signatures, docstrings, source locations   |
| Deprecation          | Struck-through uses, hover banner, completion tag   |
| Go to Definition     | Jump to defun/defmacro/set definition sites         |
| Find References      | Find all uses of a symbol across the file           |
| Document Symbols     | Outline of top-level definitions (defun, set, etc.) |
| Completion           | Scope-aware symbol completion + package-qualified   |
| Rename               | Rename a symbol at definition and all references    |

## Quick Start

### Standalone

```bash
elps lsp --stdio
```

The server reads JSON-RPC messages from stdin and writes responses to stdout.
This is the standard transport for editor integrations.

For debugging or multi-client setups, use TCP:

```bash
elps lsp --port 7998
```

### Embedded

When ELPS is embedded in another Go application, the LSP server can be started
with the embedder's custom builtins and packages:

```go
import (
    "github.com/luthersystems/elps/cmd"
    "github.com/luthersystems/elps/lsp"
)

// Option A: Embed via the CLI command (adds "lsp" subcommand to your CLI).
rootCmd.AddCommand(cmd.LSPCommand(
    cmd.WithEnv(myEnv),       // injects your custom builtins, packages, etc.
    cmd.WithRegistry(myReg),  // alternative: inject just the package registry
))

// Option B: Use the lsp package directly for full control.
srv := lsp.New(
    lsp.WithEnv(myEnv),
    lsp.WithRegistry(myReg),
)
srv.RunStdio()  // or srv.RunTCP("localhost:7998")
```

When `WithEnv` or `WithRegistry` is provided, the LSP server:
- Resolves symbols from your custom packages in hover and completion
- Includes your package exports in package-qualified completions
- Recognises your builtins in rename protection (prevents renaming builtins)
- Passes your registry to the linter for accurate arity and semantic checks

## Editor Setup

### VS Code

Install a generic LSP client extension such as
[vscode-languageclient](https://github.com/AnttiPessa/vscode-generic-lsp) or
configure the built-in LSP support.

Add to `.vscode/settings.json`:

```json
{
  "elps.lsp.path": "elps",
  "elps.lsp.args": ["lsp", "--stdio"]
}
```

Or with a generic LSP extension, configure the server command:

```json
{
  "genericLSP.serverCommand": "elps",
  "genericLSP.serverArgs": ["lsp", "--stdio"],
  "genericLSP.languageId": "elps",
  "genericLSP.fileExtensions": [".lisp"]
}
```

### Neovim (nvim-lspconfig)

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

configs.elps = {
  default_config = {
    cmd = { 'elps', 'lsp', '--stdio' },
    filetypes = { 'lisp' },
    root_dir = lspconfig.util.root_pattern('.git'),
    settings = {},
  },
}

lspconfig.elps.setup({})
```

### Helix

Add to `~/.config/helix/languages.toml`:

```toml
[[language]]
name = "elps"
scope = "source.lisp"
file-types = ["lisp"]
language-servers = ["elps-lsp"]

[language-server.elps-lsp]
command = "elps"
args = ["lsp", "--stdio"]
```

### Emacs (lsp-mode)

```elisp
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(lisp-mode . "elps"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("elps" "lsp" "--stdio"))
    :activation-fn (lsp-activate-on "elps")
    :server-id 'elps-lsp)))
```

## Features in Detail

### Diagnostics

The server publishes diagnostics automatically when you open or save a file.
During editing, diagnostics are debounced (300ms delay) to avoid thrashing.

Two kinds of diagnostics are reported:

1. **Parse errors** (severity: error) — Syntax errors from the ELPS parser.
2. **Lint warnings** (severity: varies) — Static analysis from the ELPS linter,
   including set-usage, if-arity, let-bindings, defun-structure, cond-structure,
   builtin-arity, and semantic checks when workspace analysis is available.

Diagnostics can be suppressed with `; nolint:check-name` comments, the same
syntax used by `elps lint`.

Diagnostics from the `deprecated` check carry the LSP "deprecated" tag, so
editors strike the use out instead of only underlining it. Unused-code
diagnostics carry the "unnecessary" tag, which editors render as faded text.

### Hover

Hovering over a symbol shows:

- **Kind** (function, macro, variable, builtin, special operator, type)
- **Signature** for callables (with `&optional`, `&rest`, `&key` annotations)
- **Deprecation banner** — a **Deprecated.** line quoting the notice, shown
  above the docstring when the symbol's documentation has a `Deprecated:`
  paragraph
- **Docstring** if available
- **Source location** (file:line) for user-defined symbols

### Go to Definition

Jump to where a symbol is defined. Works for:

- `defun` / `defmacro` definitions
- `set` bindings
- Cross-file definitions (when workspace scanning is active)

Builtins and special operators have no navigable source and return no result.

### Find References

Find all locations where a symbol is used. Optionally includes the declaration
site. Works within the current file; cross-file references require workspace
scanning.

### Completion

Two completion modes:

1. **Scope-based**: Type a partial symbol name and get completions from all
   visible symbols in the current scope chain (local bindings, function params,
   top-level definitions, builtins).

2. **Package-qualified**: Type `package-name:` to get completions for all
   exported symbols in that package.

Deprecated symbols are still offered, tagged deprecated so the editor renders
them struck through in the completion list.

Trigger characters: `(` and `:`.

### Document Symbols

Lists all top-level definitions in the file: functions, macros, variables, and
types. Appears in the editor's symbol outline or breadcrumb bar.

### Rename

Rename a symbol at its definition and all references within the file.

Safety checks:
- Builtins and special operators cannot be renamed (prepareRename rejects them)
- External symbols (from other files) cannot be renamed
- Cross-file rename is not yet supported

## Architecture

The LSP server is built on the `analysis` package which provides:

- **Symbol resolution**: Tracks definitions and references across scopes
- **Scope tree**: Lexical scope chain from global to nested let/lambda bodies
- **Workspace scanning**: Cross-file symbol discovery via `ScanWorkspace()`
- **Package exports**: Stdlib and workspace package export resolution

The server performs workspace scanning in the background on initialization.
Each document change triggers re-parsing and re-analysis with the workspace
index providing cross-file context.

### Fault-Tolerant Parsing

The parser is run in a fault-tolerant mode for the LSP: when a document
contains syntax errors (common during editing), the server re-parses
expression-by-expression to recover as many valid top-level expressions as
possible. This ensures that analysis, completion, and other features continue
to work even when the document is temporarily invalid.

### Position Encoding

LSP counts `Position.character` in **UTF-16 code units**, while this server
computes columns in **bytes** internally (the scanner's `Col` is a byte offset
within the line). The two agree on ASCII and diverge on any line containing a
non-ASCII character — by 1 per two-byte character, 2 per CJK character, and
2 per non-BMP character such as an emoji.

The server negotiates at `initialize`:

- If the client advertises `general.positionEncodings` (LSP 3.17) including
  `"utf-8"`, the server selects it and answers with
  `capabilities.positionEncoding: "utf-8"`. Its native byte columns are then
  correct as sent, and nothing is converted.
- Otherwise — which includes every LSP 3.16 client — the server uses UTF-16,
  as the specification requires, and converts.

What is converted today:

- **Every inbound position.** A cursor column from any request (hover,
  definition, references, highlight, completion, signature help, rename,
  prepare rename, linked editing, call hierarchy, selection range, inlay hint
  ranges) is converted to a byte column once on entry.
- **Outbound rename edits**, including cross-file edits, and the
  `prepareRename` range that goes with them. Rename is converted because its
  answer is applied to the file unread; a wrong unit there silently rewrites
  the program rather than misplacing a highlight.

What is **not** converted yet, and still ships byte columns under UTF-16:
diagnostics, hover, definition, references, document highlight, document and
workspace symbols, folding and selection ranges, call hierarchy, linked editing
ranges, inlay hint positions, code action edits, and semantic tokens. Those
misplace a range on a line with non-ASCII text; none of them writes to a file.
(Code action `; nolint:` inserts use the byte length of the line, which is
never smaller than the UTF-16 length, so clients clamp them to end of line and
they land correctly either way.)

## Limitations

- **Full document sync only**: Each edit sends the complete document content.
  Incremental sync may be added in the future.
- **UTF-16 columns are only partly implemented**: see
  [Position Encoding](#position-encoding) for exactly which responses are
  converted and which still carry byte columns.
- **Single-file analysis**: Rename and references operate within the current
  file. Cross-file rename is not yet supported.
- **No formatting on save**: Use `elps fmt` separately or configure your editor
  to run it as a formatter.
- **No semantic tokens**: Syntax highlighting must be provided by a TextMate
  grammar or tree-sitter parser, not the LSP server.
