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

Prefer `WithEnv`. Only an environment lets the server expand the macros of
the workspace (see "Architecture" below): with `WithRegistry` alone, a
function that a workspace macro defines is reported as an undefined symbol.
Plain `elps lsp`, with neither option, builds a standard library
environment for this.

When `WithEnv` or `WithRegistry` is provided, the LSP server:
- Resolves symbols from your custom packages in hover and completion
- Includes your package exports in package-qualified completions
- Recognises your builtins in rename protection (prevents renaming builtins)
- Passes your registry to the linter for accurate arity and semantic checks

## Editor Setup

Any LSP client can use the server: configure it to run `elps lsp --stdio`
(or `elps lsp`, since stdio is the default) for `*.lisp` files. An embedder
substitutes its own binary, e.g. `myapp lsp --stdio`.

Step-by-step setup for VS Code, Neovim, Emacs, Helix and JetBrains IDEs is
in `editors/<editor>/README.md` in the ELPS repository
(https://github.com/luthersystems/elps/tree/main/editors). The VS Code
extension starts the server itself and needs no configuration. Setup for the
Claude Code LSP plugin is in `docs/editors.md`
(https://github.com/luthersystems/elps/blob/main/docs/editors.md).

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
- **Captured variables** when hovering the head of a `lambda`: the local
  variables of the enclosing code that the closure uses

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

References, hover, definition, rename and call hierarchy also work from a
workspace file the editor has not opened: the server reads it from disk.
A reference view runs these on results in other files, and a command-line
client or agent sends them without opening anything. The server reads only
`.lisp` files under the workspace root, within the per-file size limit of
the workspace scan.

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

When the server has an environment (`lsp.WithEnv`, which plain
`elps lsp` and `cmd.LSPCommand` without options pass for you), the
index build loads the macros of the workspace into it, and the analysis
expands calls of those macros. A save keeps the loaded macros in step
with the file. If the save changed the file's `in-package`, `use-package`,
`export`, `defmacro`, `defun` or `set` forms, the server evaluates those
forms of the file again, so a changed macro expands with its new
definition. A macro that the save removed or renamed no longer expands.
A save that changes no such form (a function body or a comment) evaluates
nothing. A file with no `in-package` form reloads in the package the index
build gave it: the package that the file before it left active.

The server bounds this evaluation, because every analysis waits for it.
Each form has a budget of 10,000,000 steps
(`analysis.PreambleFormMaxSteps`). The index build has a deadline of 30
seconds, and a save a deadline of 2 seconds. The server logs a warning
(`window/logMessage`) for each form that stops at a limit.

At the index build, a form that uses up its step budget fails, and the
forms after it still load. When the deadline passes, no later form runs:
the server logs a warning that names the files whose macros did not load,
and loads each of them at its next save, even when the save changes
nothing.

At a save, a form that reaches a limit leaves the macros of the file with
the definitions they had before the save. The next save of the file loads
it again, even with the same text.

Limits: a `defun` or `set` that the save removed stays bound in the
environment until the server restarts, and a macro that another file
also defines keeps the definition the environment has.

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
- **Macro-defined functions in other files**: a function that a call of a
  workspace macro defines resolves in the file of the call, but another file
  that uses it reports it as an undefined symbol. The workspace index does
  not expand macro calls (luthersystems/elps#953).
