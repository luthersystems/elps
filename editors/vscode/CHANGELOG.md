# Changelog

Published extension versions track the `elps` release tag they ship with -- the
publish workflow sets `package.json` from the tag name -- so the numbering
jumps from 0.2.0 to 1.50.0.

## 1.50.0

- LSP: negotiate `positionEncoding` with the client and convert columns at the
  protocol boundary, so positions are correct in files containing non-ASCII text
- LSP: semantic tokens are measured from the source span, and the reader's `'`,
  `#^`, and `#'` heads no longer emit spurious tokens
- Rename: token end columns are counted in bytes, so renaming an identifier in a
  file with non-ASCII characters no longer corrupts the source
- DAP: the debugger waits on the debug event rather than a wall clock, removing
  timing-dependent stepping behaviour
- Analysis: `Config.MacroExpander` is honoured in `AnalyzeFile`, so macro-aware
  diagnostics match the CLI

## 0.2.0

- Full-featured extension with LSP, DAP, and syntax highlighting
- LSP client: diagnostics, hover, completion, go-to-definition, references, rename, semantic tokens, call hierarchy, inlay hints, code actions, formatting
- TextMate grammar converted from tree-sitter highlights
- DAP debugger: launch and attach modes with breakpoints, stepping, variable inspection
- Language configuration: bracket matching, auto-close, comment toggling, indentation
- Auto-discovers `elps` binary in common Go install locations
- Grammar test suite via `vscode-tmgrammar-test`
- ELPS logo icon

## 0.1.0

- Initial release: debug adapter only (DAP)
- Launch and attach modes for `elps debug`
