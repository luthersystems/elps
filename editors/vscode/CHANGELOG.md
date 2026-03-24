# Changelog

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
