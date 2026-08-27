# Changelog

Published extension versions track the `elps` release tag they ship with -- the
publish workflow sets `package.json` from the tag name -- so the numbering
jumps from 0.2.0 to 1.50.0.

## Unreleased

- Language: new `with-cleanup` special operator -- `(with-cleanup (cleanup...)
  body...)` always runs the cleanup forms, whether the body returned normally
  or signalled, and returns the last body value. It does not catch: the error
  is still live once the cleanup has run. Both halves are implicit progns.
  Syntax highlighting, formatter indentation and the language server treat it
  as a special operator.

## 1.52.0

- Language: writing through a program literal -- `stable-sort`, `(slice
  'vector ...)` and `(append 'vector ...)` on a quoted literal or a view over
  one -- now raises the catchable `modify-literal-error` condition
  (`cannot modify a program literal; take a (copy ...) first`) instead of
  silently sorting or appending to a fresh copy. Runtime-constructed lists and
  vectors keep their in-place semantics, and empty sealed inputs are exempt, so
  only code that mutated program text is affected. `handler-bind` and
  `ignore-errors` catch it like any other condition.
- Startup: the binary no longer builds a 2.2MB character-width table at init,
  and interpreter environment construction allocates about half of what it did
  (-53% bytes, -51% allocations), so the language server, linter and formatter
  start faster and analysis of large workspaces costs less memory.

## 1.51.0

- Stability: an array with unset elements no longer takes the host process down
  when the language server evaluates `aref` or `equal?` over it -- every panic
  site in the interpreter is now classified by reachability from lisp and
  enforced by a sweep test.

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
