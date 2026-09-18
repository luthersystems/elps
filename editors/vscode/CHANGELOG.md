# Changelog

Published extension versions track the `elps` release tag they ship with -- the
publish workflow sets `package.json` from the tag name -- so the numbering
jumps from 0.2.0 to 1.50.0.

## 1.62.0

- Runtime hardening across the interpreter, shipped in the bundled `elps`
  binary: closures created in `let*` initializers no longer see later
  bindings, native calls stop after a cancelled request, host panics are
  contained, deeply nested and cyclic values render with `#<depth-limit>` and
  `#<cycle>` markers instead of overflowing the stack, and error text is
  bounded and never blanked after the request that produced it ends.
- Debugger: variables, frames and DAP responses render under one shared byte
  and work budget with cancellation; the source view and error snippets read
  only through the confined loader when `--root-dir` is set.
- Formatter and parser: comments and shebang lines longer than 128 KiB are no
  longer split into code; oversized strings and symbols, malformed numeric
  literals and a UTF-8 BOM are diagnosed rather than mis-tokenised; `elps fmt`
  no longer inserts a space inside an identifier.
- Lint: new `let-recursion` migration check for closures that relied on
  implicit self-recursion through a `let` binding, plus `lisp-package-seal`,
  `builtin-shadowing`, `lambda-list`, `duplicate-binding` and
  `duplicate-keyword` checks; malformed lambda lists and an `&optional` or
  `&key` group with no names are reported statically.
- Minify: renaming a package-level name now requires static proof of package
  flow and export names; quoted function references, nested `defun`, package
  redefinitions and computed `set` targets are preserved rather than
  miscompiled.
- `elps run`: `--timeout` and `--max-steps` flags; Ctrl-C cancels evaluation
  and flushes output instead of killing the process.
- Behaviour changes to read before upgrading: integer `pow` raises on
  overflow; `max`/`min` propagate NaN; malformed lambda lists, keyword formals
  and invalid package names are errors at definition time; writes to core
  `lisp` package bindings from Lisp are refused; `json:null` encodes as
  `null`; `format-string` prints a depth marker instead of raising.

## 1.61.2

- LSP: bound parsing and disk reads for oversized documents, avoid expensive
  handlers on those documents, and remove stale workspace definitions on save.
- LSP: hover, completion and definition no longer allocate a document-sized
  line slice per request when they fall back to the word under the cursor.
- Lint: add `comparator-mutation` and `iteration-mutation` checks, with bounded
  callback analysis and quote handling that matches ELPS evaluation. These are
  conservative checks, not a proof that callbacks are free of side effects.
- MCP: generate the help prompt's analyzer table from the registered checks.
- Runtime: repair value-copy aliasing, cycles and failed-copy handling. Sort
  comparators and key functions now receive the actual elements, not per-call
  copies; mutating a sealed literal raises `modify-literal-error`.
- Update the language client and the `js-yaml` development dependency.

## 1.59.0

- Lint: `shadowing` no longer reports a binding whose initialiser references
  the name it shadows. `(let* ([ctx (default ctx (sorted-map))])` narrows one
  value rather than introducing a second meaning for the name, and it is the
  only way to default an `&optional` argument, so reporting it buried the
  shadows that matter. Measured on a large downstream codebase, this removed
  56 of 86 diagnostics.
- Lint: `shadowing` severity now follows what is hidden. Shadowing a builtin,
  special operator, macro or function is a **warning** -- while that binding
  is in scope a call to the name resolves to the local, so `(min a b)` stops
  meaning `min`. Shadowing another local stays informational. The note says
  what actually breaks rather than just "rename it". No exit codes change:
  `elps lint --fail-on` still defaults to `error`.
- Lint: the refinement exemption above deliberately does NOT apply when the
  shadowed name is callable. `(let ([car (car xs)]) (car xs))` narrows
  nothing -- the body applies an element as a function -- so it is still
  reported, at warning severity.

## 1.58.0

- Language: new `with-cleanup` special operator -- `(with-cleanup (cleanup...)
  body...)` always runs the cleanup forms, whether the body returned normally
  or signalled, and returns the last body value. It does not catch: the error
  is still live once the cleanup has run. Both halves are implicit progns.
  Syntax highlighting, formatter indentation and the language server treat it
  as a special operator.
- Lint: new `with-cleanup-forms` check (warning) flags a `with-cleanup` whose
  cleanup list is empty, or holds a bare symbol -- `(with-cleanup (release h)
  ...)` reads as a list of two bare symbols, so nothing is released, and the
  code behaves correctly right up until the body signals.

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
