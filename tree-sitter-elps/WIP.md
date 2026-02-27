# Tree-sitter Grammar for ELPS — Work in Progress

## Status: WIP (paused)

Implementation of GitHub issue #146. Work started but not complete.

## What's Done

### 1. Project Scaffold (complete)
- `package.json` — npm package config with tree-sitter metadata
- `tree-sitter.json` — tree-sitter CLI config
- `binding.gyp` — Node native build config
- Directory structure: `src/`, `test/corpus/`, `queries/`, `bindings/`

### 2. Grammar (`grammar.js`) (complete, working)
All grammar rules are written and `tree-sitter generate` succeeds cleanly:

**Atoms:**
- `integer`, `float`, `octal_integer`, `hex_integer` — all numeric literals
- `string` — double-quoted with escape sequences (currently external scanner)
- `raw_string` — triple-quoted `"""..."""` (external scanner)
- `symbol` — ELPS identifiers matching `lexer.go` character classes
- `qualified_symbol` — `package:name` notation
- `keyword` — `:name` notation

**Compound forms:**
- `nil` — `()`
- `list` — `(expr ...)`
- `bracket_list` — `[expr ...]`

**Prefix forms:**
- `quote` — `'expr`
- `function_quote` — `#'symbol`
- `expr_shorthand` — `#^expr`

**Semantic forms** (with named fields, `prec(1)` over generic list):
- `defun_form` — `(defun name (args) body...)`
- `defmacro_form` — `(defmacro name (args) body...)`
- `deftype_form` — `(deftype name (args) body...)`
- `lambda_form` — `(lambda (args) body...)`
- `let_form` — `(let/let*/flet/labels/macrolet bindings body...)`
- `defconst_form` — `(defconst name value docstring?)`

**Supporting rules:**
- `formals` — parameter list `(sym ...)`
- `let_bindings` — bracket or paren delimited `[(x 1) ...]` or `((x 1) ...)`
- `let_binding` — individual binding `[x 1]` or `(x 1)`
- `comment` — `;` to end of line
- `hashbang` — `#!...` shebang line

### 3. External Scanner (`src/scanner.c`) (complete, has issue)
Handles both `string` and `raw_string` tokenization to disambiguate `""` vs `"""`.

### 4. Test Corpus (`test/corpus/`) (complete, 75 tests)
8 test files:
- `literals.txt` — integers, floats, octal, hex, strings, raw strings, nil, booleans
- `symbols.txt` — plain, qualified, keywords, special chars, formals markers
- `lists.txt` — simple, nested, bracket, empty bracket, mixed
- `quotes.txt` — quote, function-quote, expr-shorthand, nested
- `definitions.txt` — defun, defmacro, deftype, lambda, defconst (with fields)
- `special_forms.txt` — if, cond, let, let*, set, handler-bind, thread-first, progn
- `comments.txt` — line comments, double-semicolon, inline, hashbang
- `packages.txt` — in-package, export, use-package
- `edge_cases.txt` — negative numbers, deep nesting, mixed brackets, nolint

**Test results: 64/75 passing.** The 11 failures are all string-related (see blocker below).

### 5. Not Yet Started
- `queries/highlights.scm` — highlight queries
- `queries/locals.scm` — scope/local queries
- `sync_test.go` — Go sync tests for builtin/token coverage
- `.github/workflows/tree-sitter.yml` — CI workflow
- Editor docs (`editors/neovim/`, `editors/helix/`)
- End-to-end verification (parse all `.lisp` files in repo)

## Blocker: External Scanner Not Working in `tree-sitter test`

### The Problem
The external scanner (`src/scanner.c`) works correctly with `tree-sitter parse` (parses files fine) but NOT with `tree-sitter test` (corpus tests). All 11 test failures involve strings — the test runner doesn't invoke the external scanner for `"` characters, treating them as `UNEXPECTED`.

### Why It Happens
The original grammar had `string` as an internal `token()` rule and only `raw_string` as external. The internal `string` token greedily matches `""` (empty string), which prevents the external scanner from ever seeing `"""` (raw string opener). So 1 test failed.

To fix this, both `string` and `raw_string` were moved to the external scanner. The scanner correctly disambiguates by looking ahead: `"...` → string, `""` → empty string, `"""...` → raw string. This works in `tree-sitter parse` but the test runner appears to not link/invoke the external scanner at all.

### Debug Evidence
- `tree-sitter parse /tmp/test.elps --debug` shows `lex_external` consuming `"` characters correctly
- `tree-sitter test --debug` shows `lex_external` being called but immediately falling through to `lex_internal` without consuming any characters
- Tried: `--rebuild`, `-0` (debug build), `rm -rf build/ target/`, clean `tree-sitter generate` — same behavior
- tree-sitter version: 0.24.7

### Possible Solutions to Investigate
1. **tree-sitter version issue** — Try 0.25.x or downgrade to 0.23.x
2. **Build system issue** — Check if `tree-sitter test` compiles scanner.c differently than `tree-sitter parse`
3. **Alternative approach** — Keep `string` as internal token, make it NOT match empty strings `""`, handle empty string + raw string in external scanner
4. **Workaround** — Use `token.immediate()` or `prec` tricks to make internal string not grab `""` before raw_string scanner runs
5. **Check tree-sitter-elisp** — See how other Lisp grammars handle raw/multi-line strings with external scanners

## Key Reference Files (in main ELPS codebase)

| File | Why It Matters |
|------|---------------|
| `parser/lexer/lexer.go` | Authoritative token patterns — grammar.js must match |
| `parser/token/token.go` | Token type definitions — sync test checks coverage |
| `lisp/builtins.go` | 85 builtin functions — highlight query coverage |
| `lisp/op.go` | 24 special operators — highlight query coverage |
| `lisp/macro.go` | 7 macros — highlight query coverage |

## Research Already Done

### All ELPS Builtins (85)
`+`, `-`, `*`, `/`, `=`, `<`, `<=`, `>`, `>=`, `mod`, `pow`, `max`, `min`,
`string=`, `string<`, `string<=`, `string>`, `string>=`, `to-string`, `to-bytes`, `to-int`, `to-float`, `format-string`,
`type`, `type?`, `nil?`, `list?`, `sorted-map?`, `array?`, `vector?`, `bool?`, `number?`, `int?`, `float?`, `symbol?`, `string?`, `bytes?`, `empty?`, `true?`, `tagged-value?`,
`car`, `cdr`, `rest`, `first`, `second`, `nth`, `cons`, `list`, `vector`, `concat`, `append`, `append!`, `append-bytes`, `append-bytes!`, `reverse`, `slice`, `length`, `aref`, `map`, `foldl`, `foldr`, `select`, `reject`, `zip`, `make-sequence`, `insert-index`, `insert-sorted`, `search-sorted`, `stable-sort`,
`sorted-map`, `get`, `assoc`, `assoc!`, `dissoc`, `dissoc!`, `keys`, `key?`,
`funcall`, `apply`, `compose`, `flip`, `unpack`,
`equal?`, `all?`, `any?`, `not`,
`set`, `export`, `use-package`, `in-package`, `gensym`, `identity`,
`eval`, `macroexpand`, `macroexpand-1`,
`error`, `rethrow`,
`load-string`, `load-bytes`, `load-file`, `debug-print`, `debug-stack`,
`new`, `user-data`

### All Special Operators (24)
`quote`, `quasiquote`, `function`, `set!`, `lambda`, `expr`, `thread-first`, `thread-last`,
`if`, `cond`, `and`, `or`, `progn`, `let`, `let*`, `flet`, `labels`, `macrolet`,
`dotimes`, `handler-bind`, `ignore-errors`, `assert`, `qualified-symbol`

### All Macros (7)
`defun`, `defmacro`, `deftype`, `defconst`, `curry-function`, `get-default`, `trace`
