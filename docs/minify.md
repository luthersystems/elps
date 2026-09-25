# Minifying ELPS source

`elps minify` shrinks ELPS source by shortening identifiers with deterministic,
scope-aware renaming, and writes a JSON symbol map recording every renamed
symbol so tooling (stack-trace decoding, debuggers, audits) can translate
minified names back to the originals. Output uses the formatter's compact mode,
which removes redundant whitespace and strips comments.

Renaming is conservative by design: whenever the minifier cannot prove that a
name is only ever reached through the bindings it can see, it leaves the name
alone. The rules below say exactly when that happens.

## Usage

```
elps minify file.lisp --map symbols.json      # minified source to stdout, map to symbols.json
elps minify -w a.lisp b.lisp --map map.json   # rewrite several files in place
elps minify < file.lisp                       # read stdin, write stdout
```

With no files the command reads stdin and writes the minified source to stdout.
With one file it prints to stdout unless `-w` is given. With multiple files,
use `-w` to rewrite them in place.

| Flag | Effect |
| --- | --- |
| `-w`, `--write` | Write the result back to the source files. |
| `--map path` | Write the JSON symbol map to `path`. |
| `--exclude name` | Never rename `name` (may be repeated). |
| `--exclude-file path` | Read names to exclude from a file, one per line; blank lines and lines starting with `;` are ignored (may be repeated). |
| `--workspace dir` | Workspace root for cross-file semantic resolution. |
| `--rename-exports` | Also rename exported symbols unless excluded. Literal export names and bindings preserved for dynamic evaluation are always kept. |
| `--preserve-params` | Keep function and macro parameter names (default `true`); pass `--preserve-params=false` to rename them. |

`elps minify --help` prints the same rules as this page.

## The symbol map

The map written by `--map` is a JSON object with these fields:

| Field | Contents |
| --- | --- |
| `entries` | One entry per renamed symbol: `minified`, `original`, `kind`, and where known its `file`, `line` and `col`. |
| `minified_to_original` | Lookup from each minified name to its original. |
| `original_to_minified` | Lookup from each original name to the minified names it received. |
| `excluded` | Names deliberately left unrenamed, each with `original` and a `reason`. |

The assignment maps contain only renamed symbols. A name kept by the rules
below appears under `excluded` instead, with one of these reasons:

| `reason` | Why the name was kept |
| --- | --- |
| `quoted-reference` | The name appears quoted somewhere in the inputs. |
| `dynamic-evaluation` | The program evaluates or constructs code or names at runtime. Takes precedence over `quoted-reference`. |
| `unproven-package-flow` | Package switches, imports or exports could not be proven statically. Takes precedence over `quoted-reference`. |

## Exclusion rules

### Quoted names

Symbols quoted anywhere in the input files are excluded from renaming across
all scopes and packages. This includes quoted lists (including `[...]`) and
quasiquote templates, so quoted function designators such as
`(map 'list 'twice '(1 2 3))` retain the function's name. Qualified quoted names
also protect the corresponding bare name.

Quoted names are never shortened, even with `--rename-exports`. This conservative
rule can increase output size. The symbol map records the names in `excluded`,
with `original` and `reason: "quoted-reference"`; the existing assignment maps
contain only renamed symbols.

### Dynamic evaluation

Any call to `load-string`, `load-bytes`, `load-file`, `eval`, `macroexpand`,
`macroexpand-1`, `gensym`, `type`, or `qualified-symbol` preserves **every
binding name, including lexical locals, across all input files**, even with `--rename-exports`.
The rule also covers `symbol` and `intern` when supplied by a host, and
`lisp:`-qualified spellings. References passed as function values or appearing in
quoted templates also trigger it conservatively. No bindings are renamed in such
a program. The CLI prints one warning naming the first dynamic-evaluation site,
and the symbol map records preserved bindings under `excluded` with
`reason: "dynamic-evaluation"`, taking precedence over `"quoted-reference"`.
This keeps runtime-generated names and code working, but produces larger output
and disables all identifier compression in programs using dynamic evaluation.

### Package flow and exports

Package-level bindings are renamed only when package flow and exported names can
be proven statically: every `export` argument must be a reader-quoted symbol,
a literal string, or a reader-quoted list (possibly nested) of those, and every
`in-package` / `use-package` form must be at the top level of a file with literal
package names. A variable or expression passed to `export`, a computed package
name, or a package switch/import nested inside any form (including `progn`,
`let`, `when`, functions, and macros) preserves every package-level binding name
across all input files,
even with `--rename-exports`. Lexical locals can still shorten only when dynamic
evaluation is absent. Package flow and dynamic evaluation are tracked independently.
The symbol map records `unproven-package-flow` for the package fallback, taking
precedence over `quoted-reference`; one warning names the first offending form.
If dynamic evaluation is also present, its no-renaming rule, exclusion reason,
and warning take precedence regardless of source order.
Literal export names remain preserved even with `--rename-exports`.

Proof also requires directly evaluated package forms. Any `export`, `in-package`,
or `use-package` inside `defmacro`, `macrolet`, or a quasiquote template triggers
the package fallback, even if its arguments look literal. This includes quoted
macro bodies and `unquote` forms: generated code may export different names.
Macros without these package forms still allow renaming.

Reader quoting in directly evaluated `(export 'foo)` or `(export '(a b))` supplies
proof because it cannot be shadowed. Calls to `quote` or `lisp:quote` do not:
unqualified `quote` can be shadowed. Qualified `lisp:quote` resolves in the named
package and cannot be lexically shadowed; the standard runtime seals that package
against Lisp writes. An embedder can nevertheless register a different `lisp`
package before sealing, so the minifier cannot assume the standard runtime.
Spelling an export as
`(export (quote foo))` keeps all package-level names across the input files,
even when `quote` is not shadowed. Use `(export 'foo)` to retain compression of
private names.

### Nested definitions

`defun`, `defmacro`, `set` with a quoted symbol, and `export` affect package-level
bindings at any nesting depth. For example, `(let ((k 1)) (defun helper (x) (+ x k)))`
creates a package-level `helper` that captures the lexical value of `k`. Minifying
this definition also renames its calls outside the `let` consistently.
