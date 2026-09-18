<p align="center">
  <img src="editors/vscode/images/logo.png" alt="ELPS — Embedded Lisp Interpreter" width="128">
</p>

<p align="center">
  <strong>ELPS — An embedded Lisp interpreter for Go programs</strong>
</p>

<p align="center">
  <a href="https://pkg.go.dev/github.com/luthersystems/elps"><img src="https://pkg.go.dev/badge/github.com/luthersystems/elps.svg" alt="Go Reference"></a> •
  <a href="https://luthersystems.com">Luther Systems</a> •
  <a href="https://insideout.luthersystems.com">InsideOut</a> •
  <a href="https://insideout.luthersystems.com/discord"><img src="https://img.shields.io/badge/Discord-Join%20Us-5865F2?logo=discord&logoColor=white" alt="Discord"></a>
</p>

---

ELPS (Ellipse) is a Lisp-1 dialect designed to be embedded within Go applications. It provides a standalone CLI for running, linting, formatting, debugging, and exploring ELPS Lisp code.

## Install

```bash
go install github.com/luthersystems/elps@latest
```

Or build from source:

```bash
git clone https://github.com/luthersystems/elps.git
cd elps && make
```

## Quick Start

Launch an interactive REPL:

```
$ elps repl
> (+ 3 1)
4
> (defun greet (name) (format-string "Hello, %s!" name))
> (greet "World")
"Hello, World!"
> ^D
```

Run a program:

```
$ elps run prog.lisp
```

Embed in a Go program:

```go
env := lisp.NewEnv(nil)
env.Runtime.Reader = parser.NewReader()
env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
lisp.InitializeUserEnv(env)
lisplib.LoadLibrary(env)
env.LoadString(`(debug-print "hello world")`)
```

## Editor Support

- **[VS Code Extension](editors/vscode/)** — Syntax highlighting, LSP, debugger ([Marketplace](https://marketplace.visualstudio.com/items?itemName=LutherSystems.elps-lang))
- **[Neovim](editors/neovim/)** — DAP configuration
- **[Emacs](editors/emacs/)** — DAP mode configuration
- **[Helix](editors/helix/)** — DAP configuration
- **[JetBrains](editors/jetbrains/)** — LSP4IJ plugin configuration

## CLI

| Command | Description |
|---------|-------------|
| `elps run file.lisp` | Run a Lisp source file |
| `elps repl` | Start an interactive REPL |
| `elps lsp` | Start the Language Server Protocol server |
| `elps debug file.lisp` | Start the debug adapter (DAP) |
| `elps lint file.lisp` | Run static analysis |
| `elps fmt file.lisp` | Format source code |
| `elps minify file.lisp --map symbols.json` | Minify source and record symbol assignments |
| `elps doc <query>` | Show function/package documentation |
| `elps mcp` | Start the MCP server for AI tooling |

### Minifying source

`elps minify` uses deterministic, scope-aware renaming. Any name appearing quoted
anywhere in the inputs is preserved across all scopes and packages, including
names inside quoted lists (including `[...]`) and quasiquote templates. This keeps quoted function
designators such as `(map 'list 'twice values)` working. Quoted names are not
shortened, even with `--rename-exports`, so output may be larger. The JSON symbol
map lists these names under `excluded` with `reason: "quoted-reference"`.

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

Reader quoting in `(export 'foo)` or `(export '(a b))` supplies proof because it
cannot be shadowed. Calls to `quote` or `lisp:quote` do not: spelling an export as
`(export (quote foo))` keeps all package-level names across the input files,
even when `quote` is not shadowed. Use `(export 'foo)` to retain compression of
private names. The repository's 83 source exports already use the reader-quote
idiom, so the common case is unaffected.

`defun`, `defmacro`, `set` with a quoted symbol, and `export` affect package
bindings at every nesting depth. Functions defined inside `let` still capture
its lexical values; their names remain accessible at package level.

## Documentation

- [Language Reference](docs/lang.md)
- [Embedding Guide](docs/embed.md)
- [Debugging Guide](docs/debugging-guide.md)
- [LSP Guide](docs/lsp-guide.md)
- [Go API Reference](https://pkg.go.dev/github.com/luthersystems/elps)

## Examples

- [SICP Examples](_examples/sicp) — Structure and Interpretation of Computer Programs
- [User-Defined Types](_examples/user-defined-types)
- [WASM Playground](https://luthersystems.github.io/elps/) ([source](_examples/wasm/))

## Links

- [Luther Systems](https://luthersystems.com)
- [Luther Enterprise](https://enterprise.luthersystems.com)
- [InsideOut Platform](https://insideout.luthersystems.com)

### Community & Support

- [Discord](https://insideout.luthersystems.com/discord)
- [General Inquiry Call](https://insideout.luthersystems.com/general-call)
- [Tech Call](https://insideout.luthersystems.com/tech-call)
- [contact@luthersystems.com](mailto:contact@luthersystems.com)
