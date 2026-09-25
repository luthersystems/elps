<p align="center">
  <img src="editors/vscode/images/logo.png" alt="ELPS — Embedded Lisp Interpreter" width="128">
</p>

<p align="center">
  <strong>ELPS — An embedded Lisp interpreter for Go programs</strong>
</p>

<p align="center">
  <a href="https://pkg.go.dev/github.com/luthersystems/elps"><img src="https://pkg.go.dev/badge/github.com/luthersystems/elps.svg" alt="Go Reference"></a>
  <a href="https://github.com/luthersystems/elps/actions/workflows/elps.yml"><img src="https://github.com/luthersystems/elps/actions/workflows/elps.yml/badge.svg?branch=main" alt="CI Tests"></a>
  <a href="LICENSE"><img src="https://img.shields.io/badge/License-BSD_3--Clause-blue.svg" alt="License: BSD-3-Clause"></a>
  <a href="https://insideout.luthersystems.com/discord"><img src="https://img.shields.io/badge/Discord-Join%20Us-5865F2?logo=discord&logoColor=white" alt="Discord"></a>
</p>

---

ELPS (Ellipse) is a Lisp-1 dialect designed to be embedded within Go applications. It provides a standalone CLI for running, linting, formatting, debugging, and exploring ELPS Lisp code.

## Why ELPS

- **Built for embedding.** A small Go API creates environments, loads code, and exposes Go functions and packages to Lisp ([Embedding Guide](docs/embed.md)).
- **Bounded execution.** Context cancellation, step limits, stack-height, nesting and tail-iteration limits, and allocation size caps let a host stop runaway programs ([Execution Limits](docs/lang.md#execution-limits)).
- **Fast per-request VMs.** Load a program once, publish an immutable template, and instantiate independent VMs from it ([VM Templates](docs/templates.md)).
- **Condition-based errors.** Typed errors carry stack traces and can be handled in Lisp (`handler-bind`, `ignore-errors`, `with-cleanup`) or returned to Go.
- **Standard library.** Packages for JSON, regular expressions, time, math, strings, base64, schema validation, testing and more.
- **Tooling included.** REPL, language server, DAP debugger, linter, formatter, minifier and MCP server in one binary.

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
> (defun greet (name) (format-string "Hello, {}!" name))
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
if err := lisp.GoError(lisp.InitializeUserEnv(env)); err != nil {
	log.Fatal(err)
}
if err := lisp.GoError(lisplib.LoadLibrary(env)); err != nil {
	log.Fatal(err)
}
res := env.LoadString("main.lisp", `(concat 'string "hello, " "world")`)
if err := lisp.GoError(res); err != nil {
	log.Fatal(err)
}
fmt.Println(res) // "hello, world"
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
| `elps minify file.lisp --map symbols.json` | Minify source and record symbol assignments ([details](docs/minify.md)) |
| `elps analyze file.lisp` | Run performance analysis on source files |
| `elps doc <query>` | Show function/package documentation |
| `elps mcp` | Start the MCP server for AI tooling |

## Documentation

- [Language Reference](docs/lang.md)
- [Embedding Guide](docs/embed.md)
- [VM Templates](docs/templates.md)
- [Debugging Guide](docs/debugging-guide.md)
- [LSP Guide](docs/lsp-guide.md)
- [Editor Setup](docs/editors.md)
- [Lint Checks](docs/lint-checks.md)
- [Suppressing Lint Diagnostics](docs/nolint.md)
- [Minifier](docs/minify.md)
- [Go API Reference](https://pkg.go.dev/github.com/luthersystems/elps)

## Examples

- [SICP Examples](_examples/sicp) — Structure and Interpretation of Computer Programs
- [User-Defined Types](_examples/user-defined-types)
- [Object-Oriented Dispatch](_examples/oop) — a small method system built on `deftype`
- [WASM Playground](https://luthersystems.github.io/elps/) ([source](_examples/wasm/))

## Community

- [Discord](https://insideout.luthersystems.com/discord)
- [contact@luthersystems.com](mailto:contact@luthersystems.com)
- [Luther Systems](https://luthersystems.com)

ELPS is released under the [BSD 3-Clause License](LICENSE). See [CONTRIBUTING.md](CONTRIBUTING.md) to get involved.
