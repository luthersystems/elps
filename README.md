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

- **[VS Code Extension](editors/vscode/)** — Syntax highlighting, LSP, debugger ([Marketplace](https://marketplace.visualstudio.com/items?itemName=luthersystems.elps))
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
| `elps doc <query>` | Show function/package documentation |
| `elps mcp` | Start the MCP server for AI tooling |

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
