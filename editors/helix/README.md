# ELPS for Helix

Language server (LSP) and debugger (DAP) setup for Helix. Both need the `elps`
binary on your `$PATH` (`go install github.com/luthersystems/elps@latest`).

Add the following to `.helix/languages.toml` in your project or to
`~/.config/helix/languages.toml`.

## Language Server (LSP)

```toml
[[language]]
name = "elps"
scope = "source.elps"
file-types = ["lisp", "elps"]
roots = [".git"]
comment-token = ";"
language-servers = ["elps-lsp"]

[language-server.elps-lsp]
command = "elps"
args = ["lsp", "--stdio"]
```

Run `hx --health elps` to check that Helix finds the server.

## Debugger (DAP)

Helix's DAP support is experimental. It spawns a debug adapter with a fixed
command line, but `elps debug` needs the file to debug as a command-line
argument, so Helix cannot launch it directly. Instead, start the DAP server
yourself and connect to it over TCP.

Add a debugger with an attach template to the `[[language]]` entry above:

```toml
[language.debugger]
name = "elps"
transport = "tcp"
command = "elps"
args = []

[[language.debugger.templates]]
name = "attach"
request = "attach"
completion = []
args = {}
```

Then:

1. In a terminal: `elps debug --stop-on-entry myfile.lisp` (listens on
   `localhost:4711`; use `--port N` to change it).
2. In Helix: `:debug-remote 127.0.0.1:4711 attach`

This workflow has not been verified end to end against every Helix release;
Neovim and VS Code are the tested DAP clients.

## Usage

The debug commands live under the `<space>G` menu:

| Binding     | Action            |
|-------------|-------------------|
| `<space>Gb` | Toggle breakpoint |
| `<space>Gc` | Continue          |
| `<space>Gn` | Step over         |
| `<space>Gi` | Step in           |
| `<space>Go` | Step out          |

See `elps doc --debug-guide` for the launch/attach fields the adapter accepts.
