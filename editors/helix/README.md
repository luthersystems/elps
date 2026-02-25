# ELPS DAP for Helix

Requires Helix 24.03+ with DAP support.

## Configuration

Add to `.helix/languages.toml` in your project or `~/.config/helix/languages.toml`:

```toml
[language-server.elps-dap]
command = "elps"
args = ["debug", "--stdio"]

[[language]]
name = "lisp"
file-types = ["lisp", "elps"]
debugger = { command = "elps", args = ["debug", "--stdio"], name = "elps" }

[language.debugger.templates]
name = "launch"
request = "launch"
completion = [{ name = "program", completion = "filename" }]

[[language.debugger.templates.args]]
program = "{0}"
stopOnEntry = true
```

## Attach Mode

For connecting to a running DAP server, Helix does not natively support
TCP attach. Use the launch configuration to spawn the debugger, or use
Neovim/VS Code for attach workflows.

## Usage

1. Open a `.lisp` file
2. `:debug` to start debugging
3. Use `<space>g` prefix for debug commands:
   - `<space>gb` — toggle breakpoint
   - `<space>gc` — continue
   - `<space>gn` — step over
   - `<space>gi` — step in
   - `<space>go` — step out
