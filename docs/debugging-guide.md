# Debugging Guide

ELPS ships with a full-featured debugger that supports two modes of operation:

| Feature                     | DAP Mode (editors)      | REPL Mode (CLI)      |
|-----------------------------|-------------------------|----------------------|
| Line breakpoints            | Yes                     | Yes                  |
| Conditional breakpoints     | Yes                     | Yes                  |
| Hit-count breakpoints       | Yes                     | No                   |
| Log points                  | Yes                     | No                   |
| Function breakpoints        | Yes                     | No                   |
| Exception breakpoints       | Yes                     | No                   |
| Step into / over / out      | Yes                     | Yes                  |
| Stepping granularity        | Line + instruction      | Line                 |
| Smart step-in targets       | Yes                     | No                   |
| Variable inspection         | Local, Package, Macro   | Locals               |
| Variable filtering          | `/filter <regex>`       | No                   |
| Custom scope providers      | Yes                     | No                   |
| Watch expressions           | Yes                     | No                   |
| Debug console / eval        | Yes                     | Yes (bare exprs)     |
| Auto-complete               | Yes                     | Yes                  |
| Backtrace                   | Yes (stack frames)      | Yes                  |
| Pause running program       | Yes                     | Yes (Ctrl+C)         |
| Editor integration          | VS Code, Neovim, etc.  | Terminal only        |

## Quick Start

### CLI Debug REPL

```bash
elps debug --repl myfile.lisp
```

The REPL pauses before the first expression. Type `help` for available commands.

### VS Code

1. Install the ELPS debug extension (see [Editor Setup](#editor-setup)).
2. Create `.vscode/launch.json`:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "elps",
      "request": "launch",
      "name": "Debug ELPS",
      "program": "${file}",
      "stopOnEntry": true
    }
  ]
}
```

3. Set breakpoints in the gutter and press F5.

## CLI Reference

```
elps debug [flags] file.lisp
```

| Flag              | Default | Description                                            |
|-------------------|---------|--------------------------------------------------------|
| `--repl`          | false   | Start an interactive CLI debug REPL instead of a DAP server |
| `--port N`        | 4711    | TCP port for the DAP server                            |
| `--stdio`         | false   | Use stdin/stdout for DAP communication (editor launch) |
| `--stop-on-entry` | false   | Pause before the first expression                      |
| `--root-dir DIR`  | cwd     | Root directory for file access confinement              |

**Transport modes (DAP):**

- **TCP** (default): `elps debug myfile.lisp` starts a DAP server on port 4711. The editor connects over TCP.
- **Stdio**: `elps debug --stdio myfile.lisp` communicates over stdin/stdout. Used when the editor launches `elps` as a child process.

**Examples:**

```bash
elps debug myfile.lisp                     # DAP on TCP port 4711
elps debug --port 9229 myfile.lisp         # DAP on TCP port 9229
elps debug --stdio myfile.lisp             # DAP on stdio
elps debug --stop-on-entry myfile.lisp     # Pause at first expression
elps debug --repl myfile.lisp              # Interactive CLI REPL
```

## Editor Setup

### VS Code

Install the extension from `editors/vscode/`:

```bash
# Option 1: Symlink
ln -s "$PWD/editors/vscode" ~/.vscode/extensions/elps-debug

# Option 2: Package and install
cd editors/vscode
npx @vscode/vsce package
code --install-extension elps-debug-0.1.0.vsix
```

**Launch configuration** (`.vscode/launch.json`):

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "elps",
      "request": "launch",
      "name": "Debug ELPS",
      "program": "${file}",
      "stopOnEntry": true
    }
  ]
}
```

**Attach configuration** (connect to a running DAP server):

```json
{
  "type": "elps",
  "request": "attach",
  "name": "Attach to ELPS",
  "host": "localhost",
  "port": 4711
}
```

| Attribute     | Type    | Default              | Description                        |
|---------------|---------|----------------------|------------------------------------|
| `program`     | string  | `${file}`            | Path to the `.lisp` file to debug  |
| `stopOnEntry` | boolean | `true`               | Pause before the first expression  |
| `rootDir`     | string  | `${workspaceFolder}` | Source root for file resolution    |
| `elpsPath`    | string  | `"elps"`             | Path to the `elps` binary          |

### Neovim

Requires [nvim-dap](https://github.com/mfussenegger/nvim-dap).

**Launch mode** (spawns `elps` as a child process):

```lua
local dap = require('dap')

dap.adapters.elps = {
  type = 'executable',
  command = 'elps',
  args = { 'debug', '--stdio' },
}

dap.configurations.lisp = {
  {
    type = 'elps',
    request = 'launch',
    name = 'Debug ELPS',
    program = '${file}',
    stopOnEntry = true,
  },
}
```

**Attach mode** (connect to a running DAP server):

```lua
dap.adapters.elps_attach = {
  type = 'server',
  host = '127.0.0.1',
  port = 4711,
}

dap.configurations.lisp = {
  {
    type = 'elps_attach',
    request = 'attach',
    name = 'Attach to ELPS',
  },
}
```

**File type detection** (add to your Neovim config):

```lua
vim.filetype.add({
  extension = {
    lisp = 'lisp',
    elps = 'lisp',
  },
})
```

### Helix

Requires Helix 24.03+ with DAP support.

Add to `.helix/languages.toml` or `~/.config/helix/languages.toml`:

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

**Key bindings** (Helix defaults):

| Binding     | Action            |
|-------------|-------------------|
| `<space>gb` | Toggle breakpoint |
| `<space>gc` | Continue          |
| `<space>gn` | Step over         |
| `<space>gi` | Step in           |
| `<space>go` | Step out          |

Note: Helix does not natively support TCP attach. Use the launch configuration, or use Neovim/VS Code for attach workflows.

### Emacs

Requires [dap-mode](https://emacs-lsp.github.io/dap-mode/).

```elisp
(require 'dap-mode)

;; Register the ELPS debug adapter.
(dap-register-debug-provider
 "elps"
 (lambda (conf)
   (plist-put conf :dap-server-path '("elps" "debug" "--stdio"))
   conf))

;; Launch configuration.
(dap-register-debug-template
 "ELPS: Debug File"
 (list :type "elps"
       :request "launch"
       :name "Debug ELPS"
       :program "${file}"
       :stopOnEntry t))

;; Attach configuration (connect to running DAP server).
(dap-register-debug-template
 "ELPS: Attach"
 (list :type "elps"
       :request "attach"
       :name "Attach to ELPS"
       :host "localhost"
       :port 4711))
```

For attach mode, register a TCP-based debug provider:

```elisp
(dap-register-debug-provider
 "elps-attach"
 (lambda (conf)
   (let ((host (or (plist-get conf :host) "localhost"))
         (port (or (plist-get conf :port) 4711)))
     (plist-put conf :dap-server-host host)
     (plist-put conf :dap-server-port port)
     conf)))
```

**Key commands:**

| Command                     | Action                              |
|-----------------------------|-------------------------------------|
| `M-x dap-debug`            | Select and start a debug session    |
| `M-x dap-breakpoint-toggle`| Toggle breakpoint on current line   |
| `M-x dap-continue`         | Continue execution                  |
| `M-x dap-next`             | Step over                           |
| `M-x dap-step-in`          | Step into                           |
| `M-x dap-step-out`         | Step out                            |
| `M-x dap-eval`             | Evaluate expression in debug console|

### JetBrains IDEs

Requires the [LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij) plugin.

1. Install **LSP4IJ** from the JetBrains Marketplace.
2. Go to **Settings > Languages & Frameworks > LSP4IJ > DAP**.
3. Add a new DAP server:

| Field      | Value                  |
|------------|------------------------|
| Name       | ELPS Debug             |
| Command    | `elps debug --stdio`   |
| File types | `*.lisp`, `*.elps`     |

**Launch configuration:**

1. **Run > Edit Configurations > + > DAP**
2. Set DAP Server to "ELPS Debug", Request to "launch", Program to `$FilePath$`, Stop on Entry to true.

**Attach configuration:**

1. **Run > Edit Configurations > + > DAP**
2. Set DAP Server to "ELPS Debug", Request to "attach", Host to "localhost", Port to 4711.

## REPL Mode Commands

The CLI debug REPL (`elps debug --repl`) provides a GDB-style command interface. The REPL always starts paused at the first expression (stop-on-entry is forced).

| Command              | Short | Description                                |
|----------------------|-------|--------------------------------------------|
| `continue`           | `c`   | Resume execution                           |
| `step`               | `s`   | Step into next expression                  |
| `next`               | `n`   | Step over (same call depth)                |
| `out`                | `o`   | Step out of current function               |
| `break F:L [COND]`   | `b`   | Set breakpoint at file:line with optional condition |
| `delete N`           | `d`   | Remove breakpoint by ID                    |
| `breakpoints`        | `bl`  | List all breakpoints                       |
| `backtrace`          | `bt`  | Show call stack                            |
| `locals`             | `l`   | Show local variables                       |
| `print EXPR`         | `p`   | Evaluate and print an expression           |
| `where`              | `w`   | Show source context around current location|
| `quit`               | `q`   | End the debug session                      |
| `help`               | `h`   | Show command help                          |

**Conventions:**

- Empty input repeats the last command (GDB convention). Useful for repeated stepping.
- Bare Lisp expressions (anything not matching a command) are evaluated in the paused scope with full access to local variables and function parameters. For example, if paused inside a function where `x=42`, typing `(+ x 1)` returns `43`.
- Ctrl+C pauses a running program.

**Example session:**

```
$ elps debug --repl myfile.lisp
stopped: entry
   3 | (defun add (a b)
>  4 |   (+ a b))
   5 |
(dbg) break myfile.lisp:8
breakpoint 1 set at myfile.lisp:8
(dbg) c
stopped: breakpoint 1
   7 | (set 'result
>  8 |   (add 2 3))
   9 |
(dbg) s
stopped: step
   3 | (defun add (a b)
>  4 |   (+ a b))
   5 |
(dbg) locals
  a = 2
  b = 3
(dbg) (+ a b)
5
(dbg) c
program exited
```

## DAP Features

### Breakpoints

**Line breakpoints** pause execution when a specific source line is reached. Set them via the editor's gutter or the REPL's `break` command.

**Conditional breakpoints** take a Lisp expression that is evaluated each time the breakpoint is hit. Execution only pauses when the condition is truthy (not `()`, not `false`, not an error).

**Hit-count breakpoints** pause only when the hit count satisfies a condition:

| Syntax | Meaning                                    |
|--------|--------------------------------------------|
| `>N`   | Pause when hit count is greater than N     |
| `>=N`  | Pause when hit count is greater or equal N |
| `==N`  | Pause when hit count equals N              |
| `%N`   | Pause every Nth hit (modulo)               |
| `N`    | Same as `==N`                              |

**Log points** emit a message to the debug console instead of pausing. The message template supports `{expr}` interpolation — each `{expr}` placeholder is replaced with the result of evaluating that Lisp expression.

**Function breakpoints** pause when a named function is entered. Specify function names (e.g., `add` or `user:add`) through the editor's function breakpoint UI.

**Exception breakpoints** pause when an error condition is raised. Configure via the editor's exception breakpoint UI (filter ID: `all`).

### Stepping

| Action    | DAP Request | Description                              |
|-----------|-------------|------------------------------------------|
| Step Into | `stepIn`    | Advance to the next expression (descend) |
| Step Over | `next`      | Advance at the same call depth           |
| Step Out  | `stepOut`   | Run until the current function returns   |

**Stepping granularity:** The DAP protocol supports `line` (default) and `instruction` granularity. In ELPS, line-level stepping advances to the next distinct source line; instruction-level stepping advances to the next expression (including sub-expressions on the same line).

**Smart step-in targets:** When paused on a line with multiple function calls, the editor can show which function to step into via the "Step Into Target" UI. This uses the DAP `stepInTargets` request.

### Variable Inspection

When paused, three built-in scopes are visible in the Variables panel:

- **Local** — Function-local bindings (parameters, let-bindings, intermediate values). Walks the environment chain up to but not including the root scope.
- **Package** — All exported symbols in the current package.
- **Macro Expansion** — When paused inside macro-expanded code, shows the macro name, call-site arguments, and call-site location.

Structured values (lists, arrays, sorted-maps, tagged values) are expandable in the Variables panel.

### Debug Console

The debug console evaluates Lisp expressions in the paused scope. Supports multi-expression input with progn semantics (the result of the last expression is returned). Auto-complete is available for symbols in scope.

Watch expressions are re-evaluated automatically each time execution pauses.

**Debug console commands:**

| Command            | Description                                          |
|--------------------|------------------------------------------------------|
| `/filter <regex>`  | Filter sorted-map entries by key (regex match)       |
| `/filter`          | Clear the active filter (show all entries)            |

The `/filter` command is useful for large sorted-maps with many keys. The regex matches against the formatted key name (e.g., `"apple"` for string keys, `:foo` for keywords). The filter applies to all sorted-map expansions in the Variables panel and persists across step/continue until cleared.

When the client supports `InvalidatedEvent`, the Variables panel refreshes automatically after a filter change.

## Embedding the Debugger

The debugger engine is designed for embedding in Go applications. The `debugger` package provides the core engine; the `dapserver` package provides the DAP wire protocol.

### Creating an Engine

```go
import "github.com/luthersystems/elps/lisp/x/debugger"

engine := debugger.New(
    debugger.WithStopOnEntry(true),
    debugger.WithSourceRoot("/path/to/sources"),
)
engine.Enable()

// Wire to an LEnv before evaluation starts.
env := lisp.NewEnv(nil)
env.Runtime.Debugger = engine
```

### Transport Options

```go
import "github.com/luthersystems/elps/lisp/x/debugger/dapserver"

srv := dapserver.New(engine)

// Option 1: Single TCP connection (blocks until disconnect).
srv.ServeTCP("localhost:4711")

// Option 2: Stdio (for editors that launch the adapter).
srv.ServeStdio(os.Stdin, os.Stdout)

// Option 3: TCP loop (survives client reconnections).
// Designed for long-lived embedded servers.
srv.ServeTCPLoop("localhost:4711")

// Option 4: BYO connection.
srv.ServeConn(conn)
```

### Custom Scope Providers

Implement `ScopeProvider` to expose application-specific state in the Variables panel:

```go
type ScopeProvider interface {
    Name() string                          // Scope label (e.g., "State DB")
    Expensive() bool                       // Fetch lazily if true
    Variables(env *lisp.LEnv) []ScopeVariable
}

type ScopeVariable struct {
    Name     string
    Value    string
    Type     string           // optional type annotation
    Children []ScopeVariable  // expandable sub-variables
}
```

Register at engine creation or at runtime:

```go
engine := debugger.New(
    debugger.WithScopeProviders(myProvider),
)
// or
engine.RegisterScopeProvider(myProvider)
```

### Custom Native Formatters

Implement `VariableFormatter` to control how Go types wrapped in `LNative` are displayed:

```go
type VariableFormatter interface {
    FormatValue(v any) string
    Children(v any) []NativeChild
}

type NativeChild struct {
    Name  string
    Value *lisp.LVal
}
```

Register by Go type name (`fmt.Sprintf("%T", value)`):

```go
engine := debugger.New(
    debugger.WithFormatters(map[string]debugger.VariableFormatter{
        "*myapp.State": myStateFormatter{},
    }),
)
// or
engine.RegisterFormatter("*myapp.State", myStateFormatter{})
```

For simple display-only formatting without children, use `FormatterFunc`:

```go
engine.RegisterFormatter("*myapp.ID", debugger.FormatterFunc(func(v any) string {
    return v.(*myapp.ID).String()
}))
```

### Source Library

For virtual sources (e.g., `go:embed` files), implement `lisp.SourceLibrary` and pass it to the engine:

```go
engine := debugger.New(
    debugger.WithSourceLibrary(mySourceLib),
)
// or set it later:
engine.SetSourceLibrary(mySourceLib)
```

The DAP source request handler uses this to serve file content to editors.

### Architecture Deep-Dive

See [docs/plans/debugger-design.md](plans/debugger-design.md) for the full architecture, concurrency model, and design decisions.

## Tips and Troubleshooting

### Stop-on-Entry

Use `--stop-on-entry` (CLI) or `"stopOnEntry": true` (launch.json) to pause before the first expression. This gives you time to set breakpoints before any code runs. The REPL mode always enables stop-on-entry.

### Conditional Breakpoint Syntax

Conditions are Lisp expressions evaluated in the paused scope. ELPS truthiness: `()` (nil), `false`, and errors are falsey; everything else is truthy.

```
;; Pause only when x > 10
(> x 10)

;; Pause only when name matches
(equal name "alice")
```

### Port Conflicts

If port 4711 is already in use, specify a different port:

```bash
elps debug --port 9229 myfile.lisp
```

Update the editor's attach configuration to match.

### Editor Not Connecting

1. Verify `elps` is on your PATH: `which elps`
2. For stdio mode, check the editor's debug adapter output for errors.
3. For TCP mode, verify the port is listening: `lsof -i :4711`
4. Ensure the editor's configuration uses the correct request type (`launch` vs `attach`).

### Lightweight Alternatives

For quick debugging without the full debugger, ELPS provides built-in functions:

- `(debug-print expr)` — Print an expression's value to stderr.
- `(debug-stack)` — Print the current call stack to stderr.

These work without any debugger setup and are useful for printf-style debugging.
