# ELPS for Neovim

Language server (LSP) and debugger (DAP) setup for Neovim. Both need the
`elps` binary on your `$PATH` (`go install github.com/luthersystems/elps@latest`).

## File Type Detection

Neovim already maps `.lisp` to the `lisp` filetype. To treat `.elps` files the
same way, add to your config:

```lua
vim.filetype.add({
  extension = {
    lisp = 'lisp',
    elps = 'lisp',
  },
})
```

## Language Server (LSP)

The server is started with `elps lsp --stdio` (stdio is also the default when
no transport flag is given).

**Neovim 0.11+** (built-in `vim.lsp.config`):

```lua
vim.lsp.config('elps', {
  cmd = { 'elps', 'lsp', '--stdio' },
  filetypes = { 'lisp' },
  root_markers = { '.git' },
})
vim.lsp.enable('elps')
```

**Older Neovim** (no plugin required):

```lua
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'lisp',
  callback = function()
    vim.lsp.start({
      name = 'elps',
      cmd = { 'elps', 'lsp', '--stdio' },
      root_dir = vim.fs.dirname(vim.fs.find({ '.git' }, { upward = true })[1]),
    })
  end,
})
```

## Debugger (DAP)

Requires [nvim-dap](https://github.com/mfussenegger/nvim-dap).

### Launch Mode (spawn child process)

`elps debug` takes the file to debug as a command-line argument (the DAP
`program` field alone is not enough), so the adapter is a function that builds
the command from the configuration:

```lua
local dap = require('dap')

dap.adapters.elps = function(callback, config)
  local args = { 'debug', '--stdio' }
  if config.stopOnEntry then
    table.insert(args, '--stop-on-entry')
  end
  table.insert(args, config.program)
  callback({ type = 'executable', command = 'elps', args = args })
end

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

The file must live under the working directory (or pass `--root-dir DIR` in
`args`); `elps debug` confines source loads to that root.

### Attach Mode (connect to running DAP server)

Start a DAP server from the terminal:

```bash
elps debug --stop-on-entry myfile.lisp    # listens on localhost:4711
```

or from your own Go application:

```go
listener, _ := net.Listen("tcp", ":4711")
conn, _ := listener.Accept()
srv := dapserver.New(engine)
srv.ServeConn(conn)
```

Then configure nvim-dap to connect:

```lua
dap.adapters.elps_attach = {
  type = 'server',
  host = '127.0.0.1',
  port = 4711,
}

table.insert(dap.configurations.lisp, {
  type = 'elps_attach',
  request = 'attach',
  name = 'Attach to ELPS',
})
```

See `elps doc --debug-guide` for the launch/attach fields the adapter accepts
and for the auto-attach pattern used by embedders.
