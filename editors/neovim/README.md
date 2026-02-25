# ELPS DAP for Neovim

Requires [nvim-dap](https://github.com/mfussenegger/nvim-dap).

## Launch Mode (spawn child process)

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

## Attach Mode (connect to running DAP server)

Start the DAP server in your application:

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

dap.configurations.lisp = {
  {
    type = 'elps_attach',
    request = 'attach',
    name = 'Attach to ELPS',
  },
}
```

## File Type Detection

Add to your Neovim config:

```lua
vim.filetype.add({
  extension = {
    lisp = 'lisp',
    elps = 'lisp',
  },
})
```
