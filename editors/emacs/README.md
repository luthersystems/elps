# ELPS for Emacs

Language server (LSP) and debugger (DAP) setup for Emacs. Both need the `elps`
binary on your `$PATH` (`go install github.com/luthersystems/elps@latest`).

## Language Server (LSP)

The server is started with `elps lsp --stdio` (stdio is also the default when
no transport flag is given).

### eglot (built in since Emacs 29)

```elisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(lisp-mode . ("elps" "lsp" "--stdio"))))
```

Then `M-x eglot` in a `.lisp` buffer.

### lsp-mode

```elisp
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(lisp-mode . "elps"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("elps" "lsp" "--stdio"))
    :activation-fn (lsp-activate-on "elps")
    :server-id 'elps-lsp)))
```

Then `M-x lsp` in a `.lisp` buffer.

## Debugger (DAP)

Requires [dap-mode](https://emacs-lsp.github.io/dap-mode/).

`elps debug` takes the file to debug as a command-line argument (the DAP
`program` field alone is not enough), so the provider builds the adapter
command from the configuration. For an attach request it sets no server
command, and dap-mode connects to `:host` on the `:debugServer` port instead
(dap-mode reads the TCP port only from `:debugServer`).

```elisp
(require 'dap-mode)

;; Register the ELPS debug adapter.
(dap-register-debug-provider
 "elps"
 (lambda (conf)
   (if (equal (plist-get conf :request) "attach")
       conf
     (let ((program (or (plist-get conf :program) (buffer-file-name))))
       (plist-put conf :program program)
       (plist-put conf :dap-server-path
                  (append '("elps" "debug" "--stdio")
                          (when (plist-get conf :stopOnEntry)
                            '("--stop-on-entry"))
                          (list program)))))))

;; Launch configuration (debugs the current buffer's file).
(dap-register-debug-template
 "ELPS: Debug File"
 (list :type "elps"
       :request "launch"
       :name "Debug ELPS"
       :stopOnEntry t))

;; Attach configuration (connect to a running DAP server).
(dap-register-debug-template
 "ELPS: Attach"
 (list :type "elps"
       :request "attach"
       :name "Attach to ELPS"
       :host "localhost"
       :debugServer 4711))
```

The file must live under the working directory; `elps debug` confines source
loads to that root (override with `--root-dir DIR`).

For attach, start a DAP server first, e.g. from a terminal:

```bash
elps debug --stop-on-entry myfile.lisp    # listens on localhost:4711
```

## Usage

1. `M-x dap-debug` — select "ELPS: Debug File" or "ELPS: Attach"
2. `M-x dap-breakpoint-toggle` — toggle breakpoint on current line
3. `M-x dap-continue` — continue execution
4. `M-x dap-next` — step over
5. `M-x dap-step-in` — step into
6. `M-x dap-step-out` — step out
7. `M-x dap-eval` — evaluate expression in debug console

See `elps doc --debug-guide` for the launch/attach fields the adapter accepts.
