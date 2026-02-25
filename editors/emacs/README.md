# ELPS DAP for Emacs

Requires [dap-mode](https://emacs-lsp.github.io/dap-mode/).

## Configuration

Add to your Emacs config:

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

## Attach Mode

For attach mode, configure the debug provider to connect via TCP:

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

## Usage

1. `M-x dap-debug` — select "ELPS: Debug File" or "ELPS: Attach"
2. `M-x dap-breakpoint-toggle` — toggle breakpoint on current line
3. `M-x dap-continue` — continue execution
4. `M-x dap-next` — step over
5. `M-x dap-step-in` — step into
6. `M-x dap-step-out` — step out
7. `M-x dap-eval` — evaluate expression in debug console
