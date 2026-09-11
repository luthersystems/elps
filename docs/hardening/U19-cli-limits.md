# U19 [P2] `elps run` installs no limits and no timeout; Ctrl-C is a hard kill; `MaxTailIterations` is per-loop and undocumented as such
Subsystem: cmd/run.go (and repl), lisp/runtime.go docs (:288-300 DefaultMaxTailIterations), docs/lang.md limits.

Facts: `elps run` sets no MaxSteps (0 = unlimited) and no context deadline; cmd/, repl/, main.go import no os/signal, so Ctrl-C is Go's default kill with no message or flush. The tail-iteration counter resets per loop, so
```lisp
(defun inner (k) (if (= k 0) 0 (inner (- k 1))))
(defun outer (n) (if (= n 0) 'done (progn (inner 900000) (outer (- n 1)))))
(outer 1000000)
```
runs past any timeout while a bare `(defun loop () (loop))` stops after 1e6 turns. runtime.go calls the limit "a backstop against a loop that never terminates" without saying it is per loop.

Owner decision: (1) `elps run` (and `elps debug` if it shares the runner) gain `--timeout <duration>` and `--max-steps <n>` flags, default unlimited (compat), wired to the existing WithMaxSteps / context deadline; (2) SIGINT/SIGTERM cancel the evaluation context so the interpreter reports an ordinary cancellation error, flushes output, and exits non-zero with a one-line message instead of dying silently; second Ctrl-C force-exits; (3) document MaxTailIterations precisely (per contiguous tail-call sequence, reset when a call returns; MaxSteps is the total-work bound) in runtime.go godoc and docs/lang.md limits.

DONE CRITERIA:
- [ ] Tests: `--timeout 1s` on an infinite loop exits non-zero within ~2s with the cancellation message; `--max-steps` stops a loop with step-limit-exceeded; a normal program is unaffected; SIGINT test if feasible in-process (send to self) else documented manual check with output pasted.
- [ ] Help text for the new flags; docs/lang.md limits paragraph on tail iterations vs steps.
- [ ] `go test ./cmd/... ./lisp/` green; gofmt/vet clean; committed and pushed.
