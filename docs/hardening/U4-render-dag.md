# U4 [P0] debug-print, top-level print and ERROR RENDERING expand a shared DAG exponentially and hang forever
Subsystem: lisp/lisp.go String path, lisp/render_bounded.go, debug-print builtin, error rendering (env error message formatting), cmd/run top-level printer.
Depends on U3 having landed (rebase onto the worktree branch tip before starting if the orchestrator says so; otherwise proceed).

Repro (41 cons cells total):
```lisp
(set 'x '(1))
(dotimes (i 40) (set 'x (list x x)))
(debug-print "dag" x)      ; hangs, killed by timeout, zero output
```
Also hangs: the top-level printer (`./elps run -p` printing `x`), and `(error 'boom x)` while rendering the error message - where no handler-bind can intervene. `format-string "{}"` is correctly stopped by the bounded renderer ("allocation size exceeds maximum (10485760)"), so MaxAlloc applies on one path and is skipped on three others. cycle.go:30-31 deliberately renders DAGs in full (correct), but nothing bounds the output.

Owner decision: every render path a program can trigger goes through the bounded renderer and honours Runtime.MaxAlloc and cancellation: debug-print, top-level printing, error-message rendering (including the condition data rendered into `error:` lines and stack traces), and `to-string`/`format-string` (already bounded - confirm). On overflow: ordinary allocation error where a caller can receive one; a truncation marker where no caller exists (error reporting, top-level). Document in docs/lang.md: cycles render as `#<cycle>`-style markers, DAGs render in full up to the output cap, then truncate/raise.

DONE CRITERIA:
- [ ] Red-then-green tests with a deadline for: debug-print of the 40-level DAG, error rendering of `(error 'boom x)`, top-level print via the cmd/run path or its underlying function.
- [ ] `timeout 20 ./elps run` of each repro exits within seconds with either bounded output or an ordinary error; paste rc and first line.
- [ ] Grep proof: list every call site that formats an LVal into text for the user (`.String()`, `Sprintf("%v", lval)`, etc.) in lisp/, cmd/, repl/ and state which are bounded and how; fix the unbounded ones.
- [ ] docs/lang.md rendering/limits rule written.
- [ ] `go test ./lisp/ ./cmd/... ./repl/...` green; gofmt/vet clean; committed.
