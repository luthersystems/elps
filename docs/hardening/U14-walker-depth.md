# U14 [P0] `copy`, `json:dump-string`, `format-string` and embedder `lisp.GoValue` die with `fatal error: stack overflow` on a ~2M-deep value
Subsystem: lisp/copy.go builtinCopy (~:115), lisp/lisplib/libjson/json.go (*Serializer).GoValue (~:696) and encode.go, lisp/embed.go GoValue (:130-180), lisp/detach.go, any other recursive LVal walker.
Depends on U3 (render depth) - it establishes the depth-guard mechanism for the renderer; reuse the same constant/approach so the rule is one rule. If U3 is not yet merged into your base, read scratchpad brief U3 and implement a shared helper both can use; the orchestrator will reconcile.

Repro:
```lisp
(set 'x 1)
(dotimes (i 2000000) (set 'x (list x)))
(handler-bind ((condition (lambda (c &rest d) (debug-print "caught" c)))) (copy x))
(debug-print "survived" 'ok)
```
rc=2, `runtime: goroutine stack exceeds 1000000000-byte limit` / `fatal error: stack overflow`. Same for `(format-string "{}" x)` and `(json:dump-string x)`. Thresholds: copy fine at 1.5M, fatal at 1.8M; json:dump fine at 1M, fatal at 2M. Uncatchable by handler-bind, with-cleanup, recover, IsInternalPanic - the whole embedder process dies. No documented limit fires: MaxEvalNesting/MaxPhysicalStackHeight guard eval recursion, not a builtin's own Go recursion; the value costs ~2M steps to build, inside any realistic MaxSteps. `json:load-string` of 2M `[` IS depth-guarded (json:syntax-error) - the guard exists on load and is missing on dump/copy. Survive at that depth today: to-string, equal?, reverse, flatten, sorted-map (to-string/reverse/flatten raise catchable errors, equal?/sorted-map succeed).

Owner decision: every recursive walk over program-built values has a depth guard that raises an ordinary ELPS error ("value nesting depth exceeds maximum: N") or, for walkers that cannot error (printing to a writer), a truncation marker. One shared constant, documented in docs/lang.md limits section next to the existing eval-nesting limit, with the embedder knob if the runtime already has a pattern for such knobs (WithMax...). The guard must be cheap (a counter, not a map). Produce an inventory table: walker -> file:line -> guard (explicit stack / depth counter / existing limit) -> test.

DONE CRITERIA:
- [ ] Red-then-green tests with a deadline for copy, json:dump-string, format-string, lisp.GoValue (embedder API), detach/template publication if reachable, at 3M depth: ordinary error, process alive.
- [ ] Control tests: 100k-deep values still copy/dump/format correctly (no false positives at realistic depth).
- [ ] Inventory table in the report and as a comment block near the shared constant.
- [ ] docs/lang.md limits section updated.
- [ ] `go test ./lisp/...` green; gofmt/vet clean; committed and pushed.
