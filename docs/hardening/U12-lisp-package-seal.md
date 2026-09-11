# U12 [P2, embedder integrity] Ordinary code can permanently corrupt the shared `lisp` package with a qualified `set`
Subsystem: lisp/ set/set! builtins, package binding logic (lisp/package.go, env.go), lint/ migration diagnostic, docs/lang.md Packages.

Repro:
```lisp
(set 'lisp:if 1)
(in-package 'mypkg)
(use-package 'lisp)
(if true 'a 'b)      ; error: first element of expression is not a function: 1
```
Works for any core name (lisp:car, lisp:lambda, lisp:quasiquote). The current package survives (it holds imported copies) so the damage appears only in the NEXT package created or library loaded; for an embedder with one Runtime behind many requests this is cross-request corruption. Precedent: `set` already refuses `true`/`false` with "cannot rebind constant".

Owner decision: after InitializeUserEnv / LoadLibrary, bindings of the `lisp` package are sealed against redefinition from Lisp code: `set`, `set!`, `defun`, `defmacro`, `export`-then-shadow on a `lisp:`-qualified name (or on any name while `(in-package 'lisp)`) raise an ordinary error "cannot rebind lisp package binding: if". Shadowing in the CURRENT user package (`(defun car ...)` unqualified in package user) stays allowed - that is ordinary Lisp-1 behaviour and documented. Find how the Go side registers builtins so the seal is set once, after registration, and embedders adding their own packages via elpsutil are unaffected (test that). If the repo's own tests/examples rebind lisp: names in a way that signals real customer use, fall back to documented-only and say so.
Add a lint analyzer (see .claude/skills/add-linter-check/SKILL.md) that flags `(set 'lisp:<name> ...)` / `(set! 'lisp:<name> ...)` / `(defun lisp:<name> ...)` statically, with valid look-alikes (`(set 'mypkg:x 1)`, `(get m 'lisp:car)`) not flagged.

DONE CRITERIA:
- [ ] Red-then-green tests: the repro raises the new error at the `set`; the next package's `if` still works; unqualified shadowing in the user package still works; an embedder-registered package is not sealed.
- [ ] Lint analyzer registered in DefaultAnalyzers with tests for the broken pattern and look-alikes; analyzer count test updated.
- [ ] docs/lang.md Packages: the seal, the error, and the unqualified-shadowing rule.
- [ ] `go test ./lisp/... ./lint/...` green; gofmt/vet clean; committed.

## Additional (probe D-F9): unqualified rebinding of core names
`(set 'if 1)`, `(set 'lambda 1)`, `(set 'quote 1)`, `(defun car ...)` in the user package succeed and break the language for everything evaluated afterwards in that package (including the enclosing handler-bind's own handler). This is Lisp-1 shadowing and stays LEGAL at runtime, but: (1) extend or add a lint analyzer so a TOP-LEVEL `set`/`set!`/`defun`/`defmacro` whose name is an export of the `lisp` package gets a warning naming the shadowed builtin/special operator (the existing `shadowing` analyzer at lint/analyzers.go:~1095 covers local bindings only) with look-alikes not flagged (local `(let ((list ...)))` is already handled; a defun named like a lisp export inside another package that does not use-package lisp - still flag, lisp is always used); (2) document under Packages that special-operator and builtin names are ordinary symbols, shadowing is per-package, already-imported packages keep old values so damage appears in packages created later.
Add to DONE CRITERIA: [ ] top-level lisp-export shadowing lint with tests; [ ] doc paragraph.
