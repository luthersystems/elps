# U18 [P1/P2] Package builtins: `export` swallows list errors, `in-package` mutates before validating, qualified export poisons use-package, unreachable package names accepted
Subsystem: lisp/builtins.go in-package (~:566-590) and export (~:600-620), lisp/package.go, docs/lang.md Packages.

D-F6 [P1] `(export '(a 1 b))` returns nil (success) and exports only `a`: builtins.go:615-616 `case LSExpr: builtinExport(env, arg)` discards the return. Fix: propagate; validate the whole list BEFORE exporting anything so a failed export exports nothing.
D-F8 [P2] `(in-package 'docp2 1)` errors "docstring argument is not a string" AFTER switching the package (assignment at :575, check at :588); a program that catches the error keeps compiling into the wrong package. Fix: validate all arguments, then mutate.
D-F10 [P2] `(export 'qe:f)` from package qf succeeds; every later `(use-package 'qf)` fails with `package qf: <native code>: unbound symbol: qe:f` - permanently unimportable, error names neither the form nor the file. Owner decision: `export` rejects a qualified symbol with an ordinary error at export time. Also improve the use-package failure message for an export that is unbound at import time (`(export 'never-defined)`) to name the exporting package and the symbol clearly - keep the existing deferred behaviour (documented) but make it diagnosable.
D-F13 [P2] `(in-package "")`, `(in-package "a:b")`, `(in-package ':kw)`, `(use-package ':kw)` succeed and create packages that no qualified symbol can name (docs/lang.md:1246-1248 requires both halves of a qualified symbol to be identifiers). Owner decision: in-package/use-package validate the name as a symbol identifier per the reader's rules (non-empty, no `:`, not a keyword); ordinary error otherwise. Check that the Go embedding API (env.InPackage / package registration by Go code) is not affected or gets the same validation deliberately - report.

DONE CRITERIA:
- [ ] Red-then-green tests for each of the four items, plus valid look-alikes: `(export '(a b))`, `(export "a")`, `(in-package 'p "doc")`, `(in-package "valid-name")`.
- [ ] docs/lang.md Packages: export validation, in-package argument rules, qualified export rejection, deferred unbound-export semantics with the improved message.
- [ ] `go test ./lisp/...` green; gofmt/vet clean; committed and pushed.
