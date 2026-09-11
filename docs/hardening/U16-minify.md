# U16 [P1] `elps minify` silently miscompiles idiomatic programs (rc=0, broken output)
Subsystem: minifier/ (minifier.go ~:894-897 "LQuote is skipped ... a quoted form is data, not a reference"), cmd/minify, tests using _examples/.

Repro A (quoted symbol names a function - the canonical ELPS way to pass one: `(map 'list 'f ..)`, `(sort '< ..)`, `(foldl 'cons ..)`):
```lisp
(defun twice (x) (* 2 x))
(debug-print "r" (map 'list 'twice '(1 2 3)))
```
minify renames the defun to `x1` but leaves `'twice` -> running the output: `unbound symbol: 'twice`.
Repro B (`defun` inside `let` creates a TOP-LEVEL binding in ELPS):
```lisp
(let ([k 1]) (defun helper (x) (+ x k)))
(debug-print "r" (helper 1))
```
minify scopes `helper` as local -> `unbound symbol: helper`. Both shapes exist in the repo's own examples: `_examples/random/oop.lisp`, `_examples/sicp/sicp.lisp` (`(accumulate 'append ...)`), `_examples/user-defined-types/phone_solved.lisp` all break when minified and run clean unminified. `elps minify --help` promises "deterministic, scope-aware symbol renaming".

Owner decision: (1) any symbol that appears QUOTED anywhere in the program (including inside quoted lists and quasiquote templates) is excluded from renaming - conservative and always semantics-preserving; the symbol map should record why (quoted-reference). (2) `defun`/`defmacro`/`set`(with a quoted symbol)/`export` at ANY nesting depth bind at package level and are treated as global for renaming, exactly as the evaluator does. (3) Add a regression harness: for every `.lisp` under _examples/ (and any other runnable corpus the repo has), minify then run both and require identical stdout and exit code. Document the two rules and the exclusion in the minify help/README section.

DONE CRITERIA:
- [ ] Red-then-green unit tests for A and B; example-corpus equivalence test red (3 failing examples) then green.
- [ ] Symbol map output marks quoted-excluded symbols.
- [ ] Help text / docs updated with the rules and their cost (quoted names are not shortened).
- [ ] `go test ./minifier/... ./cmd/...` green; gofmt/vet clean; committed and pushed.
