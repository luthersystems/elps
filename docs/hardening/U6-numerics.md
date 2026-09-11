# U6 [P1/P2] Numeric semantics: pow overflow, NaN in max/min, documentation of division by zero and float printing
Subsystem: lisp/builtins.go numeric builtins, docs/lang.md Numbers.

F5 [P1] `pow` wraps silently: `(list (pow 2 64) (pow 10 19) (pow 10 20) (pow 3 40))` -> `'(0 -8446744073709551616 7766279631452241920 -6289078614652622815)`. docs/lang.md:72-74 documents wraparound for `+ - *` only; `math:abs` already raises `integer overflow` on MinInt.
Owner decision: integer `pow` raises the same integer-overflow condition math:abs uses; document next to the wraparound paragraph.

F7 [P1] `(let ([n (/ 0.0 0.0)]) (list (max n 1) (max 1 n) (min n 1) (min 1 n)))` -> `'(NaN 1 NaN 1)`: order-dependent. `(stable-sort < (list 3.0 n 1.0 2.0 n 0.5))` returns an unsorted list silently.
Owner decision: max/min propagate NaN regardless of argument order (Go math.Max/Min semantics); keep int results int when no float is present. Sort: do not change; document that a comparator that is not a strict weak ordering (e.g. `<` over NaN) yields an unspecified order and no error.

F8 [P2] doc-only: `(/ 1 0)` -> `+Inf`, `(/ 0.0 0.0)` -> NaN, while `(mod 1 0)` -> error "second argument is zero". Document both with the `(/ total count)` footgun and the guard.
F12 [P2] doc-only: floats print without a decimal point (`100.0` renders `100`; `(to-string 1.0)` -> "1"; json:dump-string 100.0 -> `100`). Document the external representation and that `to-float` recovers the type; do NOT change printing.

DONE CRITERIA:
- [ ] Red-then-green tests: pow overflow cases above raise the condition; `(pow 2 62)`, `(pow -2 63)`, `(pow 10 18)`, `(pow 2 0)`, `(pow 0 0)`, negative exponents and float operands unchanged; max/min NaN commutativity in both positions and with 3+ args.
- [ ] docs/lang.md Numbers: pow overflow rule; NaN in max/min and comparators; division by zero; number printing. Docstrings of pow/max/min/`/`/mod updated.
- [ ] `go test ./lisp/` green; gofmt/vet clean; committed.
