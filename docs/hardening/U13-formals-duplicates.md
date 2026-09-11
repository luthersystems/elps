# U13 [P2] Malformed lambda lists are accepted at definition time; duplicate names are silently last-wins and unlinted
Subsystem: formals parsing (lisp/env.go / lisp/lisp.go where lambda/defun/labels/flet build LFun), lint/ analyzers, docs/lang.md Functions.

B-F7: `(defun f (&rest a &rest b) a)`, `(lambda (&rest) 1)`, `(lambda (&rest a &optional b) ..)`, `(lambda (&rest a &key b) ..)`, `(lambda (&rest a b) ..)`, `(lambda (&bogus a) ..)` all evaluate to a function object and only fail when CALLED ("formal argument list contains a control symbol at an invalid location: &rest" / "invalid control symbol `&bogus'"). Yet `(lambda (&key (a 1)) a)` IS rejected eagerly - inconsistent. `elps lint` is silent on all of them.
Owner decision: validate the full lambda list when the function is created (lambda, defun, defmacro, labels, flet) with the same messages the call path uses today; keep the call-path check as a backstop. Add a lint analyzer for malformed lambda lists (unknown `&` control symbol, `&rest` without a name or followed by more formals, `&optional`/`&key` after `&rest`, duplicate control symbols).

B-F8: duplicates are silently rightmost-wins everywhere: `(let ((x 1) (x 2)) x)` -> 2, `(let* ((x 1) (x (+ x 1))) x)` -> 2, `(labels ((f (x) 1) (f (x) 2)) (f 0))` -> 2, `((lambda (x x) x) 1 2)` -> 2, and `(kf 1 :b 2 :b 3)` -> `'(1 3)` (Common Lisp takes the LEFTMOST keyword). `grep -i duplicate docs/lang.md` is empty.
Owner decision: (a) duplicate formal names in ONE lambda list `(lambda (x x) ..)` are an error at creation (never meaningful); (b) duplicate names in let/let*/labels/flet stay legal at runtime (let* rebinding is idiomatic) but get a lint WARNING for `let`, `labels`, `flet` (not `let*`); (c) duplicate keyword arguments at a call site: keep rightmost-wins at runtime (compat) but DOCUMENT it explicitly, noting it differs from Common Lisp, and lint a literal duplicate keyword in a call form.

DONE CRITERIA:
- [ ] Red-then-green tests: each malformed lambda list above errors at creation with the existing message; `(lambda (x x) x)` errors; valid lists (`(a &optional b)`, `(a &rest r)`, `(a &key k)`, `(&optional a &rest r)`, `(&key a b)`) still work.
- [ ] Lint analyzer(s) with tests: malformed lambda lists; duplicate names in let/labels/flet/lambda; literal duplicate keyword at call site; look-alikes not flagged (let* rebinding, nested lets, same name in different labels bodies). DefaultAnalyzers count test updated.
- [ ] docs/lang.md Functions: eager validation; duplicate rules (a)(b)(c).
- [ ] `go test ./lisp/... ./lint/...` green; gofmt/vet clean; committed.
