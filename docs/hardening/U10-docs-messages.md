# U10 [P2] Documentation and diagnostic-message sweep (no semantic changes)
Subsystem: docs/lang.md, builtin docstrings, error message text in lisp/builtins.go and lisp/lisplib/libstring, libmath package summary.

Items:
1. A-F11: `'x` (reader quote, `(type ''a)` -> 'quote) and `(quote x)` (a list) are distinct values, not `equal?`, both evaluate to x. Document under Quoted Expressions with the macro-author implication (a macro inspecting `(car form)` must handle both spellings).
2. A-F14: `'(1 . 2)` is a 3-element list because `.` is an ordinary symbol; `(length '(a . b . c))` -> 5. Document that dotted-pair notation is not supported, next to Symbols where `.` is mentioned (docs/lang.md:33-34).
3. C-F6: `slice 'string` is byte-indexed and can split a rune: `(slice 'string "José" 0 4)` -> "Jos\xc3" (invalid UTF-8, becomes U+FFFD in json:dump/string:uppercase). `string:split` with "" is rune-aware. Document in `./elps doc slice` and the Strings section: indices are bytes, results may be invalid UTF-8, and name the rune-safe alternative (string:split "" / a helper if one exists).
4. C-F8: `(reverse 'list "hello")` -> "first argument is not a proper sequence: symbol" - wrong argument and wrong type (builtins.go:2219-2221 reports args.Cells[0].Type). Fix to "second argument is not a proper sequence: string" as slice does (builtins.go:2248-2250). Test it.
5. C-F11 docstring/message defects: `string:split` docstring mentions a limit argument but the function is 2-arity - fix the docstring (or implement the limit if trivial and consistent with Go strings.SplitN; owner prefers fixing the docstring); math package summary claims "rounding" but exports no round/trunc - fix summary; `format-string` silently ignores surplus arguments while too few is an error - document; `format-string` accepts padded indices `{ 0 }` - document or reject (owner: document); typos in user-visible errors: aref "dimenion", stable-sort "arument", car/cdr "argument is not a list array" (missing separator/wording) - fix and grep for other typos in Errorf strings across lisp/ and lisplib/ (e.g. run a spell pass over quoted strings).
6. A-F10 placeholder: do NOT touch #x/#o docs; U2 owns them.

DONE CRITERIA:
- [ ] Each of items 1-5 has its doc text or message fix; item 4 and the typo fixes have tests asserting the exact message.
- [ ] `./elps doc -m` reports nothing; `./elps doc --guide` renders the new sections.
- [ ] `go test ./lisp/... ` green; gofmt/vet clean; committed.

## Additional doc items (probe B):
7. B-F6: `in-package` inside a function body switches the caller's current package for every following top-level form (`(defun switcher () (in-package 'otherpkg) 'ok)` then `(switcher)` -> later `gv` unbound). Load-file/load-string DO restore the package around a load. Document under Packages: in-package is an ordinary evaluated form, not a file-level declaration; libraries must not call it from helpers; loads are the only automatic restore.
8. B-F9: `handler-bind` blocks tail-call optimisation exactly like `with-cleanup` (a 1e6 self-call through `(handler-bind () (loop1 ...))` hits "physical stack height exceeded maximum: 25001"). The guide documents only with-cleanup (L~2074-2089). Add handler-bind to that subsection with the same worked example and the recommended restructure (loop outside, handler inside per iteration).
9. cond nits: `(cond (true))` and `(cond (else))` return `()` rather than the test value (Common Lisp returns the test value) - document the ELPS rule; `else` is matched as a bare symbol while docs spell `:else`, and a non-final `:else` is silently accepted where a non-final `else` errors - document the supported spelling(s) and make the two behave the same if trivially possible (owner: prefer making non-final `:else` an error like `else`; otherwise document).
10. `funcall`/`apply`/`map` accept a quoted symbol and resolve it to the function - document as a convenience.
Add to DONE CRITERIA: [ ] items 7-10 documented (item 9's behaviour change, if made, has a test).
