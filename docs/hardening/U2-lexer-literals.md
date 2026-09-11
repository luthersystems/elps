# U2 [P1/P2] Lexer literal diagnostics: oversized strings, malformed numbers, undocumented #x/#o
Subsystem: parser/lexer, parser/token, docs/lang.md Numbers + Strings.

F4 [P1] A string literal >= 131072 bytes fails with `scan-error: unexpected rune ' '` (col points at the opening quote). Repro: `python3 -c "print('(length \"'+'a'*131072+'\")')" > .tmp/s.lisp && ./elps run .tmp/s.lisp`. 131071 is fine. Raw `"""` strings say `unterminated raw-string literal`. Suspect lexer.go:171-179 reaches the generic fallback because scanner.Err() is nil there; limit is scanner.go:52 DefaultBufSize.
Owner decision: customers embed certs/base64/JSON blobs in .lisp config. If the scanner can accumulate a string token across window slides, support large strings (bounded by MaxAlloc if the reader has one). Otherwise emit the accurate "token exceeds maximum allowable size (131072 bytes)" at the opening quote and document the limit under Strings.

F9 [P2] Malformed numeric literals silently split into two tokens: `'(0x10)` -> `'(0 x10)`, `'(1_000)` -> `'(1 _000)`, `'(1.2.3)` -> `'(1.2 .3)`. docs/lang.md:23-35 already rejects `a:1` with invalid-symbol for the same reason.
Owner decision: a number token immediately followed by a symbol/number constituent (no delimiter) is a scan error naming the literal, e.g. `invalid numeric literal "0x10" (hex is spelled #x10)`. Before committing, prove the repo's own sources still parse: run `./elps fmt --check` (or equivalent) over every .lisp file in the repo and the formatter/lsp/fuzz seed corpora.

F10 [P2] `#x10` and `#o17` work (with overflow errors) but are undocumented; `#b101` gives `invalid dispatch macro character 'b'`.
Owner decision: document `#x`/`#o` in docs/lang.md Numbers with the overflow behaviour. Do NOT add `#b`; make the dispatch error list the supported prefixes. Fix the stale TODO at lexer.go:347 ("support octal and hex integer literals").

DONE CRITERIA:
- [ ] Red-then-green tests: string at 131071/131072/200000 (either supported or accurate error at the quote), raw string same; `0x10`, `1_000`, `1.2.3`, `1e5x`, plus valid look-alikes `1.5`, `-1`, `1e5`, `.5`, `x10`, `(1 .3)` still parse.
- [ ] All repo .lisp sources still parse and format identically (paste the command and its summary).
- [ ] docs/lang.md Numbers documents `#x`/`#o`; Strings documents the literal size rule.
- [ ] `go test ./parser/... ./formatter/... ./lsp/... ./lint/...` green; gofmt/vet clean; committed.

## Additional lexer items (probe D):
D-F2 symbol shape: a 140000-char symbol `(set 'aaaa...a 1)` is split by the same window wedge - `elps fmt` INSERTS a space at byte ~131078 (output one byte longer, one identifier becomes two), rc=0, no diagnostic. Treat exactly like the string case: either accumulate across window slides or raise the accurate "token exceeds maximum allowable size" error at the token start; never split. Test at 131071/131072/140000 for symbols in run and fmt.
D-F12 BOM: a file starting with the UTF-8 BOM (EF BB BF) fails every tool with `scan-error: unexpected text starting with '﻿'`. Owner decision: the lexer skips a single leading BOM at offset 0 (only there), the formatter preserves it on output only if it was present (or drops it - pick one, document, and test round-trip), and the error for a BOM anywhere else names it ("unexpected byte-order mark"). Windows editors write BOMs by default.
Add to DONE CRITERIA: [ ] symbol split tests (run + fmt) red-then-green; [ ] BOM at offset 0 accepted by run/lint/fmt/minify/analyze with tests, BOM elsewhere named in the error.
