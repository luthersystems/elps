# U7 [P1/P2] sorted-map key semantics: JSON-decoded maps reject symbol keys; symbol spelling is sticky; get-default on nil
Subsystem: lisp/jsonmap.go, lisp/maps.go, get-default macro, docs/lang.md sorted-map section.

C-F1 [P1] `(set 'doc (json:load-string "{\"a\":1}")) (get doc 'a)` -> `error: sorted-map decoded from json cannot hold key with type 'symbol`. Same for key?, get-default, assoc!, dissoc!, `:a`, nested maps. `(sorted-map? doc)` is true and `(type doc)` is 'sorted-map, so nothing warns the caller. docs/lang.md:889-892 and :933 promise symbol/string keys are interchangeable for get/key?/assoc/dissoc. `(copy doc)` converts to the stock map and then works. Cause: jsonMap.Get/Set/Del reject non-LString.
Owner decision: reads (get, key?, get-default) on a JSON-decoded map accept a symbol/keyword key by using its name as the string key, exactly like the stock map. For writes (assoc!/dissoc!) either store under the string spelling (preferred if the jsonMap representation allows) or convert to the stock map on first symbol write - whichever keeps json:dump round-trips stable. Document that JSON-decoded maps print/dump keys as strings.

C-F10 [P2] `(keys (sorted-map "a" 1 'a 2))` -> `'('a)`; `(let ((m (sorted-map "a" 1))) (assoc! m 'a 2) (keys m))` -> `'('a)`; a later string write never restores the string spelling (maps.go:216-227 puttype only sets symbolkey). docs/lang.md:930-932: "A map remembers whether a key was written as a symbol or a string and prints it back that way".
Owner decision: last write wins - the spelling of the most recent assoc/assoc! for that key is what keys/printing return. Document "last write wins".

C-F9 [P2] `(get () "k")` -> `()` (documented, docs/lang.md:927-929) but `(get-default () "k" 42)` -> error "first argument is not a map: list".
Owner decision: get-default on `()` returns the default, consistent with get. Document.

A-F13 [P2] doc-only: sorted-map keys must be strings or symbols; `(sorted-map 1 'a)` -> "unhashable type: int". Document in the sorted-map section and in the `sorted-map`/`assoc` docstrings.
C-F7 [P2] doc-only: `(json:dump-string (sorted-map :height 1))` -> `{":height":1}` (colon kept, round-trips only via the string ":height"). Document under JSON with the recommendation to use string keys for interchange.

DONE CRITERIA:
- [ ] Red-then-green tests for each of C-F1 (get/key?/get-default/assoc!/dissoc! with 'a and :a on decoded maps, nested), C-F10 (both write orders, keys and printed form), C-F9.
- [ ] json:dump of a decoded map after symbol writes still produces string keys (test).
- [ ] docs/lang.md sorted-map + JSON sections and docstrings updated as listed.
- [ ] `go test ./lisp/...` green; gofmt/vet clean; committed.
