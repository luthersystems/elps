# U8 [P2] `json:null` serializes as the JSON STRING "json:null"
Subsystem: lisp/lisplib/libjson/encode.go encodeLSymbol (:698-704), libjson docs.

Repro: `(json:dump-string json:null)` -> `"\"json:null\""`; `(json:dump-string (sorted-map "k" json:null))` -> `{"k":"json:null"}`. Docstring: "The JSON null sentinel symbol. Used to represent null in JSON serialization." The deprecated Serializer.GoValue (json.go:710) did map s.Null to nil; the rewrite dropped it.
Owner decision: `json:null` encodes as `null` at every position (value, map value, array element, nested). Decoding is unchanged (null -> `()` today; confirm and document whether json:null is ever produced by load, or is output-only). Document in the json package docs and docs/lang.md JSON section.

DONE CRITERIA:
- [ ] Red-then-green tests: json:null at top level, as map value, in vector/list, nested, and `(json:dump-string (list json:null ()))` -> `[null,null]`.
- [ ] Round-trip documented: what load returns for null and that json:null is a serialization sentinel.
- [ ] `go test ./lisp/lisplib/libjson/...` green; gofmt/vet clean; committed.
