# U17 [P1] `golang:struct-field` raises `internal-panic` on a promoted field reached through a nil embedded pointer
Subsystem: lisp/lisplib/libgolang/libgolang.go (:179 `v.FieldByName(field.Str)`; derefPtr handles only the top-level pointer; BuiltinString :97 lacks the `lval.Type != lisp.LNative` gate the other three have).

Repro (Go test in libgolang):
```go
type B struct{ X int }
type A struct{ *B }           // ordinary shape, e.g. struct{ *http.Request }
env.PutGlobal(lisp.Symbol("nv"), lisp.Native(A{}))
env.LoadString("t", `(golang:struct-field nv "X")`)  // internal-panic: reflect: indirection through nil pointer to embedded struct
```
Also for `&A{}` and a two-level chain `struct{ *Mid }` / `Mid struct{ *B }`. Every other bad input (nil payload, non-struct, unexported, missing) already returns a clean env.Errorf.

Owner decision: ordinary error ("field X is reached through a nil embedded pointer") - walk the field index path (reflect.Type.FieldByName gives the index; check each intermediate pointer for nil before FieldByIndex, or use FieldByIndexErr). Add the missing LNative gate to BuiltinString. Sweep the rest of libgolang (and lisp/embed.go GoValue / lisp/native.go conversions reachable from Lisp) for reflect calls that panic on nil/invalid Values (Elem on nil, Int on wrong kind, Len on non-collection, Interface on unexported) and guard each with a test; the existing lisplib TestBuiltinRegistryNeverPanics is the model - see if it can be extended with native payload shapes.

DONE CRITERIA:
- [ ] Red-then-green tests: nil embedded pointer (value receiver, pointer receiver, two-level), plus non-nil embedded pointer still reads correctly.
- [ ] BuiltinString gate added with a test.
- [ ] Sweep report: reflect call sites checked, with which are guarded and how.
- [ ] `go test ./lisp/lisplib/libgolang/... ./lisp/` green; gofmt/vet clean; committed and pushed.
