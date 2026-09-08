// Copyright © 2026 The ELPS authors

// Package templatepolicy contains contracts shared only by ELPS's runtime,
// standard library, and isolation-test tooling. Downstream native payloads use
// lisp.TemplateWithNativePolicy instead of depending on this internal package.
package templatepolicy

// Immutable declares that an audited native struct value and everything
// reachable from it are immutable and safe to share across runtimes. Read-only
// methods alone do not satisfy this contract if another owner can mutate the
// same data. Automatic admission applies only to struct values, not pointers:
// pointer method sets inherit this marker, but the whole pointee is writable.
// Pointer forms require explicit TemplateWithNativePolicy approval instead.
//
// The unexported method prevents downstream method-name lookalikes from
// bypassing TemplateWithNativePolicy. Only audited repository implementations
// embed Marker. This remains a code-review contract, not a proof of immutability.
type Immutable interface {
	templateImmutable()
}

// Marker is embedded only in audited, repository-owned immutable struct values.
// Downstream code cannot import this internal package and must use an explicit
// native policy. Embedding this marker does not inspect the enclosing payload.
type Marker struct{}

func (Marker) templateImmutable() {}
