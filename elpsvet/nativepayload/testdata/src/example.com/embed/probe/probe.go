// Package probe stands in for an embedder's capture declaration: a builtin
// that calls Capture cannot run while a template is built.
package probe

type Context interface{ TxID() string }

func Capture(ctx Context, op string) {}
