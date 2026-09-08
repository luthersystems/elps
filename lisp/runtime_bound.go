// Copyright © 2026 The ELPS authors

package lisp

// RuntimeBound declares a native payload's runtime affinity. Checked builds
// (-tags elpscheck) reject its use from a different Runtime. BoundRuntime returns
// nil for an unbound value; that disables affinity checks, not template admission.
//
// Checks run at three boundaries:
//   - Use: LNative values crossing Put, PutGlobal and evaluation are checked.
//     This is shallow; values nested in containers are not traversed here.
//   - Template instantiation: all admitted native payloads are checked against
//     the fresh Runtime. Publication admits only immutable natives; a bound
//     service must instead be created separately in each VM.
//   - Strict detach: NativeCloner results must be unbound because the clone
//     method does not know the destination Runtime. A within-VM copy may remain
//     bound to its own Runtime.
//
// RuntimeBound is diagnostic, not an immutability declaration. Neither this
// interface nor NativeCloner makes mutable payloads admissible in NewTemplate.
// Shared immutable declarations remain trusted host contracts; falsely declaring
// a runtime-bound handle immutable is a contract violation, checked here when
// that handle implements RuntimeBound.
type RuntimeBound interface {
	BoundRuntime() *Runtime
}
