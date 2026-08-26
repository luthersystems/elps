// Copyright © 2026 The ELPS authors

package lisp

// RuntimeBound is the opt-in runtime-affinity protocol for native payloads
// (issue #546).  Some native payloads are only meaningful inside the
// Runtime they were built for — a handle onto that runtime's storage, a
// cache keyed by its package registry, an accumulator whose contents only
// mean anything to one environment tree.  Such a payload declares the
// binding here, and checked builds assert it is never used from a foreign
// Runtime.
//
// BoundRuntime returns the Runtime the payload is affined to, or nil while
// the payload is UNBOUND.  A nil return disables all checking for that
// payload, which is what lets a type whose instances are sometimes bound
// and sometimes free — a pooled buffer before checkout, a clone minted for
// a fork it has not been handed to yet — answer with what is true at the
// moment it is asked instead of lying in one direction or the other.
//
// Declaring the interface costs nothing.  This file compiles into every
// build, but no production build ever calls BoundRuntime: enforcement lives
// entirely behind `-tags elpscheck`, where there are two points.
//
//   - USE TIME — the ownership checker's instrumented points (LEnv.Put,
//     LEnv.PutGlobal, and entry to env.eval).  Every LNative value crossing
//     one of them has its payload checked against that environment's
//     Runtime.  This inherits the checker's documented SHALLOWNESS (see
//     lisp/ownership_check_elpscheck.go): only the *LVal that actually
//     crosses the point is examined, never its Cells, so a native riding
//     inside a list or a sorted map is invisible here.
//   - FORK TIME — the fork walker visits every native payload reachable
//     from the template, whatever container it rides in, and checks each
//     one against the fork's runtime.  That is the deep half, and it is
//     what covers the natives use-time checking cannot see.
//
// # Forking
//
// A fork lands on a DIFFERENT Runtime.  That is not incidental, it is
// Fork's whole purpose and the concurrency contract the Runtime doc comment
// states.  So a bound payload that reaches a fork by the default
// share-by-reference policy IS a violation, and it is caught at fork time
// rather than later, inside the fork, when some request finally touches the
// template's handle.  Failing at the fork is the point: the fork is where
// the mistake was made and where the stack still names the culprit.
//
// A bound payload type that wants to survive forking implements
// NativeCloner (or the embedder substitutes it per fork with
// ForkWithNativeReplacer) and returns a clone that is unbound, or bound to
// the destination.  A clone that copies the template's binding still trips
// the fork-time check, deliberately: such a clone is independent in its
// bytes yet still tethered to the runtime it came from, which is exactly
// what NativeCloner's own contract already forbids — a clone must retain no
// reference into the Runtime or LEnv tree the receiver lives in.
//
// # What this closes
//
// This is the type-level replacement for a load-time dynamic probe: an
// embedder walking a loaded environment and asking each native whether it
// reads runtime state out of a context.  A probe answers only for the
// natives that exist on the day it runs, and only for the ones whose state
// reads take the shape it recognizes.  A declared binding answers for two
// cases it cannot:
//
//   - A NEW builtin that captures runtime state and forgets to declare it.
//     A probe cannot flag what it never saw; the affinity check catches the
//     payload the first time it crosses a runtime boundary, in whatever
//     test happens to fork or share it.
//   - A mutable native that never touches the context at all — a cache, a
//     counter, a pooled buffer.  There is no context read to probe for, yet
//     the payload is just as unsafe to share, and declaring the binding
//     says so directly instead of leaving it to be inferred.
type RuntimeBound interface {
	BoundRuntime() *Runtime
}
