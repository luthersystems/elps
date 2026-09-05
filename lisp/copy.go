// Copyright © 2026 The ELPS authors

package lisp

// deepCopy returns a copy of v whose data containers share no storage with
// v: lists, vectors (the LArray backing storage included -- which Copy used
// to share and, since #604, rebuilds too), sorted-maps and bytes are rebuilt
// with fresh backing, recursively, and the seal (lisp/seal.go) is cleared on
// every node of the result.  It is the engine behind the lisp `copy` builtin
// (issue #378).
//
// deepCopy is NOT detach with a friendlier name.  detach exists to move a
// value between Runtimes and therefore refuses the two shapes it cannot
// hermetically clone — LFun (a closure captures its defining LEnv and,
// through it, the whole source runtime) and LNative (arbitrary Go data).
// deepCopy is a WITHIN-ENV ownership primitive, so those same two shapes
// are shared by reference instead of rejected:
//
//   - LFun: lisp code has no way to mutate a function's internals (its
//     payload fields are unexported — issue #382), and refusing functions
//     would make `(copy m)` fail on any map that happens to hold a
//     callback.
//
//     Sharing an LFun is NOT equivalent to copying it, and the difference
//     is observable: a closure captures BINDINGS, not values, so a lambda
//     carried into the copy keeps reading and writing the containers its
//     defining scope holds.  For the object-with-methods idiom — a map of
//     lambdas over a shared `let` binding — `((get (copy m) "bump"))`
//     mutates what the ORIGINAL closes over, and a mutation applied to the
//     copy is invisible to the copy's own methods.  Nothing at this layer
//     can fix that: copying the LFun would copy a pointer to the same
//     *LEnv, and copying the environment is a different primitive (a fork,
//     not a copy) with its own semantics to settle.  Callers who need an
//     independent object must rebuild it through its constructor.
//     TestCopySharedClosureKeepsTheOriginalsBindings pins the behaviour and
//     docs/func.md warns lisp authors about it.
//
//   - LNative: an opaque Go value the kernel cannot clone.  Sharing it is
//     the same guarantee the rest of the language already gives it.
//
//     A payload implementing NativeCloner (lisp/fork.go) is the exception,
//     and it is the payload's own doing: it has stated what a duplicate of
//     itself is, so `copy` takes it at its word and the copy holds a clone
//     rather than the original handle.  A stateful native that must not be
//     shared with a copy therefore has exactly one lever to pull, and it is
//     the same lever Fork and detach honour (issue #546).
//
//   - The three singletons (Nil(), Bool(true), Bool(false)) are shared,
//     immutable, and unmutable from lisp; copying them would allocate a
//     distinct value with no observable difference (see lisp/singleton.go).
//
// Strings, ints and floats are immutable values in this implementation and
// travel with the struct copy regardless.
//
// Internal aliasing is preserved exactly as detach preserves it: a value
// reachable along two paths is copied once, and a cycle becomes the same
// cycle in the copy rather than infinite recursion.  The same holds one
// level down for the payloads a header points at: two DISTINCT *LVals over
// one *MapData, one *[]byte or one NativeCloner handle — what
// `(quasiquote (unquote a))` produces — share one rebuilt payload in the
// copy, so a write through either is still visible through the other
// (issue #585; the walker's payload memos, lisp/detach.go).
//
// That preservation stops at the Cells backing array.  Two DISTINCT
// *LVals that share a Cells backing array — what cdr, rest and (slice
// 'list …) produce, and what lets `append 'vector` write a sibling's spare
// capacity — land in the copy with separate backing arrays, so an in-place
// write through one is no longer visible through the other:
//
//	(set 'l (list 9 3 1 2))
//	(stable-sort < (cdr l))     ; l becomes '(9 1 2 3)
//	(set 'c (copy (list l (cdr l))))
//	(stable-sort < (nth c 1))   ; (nth c 0) is unchanged
//
// The copy has strictly fewer accidental aliases than the original, which
// is the safe direction, but it is not "sharing preserved" and callers
// must not rely on a backing-array alias surviving a copy.
// TestCopyDoesNotPreserveBackingArraySharing pins it.
//
// The error return survives only for shapes lisp code cannot hold — the
// internal marker types and an unrecognized Native payload — so that a
// kernel bug surfaces as an ordinary catchable condition rather than a
// silently half-shared copy.
func (v *LVal) deepCopy() (*LVal, error) {
	if v == nil {
		return nil, nil
	}
	d := &detacher{seen: make(map[*LVal]*LVal), shareOpaque: true}
	return d.detach(v)
}

// builtinCopy implements the `copy` builtin: take ownership of a value.
//
// The seal design (lisp/seal.go) makes program literals — quoted data,
// macro arguments, defun bodies — shared, read-only storage.  `copy` is
// the sanctioned way for lisp code to obtain a private, freely mutable
// version of data whose provenance it does not control, replacing the
// one-level `(concat 'list x)` idiom, hand-rolled recursive copiers and
// json:dump/json:load round-trips used as deep copies.
//
// It is deliberately unconditional: there is no `sealed?` predicate to
// pair it with, because the seal bit reports program-text provenance and
// not exclusive ownership — an unsealed value can still be aliased by
// another binding, a container or a closure (issue #378).  Code that
// intends to mutate data it did not construct copies first, full stop.
func builtinCopy(env *LEnv, args *LVal) *LVal {
	cp, err := args.Cells[0].deepCopy()
	if err != nil {
		return env.Errorf("value cannot be copied: %v", err)
	}
	return cp
}
