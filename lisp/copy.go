// Copyright © 2026 The ELPS authors

package lisp

// deepCopy returns a copy of v whose data containers share no storage with
// v: lists, vectors (including the LArray backing storage that Copy
// deliberately shares), sorted-maps and bytes are rebuilt with fresh
// backing, recursively, and the seal (lisp/seal.go) is cleared on every
// node of the result.  It is the engine behind the lisp `copy` builtin
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
//     payload fields are unexported — issue #382), and the environment the
//     closure captured is the environment the copy runs in.  A shared
//     function value is therefore indistinguishable from a copied one, and
//     refusing it would make `(copy m)` fail on any map that happens to
//     hold a callback.
//   - LNative: an opaque Go value the kernel cannot clone.  Sharing it is
//     the same guarantee the rest of the language already gives it.
//   - The three singletons (Nil(), Bool(true), Bool(false)) are shared,
//     immutable, and unmutable from lisp; copying them would allocate a
//     distinct value with no observable difference (see lisp/singleton.go).
//
// Strings, ints and floats are immutable values in this implementation and
// travel with the struct copy regardless.
//
// Internal aliasing is preserved exactly as detach preserves it: a value
// reachable along two paths is copied once, and a cycle becomes the same
// cycle in the copy rather than infinite recursion.
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
