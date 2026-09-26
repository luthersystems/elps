// Copyright © 2026 The ELPS authors

package lisp

// Sharing bombs: why some walkers switch to an identity memo part way.
//
// A value is a graph, not a tree.  (set! x (list x x)), repeated D times,
// costs D evaluation steps and D two-cell lists, yet the value it builds has
// 2^D paths from its root.  A walker that treats it as a tree -- recursing
// into every cell of every container it reaches, with no memory of the
// containers it has already finished -- does 2^D work inside one builtin
// step.  At D=40 that is hours of work, and none of it is charged as a step,
// polls the context or counts toward MaxAlloc, so neither --timeout nor
// --max-steps can stop it; where the walk also allocates per path, D≈30 is
// enough to exhaust the host's memory.  For an embedder running untrusted
// transactions that is a limit bypass: one transaction wedges or kills the
// node.
//
// lisp/cycle.go's guard does not help.  It bounds a walk's DEPTH (and the
// work spent unrolling a real cycle), not depth times width: the D-level
// sharing chain above has no cycle and is only D deep.
//
// The walkers fixed against this class keep their original tree walk -- so
// every ordinary value takes exactly the path, the allocations and the
// result it always did -- and count the work they do: one per container
// entered plus one per cell it holds.  Cells count because a wide container
// reached again costs its width every time; counting containers alone would
// let (list w w ...) over a million-cell w do budget x million work before
// the memo switched on.  Past sharedWalkBudget they begin to memoise each
// container they finish, by identity, and a container reached again is
// answered from the memo instead of being walked again.  The walk is then
// linear in the number of DISTINCT containers (and their cells), which is
// what the program paid to build.
//
// On a tree the memo is never hit, so it changes nothing observable: the
// same result, the same errors, the same evaluation steps.  On a DAG larger
// than the budget, a memo hit returns the one result built for the shared
// container, so the output SHARES that subtree where the tree walk would
// have built one copy per path: the output has the same sharing as the
// input.  That is a deliberate behaviour change, and a narrow one: results
// are equal? to what the tree walk built, the value depth limit fails where
// it did, and steps are unchanged except for the duplicated-rebuild charge
// below.  What can see it is identity -- a Go
// embedder comparing pointers, or a debugger, whose per-node expansion IDs
// follow the sharing (a shared container gets one ID, not one per path).
// Work a memo cannot remove -- quasiquote re-evaluating an unquote under a
// shared list -- is charged in evaluation steps, but only once the
// DUPLICATED work passes its own allowance; see findAndUnquote.
//
// sharedWalkBudget is chosen well above the size of any expansion or
// template a program writes by hand, so the memo stays off -- and allocates
// nothing -- on the hot paths, and well below the point where the tree walk
// costs anything measurable.  Nothing depends on the exact number.
const sharedWalkBudget = 4096

// sharedMemoGrain is the least work a finished container's own walk must
// have cost for a memoising walker to record it.  A container too cheap to
// record costs less than the grain each time it is reached again, so the
// walk stays linear in the containers it does record (times the grain),
// while a large TREE -- which never reaches a container twice -- does not
// pay a map insert for every small container in it.
const sharedMemoGrain = 64
