// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
	"maps"
)

// MaxConditionDepth is the most parents a condition can have above it,
// counting its parent, its parent's parent and so on
// (luthersystems/elps#831).  DefineCondition refuses a definition that makes
// a chain longer.  handler-bind walks the chain for each clause without
// charging a step, so the bound keeps that walk small.
const MaxConditionDepth = 64

// CondCatchAll is the type specifier handler-bind and condition-is? treat
// as the root of every condition.  It is not a condition elps raises, and it
// cannot take part in a definition.
const CondCatchAll = "condition"

// conditionEntry is one condition's place in a runtime's defined
// hierarchy: its defined parent ("" when it has none), how many ancestors it
// has, and how many conditions name it as their parent.
type conditionEntry struct {
	parent   string
	depth    int
	children int
}

// DefineCondition makes parent the parent of condition child in this
// runtime (luthersystems/elps#831), so a handler-bind clause for parent, or
// for any ancestor of parent, catches child.  Lisp code calls it through
// define-condition.
//
// A condition has at most one parent, and gets it before its children:
// define a hierarchy from the root down.  Defining the parent a condition
// already has does nothing.  DefineCondition returns an error, and changes
// nothing, when:
//
//   - child or parent is empty, or child equals parent;
//   - child or parent is condition (the catch-all root) or internal-panic;
//   - child already has a different parent, built in (argument-error) or
//     defined;
//   - child already has children (error has argument-error), which also
//     rules out a cycle;
//   - child would have more than MaxConditionDepth ancestors.
//
// The table belongs to the runtime.  A template publishes a copy of it, and
// every VM forked from the template reads that copy until its first
// definition copies it.  A definition in one VM is seen by no other VM, by
// the template, or by the source.  A definition never changes how an error
// renders: an error whose condition has a defined parent renders with its
// condition name, as before.
func (rt *Runtime) DefineCondition(child, parent string) error {
	switch {
	case child == "" || parent == "":
		return errors.New("condition name is empty")
	case child == parent:
		return fmt.Errorf("%s cannot be its own parent", child)
	}
	for _, name := range [...]string{child, parent} {
		switch name {
		case CondCatchAll:
			return errors.New("condition is the root of every condition and cannot be defined")
		case CondInternalPanic:
			return errors.New("internal-panic cannot be defined")
		}
	}
	if old := rt.ConditionParent(child); old != "" {
		if old == parent {
			return nil
		}
		return fmt.Errorf("%s already has parent %s", child, old)
	}
	// A condition with no children is a leaf, so giving it a parent moves no
	// other condition: no chain but child's can grow, and parent cannot be
	// child's descendant.
	if rt.conditionHasChildren(child) {
		return fmt.Errorf("%s already has children; define a condition's parent before its children", child)
	}
	depth := rt.conditionDepth(parent) + 1
	if depth > MaxConditionDepth {
		return fmt.Errorf("%s would have more than %d ancestors", child, MaxConditionDepth)
	}
	if rt.conditionParentsShared {
		rt.conditionParents = maps.Clone(rt.conditionParents)
		rt.conditionParentsShared = false
	}
	if rt.conditionParents == nil {
		rt.conditionParents = make(map[string]conditionEntry, 2)
	}
	rt.conditionParents[child] = conditionEntry{parent: parent, depth: depth}
	p := rt.conditionParents[parent]
	p.children++
	rt.conditionParents[parent] = p
	return nil
}

// conditionHasChildren reports whether some condition has c as its parent,
// built in or defined.
func (rt *Runtime) conditionHasChildren(c string) bool {
	return conditionParent(CondArgumentError) == c || rt.conditionParents[c].children > 0
}

// conditionDepth returns how many ancestors condition c has.
func (rt *Runtime) conditionDepth(c string) int {
	if p := conditionParent(c); p != "" {
		return 1 + rt.conditionDepth(p)
	}
	return rt.conditionParents[c].depth
}

// ConditionParent returns the parent of condition c in this runtime: its
// built-in parent (argument-error's is error), else the one DefineCondition
// gave it, else "".
func (rt *Runtime) ConditionParent(c string) string {
	if p := conditionParent(c); p != "" {
		return p
	}
	if rt == nil {
		return ""
	}
	return rt.conditionParents[c].parent
}

// ConditionIsA reports whether condition c is ancestor or a descendant of
// it in this runtime's hierarchy: elps's built-in parents and the ones
// DefineCondition added.  It is the runtime's errors.Is.  Like the package
// function ConditionIsA, it treats the catch-all condition as a name, not as
// the root: ConditionIsA(c, "condition") is false unless c is "condition".
func (rt *Runtime) ConditionIsA(c, ancestor string) bool {
	return rt.conditionDistance(c, ancestor) >= 0
}

// conditionDistance returns how many parent links separate condition c from
// ancestor: 0 when they are equal, 1 for c's parent and so on, or -1 when
// ancestor is not c or one of its ancestors.  DefineCondition bounds the
// walk at MaxConditionDepth links.
func (rt *Runtime) conditionDistance(c, ancestor string) int {
	for d := 0; c != ""; d++ {
		if c == ancestor {
			return d
		}
		c = rt.ConditionParent(c)
	}
	return -1
}
