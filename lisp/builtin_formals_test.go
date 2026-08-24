// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"strings"
)

// builtinFormalsSnapshot is a rendering of every builtin's formal argument
// list, and the third process-wide guard TestMain installs.
//
// WHY IT IS NEEDED.  The builtin tables (langBuiltins, userBuiltins) are
// package-level vars whose Formals() lists are constructed exactly once, when
// this package is initialized, and sealed there.  AddBuiltins aliases the
// sealed f.Formals() straight into LFun.Cells[0] with no copy
// (registrationFormals, env.go), so ONE formals list is shared by every LEnv
// the process will ever create -- including every environment created after a
// write to it.  A test that walks a value, reaches a function through it and
// appends a cell to the list it finds in Cells[0] has therefore edited the
// standard library for the remainder of the process, and nothing in that test
// is looking.
//
// That is not hypothetical: FuzzCyclicValueWalks did exactly this from its
// own seed corpus, and every test in the package passed while it happened
// (issue #398).  The singleton snapshot above cannot see it -- the singletons
// are Nil and the two Bools, not the builtin table -- and main has no
// sealed-AST verifier, so before this snapshot there was no second observer
// at all.
//
// The rendering, rather than the pointers, is what is compared: an append
// that fits in the existing slice capacity leaves the *LVal identical and the
// backing array edited in place, which a pointer comparison would miss.
type builtinFormalsSnapshot struct {
	rendered string
}

func takeBuiltinFormalsSnapshot() builtinFormalsSnapshot {
	return builtinFormalsSnapshot{rendered: renderBuiltinFormals()}
}

func renderBuiltinFormals() string {
	var b strings.Builder
	for _, def := range DefaultBuiltins() {
		fmt.Fprintf(&b, "%s %s\n", def.Name(), def.Formals().String())
	}
	return b.String()
}

// verify reports the builtins whose formals differ from the snapshot, or ""
// when the table is intact.
func (s builtinFormalsSnapshot) verify() string {
	now := renderBuiltinFormals()
	if now == s.rendered {
		return ""
	}
	was, is := strings.Split(s.rendered, "\n"), strings.Split(now, "\n")
	var out strings.Builder
	for i := range max(len(was), len(is)) {
		var wl, il string
		if i < len(was) {
			wl = was[i]
		}
		if i < len(is) {
			il = is[i]
		}
		if wl != il {
			fmt.Fprintf(&out, "  - was: %s\n    now: %s\n", wl, il)
		}
	}
	return out.String()
}
