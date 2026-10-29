// Copyright © 2026 The ELPS authors

// Package progload runs the forms a development tool evaluates one at a
// time, such as the REPL and the MCP eval and test tools, as the program's
// load (luthersystems/elps#908).  A person who types or sends forms is
// loading their program, so s:defmethod and the other load-time forms work
// there.  The rule lives here so that every such tool follows it the same
// way.  An embedder's transactions must not run through it.
package progload

import "github.com/luthersystems/elps/lisp"

// Run calls eval as the program's load of env (lisp.Runtime.BeginLoad) and
// ends the load when eval returns, also on a panic.  Call it from the
// outer entry point of a tool, on the goroutine that evaluates the forms
// and around those forms only: the load depth is unsynchronized runtime
// state, so a load another goroutine runs on env at the same time races
// with it.
func Run(env *lisp.LEnv, eval func()) {
	defer env.Runtime.BeginLoad()()
	eval()
}
