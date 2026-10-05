// Copyright © 2026 The ELPS authors

package codewalk

import (
	"github.com/luthersystems/elps/internal/codewalk/hook"
	"github.com/luthersystems/elps/lisp"
)

// Calls visits executable calls without validating binding grammar.
// It skips quoted data, quasiquote, signatures and binding structure.
// The formals callback optionally receives signature events by value before grammar validation.
// A nil formals callback avoids constructing events. Calls allocates no walk state.
func Calls(exprs []*lisp.LVal, visit func(*lisp.LVal), formals func(Node)) {
	calls := callVisitor{visit: visit, formals: formals}
	for _, expr := range exprs {
		syntax(expr, nil, 0, syntaxVisitors{visit: nil, stop: nil, calls: &calls})
	}
}

type callVisitor struct {
	visit   func(*lisp.LVal)
	formals func(Node)
}

// callFormals holds the formals owner, binding and traversal context.
type callFormals struct {
	// owner is the form that owns the formals.
	owner *lisp.LVal
	// formals contains the argument symbols.
	formals *lisp.LVal
	// binding is the local function binding.
	binding *lisp.LVal
	// op is the canonical operator name.
	op string
	// depth is the traversal depth.
	depth int
	// role is the formals role.
	role FormalsRole
}

func emitCallFormals(visit func(Node), opts callFormals) {
	owner, formals, binding, op, depth, role := opts.owner, opts.formals, opts.binding, opts.op, opts.depth, opts.role

	visit(Node{Event: FormalsOccurrence, Node: formals, Formals: formals,
		Owner: owner, Binding: binding, Op: op, Depth: depth, Role: role})
}

var syntaxCall func(*lisp.LVal) (string, *hook.CallPolicy)

func init() {
	var ok bool
	syntaxCall, ok = hook.SyntaxCall.(func(*lisp.LVal) (string, *hook.CallPolicy))
	if !ok {
		panic("codewalk: lisp did not inject the call policy")
	}
}
