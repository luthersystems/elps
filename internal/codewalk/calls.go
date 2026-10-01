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
		syntax(expr, nil, 0, nil, nil, &calls)
	}
}

type callVisitor struct {
	visit   func(*lisp.LVal)
	formals func(Node)
}

func emitCallFormals(visit func(Node), owner, formals, binding *lisp.LVal, op string, depth int, role FormalsRole) {
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
