package walkfixture

import l "github.com/luthersystems/elps/lisp"

func engine(v *l.LVal) { // want "value walker github.com/luthersystems/elps/internal/walkfixture.engine"
	if v.Type == l.LSExpr {
		engine(v.Cells[0])
	}
}

// marked is inside elps, where only valueWalkerFunctions records an audit.
//
//elpsvet:allow-valwalker fixture walks only cons cells safely
func marked(v *l.LVal) { // want "value walker github.com/luthersystems/elps/internal/walkfixture.marked"
	if v.Type == l.LSExpr {
		marked(v.Cells[0])
	}
}
