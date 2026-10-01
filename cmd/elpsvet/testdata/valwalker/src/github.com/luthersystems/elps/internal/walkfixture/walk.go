package walkfixture

import l "github.com/luthersystems/elps/lisp"

func engine(v *l.LVal) { // want "value walker github.com/luthersystems/elps/internal/walkfixture.engine"
	if v.Type == l.LSExpr {
		engine(v.Cells[0])
	}
}
