package valwalk

import l "github.com/luthersystems/elps/lisp"

func engine(v *l.LVal) {
	if v.Type == l.LSExpr {
		engine(v.Cells[0])
	}
}
