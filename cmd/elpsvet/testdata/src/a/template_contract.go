package a

import "github.com/luthersystems/elps/lisp"

// Template sharing has the same ownership boundary as Program. These controls
// pin the concrete writes forbidden by the public Template documentation.
func overwriteSharedTemplateHeader(value, replacement *lisp.LVal) {
	*value = *replacement // want `whole-value write through \*lisp\.LVal`
}

func replaceSharedTemplateFormals(function, formals *lisp.LVal) {
	function.Cells[0] = formals // want `write to LVal field \.Cells element`
}

func replaceSharedTemplateBody(function, expression *lisp.LVal) {
	body := function.Cells[1:]
	body[0] = expression // want `index write through a local slice alias of lisp\.LVal backing storage`
}
