package nested

import alias "github.com/luthersystems/elps/lisp"

func f(t alias.LType) {
	switch t { // want "lisp.LType switch misses constants:"
	case alias.LInt:
	}
}
