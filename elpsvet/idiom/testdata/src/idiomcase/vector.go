package idiomcase

import "github.com/luthersystems/elps/lisp"

func vector(a, b *lisp.LVal, cells []*lisp.LVal) []*lisp.LVal {
	return []*lisp.LVal{
		lisp.Vector([]*lisp.LVal{a, b}), // want `use lisp.Cells\{...\}.Vector\(\), which builds the same vector`
		lisp.Array(nil, cells),          // want `use lisp.Vector\(cells\), which is Array\(nil, ...\)`
		lisp.Vector(cells),
		lisp.Array(a, cells),
	}
}
