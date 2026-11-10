// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

// callListsReused is false under elpscheck: put poisons each returned list
// and keeps none, so every call form allocates its list and an allocation
// pin counts one more than in the default build.
const callListsReused = false

// callListPoison fills the cells of a returned call list.
//
//elpsvet:allow a marker the checked build stores in dead storage; nothing writes it
var callListPoison = &LVal{Type: LInvalid, Str: "released call list"}

// poisonCallList is the elpscheck form of returning a call list: the list is
// never reused.  Its header becomes an invalid value and each cell holds
// callListPoison, so code that kept the list or its cells past the call
// reads an invalid value instead of the next call's arguments.  It panics
// when v was returned before.  full is the whole backing array of v's cells.
func poisonCallList(v *LVal, full []*LVal) {
	if full[0] == callListPoison {
		panic("lisp: a call list was returned twice")
	}
	for i := range full {
		full[i] = callListPoison
	}
	*v = LVal{Type: LInvalid, Str: "released call list"} //elps:mutates a dead call list, which evalSExprCells built for one call
}
