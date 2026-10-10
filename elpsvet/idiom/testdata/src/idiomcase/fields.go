package idiomcase

import "github.com/luthersystems/elps/lisp"

// A hand-written map guard followed by a map read gets the MapView hint.
func mapGuard(desc *lisp.LVal) int {
	if desc.Type != lisp.LSortMap { // want `mv, ok := lisp.AsMap\(desc\) makes this check once and returns a lisp.MapView`
		return 0
	}
	return len(desc.MapKeys().Cells)
}

// A guard with no later map read is not reported.
func mapGuardKept(desc *lisp.LVal) bool {
	if desc.Type != lisp.LSortMap {
		return false
	}
	return true
}

// A read of another value is not reported.
func mapGuardOther(desc, other *lisp.LVal) int {
	if desc.Type != lisp.LSortMap {
		return 0
	}
	return len(other.MapKeys().Cells)
}
