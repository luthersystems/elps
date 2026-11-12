// Fixtures for the location stores spelled through the untyped source word
// (luthersystems/elps#947): locWord in a composite literal, a direct
// unsafe.Pointer conversion, and the unexported setters.  Each must taint
// exactly as the typed field write it replaced.

package lisp

import (
	"unsafe"

	"github.com/luthersystems/elps/parser/token"
)

func locWord(loc *token.Location) unsafe.Pointer { return unsafe.Pointer(loc) }

func (v *LVal) setPlainLoc(loc *token.Location) { v.source = loc }

func (v *LVal) setLoc(loc *token.Location) { v.source = loc }

func (v *LVal) setNodeAux(loc *token.Location, _, _ any) { v.source = loc }

func (env *LEnv) wordLiteral() *LVal {
	return &LVal{
		word: locWord(env.loc), // want `LVal composite literal field \.word stores a runtime-owned \*token\.Location`
	}
}

func (env *LEnv) wordConversion() *LVal {
	return &LVal{
		word: unsafe.Pointer(env.loc), // want `LVal composite literal field \.word stores a runtime-owned \*token\.Location`
	}
}

func (env *LEnv) wordLiteralCopied() *LVal {
	return &LVal{word: locWord(copyLocation(env.loc))}
}

func (env *LEnv) setters(v *LVal) {
	v.setPlainLoc(env.loc)          // want `setPlainLoc call stores a runtime-owned \*token\.Location`
	v.setLoc(env.loc)               // want `setLoc call stores a runtime-owned \*token\.Location`
	v.setNodeAux(env.loc, nil, nil) // want `setNodeAux call stores a runtime-owned \*token\.Location`
	v.setPlainLoc(copyLocation(env.loc))
	v.setLoc(env.loc) //elps:aliases fixture justification for this line only
}
