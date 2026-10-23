package idiomcase

import "github.com/luthersystems/elps/lisp"

var (
	coreAssoc        = lisp.BuiltinFunc("assoc!")        // want `lisp has a Go helper for assoc!: call LEnv\.MapPut`
	coreToString     = lisp.BuiltinFunc("to-string")     // want `lisp has a Go helper for to\-string: call LEnv\.ToString`
	coreFormatString = lisp.BuiltinFunc("format-string") // want `lisp has a Go helper for format\-string: call LEnv\.FormatString`
	coreAssocCopy    = lisp.BuiltinFunc("assoc")         // want `lisp has a Go helper for assoc: call LEnv\.MapAssoc`
	coreFirst        = lisp.BuiltinFunc("first")         // want `lisp has a Go helper for first: call LEnv\.First`
	coreSecond       = lisp.BuiltinFunc("second")        // want `lisp has a Go helper for second: call LEnv\.Second`
	coreDissoc       = lisp.BuiltinFunc("dissoc")        // want `lisp has a Go helper for dissoc: call LEnv\.MapDissoc`
	coreDissocM      = lisp.BuiltinFunc("dissoc!")       // want `lisp has a Go helper for dissoc!: call LEnv\.MapDelete`
	coreCar          = lisp.BuiltinFunc("car")           // want `lisp has a Go helper for car: call LEnv\.Car`
	coreCdr          = lisp.BuiltinFunc("cdr")           // want `lisp has a Go helper for cdr: call LEnv\.Cdr`
	coreRest         = lisp.BuiltinFunc("rest")          // want `lisp has a Go helper for rest: call LEnv\.Rest`
	coreLength       = lisp.BuiltinFunc("length")        // the stub LEnv has no Length: no hint
	coreVector       = lisp.BuiltinFunc("vector")        // want `lisp has a Go helper for vector: write lisp\.Cells\{\.\.\.\}\.Vector\(\) \(it makes no context check\)`
	coreIsBool       = lisp.BuiltinFunc("bool?")         // want `lisp has a Go helper for bool\?: write v\.IsSymbol\(lisp\.TrueSymbol\) \|\| v\.IsSymbol\(lisp\.FalseSymbol\) \(it makes no context check\)`
)

func helpers(env *lisp.LEnv, m, k, v *lisp.LVal) {
	_ = env.CallBuiltin(coreAssoc, m, k, v)                  // want `use env.MapPut, which returns the same value or error from the same checks`
	_ = env.CallBuiltin(lisp.BuiltinFunc("assoc!"), m, k, v) // want `use env.MapPut`
	_ = env.CallBuiltin(coreGet, m, k)                       // want `use env.MapLookup`
	_ = env.CallBuiltin(coreFirst, v)                        // want `use env.First`
	_ = env.CallBuiltin(coreSecond, v)                       // want `use env.Second`
	_ = env.CallBuiltin(coreAssocCopy, m, k, v)              // want `use env.MapAssoc`
	_ = env.CallBuiltin(coreDissoc, m, k)                    // want `use env.MapDissoc`
	_ = env.CallBuiltin(coreDissocM, m, k)                   // want `use env.MapDelete`
	_ = env.CallBuiltin(coreCar, v)                          // want `use env.Car`
	_ = env.CallBuiltin(coreCdr, v)                          // want `use env.Cdr`
	_ = env.CallBuiltin(coreRest, v)                         // want `use env.Rest`
}

// Same checks: no idiom drops a check that the CallBuiltin call makes.
func helpersNoFix(env *lisp.LEnv, m, k, v, f *lisp.LVal, args []*lisp.LVal) {
	// An arity error is CallBuiltin's; the helper has fixed arguments.
	_ = env.CallBuiltin(coreAssoc, m, k)
	_ = env.CallBuiltin(coreGet, m, k, v)
	// A format that is not lisp.String(...) may not be a string, and
	// format-string checks that.
	_ = env.CallBuiltin(coreFormatString, f, v)
	// A spread argument list has an unknown count.
	_ = env.CallBuiltin(coreFormatString, args...)
	// ToString and FormatString return (string, error), which does not fit
	// the *LVal place of the call.
	_ = env.CallBuiltin(coreToString, v)
	_ = env.CallBuiltin(coreFormatString, lisp.String("{}: {}"), k, v)
	// A helper with fixed arguments does not fit another count.
	_ = env.CallBuiltin(coreCar, v, v)
}
