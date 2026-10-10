package idiomcase

import "github.com/luthersystems/elps/lisp"

var coreSortedMap = lisp.BuiltinFunc("sorted-map")

type status string

func mapOf(env *lisp.LEnv, id, desc *lisp.LVal, typ string, n int) {
	_ = env.MapOf("id", id, "type", typ, "n", n, "f", 1.5, "ok", true, "nil", nil, "b", []byte("x"), "l", lisp.Cells{id})
	_ = env.MapOf(id, 1)
	_ = env.MapOf(3, id)              // want `MapOf key of type int panics at run time`
	_ = env.MapOf("s", status("x"))   // want `MapOf value of type idiomcase.status panics at run time`
	_ = env.MapOf("s", []string{"x"}) // want `MapOf value of type \[\]string panics at run time`
	_ = env.MapOf("i", int64(1))      // want `MapOf value of type int64 panics at run time`
}

func sortedMap(env *lisp.LEnv, id, desc *lisp.LVal) *lisp.LVal {
	return env.CallBuiltin(coreSortedMap, // want `use env.MapOf with Go string keys`
		lisp.String("id"), id,
		lisp.String("description"), desc)
}

// A key that is not a literal string is not rewritten.
func sortedMapComputed(env *lisp.LEnv, k, v *lisp.LVal) *lisp.LVal {
	return env.CallBuiltin(coreSortedMap, k, v)
}
