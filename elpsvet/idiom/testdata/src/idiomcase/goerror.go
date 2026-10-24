package idiomcase

import "github.com/luthersystems/elps/lisp"

func goErr(env *lisp.LEnv, v *lisp.LVal) bool {
	if lisp.GoError(v) != nil { // want `use v.IsError\(\), which is the same test`
		return true
	}
	if nil == lisp.GoError(env.Error("x")) { // want `use !env.Error\("x"\).IsError\(\)`
		return false
	}
	return lisp.GoError(v) == nil // want `use !v.IsError\(\)`
}

// The error value itself is used: no rewrite.
func goErrKept(v *lisp.LVal) error {
	if err := lisp.GoError(v); err != nil {
		return err
	}
	return nil
}
