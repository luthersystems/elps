package a

import "github.com/luthersystems/elps/lisp"

var echoText = lisp.Func1E(func(env *lisp.LEnv, in lisp.Text) ([]byte, error) {
	return in, nil // want `Func1E body returns the storage of argument in`
})

var echoSlice = lisp.Func1E(func(env *lisp.LEnv, in lisp.Text) ([]byte, error) {
	return in[1:], nil // want `Func1E body returns the storage of argument in`
})

var echoCells = lisp.Func1E(func(env *lisp.LEnv, v *lisp.LVal) (lisp.Cells, error) {
	return lisp.Cells(v.Cells), nil // want `Func1E body returns the storage of argument v`
})

var echoBytes = lisp.Func2E(bytesOf)

func bytesOf(env *lisp.LEnv, v *lisp.LVal, n int) ([]byte, error) {
	if n > 0 {
		return v.Bytes(), nil // want `Func2E body returns the storage of argument v`
	}
	return nil, nil
}

// Fresh results are fine.
var fresh = lisp.Func1E(func(env *lisp.LEnv, in lisp.Text) ([]byte, error) {
	out := make([]byte, len(in))
	copy(out, in)
	return out, nil
})

var fromString = lisp.Func1E(func(env *lisp.LEnv, s string) ([]byte, error) {
	return []byte(s), nil
})

var notStorage = lisp.Func1E(func(env *lisp.LEnv, s string) (string, error) {
	return s, nil
})

var allowed = lisp.Func1E(func(env *lisp.LEnv, in lisp.Text) ([]byte, error) {
	return in, nil //elps:aliases the caller owns a private copy of the bytes
})
