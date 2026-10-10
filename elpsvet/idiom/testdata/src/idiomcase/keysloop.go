package idiomcase

import "github.com/luthersystems/elps/lisp"

func keysLoop(env *lisp.LEnv, m *lisp.LVal) *lisp.LVal {
	keys := env.CallBuiltin(coreKeys, m) // want `env.MapRange\(m, fn\) walks the entries with the checks of keys`
	if keys.IsError() {
		return keys
	}
	for _, k := range keys.Cells {
		_ = k
	}
	return nil
}

func keysResultLoop(env *lisp.LEnv, m *lisp.LVal) error {
	keys, err := lisp.Result(env.CallBuiltin(coreKeys, m)) // want `env.MapRange\(m, fn\)`
	if err != nil {
		return err
	}
	for range keys.Cells {
	}
	return nil
}

// The list is returned, not walked: no hint.
func keysReturned(env *lisp.LEnv, m *lisp.LVal) *lisp.LVal {
	keys := env.CallBuiltin(coreKeys, m)
	return keys
}
