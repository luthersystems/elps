package idiomcase

import "github.com/luthersystems/elps/lisp"

func keysLoop(env *lisp.LEnv, m *lisp.LVal) *lisp.LVal {
	keys, err := env.CallBuiltin(coreKeys, m) // want `env.MapRange\(m, fn\) walks the entries with the checks of keys`
	if err != nil {
		return lisp.Error(err)
	}
	for _, k := range keys.Cells {
		_ = k
	}
	return nil
}

// The list is returned, not walked: no hint.
func keysReturned(env *lisp.LEnv, m *lisp.LVal) *lisp.LVal {
	keys, _ := env.CallBuiltin(coreKeys, m)
	return keys
}
