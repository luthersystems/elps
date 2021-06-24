package lint

import (
	"embed"
	"fmt"

	"github.com/luthersystems/elps/lisp"
)

//go:embed *.lisp
var files embed.FS

func loadFile(env *lisp.LEnv, path string) *lisp.LVal {
	f, err := files.Open(path)
	if err != nil {
		return lisp.Error(fmt.Errorf("failed to open file: %w", err))
	}
	lerr := env.Load(path, f)
	if lerr.Type == lisp.LError {
		return lerr
	}
	return lisp.Nil()
}

func Load(env *lisp.LEnv) *lisp.LVal {
	return loadFile(env, "lint.lisp")
}
