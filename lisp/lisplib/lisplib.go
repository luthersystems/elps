// Copyright © 2018 The ELPS authors

// Package lisplib is used to conveniently load the standard library for the
// elps environment
package lisplib

import (
	"bytes"
	"fmt"

	"github.com/luthersystems/elps/internal/stdlib"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// LoadLibrary loads the standard library into env and returns env to the
// default user package. It includes the mutable testing registry, so an env
// loaded this way cannot be published as a template. Use LoadRuntimeLibrary
// before publication and load libtesting separately in each VM if needed.
func LoadLibrary(env *lisp.LEnv) *lisp.LVal {
	return stdlib.Load(env, true)
}

// LoadRuntimeLibrary loads the standard library except the mutable testing
// registry, then selects the default user package. It is the library set for
// template-based embedders. NewTemplate still requires an explicit builtin
// sharing policy; this loader does not audit host callbacks or native values
// created later by application initialization.
func LoadRuntimeLibrary(env *lisp.LEnv) *lisp.LVal {
	return stdlib.Load(env, false)
}

// NewDocEnv creates a standard ELPS environment with the stdlib loaded,
// suitable for documentation queries. Embedders can extend this env with
// their own packages, or create their own env and use the libhelp.Render*
// functions and libhelp.CheckMissing directly.
func NewDocEnv() (*lisp.LEnv, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	env.Runtime.Stderr = &bytes.Buffer{}
	rc := lisp.InitializeUserEnv(env)
	if !rc.IsNil() {
		return nil, fmt.Errorf("initialize-user-env returned non-nil: %v", rc)
	}
	rc = LoadLibrary(env)
	if !rc.IsNil() {
		return nil, fmt.Errorf("load-library returned non-nil: %v", rc)
	}
	return env, nil
}
