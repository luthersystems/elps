// Copyright © 2018 The ELPS authors

package debugger

import "github.com/luthersystems/elps/lisp"

// ScopeProvider supplies a custom variable scope visible in the debugger's
// Variables panel (e.g., "State DB" in VS Code). Embedders implement this
// interface and register providers via WithScopeProviders or
// RegisterScopeProvider on the Engine.
//
// The Variables method is called on the DAP server goroutine while the
// interpreter is paused — it must not resume execution or modify engine state.
type ScopeProvider interface {
	// Name returns the scope label displayed in the debugger UI.
	Name() string
	// Expensive indicates whether the scope should be fetched lazily.
	Expensive() bool
	// Variables returns the variables to display for this scope.
	// The env is the paused interpreter environment.
	Variables(env *lisp.LEnv) []ScopeVariable
}

// ScopeVariable represents a single variable in a custom scope. Values are
// strings (not LVal) because custom scopes often expose non-LVal data such
// as raw database entries.
type ScopeVariable struct {
	Name     string          // display name
	Value    string          // display value
	Type     string          // optional type annotation
	Children []ScopeVariable // expandable sub-variables (nil = leaf)
}
