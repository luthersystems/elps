// Package lisp is a minimal stub of github.com/luthersystems/elps/lisp for
// analysistest.  Only the shapes the elpsfreshness and elpsescape analyzers
// inspect matter: the LVal struct fields, the constructor names, the method
// names, and the *token.Location-typed runtime state (LVal.source,
// LEnv.loc) plus the copyLocation cleanser.
//
// LEnv.loc is UNEXPORTED here because it is unexported in the real package
// after the #382 privatization.  That is load-bearing for the fixtures: the
// in-package retro-catch shapes (retro.go) read it directly, exactly as the
// kernel does, while an external package can no longer reach an env's
// location field at all -- which is why the cross-package fixture (esc/)
// takes its taint from the struct fields it does own.
package lisp

import "github.com/luthersystems/elps/parser/token"

type LType int

const LError LType = 1

type LFunType int

type LBuiltin func(env *LEnv, args *LVal) *LVal

type LVal struct {
	Native  interface{}
	source  *token.Location
	Str     string
	Cells   []*LVal
	Type    LType
	Int     int
	Float   float64
	FunType LFunType
	Quoted  bool
	Spliced bool
}

func (v *LVal) SetSource(loc *token.Location) {
	v.source = loc
}

// Source mirrors the real (*LVal).Source: the location comes back BY VALUE
// alongside a boolean, so nothing the caller holds can alias v's storage.
// It needs no freshLocation fact — the fact summarises single
// *token.Location results, and a value result is unaliasable by
// construction.
func (v *LVal) Source() (token.Location, bool) {
	if v == nil || v.source == nil {
		return token.Location{}, false
	}
	return *v.source, true
}

// ErrorVal mirrors lisp.ErrorVal, whose Source delegates to (*LVal).Source
// and is value-returning for the same reason.
type ErrorVal LVal

func (e *ErrorVal) Source() (token.Location, bool) { return (*LVal)(e).Source() }

type CallStack struct{}

func (s *CallStack) Copy() *CallStack { return &CallStack{} }

type Runtime struct {
	Stack *CallStack
}

type LEnv struct {
	loc     *token.Location
	Runtime *Runtime
}

// Source mirrors the real accessor: the exported read of the env's location
// hands out a COPY, because the evaluator rebinds and mutates its own
// register (#382).  It is the SANCTIONED way out of an environment, and the
// freshLocation fact must prove it — through copyLocation, one call deep —
// so no caller in any package has to annotate.
func (env *LEnv) Source() *token.Location { // want Source:"freshLocation"
	if env == nil {
		return nil
	}
	return copyLocation(env.loc)
}

// LocRef is Source's counterexample: the same signature, the same
// not-constructed-here receiver, but it hands out the env's OWN pointer.
// No fact, so the call site stays flagged.  Source and LocRef differ only
// in what the callee does, which is exactly what the fact is for.
func (env *LEnv) LocRef() *token.Location {
	return env.loc
}

// LocMaybe proves the summary is all-or-nothing: one leaking return sinks
// the whole function, even though the other return is a fresh literal.
func (env *LEnv) LocMaybe() *token.Location {
	if env.loc == nil {
		return &token.Location{File: "<native code>"}
	}
	return env.loc
}

// LocDeref mirrors the parser's deref-copy idiom as an accessor: the local
// is this call's own struct, so its address is fresh.
func (env *LEnv) LocDeref() *token.Location { // want LocDeref:"freshLocation"
	if env.loc == nil {
		return nil
	}
	cp := *env.loc
	return &cp
}

func copyLocation(loc *token.Location) *token.Location { // want copyLocation:"freshLocation"
	if loc == nil {
		return nil
	}
	cp := *loc
	return &cp
}

var singletonNil = &LVal{}
var singletonTrue = &LVal{Str: "true"}
var singletonFalse = &LVal{Str: "false"}

func Bool(b bool) *LVal {
	if b {
		return singletonTrue
	}
	return singletonFalse
}

func Nil() *LVal { return singletonNil }

func Int(x int) *LVal        { return &LVal{Int: x} }
func Float(x float64) *LVal  { return &LVal{Float: x} }
func String(s string) *LVal  { return &LVal{Str: s} }
func Symbol(s string) *LVal  { return &LVal{Str: s} }
func QSymbol(s string) *LVal { return &LVal{Str: s} }
func Bytes(b []byte) *LVal   { return &LVal{Native: &b} }

func Native(v interface{}) *LVal { return &LVal{Native: v} }

func SExpr(cells []*LVal) *LVal { return &LVal{Cells: cells} }
func QExpr(cells []*LVal) *LVal { return &LVal{Cells: cells, Quoted: true} }

func Array(dims *LVal, cells []*LVal) *LVal { return &LVal{Cells: cells} }

func Fun(fid string, formals *LVal, fn LBuiltin) *LVal { return &LVal{Str: fid} }

func Formals(argSymbols ...string) *LVal { return &LVal{} }

func Errorf(format string, v ...interface{}) *LVal { return &LVal{Str: format} }

func Quote(v *LVal) *LVal {
	cp := &LVal{}
	*cp = *v
	cp.Quoted = true
	return cp
}

func (v *LVal) Copy() *LVal {
	cp := &LVal{}
	*cp = *v
	return cp
}

func (v *LVal) detach() (*LVal, error) { return v.Copy(), nil }

func (v *LVal) Bytes() []byte {
	b, _ := v.Native.(*[]byte)
	if b == nil {
		return nil
	}
	return *b
}

func (env *LEnv) Errorf(format string, v ...interface{}) *LVal {
	return &LVal{Str: format}
}
