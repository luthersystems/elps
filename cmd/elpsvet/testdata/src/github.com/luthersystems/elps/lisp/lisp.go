// Package lisp is a minimal stub of github.com/luthersystems/elps/lisp for
// analysistest.  Only the shapes the elpsfreshness and elpsescape analyzers
// inspect matter: the LVal struct fields, the constructor names, the method
// names, and the *token.Location-typed runtime state (LVal.source,
// LEnv.Loc) plus the copyLocation cleanser.
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

type CallStack struct{}

func (s *CallStack) Copy() *CallStack { return &CallStack{} }

type Runtime struct {
	Stack *CallStack
}

type LEnv struct {
	Loc     *token.Location
	Runtime *Runtime
}

func copyLocation(loc *token.Location) *token.Location {
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

func (v *LVal) Detach() (*LVal, error) { return v.Copy(), nil }

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
