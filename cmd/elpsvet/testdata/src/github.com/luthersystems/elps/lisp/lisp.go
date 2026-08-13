// Package lisp is a minimal stub of github.com/luthersystems/elps/lisp for
// analysistest.  Only the shapes the elpsfreshness analyzer inspects matter:
// the LVal struct fields, the constructor names, and the method names.
package lisp

type LType int

// The type tags elpsseal fixtures branch on.
const (
	LSExpr LType = iota
	LArray
	LBytes
)

type LFunType int

type LBuiltin func(env *LEnv, args *LVal) *LVal

type LVal struct {
	Native  interface{}
	Str     string
	Cells   []*LVal
	Type    LType
	Int     int
	Float   float64
	FunType LFunType
	Quoted  bool
	Spliced bool

	// sealed mirrors the real field: unexported, so only package lisp can
	// propagate it.  External packages get IsSealed and a copy.
	sealed bool
}

func (v *LVal) IsSealed() bool { return v != nil && v.sealed }

type LEnv struct{}

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
func Bytes(b []byte) *LVal   { return &LVal{Native: &b} } // want Bytes:"borrowsLValBacking\\(0\\)"

func Native(v interface{}) *LVal { return &LVal{Native: v} }

func SExpr(cells []*LVal) *LVal { return &LVal{Cells: cells} }               // want SExpr:"borrowsLValBacking\\(0\\)"
func QExpr(cells []*LVal) *LVal { return &LVal{Cells: cells, Quoted: true} } // want QExpr:"borrowsLValBacking\\(0\\)"

func Vector(cells []*LVal) *LVal { return Array(nil, cells) } // want Vector:"borrowsLValBacking\\(0\\)"

func Array(dims *LVal, cells []*LVal) *LVal { return &LVal{Cells: cells} } // want Array:"borrowsLValBacking\\(1\\)"

// seqCells mirrors the real in-package helper: it returns a sequence's live
// backing array.
func seqCells(v *LVal) []*LVal { return v.Cells }

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
