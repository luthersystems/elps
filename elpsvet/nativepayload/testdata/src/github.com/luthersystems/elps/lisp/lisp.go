// Package lisp is a minimal stub of github.com/luthersystems/elps/lisp for
// analysistest: the LVal struct with its Native and Type fields, the
// kernel header constants and slot types, and the Native, NativeOf and
// Value constructors.  Value's type switch mirrors the real one.

package lisp

type LType int

const (
	LNative LType = iota
	LString
	LInt
	LBytes
	LSortMap
)

type LVal struct {
	Native interface{}
	Str    string
	Cells  []*LVal
	Type   LType
	Int    int
}

type MapData struct{ m map[string]*LVal }

type LEnv struct {
	Runtime *Runtime
}

type Runtime struct{}

func Native(v interface{}) *LVal {
	return &LVal{Type: LNative, Native: v}
}

// NativeOf stores x exactly as Native does.
func NativeOf[T any](x T) *LVal {
	return Native(x)
}

func Value(v interface{}) *LVal {
	switch v := v.(type) {
	case bool:
		return Bool(v)
	case string:
		return String(v)
	case []byte:
		return Bytes(v)
	case int:
		return Int(v)
	case float64:
		return Float(v)
	case []*LVal:
		return QExpr(v)
	default:
		return Native(v)
	}
}

func Bool(b bool) *LVal     { return &LVal{} }
func String(s string) *LVal { return &LVal{Type: LString, Str: s} }
func Bytes(b []byte) *LVal  { return &LVal{Type: LBytes, Native: &b} }
func Int(x int) *LVal       { return &LVal{Type: LInt, Int: x} }
func Float(x float64) *LVal { return &LVal{} }
func QExpr(c []*LVal) *LVal { return &LVal{Cells: c} }
func Nil() *LVal            { return &LVal{} }
func SortedMap() *LVal      { return &LVal{Type: LSortMap, Native: &MapData{}} }
