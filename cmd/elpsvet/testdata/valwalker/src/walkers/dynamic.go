package walkers

import (
	l "github.com/luthersystems/elps/lisp"
	"reflect"
	"unsafe"
)

func invokeReflect(f func(*l.LVal), v *l.LVal) {
	reflect.ValueOf(f).Call([]reflect.Value{reflect.ValueOf(v)})
}
func invokeUnsafe(f func(*l.LVal), v *l.LVal) {
	addr := unsafe.Pointer(&f)
	(*(*func(*l.LVal))(addr))(v)
}
