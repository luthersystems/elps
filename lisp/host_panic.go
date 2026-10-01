// Copyright © 2026 The ELPS authors

package lisp

import (
	"io"
	"reflect"
	"runtime"
	"strconv"
)

// recoverPanic must be deferred directly, so recover observes the caller's
// panic. It does not begin an evaluation or charge an evaluation step.
func (env *LEnv) recoverPanic(result **LVal) {
	if p := recover(); p != nil {
		*result = env.panicError(p)
	}
}

func (env *LEnv) panicError(p any) *LVal {
	rethrowOwnershipViolation(p)
	// Recovery must not re-enter a debugger that may itself have panicked
	// while holding a lock. Ordinary errors still notify the debugger.
	lerr := env.newErrorConditionf(CondInternalPanic,
		"internal error (recovered panic): %s", panicDescription(p))
	buf := make([]byte, 16*1024)
	n := runtime.Stack(buf, false)
	lerr.CallStack().GoStack = buf[:n]
	return lerr
}

// panicDescription does not call application formatting methods. A panic
// payload may refer to the very locked or damaged object that caused the
// fault; invoking its String/Error/Format method could deadlock recovery.
func panicDescription(p any) string {
	if p == nil {
		return "<nil>"
	}
	v := reflect.ValueOf(p)
	t := v.Type()
	runtimeType := t
	if runtimeType.Kind() == reflect.Pointer {
		runtimeType = runtimeType.Elem()
	}
	if runtimeType.PkgPath() == "runtime" {
		if err, ok := p.(runtime.Error); ok {
			return err.Error()
		}
	}
	switch v.Kind() {
	case reflect.String:
		return v.String()
	case reflect.Bool:
		return strconv.FormatBool(v.Bool())
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return strconv.FormatInt(v.Int(), 10)
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return strconv.FormatUint(v.Uint(), 10)
	case reflect.Float32, reflect.Float64:
		return strconv.FormatFloat(v.Float(), 'g', -1, t.Bits())
	case reflect.Complex64, reflect.Complex128:
		return strconv.FormatComplex(v.Complex(), 'g', -1, t.Bits())
	default:
		return "<panic value of type " + t.String() + ">"
	}
}

// hostReadPanic crosses a Reader's Go error return without losing the panic
// marker to the ordinary parser-error wrapping performed by Load methods.
type hostReadPanic struct{ value *LVal }

func (p *hostReadPanic) Error() string { return p.value.String() }

func (env *LEnv) recoverReadPanic(err *error) {
	if p := recover(); p != nil {
		*err = &hostReadPanic{value: env.panicError(p)}
	}
}

func (env *LEnv) readSource(reader Reader, name string, r io.Reader) ([]*LVal, error) {
	var exprs []*LVal
	var err error
	func() {
		defer env.recoverReadPanic(&err)
		exprs, err = reader.Read(name, r)
	}()
	return exprs, err
}

func (env *LEnv) readSourceLocation(reader LocationReader, name, loc string, r io.Reader) ([]*LVal, error) {
	var exprs []*LVal
	var err error
	func() {
		defer env.recoverReadPanic(&err)
		exprs, err = reader.ReadLocation(name, loc, r)
	}()
	return exprs, err
}

func (env *LEnv) readSourceBytes(r io.Reader) ([]byte, error) {
	var src []byte
	var err error
	func() {
		defer env.recoverReadPanic(&err)
		src, err = io.ReadAll(r)
	}()
	return src, err
}

func (env *LEnv) sourceReadError(err error) *LVal {
	// This private wrapper is passed directly from our recovery boundary.
	// errors.As would invoke arbitrary host Unwrap/As methods outside it.
	if p, ok := err.(*hostReadPanic); ok { //nolint:errorlint // exact internal wrapper; do not invoke host error traversal hooks
		return p.value
	}
	return env.Error(err)
}

func (env *LEnv) readLibrarySource(ctx SourceContext, loc string) (string, string, []byte, *LVal) {
	var name string
	var path string
	var src []byte
	var lerr *LVal
	func() {
		defer env.recoverPanic(&lerr)
		var err error
		name, path, src, err = env.Runtime.Library.LoadSource(ctx, loc)
		if err != nil {
			lerr = env.Errorf("library error: %v", err)
		}
	}()
	return name, path, src, lerr
}
