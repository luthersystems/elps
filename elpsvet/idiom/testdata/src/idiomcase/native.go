package idiomcase

import "github.com/luthersystems/elps/lisp"

type handle struct{ n int }

func nativeIf(v *lisp.LVal) *handle {
	if v.Type == lisp.LNative {
		h, _ := v.Native.(*handle) // want `lisp.NativeValue\[\*handle\]\(v\) tests v.Type == lisp.LNative and the payload's type in one call`
		return h
	}
	return nil
}

func nativeEarlyReturn(env *lisp.LEnv, v *lisp.LVal) (*handle, *lisp.LVal) {
	if v.Type != lisp.LNative {
		return nil, env.Errorf("not a handle")
	}
	h, ok := v.Native.(*handle) // want `lisp.NativeValue\[\*handle\]\(v\)`
	if !ok {
		return nil, env.Errorf("not a handle")
	}
	return h, nil
}

func nativeOrReturn(v *lisp.LVal, ok bool) *handle {
	if !ok || v.Type != lisp.LNative {
		return nil
	}
	h, _ := v.Native.(*handle) // want `lisp.NativeValue`
	return h
}

func nativeAfter(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
	h, ok := v.Native.(*handle) // want `lisp.NativeValue\[\*handle\]\(v\)`
	if v.Type != lisp.LNative || !ok {
		return env.Errorf("not a handle")
	}
	_ = h
	return v
}

func nativeCase(v *lisp.LVal) string {
	switch v.Type {
	case lisp.LNative:
		if _, ok := v.Native.(*handle); ok { // want `lisp.NativeValue`
			return "handle"
		}
	}
	return ""
}

func nativeAnd(args *lisp.LVal) bool {
	if len(args.Cells) > 0 && args.Cells[0].Type == lisp.LNative {
		if h, ok := args.Cells[0].Native.(*handle); ok && h != nil { // want `lisp.NativeValue\[\*handle\]\(args.Cells\[0\]\)`
			return true
		}
	}
	return false
}

// Without a type test, NativeValue would add one: no hint.
func nativeUnguarded(v *lisp.LVal) *handle {
	h, _ := v.Native.(*handle)
	return h
}

// A test under || does not select the type.
func nativeOrSelect(v *lisp.LVal, ok bool) *handle {
	if v.Type == lisp.LNative || ok {
		h, _ := v.Native.(*handle)
		return h
	}
	return nil
}

// A test of another value does not guard v.
func nativeOther(v, w *lisp.LVal) *handle {
	if w.Type != lisp.LNative {
		return nil
	}
	h, _ := v.Native.(*handle)
	return h
}

// A test that does not return does not guard what follows.
func nativeNoReturn(v *lisp.LVal) *handle {
	if v.Type != lisp.LNative {
		v = lisp.Nil()
	}
	h, _ := v.Native.(*handle)
	return h
}

// A test outside a closure does not guard the closure when it runs.
func nativeClosure(v *lisp.LVal) func() *handle {
	if v.Type != lisp.LNative {
		return nil
	}
	return func() *handle {
		h, _ := v.Native.(*handle)
		return h
	}
}

// A test after a use of the result does not guard the use.
func nativeUsedFirst(v *lisp.LVal) int {
	if h, ok := v.Native.(*handle); ok {
		return h.n
	}
	if v.Type != lisp.LNative {
		return 0
	}
	return 1
}
