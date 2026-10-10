package lisp

// The helpers keep their own compares: a rewrite would make them call
// themselves.

// IsError calls errorType, so errorType keeps its compare too.
func (v *LVal) IsError() bool { return v != nil && errorType(v) }

func errorType(v *LVal) bool { return v.Type == LError }

func (env *LEnv) CheckAlloc(n int) *LVal {
	if msg := env.Runtime.CheckAlloc(n); msg != "" {
		return env.Errorf("%s", msg)
	}
	return Nil()
}
