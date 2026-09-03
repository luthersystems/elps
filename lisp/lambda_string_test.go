// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// TestLambdaStringRendersFormals pins the text (*LVal).str produces for a
// function value.  Printing used to concatenate the formals with the
// function's own environment scope and unquote the joined list; that scope
// was always empty, so the concatenation was doing nothing but allocating a
// list and creating the write that became issue #333.  The formals now
// render directly, and every expected string below was captured from the
// concatenating implementation -- they are what elps printed before, not
// what this one happens to print.
func TestLambdaStringRendersFormals(t *testing.T) {
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	lambda := func(t *testing.T, formals *LVal, body ...*LVal) *LVal {
		t.Helper()
		fn := env.Lambda(formals, body)
		if fn.Type == LError {
			t.Fatalf("Lambda: %v", fn)
		}
		return fn
	}
	syms := func(names ...string) *LVal {
		cells := make([]*LVal, len(names))
		for i := range names {
			cells[i] = Symbol(names[i])
		}
		return QExpr(cells)
	}

	tests := []struct {
		name string
		fn   func(t *testing.T) *LVal
		want string
	}{{
		name: "no-formals",
		fn:   func(t *testing.T) *LVal { return lambda(t, QExpr(nil), Int(1)) },
		want: "(lambda () 1)",
	}, {
		name: "positional",
		fn:   func(t *testing.T) *LVal { return lambda(t, syms("x", "y"), Symbol("x")) },
		want: "(lambda (x y) x)",
	}, {
		name: "rest",
		fn:   func(t *testing.T) *LVal { return lambda(t, syms("x", "&rest", "r"), Symbol("x")) },
		want: "(lambda (x &rest r) x)",
	}, {
		name: "optional",
		fn:   func(t *testing.T) *LVal { return lambda(t, syms("x", "&optional", "y"), Symbol("x")) },
		want: "(lambda (x &optional y) x)",
	}, {
		name: "key",
		fn:   func(t *testing.T) *LVal { return lambda(t, syms("&key", "k"), Symbol("k")) },
		want: "(lambda (&key k) k)",
	}, {
		name: "multi-form-body",
		fn:   func(t *testing.T) *LVal { return lambda(t, syms("x"), Symbol("x"), Int(2)) },
		want: "(lambda (x) x 2)",
	}, {
		// The quote prefix belongs to the function value, not to its
		// formals: exprString ignores the quoted flag on the list it
		// renders, exactly as the concatenated-and-unquoted list did.
		name: "quoted-function-value",
		fn:   func(t *testing.T) *LVal { return Quote(lambda(t, syms("x"), Symbol("x"))) },
		want: "'(lambda (x) x)",
	}, {
		// Formals() hands back a QExpr, i.e. a quoted list.  It must still
		// print as (a b) and never as '(a b).
		name: "go-built-formals",
		fn: func(t *testing.T) *LVal {
			return &LVal{
				Type:   LFun,
				Native: &funData{env: env, fid: "_funtest0", pkg: DefaultUserPackage},
				Cells:  []*LVal{Formals("a", "b"), Symbol("a")},
			}
		},
		want: "(lambda (a b) a)",
	}, {
		name: "go-built-formals-varargs",
		fn: func(t *testing.T) *LVal {
			return &LVal{
				Type:   LFun,
				Native: &funData{env: env, fid: "_funtest1", pkg: DefaultUserPackage},
				Cells:  []*LVal{Formals("a", VarArgSymbol, "rest"), Symbol("a")},
			}
		},
		want: "(lambda (a &rest rest) a)",
	}}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			fn := test.fn(t)
			if got := fn.String(); got != test.want {
				t.Errorf("String() = %q, want %q", got, test.want)
			}
			// Printing is a pure read: the same value prints the same way
			// twice, and nothing it touched changed underneath it.
			if got := fn.String(); got != test.want {
				t.Errorf("second String() = %q, want %q", got, test.want)
			}
		})
	}
}

// TestBuiltinStringUnchanged pins the other arm of the LFun case, which
// never reached the formals at all.
func TestBuiltinStringUnchanged(t *testing.T) {
	fn := FunInPackage(DefaultUserPackage, "_funtest2", Formals("a"), func(env *LEnv, args *LVal) *LVal { return Nil() })
	if got, want := fn.String(), "#<builtin>"; got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
	if got, want := Quote(fn).String(), "'#<builtin>"; got != want {
		t.Errorf("quoted String() = %q, want %q", got, want)
	}
}
