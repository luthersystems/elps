// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"reflect"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Issue #629: the live runtime never records a Go trace. Accepting host-created
// diagnostic storage would change error handling when NewVM resets that state.
func TestTemplateRejectsLiveDiagnosticStorage(t *testing.T) {
	for _, tc := range []struct {
		name  string
		trace []byte
	}{
		{"empty-zero-capacity", make([]byte, 0)},
		{"hidden-zero-capacity", []byte("hidden")[:0:0]},
		{"empty-spare-capacity", make([]byte, 0, 8)},
		{"nonempty", []byte("trace")},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			env.Runtime.Package = env.Runtime.Registry.DefinePackage("user")
			env.Runtime.Stack.GoStack = tc.trace
			wantBacking := string(tc.trace[:cap(tc.trace)])
			approvals := 0
			tmpl, err := lisp.NewTemplate(env,
				lisp.TemplateWithNativePolicy(func(any) bool { approvals++; return true }),
				lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { approvals++; return true }))
			if tmpl != nil || err == nil || !strings.Contains(err.Error(), "retained diagnostic stack: live runtime GoStack must be nil") {
				t.Fatalf("live diagnostics admitted or wrong reason: template-present=%t error=%v", tmpl != nil, err)
			}
			if approvals != 0 {
				t.Fatal("live diagnostics reached a sharing policy")
			}
			got := env.Runtime.Stack.GoStack
			if !reflect.DeepEqual(got, tc.trace) || cap(got) != cap(tc.trace) || reflect.ValueOf(got).Pointer() != reflect.ValueOf(tc.trace).Pointer() || string(got[:cap(got)]) != wantBacking {
				t.Fatal("rejection changed the source diagnostic storage")
			}
		})
	}
}

// A *[]byte is normally a supported payload, but this particular header is a
// reference into the live diagnostics even when its slice is initially nil.
func TestTemplateRejectsLiveDiagnosticByteHeaderAliases(t *testing.T) {
	for typ := lisp.LInvalid + 1; typ < lisp.LMarkTerminal; typ++ {
		for _, sealed := range []bool{false, true} {
			name := typ.String() + "/mutable"
			if sealed {
				name = typ.String() + "/sealed-parent"
			}
			t.Run(name, func(t *testing.T) {
				env := lisp.NewEnv(nil)
				env.Runtime.Package = env.Runtime.Registry.DefinePackage("user")
				header := &env.Runtime.Stack.GoStack
				value := &lisp.LVal{Type: typ, Native: header}
				root := lisp.QExpr([]*lisp.LVal{value})
				if sealed {
					root.SealAST()
					if !root.IsSealed() {
						t.Fatal("fixture did not seal its root")
					}
				}
				if got := env.PutGlobal(lisp.Symbol("saved"), root); got.Type == lisp.LError {
					t.Fatal(got)
				}
				approvals := 0
				tmpl, err := lisp.NewTemplate(env,
					lisp.TemplateWithNativePolicy(func(any) bool { approvals++; return true }),
					lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { approvals++; return true }))
				if tmpl != nil || err == nil || !strings.Contains(err.Error(), "retained diagnostic stack: byte header aliases live runtime GoStack") {
					t.Fatalf("live diagnostic alias admitted or wrong reason: template-present=%t error=%v", tmpl != nil, err)
				}
				if approvals != 0 {
					t.Fatal("live diagnostic alias reached a sharing policy")
				}
				if env.Runtime.Stack.GoStack != nil || value.Native != header || value.Type != typ || root.Cells[0] != value {
					t.Fatal("rejection changed the source diagnostic alias")
				}
			})
		}
	}
}

func TestTemplateRejectsDiagnosticAliasBeforeLispMutation(t *testing.T) {
	load := func() *lisp.LEnv {
		env := templateTestEnv(t)
		value := lisp.Bytes(nil)
		value.Native = &env.Runtime.Stack.GoStack
		if got := env.PutGlobal(lisp.Symbol("diagnostic-bytes"), value); got.Type == lisp.LError {
			t.Fatal(got)
		}
		return env
	}
	evaluate := func(env *lisp.LEnv) *lisp.LVal {
		if env.Runtime.Debugger != nil {
			t.Fatal("fixture unexpectedly has a debugger")
		}
		if got := env.LoadString("mutation.lisp", `(append-bytes! diagnostic-bytes "!")`); got.Type != lisp.LBytes || string(got.Bytes()) != "!" {
			t.Fatalf("ordinary Lisp byte append failed: %v", got)
		}
		return env.LoadString("request.lisp", `(ignore-errors (error 'internal-panic "ordinary"))`)
	}
	cold, source := load(), load()
	want := evaluate(cold)
	if want.Type != lisp.LError || !lisp.IsInternalPanic(want) {
		t.Fatalf("cold fixture did not expose the diagnostic marker alias: %v", want)
	}
	// Before the fix, publication accepted this still-nil header and detached
	// the alias: the same Lisp request on NewVM returned () instead of an error.
	tmpl, err := lisp.NewTemplate(source, templateCorePolicy())
	if tmpl != nil || err == nil || !strings.Contains(err.Error(), "retained diagnostic stack: byte header aliases live runtime GoStack") {
		t.Fatalf("Lisp-observable diagnostic alias admitted: template-present=%t error=%v", tmpl != nil, err)
	}
	if source.Runtime.Stack.GoStack != nil {
		t.Fatal("rejection modified the source's initially nil diagnostic storage")
	}
	if got := evaluate(source); got.Type != want.Type || got.String() != want.String() {
		t.Fatalf("rejection changed the source behavior: cold=%v source=%v", want, got)
	}
}
