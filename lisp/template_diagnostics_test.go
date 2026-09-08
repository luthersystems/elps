// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"reflect"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Issue #629: diagnostic stacks are categorically outside the template graph,
// not an opaque native payload that a host's sharing policy can approve.
func TestTemplateRejectsDiagnosticPayloads(t *testing.T) {
	for typ := lisp.LInvalid + 1; typ < lisp.LMarkTerminal; typ++ {
		for _, payload := range []struct {
			name  string
			stack any
		}{
			{"nil", (*lisp.CallStack)(nil)},
			{"empty", &lisp.CallStack{}},
			{"recorded", &lisp.CallStack{Frames: []lisp.CallFrame{{Name: "saved"}}, GoStack: []byte("trace")}},
			{"empty-value", lisp.CallStack{}},
			{"recorded-value", lisp.CallStack{Frames: []lisp.CallFrame{{Name: "saved"}}, GoStack: []byte("trace")}},
		} {
			t.Run(typ.String()+"/"+payload.name, func(t *testing.T) {
				env := lisp.NewEnv(nil)
				env.Runtime.Package = env.Runtime.Registry.DefinePackage("user")
				value := &lisp.LVal{Type: typ, Native: payload.stack}
				if got := env.PutGlobal(lisp.Symbol("saved"), lisp.QExpr([]*lisp.LVal{value})); got.Type == lisp.LError {
					t.Fatal(got)
				}
				approvals := 0
				tmpl, err := lisp.NewTemplate(env,
					lisp.TemplateWithNativePolicy(func(any) bool { approvals++; return true }),
					lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { approvals++; return true }))
				if tmpl != nil || err == nil || !strings.Contains(err.Error(), "retained diagnostic stack") {
					t.Fatalf("diagnostic payload admitted or wrong reason: template-present=%t error=%v", tmpl != nil, err)
				}
				if approvals != 0 {
					t.Fatalf("diagnostic payload reached an approval policy %d times", approvals)
				}
				if value.Type != typ || !reflect.DeepEqual(value.Native, payload.stack) {
					t.Fatal("rejection changed the source header or payload")
				}
				if stack, ok := payload.stack.(*lisp.CallStack); ok && value.Native != stack {
					t.Fatal("rejection changed the source diagnostic identity")
				}
			})
		}
	}
}

func TestTemplateRejectsSavedErrorStackAliases(t *testing.T) {
	for _, sealed := range []bool{false, true} {
		t.Run(map[bool]string{false: "error-and-bytes", true: "sealed-descendant"}[sealed], func(t *testing.T) {
			env := lisp.NewEnv(nil)
			env.Runtime.Package = env.Runtime.Registry.DefinePackage("user")
			trace := []byte("original trace")
			value := lisp.Errorf("saved error")
			value.SetCallStack(&lisp.CallStack{GoStack: trace})
			stack := value.CallStack()
			if sealed {
				// Even a sealed descendant must not conceal a known diagnostic
				// stack attached by a host before sealing the value.
				value = lisp.String("sealed")
				value.Native = stack
				value = lisp.QExpr([]*lisp.LVal{value})
				value.SealAST()
				if !value.IsSealed() || !value.Cells[0].IsSealed() {
					t.Fatal("fixture did not create a sealed descendant")
				}
			}
			for name, entry := range map[string]*lisp.LVal{"saved": value, "trace": lisp.Bytes(trace)} {
				if got := env.PutGlobal(lisp.Symbol(name), entry); got.Type == lisp.LError {
					t.Fatal(got)
				}
			}
			tmpl, err := lisp.NewTemplate(env)
			if tmpl != nil || err == nil || !strings.Contains(err.Error(), "retained diagnostic stack") {
				t.Fatalf("saved diagnostics admitted or wrong reason: template-present=%t error=%v", tmpl != nil, err)
			}
			trace[0] = 'X'
			if string(stack.GoStack) != "Xriginal trace" {
				t.Fatal("failed publication changed the original diagnostic/byte alias")
			}
		})
	}
}

// Ordinary request errors still carry diagnostics without a debugger. Only
// retaining such an error in a graph being published is unsupported (#629).
func TestTemplateRequestErrorsWithoutDebugger(t *testing.T) {
	load := func() *lisp.LEnv {
		env := templateTestEnv(t)
		got := env.LoadString("definitions.lisp", `(defun descend (n)
			(if (= n 0) (error 'failed "bottom") (+ 1 (descend (- n 1)))))`)
		if got.Type == lisp.LError {
			t.Fatal(got)
		}
		return env
	}
	source := load()
	if source.Runtime.Stack.GoStack != nil {
		t.Fatal("normal initialization populated the live runtime GoStack")
	}
	tmpl, err := lisp.NewTemplate(source, templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	cold := load()
	want := cold.LoadString("request.lisp", "(descend 4)")
	if want.Type != lisp.LError || want.Str != "failed" || len(want.Cells) != 1 || want.Cells[0].Str != "bottom" || want.CallStack() == nil || len(want.CallStack().Frames) == 0 {
		t.Fatalf("cold request did not produce a recorded error: %v", want)
	}
	var previous *lisp.CallStack
	for range 2 {
		vm, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		if vm.Runtime.Debugger != nil {
			t.Fatal("request fixture unexpectedly has a debugger")
		}
		got := vm.LoadString("request.lisp", "(descend 4)")
		if got.Type != lisp.LError || got.String() != want.String() {
			t.Fatalf("request error differs: cold=%v template=%v", want, got)
		}
		stack := got.CallStack()
		if stack == nil || stack == want.CallStack() || stack == previous || !reflect.DeepEqual(stack, want.CallStack()) {
			t.Fatal("request diagnostics missing, shared, or changed")
		}
		previous = stack
		if result := vm.LoadString("next-request.lisp", "(+ 20 22)"); result.Type != lisp.LInt || result.Int != 42 {
			t.Fatalf("VM did not recover after request error: %v", result)
		}
		if vm.Runtime.Stack.GoStack != nil {
			t.Fatal("ordinary request error populated the live runtime GoStack")
		}
		if again, err := lisp.NewTemplate(vm, templateCorePolicy()); again == nil || err != nil {
			t.Fatalf("unretained request errors must not prevent later publication: %v", err)
		}
		if result := vm.PutGlobal(lisp.Symbol("saved-error"), got); result.Type == lisp.LError {
			t.Fatal(result)
		}
		if saved, err := lisp.NewTemplate(vm, templateCorePolicy()); saved != nil || err == nil || !strings.Contains(err.Error(), "retained diagnostic stack") {
			t.Fatalf("retained ordinary error admitted: template-present=%t error=%v", saved != nil, err)
		}
	}
}
