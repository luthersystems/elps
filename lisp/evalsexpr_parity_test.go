// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// TestEvalSExprMatchesEval pins that eval's LSExpr arm, which has
// evalSExpr's code written out to save a Go frame per call form, still
// agrees with evalSExpr, which EvalSExpr reaches.  Each form is a call
// form that is not a macro call (EvalSExpr returns a macro's expansion
// mark, which eval then evaluates).  Both paths run each form twice, so
// the second run takes a reused call list where the call allows one, and
// again with a profiler set, so a regular function goes through
// funCallTraced.  The test compares results only: it does not see call-list
// reuse or a debugger hook.
func TestEvalSExprMatchesEval(t *testing.T) {
	const program = `
(defun add (a b) (+ a b))
(defun rest-list (&rest xs) xs)
(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1)))))
(defun loop-to (n acc) (if (= n 0) acc (loop-to (- n 1) (+ acc 1))))
(defun fails () (error 'my-condition "failed here"))
`
	forms := []string{
		`(add 1 2)`,
		`(rest-list 1 2 3)`,
		`(fact 10)`,
		`(loop-to 50 0)`,
		`(if true 'yes 'no)`,
		`(let ([x 1]) (+ x 1))`,
		`(progn 1 2 3)`,
		`(map 'list (lambda (x) (add x 1)) '(1 2 3))`,
		`(fails)`,
		`(add 1)`,
		`(no-such-function 1)`,
		`(1 2 3)`,
		`(add (fails) 2)`,
		`()`,
	}
	newEnv := func(t *testing.T, profiled bool) *lisp.LEnv {
		t.Helper()
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
		if rc := lisp.InitializeUserEnv(env); rc.IsError() {
			t.Fatal(rc)
		}
		if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.IsError() {
			t.Fatal(rc)
		}
		if rc := env.LoadString("parity.lisp", program); rc.IsError() {
			t.Fatal(rc)
		}
		if profiled {
			env.Runtime.Profiler = countingProfiler{calls: new(int)}
		}
		return env
	}
	// An error's condition and data, without its location: eval sets the
	// location from the form before it calls, and EvalSExpr does not.
	render := func(v *lisp.LVal) string {
		if v.IsError() {
			return v.Str + ": " + lisp.SExpr(v.Cells).String()
		}
		return v.String()
	}
	for _, profiled := range []bool{false, true} {
		for _, src := range forms {
			name := src
			if profiled {
				name = "profiled " + src
			}
			t.Run(name, func(t *testing.T) {
				checkEvalSExprParity(t, newEnv(t, profiled), newEnv(t, profiled), src, render)
			})
		}
	}
}

// countingProfiler is a Profiler that counts the calls it sees.
type countingProfiler struct{ calls *int }

func (p countingProfiler) Start(*lisp.LVal) func() {
	*p.calls++
	return func() {}
}

// checkEvalSExprParity runs src twice through viaEval.Eval and
// viaSExpr.EvalSExpr and fails if the rendered results differ.
func checkEvalSExprParity(t *testing.T, viaEval, viaSExpr *lisp.LEnv, src string, render func(*lisp.LVal) string) {
	t.Helper()
	for i := range 2 {
		a, err := viaEval.Runtime.Reader.Read("form", strings.NewReader(src))
		if err != nil {
			t.Fatal(err)
		}
		b, err := viaSExpr.Runtime.Reader.Read("form", strings.NewReader(src))
		if err != nil {
			t.Fatal(err)
		}
		got, want := render(viaEval.Eval(a[0])), render(viaSExpr.EvalSExpr(b[0]))
		if got != want {
			t.Errorf("run %d: Eval gave %s, EvalSExpr gave %s", i, got, want)
		}
	}
}
