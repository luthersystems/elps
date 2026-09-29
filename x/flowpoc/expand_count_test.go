package flowpoc

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

var goExpansions int

func newEnv(t testing.TB) *lisp.LEnv { return newEnvLib(t, true) }

func newEnvLib(t testing.TB, stdlib bool) *lisp.LEnv {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if !stdlib {
	} else if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	// A Go macro that counts its expansions: (go-inc x) => (+ x 1)
	env.AddMacros(true, elpsutil.Function("go-inc", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		goExpansions++
		return lisp.SExpr([]*lisp.LVal{lisp.Symbol("+"), args.Cells[0], lisp.Int(1)})
	}))
	return env
}

func load(t testing.TB, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("t.lisp", src)
	if v.Type == lisp.LError {
		t.Fatal(v)
	}
	return v
}

// TestExpansionCount: a defun whose body calls a macro, called N times.
func TestExpansionCount(t *testing.T) {
	const N = 10000
	env := newEnv(t)
	goExpansions = 0
	load(t, env, `
(set 'lisp-expansions 0)
(defmacro lisp-inc (x) (set! lisp-expansions (+ lisp-expansions 1)) (quasiquote (+ (unquote x) 1)))
(defun f (n) (lisp-inc n))
(defun g (n) (go-inc n))
(dotimes (i 10000) (f i) (g i))`)
	lc := load(t, env, `lisp-expansions`).Int
	t.Logf("calls=%d  lisp-macro expansions=%d  go-macro expansions=%d", N, lc, goExpansions)
}

func BenchmarkMacroInDefun(b *testing.B) {
	env := newEnv(b)
	load(b, env, `
(defmacro lisp-inc (x) (quasiquote (+ (unquote x) 1)))
(defun f (n) (lisp-inc n))
(defun g (n) (go-inc n))
(defun h (n) (+ n 1))`)
	for _, name := range []string{"h", "f", "g"} {
		b.Run(name, func(b *testing.B) {
			fn := name
			for i := 0; i < b.N; i++ {
				v := env.CallGlobal(fn, lisp.Int(i))
				if v.Type == lisp.LError {
					b.Fatal(v)
				}
			}
		})
	}
}
