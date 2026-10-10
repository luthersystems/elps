package idiomcase

import "github.com/luthersystems/elps/lisp"

var docFun = lisp.FunInPackage("p", "doc", lisp.Formals(), plainBuiltin)

func plainBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return args }

func init() {
	docFun.Cells[1] = lisp.String("pkg doc") // want `lisp.FunInPackageDoc\(pkg, fid, formals, fn, doc\) sets the docstring when it builds docFun`
}

func funDoc(fn lisp.LBuiltin, doc string) *lisp.LVal {
	f := lisp.FunInPackage("p", "f", lisp.Formals("x"), fn)
	f.Cells[1] = lisp.String(doc) // want `lisp.FunInPackageDoc`
	return f
}

func funDocStr(fn lisp.LBuiltin, doc string) *lisp.LVal {
	f := lisp.FunInPackage("p", "f", lisp.Formals("x"), fn)
	f.Cells[1].Str = doc // want `lisp.FunInPackageDoc`
	return f
}

// The value may come from elsewhere: no hint.
func funDocMixed(fn lisp.LBuiltin, other *lisp.LVal, doc string) *lisp.LVal {
	f := lisp.FunInPackage("p", "f", lisp.Formals("x"), fn)
	if other != nil {
		f = other
	}
	f.Cells[1] = lisp.String(doc)
	return f
}

// Another cell is not the docstring.
func funFormals(fn lisp.LBuiltin) *lisp.LVal {
	f := lisp.FunInPackage("p", "f", lisp.Formals("x"), fn)
	f.Cells[0] = lisp.Formals("y")
	return f
}

// A plain list is not a function value.
func listDoc(doc string) {
	l := lisp.Nil()
	l.Cells[1] = lisp.String(doc)
}
