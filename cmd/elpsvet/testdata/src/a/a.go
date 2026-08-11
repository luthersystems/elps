// Package a exercises the elpsfreshness analyzer.
package a

import (
	"github.com/luthersystems/elps/lisp"
)

func someCall() *lisp.LVal { return &lisp.LVal{} }

// issue333 reconstructs the #333 corruption: unquoting the result of a call
// the writer does not own.
func issue333() *lisp.LVal {
	s := someCall()
	s.Quoted = false // want `write to LVal field \.Quoted on a lisp\.LVal this function did not construct`
	return s
}

// issue334 reconstructs the #334 shape: writing a field on a parameter.
func issue334(v *lisp.LVal) {
	v.Cells = nil // want `write to LVal field \.Cells on a lisp\.LVal this function did not construct`
}

func paramElementWrite(v, x *lisp.LVal) {
	v.Cells[0] = x           // want `write to LVal field \.Cells element on a lisp\.LVal this function did not construct`
	v.Cells[1].Quoted = true // want `write to LVal field \.Quoted on a lisp\.LVal this function did not construct`
}

func paramBytesWrite(v *lisp.LVal) {
	v.Bytes()[0] = 1 // want `write to LVal \.Bytes\(\) backing array on a lisp\.LVal this function did not construct`
}

func paramIncDec(v *lisp.LVal) {
	v.Int++ // want `write to LVal field \.Int on a lisp\.LVal this function did not construct`
}

func paramWholeWrite(v *lisp.LVal) {
	cp := &lisp.LVal{}
	*cp = *v // ok: cp is fresh
	*v = *cp // want `whole-value write through \*lisp\.LVal on a lisp\.LVal this function did not construct`
}

// freshComposite writes to values constructed in this function: no flags.
func freshComposite() *lisp.LVal {
	v := &lisp.LVal{}
	v.Str = "x"
	w := lisp.SExpr(nil)
	w.Cells = append(w.Cells, v)
	w.Cells[0] = v
	u := new(lisp.LVal)
	u.Int = 1
	u.Int++
	return w
}

// freshChain roots a deep write at a fresh constructor result.
func freshChain() *lisp.LVal {
	v := lisp.QExpr([]*lisp.LVal{lisp.Symbol("a")})
	v.Cells[0].Quoted = true
	return v
}

// copyDetachPattern is the Copy/Detach idiom: a struct copy through a fresh
// pointer stays fresh.
func copyDetachPattern(v *lisp.LVal) *lisp.LVal {
	cp := &lisp.LVal{}
	*cp = *v
	cp.Quoted = true
	c := v.Copy()
	c.Str = "y"
	d, _ := v.Detach()
	d.Cells = nil
	return cp
}

// valueCopy writes to a value-typed local: Go value semantics make the
// top-level struct this function's own.
func valueCopy(v *lisp.LVal) lisp.LVal {
	v2 := *v
	v2.Quoted = false
	return v2
}

// tracking follows := and = of tracked values, and reassignment kills
// freshness.
func tracking(other *lisp.LVal) {
	v := lisp.SExpr(nil)
	w := v
	w.Cells = nil // ok: w tracks fresh v
	v = other
	v.Str = "x" // want `write to LVal field \.Str on a lisp\.LVal this function did not construct`
}

// singletons: Nil and Bool return shared singletons, NOT fresh values.
func singletons() {
	n := lisp.Nil()
	n.Cells = append(n.Cells, lisp.Int(1)) // want `write to LVal field \.Cells on a lisp\.LVal this function did not construct`
	b := lisp.Bool(true)
	b.Str = "false" // want `write to LVal field \.Str on a lisp\.LVal this function did not construct`
}

// envError results are freshly allocated error values.
func envError(env *lisp.LEnv) *lisp.LVal {
	lerr := env.Errorf("boom")
	lerr.Cells = append(lerr.Cells, lisp.String("detail"))
	return lerr
}

//elps:mutates — this function deliberately rewrites caller-owned values.
func annotatedFunc(v *lisp.LVal) {
	v.Quoted = false
	v.Str = "rewritten"
}

func annotatedLine(v *lisp.LVal) {
	v.Quoted = false //elps:mutates deliberate in-place unquote
	//elps:mutates deliberate rewrite on the next line
	v.Str = "rewritten"
	v.Int = 1 // want `write to LVal field \.Int on a lisp\.LVal this function did not construct`
}

type holder struct {
	node *lisp.LVal
}

// structField writes through a field of an untracked struct: unknown root.
func structField(h *holder) {
	h.node.Str = "x" // want `write to LVal field \.Str on a lisp\.LVal this function did not construct`
}
