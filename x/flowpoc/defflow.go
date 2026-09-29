// Copyright © 2026 The ELPS authors

// Package flowpoc is a proof of concept (substrate#536): a Go-implemented
// defflow macro that compiles a straight-line flow into per-label clause
// lambdas plus saved-variable lists, entirely in Go, once, at the defflow
// call.  Storage is stubbed: the emitted code calls flow:goto, flow:vars,
// flow:var, flow:done and flow:machine, which the embedder defines.
//
// Supported subset: (defflow NAME (params...) [opts] body...) with let,
// let*, progn, if, when, while/set!, (await-input 'L ...),
// (await-reply 'L REQ ...), (done ...).
package flowpoc

import (
	"fmt"
	"sort"

	"github.com/luthersystems/elps/lisp"
)

// cont builds the code that continues with the value expression v (nil when
// the value is discarded).
type cont func(v *lisp.LVal) *lisp.LVal

type compiler struct {
	env     *lisp.LEnv
	name    string
	joins   []*lisp.LVal // (defun NAME--jN ...) forms
	clauses []clause
	labels  map[string]bool
	err     *lisp.LVal
}

type clause struct {
	label string
	saved []string
	body  *lisp.LVal // (lambda ($result $vars) ...)
}

func sym(s string) *lisp.LVal             { return lisp.Symbol(s) }
func list(cells ...*lisp.LVal) *lisp.LVal { return lisp.SExpr(cells) }

// quoted builds 'v.  There is no exported "quote this" constructor for an
// arbitrary form that the evaluator treats as (quote v); lisp.Quote is it.
func quoted(v *lisp.LVal) *lisp.LVal { return lisp.Quote(v) }

func headName(form *lisp.LVal) string {
	if form.Type != lisp.LSExpr || form.IsQuoted() || len(form.Cells) == 0 {
		return ""
	}
	if h := form.Cells[0]; h.Type == lisp.LSymbol {
		return h.Str
	}
	return ""
}

func (c *compiler) fail(form *lisp.LVal, format string, args ...any) *lisp.LVal {
	if c.err == nil {
		c.err = c.env.Errorf(format, args...)
		// FRICTION: no API to attach form's source location to an error
		// built outside eval; env.Errorf uses env.loc (the defflow call).
		_ = form
	}
	return lisp.Nil()
}

// owned reports whether form contains an await or done in any position.
// PoC: a plain tree scan, not a code walk (quoted data would count).
func owned(form *lisp.LVal) bool {
	switch headName(form) {
	case "await-input", "await-reply", "done":
		return true
	}
	// FRICTION: a let binding [x (await ...)] reads as a QUOTED list, the
	// same value as '(x (await ...)), so a scan cannot tell a binding from
	// data without knowing the enclosing form's shape.
	if form.Type == lisp.LSExpr && headName(form) != "quote" {
		for _, x := range form.Cells {
			if owned(x) {
				return true
			}
		}
	}
	return false
}

func labelOf(v *lisp.LVal) (string, bool) {
	if v.Type == lisp.LSymbol {
		return v.Str, true
	}
	if headName(v) == "quote" && len(v.Cells) == 2 && v.Cells[1].Type == lisp.LSymbol {
		return v.Cells[1].Str, true
	}
	return "", false
}

// freeVars returns the names in scope that form references freely, using
// the #747 code walker without expansion (an unknown macro's arguments are
// treated as a call's, which over-approximates: saves more, never less).
func freeVars(form *lisp.LVal, scope []string) []string {
	in := map[string]bool{}
	for _, s := range scope {
		in[s] = true
	}
	seen := map[string]bool{}
	w := &lisp.CodeWalker{KeepGoing: true, Visit: func(n *lisp.WalkNode) bool {
		if (n.Event == lisp.WalkRef || n.Event == lisp.WalkSet) && !n.Bound && in[n.Node.Str] {
			seen[n.Node.Str] = true
		}
		return true
	}}
	w.Walk(form)
	out := make([]string, 0, len(seen))
	for s := range seen {
		out = append(out, s)
	}
	sort.Strings(out)
	return out
}

func extend(scope []string, names ...string) []string {
	s := make([]string, 0, len(scope)+len(names))
	s = append(s, scope...)
	return append(s, names...)
}

func seq(v, rest *lisp.LVal) *lisp.LVal {
	if v == nil {
		return rest
	}
	return list(sym("progn"), v, rest)
}

func (c *compiler) compileSeq(forms []*lisp.LVal, scope []string, k cont) *lisp.LVal {
	if len(forms) == 0 {
		return k(nil)
	}
	if len(forms) == 1 {
		return c.compile(forms[0], scope, k)
	}
	return c.compile(forms[0], scope, func(v *lisp.LVal) *lisp.LVal {
		return seq(v, c.compileSeq(forms[1:], scope, k))
	})
}

// materialize turns k into a global join function over its live locals so
// that two branches (or a loop and later clauses) can share it.
func (c *compiler) materialize(k cont, scope []string) cont {
	jv := "$jv"
	body := k(sym(jv))
	params := freeVars(body, scope)
	name := fmt.Sprintf("%s--j%d", c.name, len(c.joins)+1)
	formals := []*lisp.LVal{sym(jv)}
	for _, p := range params {
		formals = append(formals, sym(p))
	}
	c.joins = append(c.joins, list(sym("defun"), sym(name), list(formals...), body))
	return func(v *lisp.LVal) *lisp.LVal {
		if v == nil {
			v = lisp.Nil()
		}
		call := []*lisp.LVal{sym(name), v}
		for _, p := range params {
			call = append(call, sym(p))
		}
		return list(call...)
	}
}

func (c *compiler) compile(form *lisp.LVal, scope []string, k cont) *lisp.LVal {
	if c.err != nil {
		return lisp.Nil()
	}
	if !owned(form) {
		return k(form)
	}
	switch op := headName(form); op {
	case "progn":
		return c.compileSeq(form.Cells[1:], scope, k)
	case "let", "let*":
		if len(form.Cells) < 2 || form.Cells[1].Type != lisp.LSExpr {
			return c.fail(form, "defflow: malformed %s", op)
		}
		// PoC: let is treated as let* (sequential).  Parallel let with an
		// await in a later init would need temporaries.
		return c.compileBindings(form.Cells[1].Cells, form.Cells[2:], scope, k)
	case "when":
		return c.compile(list(sym("if"), form.Cells[1], list(append([]*lisp.LVal{sym("progn")}, form.Cells[2:]...)...), lisp.Nil()), scope, k)
	case "if":
		if len(form.Cells) < 3 || len(form.Cells) > 4 {
			return c.fail(form, "defflow: malformed if")
		}
		if owned(form.Cells[1]) {
			return c.fail(form, "defflow: await in an if test is not supported")
		}
		k2 := c.materialize(k, scope)
		els := lisp.Nil()
		if len(form.Cells) == 4 {
			els = form.Cells[3]
		}
		return list(sym("if"), form.Cells[1], c.compile(form.Cells[2], scope, k2), c.compile(els, scope, k2))
	case "while":
		return c.compileWhile(form, scope, k)
	case "await-input", "await-reply":
		return c.compileAwait(form, op, scope, k)
	case "done":
		return list(append([]*lisp.LVal{sym("flow:done")}, form.Cells[1:]...)...)
	default:
		return c.fail(form, "defflow: %s may not contain an await or done; awaits must be directly in the flow body", op)
	}
}

func (c *compiler) compileBindings(binds, body []*lisp.LVal, scope []string, k cont) *lisp.LVal {
	if len(binds) == 0 {
		return c.compileSeq(body, scope, k)
	}
	b := binds[0]
	if b.Type != lisp.LSExpr || len(b.Cells) != 2 || b.Cells[0].Type != lisp.LSymbol {
		return c.fail(b, "defflow: malformed binding")
	}
	name := b.Cells[0].Str
	return c.compile(b.Cells[1], scope, func(v *lisp.LVal) *lisp.LVal {
		if v == nil {
			v = lisp.Nil()
		}
		inner := extend(scope, name)
		return list(sym("let"), list(list(sym(name), v)), c.compileBindings(binds[1:], body, inner, k))
	})
}

func (c *compiler) compileWhile(form *lisp.LVal, scope []string, k cont) *lisp.LVal {
	test, body := form.Cells[1], form.Cells[2:]
	if owned(test) {
		return c.fail(form, "defflow: await in a while test is not supported")
	}
	after := c.materialize(k, scope)
	afterCall := after(nil)
	// Loop parameters: locals the loop or its continuation reads or sets.
	params := freeVars(list(append([]*lisp.LVal{sym("progn"), test, afterCall}, body...)...), scope)
	name := fmt.Sprintf("%s--j%d", c.name, len(c.joins)+1)
	c.joins = append(c.joins, nil) // reserve the slot; the body refers to itself
	slot := len(c.joins) - 1
	call := func() *lisp.LVal {
		cells := []*lisp.LVal{sym(name)}
		for _, p := range params {
			cells = append(cells, sym(p))
		}
		return list(cells...)
	}
	loopBody := c.compileSeq(body, params, func(v *lisp.LVal) *lisp.LVal { return seq(v, call()) })
	formals := make([]*lisp.LVal, len(params))
	for i, p := range params {
		formals[i] = sym(p)
	}
	c.joins[slot] = list(sym("defun"), sym(name), list(formals...),
		list(sym("if"), test, loopBody, afterCall))
	return call()
}

func (c *compiler) compileAwait(form *lisp.LVal, op string, scope []string, k cont) *lisp.LVal {
	if len(form.Cells) < 2 {
		return c.fail(form, "defflow: %s needs a label", op)
	}
	label, ok := labelOf(form.Cells[1])
	if !ok {
		return c.fail(form, "defflow: %s label must be a quoted symbol", op)
	}
	if c.labels[label] {
		return c.fail(form, "defflow: duplicate await label %s", label)
	}
	c.labels[label] = true
	rest := form.Cells[2:]
	for _, a := range rest {
		if owned(a) {
			return c.fail(form, "defflow: await arguments may not await")
		}
	}
	body := k(sym("$result"))
	saved := freeVars(body, scope)
	binds := make([]*lisp.LVal, len(saved))
	pairs := []*lisp.LVal{sym("flow:vars")}
	for i, s := range saved {
		binds[i] = list(sym(s), list(sym("flow:var"), sym("$vars"), lisp.String(s)))
		pairs = append(pairs, lisp.String(s), sym(s))
	}
	c.clauses = append(c.clauses, clause{
		label: label,
		saved: saved,
		body: list(sym("lambda"), list(sym("$result"), sym("$vars")),
			list(sym("let"), list(binds...), body)),
	})
	kind := "input"
	var req *lisp.LVal = lisp.Nil()
	opts := rest
	if op == "await-reply" {
		kind = "reply"
		if len(rest) == 0 {
			return c.fail(form, "defflow: await-reply needs a request")
		}
		req, opts = rest[0], rest[1:]
	}
	return list(sym("flow:goto"), lisp.String(label), lisp.String(kind), req,
		list(append([]*lisp.LVal{sym("list")}, opts...)...), list(pairs...))
}

// Compile compiles (defflow NAME (params...) [opts-list] body...) given as
// the macro's argument list and returns the expansion.
func Compile(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, []clause) {
	if len(args.Cells) < 3 || args.Cells[0].Type != lisp.LSymbol || args.Cells[1].Type != lisp.LSExpr {
		return env.Errorf("defflow: expected (defflow NAME (params...) body...)"), nil
	}
	c := &compiler{env: env, name: args.Cells[0].Str, labels: map[string]bool{}}
	var params []string
	for _, p := range args.Cells[1].Cells {
		params = append(params, p.Str)
	}
	body := args.Cells[2:]
	var opts = lisp.Nil()
	if len(body) > 1 && body[0].Type == lisp.LSExpr && len(body[0].Cells) > 0 && body[0].Cells[0].Type == lisp.LSymbol && body[0].Cells[0].Str[0] == ':' {
		opts, body = body[0], body[1:]
	}
	start := c.compileSeq(body, params, func(v *lisp.LVal) *lisp.LVal {
		return list(sym("flow:done"), sym(":outcome"), lisp.String("fell-off-end"))
	})
	if c.err != nil {
		return c.err, nil
	}
	clauseMap := []*lisp.LVal{sym("sorted-map"), lisp.String("start"),
		list(sym("lambda"), args.Cells[1], start)}
	savedMap := []*lisp.LVal{sym("sorted-map")}
	for _, cl := range c.clauses {
		clauseMap = append(clauseMap, lisp.String(cl.label), cl.body)
		names := make([]*lisp.LVal, len(cl.saved))
		for i, s := range cl.saved {
			names[i] = lisp.String(s)
		}
		savedMap = append(savedMap, lisp.String(cl.label), lisp.QExpr(names))
	}
	out := []*lisp.LVal{sym("progn")}
	out = append(out, c.joins...)
	out = append(out, list(sym("set"), quoted(sym(c.name)),
		list(sym("flow:machine"), lisp.String(c.name), quoted(opts), list(clauseMap...), list(savedMap...))))
	return list(out...), c.clauses
}

// Macro is the Go macro body for defflow.  It compiles once per call (a
// top-level form is evaluated once per load) and pre-expands every macro
// in the generated code so that clause lambdas and join functions contain
// no macro calls, which elps would otherwise re-expand on every call.
func Macro(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	out, _ := Compile(env, args)
	if out.Type == lisp.LError {
		return out
	}
	return env.MacroExpandAll(out)
}
