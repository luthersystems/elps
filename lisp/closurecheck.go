// Copyright © 2026 The ELPS authors

package lisp

import (
	"cmp"
	"fmt"
	"math"
	"slices"
	"strconv"
	"strings"
)

// ClosureIssueKind classifies a ClosureIssue.
type ClosureIssueKind uint8

// The kinds of ClosureIssue.
const (
	// ClosureUnbound is a global the code reads, calls, names with
	// function or assigns with set!, and that no binding defines.
	ClosureUnbound ClosureIssueKind = iota + 1
	// ClosureNotFunction is a call of a global bound to a value that is
	// not a function.
	ClosureNotFunction
	// ClosureArity is a call of a global function or macro with a number
	// of arguments its formals refuse.
	ClosureArity
)

// ClosureIssue is one finding of LEnv.CheckClosure.
type ClosureIssue struct {
	// Name is the symbol as the code spells it.
	Name string
	// Message says what is wrong, for example "calls f with 3
	// argument(s); f takes 1 to 2".
	Message string
	// Args is the number of arguments the call passes, for ClosureArity
	// and ClosureNotFunction, else 0.
	Args int
	// Kind is the kind of finding.
	Kind ClosureIssueKind
}

// CheckClosure checks the code of fn, a function value built by lambda,
// defun or a durable load, against env's current globals.  It reports each
// global the code reads, calls, names with function or assigns with set!
// that is not defined; each call of a global bound to a value that is not a
// function; and each call of a global function or macro whose argument
// count its formals (required, &optional, &rest and &key) refuse.  Thread
// steps count the threaded argument.
//
// A symbol resolves as it would when fn runs: a local the code binds or a
// name fn's captured frames bind (in any frame up to the root environment)
// is a local; any other symbol is a global of fn's package, or of the
// package that qualifies it, in env's registry.  The code is walked as
// macroexpand-all walks it, by the evaluator's own grammar of the builtin
// special forms (lisp.CodeWalker).  A builtin macro (implemented in Go) is
// expanded and its expansion checked.  A macro written in Lisp is not run:
// its call's argument count is checked, and its arguments are not.  The
// arguments of an embedder's special operator, of a local macro, of a
// builtin macro whose expansion fails and of a form nested past the nesting
// limit are not checked either.  It cannot see a name the code computes
// (funcall or apply of a value, eval, a symbol in quoted data).
//
// The findings are sorted by name, kind and argument count, each once.  The
// walk charges one step per list it walks and per macro it expands, as
// macroexpand-all does, so it stops at env's step budget, step limit or
// cancelled context with that error.  It evaluates no Lisp code and changes
// nothing.  It returns an error for a value that is not such a function.
func (env *LEnv) CheckClosure(fn *LVal) ([]ClosureIssue, *LVal) {
	if fn == nil || fn.Type != LFun || fn.Builtin() != nil || fn.IsSpecialFun() || fn.funEnv() == nil || len(fn.Cells) == 0 {
		return nil, env.Errorf("check closure: not a lambda")
	}
	pkg := env.Runtime.Registry.Package(fn.Package())
	if pkg == nil {
		return nil, env.Errorf("check closure: unknown package: %q", fn.Package())
	}
	c := &closureCheck{env: env, pkg: pkg, captured: map[string]bool{}, skip: -1}
	for fr := fn.funEnv(); fr != nil; fr = fr.Parent() {
		for name := range fr.Bindings() {
			c.captured[name] = true
		}
	}
	maxDepth := env.Runtime.MaxEvalNestingDepth()
	if maxDepth == 0 { // the embedder disabled the nesting limit
		maxDepth = math.MaxInt
	}
	w := &CodeWalker{
		SpecialOp:     c.specialOp,
		Expand1:       c.expandBuiltin,
		Visit:         c.visit,
		KeepGoing:     true,
		MaxDepth:      maxDepth,
		MaxExpansions: env.Runtime.MaxMacroExpansions(),
		step:          func() *LVal { return env.checkLimits(env.evalCtx) },
	}
	// Walk (lambda FORMALS BODY...): the walker binds the formals.
	form := make([]*LVal, 0, len(fn.Cells)+1)
	form = append(form, Symbol(env.Runtime.Registry.Lang+":lambda"))
	form = append(form, fn.Cells...)
	if r := w.Walk(SExpr(form)); r.IsError() {
		return nil, r
	}
	// KeepGoing walks a failed expansion as a call, so a limit the
	// expansion crossed is checked here.
	if lerr := env.limitViolation(env.evalCtx); lerr != nil {
		return nil, lerr
	}
	slices.SortFunc(c.issues, func(a, b ClosureIssue) int {
		return cmp.Or(cmp.Compare(a.Name, b.Name), cmp.Compare(a.Kind, b.Kind), cmp.Compare(a.Args, b.Args))
	})
	return slices.CompactFunc(c.issues, func(a, b ClosureIssue) bool {
		return a.Name == b.Name && a.Kind == b.Kind && a.Args == b.Args
	}), nil
}

// closureCheck is the state of one CheckClosure.
type closureCheck struct {
	env      *LEnv
	pkg      *Package
	captured map[string]bool
	// threads holds the enclosing thread-first and thread-last forms, whose
	// step calls take one more argument than they spell.
	threads []walkedForm
	// call is the call form whose head the next WalkRef is, and calls its
	// argument count.
	call   *LVal
	issues []ClosureIssue
	calls  int
	// skip, when nonnegative, ignores the events deeper than it: the
	// arguments of a macro call.
	skip int
}

// walkedForm is a form the walk is inside, at its depth.
type walkedForm struct {
	form  *LVal
	depth int
}

// isThreadStep reports whether the call n is a step of the innermost
// thread form: one of its arguments after the first.
func (c *closureCheck) isThreadStep(n *WalkNode) bool {
	if len(c.threads) == 0 {
		return false
	}
	t := c.threads[len(c.threads)-1]
	return t.depth == n.Depth-1 && len(t.form.Cells) > 2 && slices.Contains(t.form.Cells[2:], n.Node)
}

// global returns the global binding of name, or nil when it has none.
func (c *closureCheck) global(name string) *LVal {
	pkg, sym := c.pkg, name
	if i := strings.IndexByte(name, ':'); i >= 0 {
		pkg, sym = c.env.Runtime.Registry.Package(name[:i]), name[i+1:]
		if pkg == nil || strings.IndexByte(sym, ':') >= 0 {
			return nil
		}
	}
	v := pkg.Get(Symbol(sym))
	if v.IsError() {
		return nil
	}
	return v
}

// local reports whether name is a captured local.  A qualified name never
// is.
func (c *closureCheck) local(name string) bool {
	return strings.IndexByte(name, ':') < 0 && c.captured[name]
}

// expandBuiltin expands a call of a builtin macro (one elps or the
// embedder implements in Go), which evaluates no Lisp code.  A macro
// written in Lisp is not expanded: visit checks its call's arity only.
func (c *closureCheck) expandBuiltin(form *LVal) (*LVal, bool) {
	head := form.Cells[0]
	if c.local(head.Str) {
		return nil, false
	}
	v := c.global(head.Str)
	if v == nil || v.Type != LFun || !v.IsMacro() || v.Builtin() == nil {
		return nil, false
	}
	return c.env.callMacro(v, form)
}

// specialOp classifies a head as the evaluator would in fn's package.
func (c *closureCheck) specialOp(head *LVal) (string, bool) {
	if head == nil || head.Type != LSymbol || c.local(head.Str) {
		return "", false
	}
	v := c.global(head.Str)
	if v == nil {
		return "", false
	}
	return c.env.specialOpOf(v)
}

func (c *closureCheck) find(is ClosureIssue) {
	c.issues = append(c.issues, is)
}

func (c *closureCheck) visit(n *WalkNode) bool {
	if c.skip >= 0 {
		if n.Depth > c.skip {
			return true
		}
		c.skip = -1
	}
	switch n.Event {
	case WalkForm:
		for len(c.threads) > 0 && c.threads[len(c.threads)-1].depth >= n.Depth {
			c.threads = c.threads[:len(c.threads)-1]
		}
		switch k := specialFormKind(n.Op); {
		case n.Op == "":
			c.call, c.calls = n.Node, len(n.Node.Cells)-1
			if c.isThreadStep(n) {
				c.calls++
			}
		case k == kindThreadFirst || k == kindThreadLast:
			c.threads = append(c.threads, walkedForm{form: n.Node, depth: n.Depth})
		}
	case WalkRef:
		if n.Bound || c.local(n.Node.Str) {
			return true
		}
		v := c.global(n.Node.Str)
		if !n.Head || c.call == nil {
			if v == nil {
				c.find(ClosureIssue{Kind: ClosureUnbound, Name: n.Node.Str, Message: "reads " + n.Node.Str + ", which is not defined"})
			}
			return true
		}
		args := c.calls
		c.call = nil
		switch {
		case v == nil:
			c.find(ClosureIssue{Kind: ClosureUnbound, Name: n.Node.Str, Message: "calls " + n.Node.Str + ", which is not defined"})
		case v.Type != LFun:
			c.find(ClosureIssue{Kind: ClosureNotFunction, Name: n.Node.Str, Args: args,
				Message: fmt.Sprintf("calls %s, which is bound to a value of type %v, not a function", n.Node.Str, v.Type)})
		default:
			c.arity(n.Node.Str, v, args)
			if v.IsMacro() {
				// The arguments of a macro are not code this check
				// can read without expanding it.
				c.skip = n.Depth - 1
			}
		}
	case WalkSet:
		if !n.Bound && !c.local(n.Node.Str) && c.global(n.Node.Str) == nil {
			c.find(ClosureIssue{Kind: ClosureUnbound, Name: n.Node.Str, Message: "sets " + n.Node.Str + ", which is not defined"})
		}
	case WalkBind, WalkDefine, WalkLiteral, WalkData, WalkEnter, WalkLeave, walkEnd, walkFormalsOccurrence:
	}
	return true
}

// arity reports a call of fn with n arguments that fn's formals refuse.
func (c *closureCheck) arity(name string, fn *LVal, n int) {
	r, ok := formalsArity(fn)
	if !ok || r.accepts(n) {
		return
	}
	c.find(ClosureIssue{Kind: ClosureArity, Name: name, Args: n,
		Message: fmt.Sprintf("calls %s with %d argument(s); %s takes %s", name, n, name, r)})
}

// arityRule is the argument counts a function's formals accept: at least
// lo, at most lo+opt unless rest, and, with keys, an even number after the
// first lo+opt (the binder refuses an odd number of keyword arguments).
type arityRule struct {
	lo, opt    int
	rest, keys bool
}

func (r arityRule) accepts(n int) bool {
	switch {
	case n < r.lo:
		return false
	case n <= r.lo+r.opt:
		return true
	case r.keys:
		return (n-r.lo-r.opt)%2 == 0
	}
	return r.rest
}

func (r arityRule) String() string {
	takes := strconv.Itoa(r.lo)
	switch {
	case r.keys && r.opt > 0:
		takes += " to " + strconv.Itoa(r.lo+r.opt) + " plus keyword pairs"
	case r.keys:
		takes += " plus keyword pairs"
	case r.rest:
		takes = "at least " + takes
	case r.opt > 0:
		takes += " to " + strconv.Itoa(r.lo+r.opt)
	}
	return takes
}

// formalsArity reads fn's formals as the binder does, and reports false
// when there are none to read.
func formalsArity(fn *LVal) (arityRule, bool) {
	if len(fn.Cells) == 0 || fn.Cells[0] == nil || fn.Cells[0].Type != LSExpr {
		return arityRule{}, false
	}
	var r arityRule
	optional := false
	for _, f := range fn.Cells[0].Cells {
		if f == nil || f.Type != LSymbol {
			return arityRule{}, false
		}
		switch f.Str {
		case OptArgSymbol:
			optional = true
		case VarArgSymbol:
			r.rest = true
			return r, true
		case KeyArgSymbol:
			r.keys = true
			return r, true
		default:
			if optional {
				r.opt++
			} else {
				r.lo++
			}
		}
	}
	return r, true
}
