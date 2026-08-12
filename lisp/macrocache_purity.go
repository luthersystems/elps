// Copyright © 2026 The ELPS authors

package lisp

// Conservative syntactic purity proof for user (defmacro) macros — the
// admission test for per-callsite expansion caching (see macrocache.go).
//
// A user macro is admitted only when its body is, syntactically, a pure
// template instantiation.  The grammar accepted OUTSIDE templates:
//
//	body     := doc-string? expr+
//	expr     := atom                       ; string/int/float constants
//	          | 'anything                  ; fully-quoted constant data
//	          | formal | local | true | false
//	          | (quasiquote template)
//	          | (if expr expr expr?)
//	          | (progn expr*)
//	          | (let  ([local binding]*) expr+)
//	          | (let* ([local binding]*) expr+)
//	binding  := (gensym)                   ; marks local as a gensym local
//	          | expr
//
// and INSIDE a template every shape is allowed — template content is
// expansion output, identical whether cached or re-expanded — except:
//
//   - (unquote x) / (unquote-splicing x): x must be a bare formal or local
//     symbol.  Any computed unquote is rejected (its value could depend on
//     runtime state).
//   - an unquoted GENSYM local under quote — 'x, (quote x), or any
//     enclosing quote level — is rejected: the expansion would capture a
//     fresh symbol AS DATA, and caching would freeze what re-expansion
//     mints fresh (the one observable gensym-identity difference).
//     Formals under quote are fine: re-expansion splices the same argument
//     node either way.
//   - nested quasiquote is rejected outright (level bookkeeping is not
//     worth the audit surface for a POC).
//
// Everything else — free symbol reads, calls to any function, gensym in
// non-binding position, unsealed (runtime-constructed) macro bodies —
// fails the proof and the callsite quietly bypasses the cache.
//
// KNOWN BOUNDARY: the prover recognizes the structural operators (if, let,
// let*, progn, quasiquote, gensym, quote, unquote, unquote-splicing) by
// name, assuming they resolve to the kernel bindings.  A program that
// shadows those names with different semantics in the macro's package could
// fool the analysis; that pattern is outside the supported embedder model
// (the same assumption underlies quasiquote processing itself, which
// matches "unquote" by name — see getUnquoteType).

// proveUserMacroPure reports whether fun (an LFunMacro with lisp cells:
// Cells[0] formals, Cells[1:] body) provably performs a pure structural
// rewrite, making its expansions safe to cache per callsite.
func proveUserMacroPure(fun *LVal) bool {
	formals := fun.Cells[0]
	if formals.Type != LSExpr {
		return false
	}
	scope := make(map[string]symKind, len(formals.Cells))
	for _, f := range formals.Cells {
		if f.Type != LSymbol {
			return false
		}
		if f.Str == VarArgSymbol || f.Str == OptArgSymbol || f.Str == KeyArgSymbol {
			continue
		}
		scope[f.Str] = symFormal
	}
	body := fun.Cells[1:]
	if len(body) == 0 {
		return false
	}
	for _, e := range body {
		if !pureMacroExpr(e, scope) {
			return false
		}
	}
	return true
}

type symKind uint8

const (
	symFormal symKind = iota
	symGensym
)

// kernelOp strips a "lisp:" package qualifier so the operator checks accept
// both spellings; any other qualifier is left intact (and thus rejected by
// the operator switch).
func kernelOp(name string) string {
	const q = "lisp:"
	if len(name) > len(q) && name[:len(q)] == q {
		return name[len(q):]
	}
	return name
}

func pureMacroExpr(e *LVal, scope map[string]symKind) bool {
	if e == nil {
		return false
	}
	if isSingleton(e) {
		return true
	}
	if !e.sealed {
		// Runtime-constructed body; no shared identity, no proof.
		return false
	}
	if e.quoted || e.Type == LQuote {
		// Fully-quoted constant data: deterministic expansion content.
		return true
	}
	switch e.Type {
	case LString, LInt, LFloat:
		return true
	case LSymbol:
		if _, ok := scope[e.Str]; ok {
			return true
		}
		return e.Str == TrueSymbol || e.Str == FalseSymbol
	case LSExpr:
		if len(e.Cells) == 0 {
			return true
		}
		head := e.Cells[0]
		if head.Type != LSymbol || head.quoted {
			return false
		}
		switch kernelOp(head.Str) {
		case "quasiquote":
			return len(e.Cells) == 2 && pureMacroTemplate(e.Cells[1], scope, 0)
		case "if":
			if len(e.Cells) < 3 || len(e.Cells) > 4 {
				return false
			}
			return allPureMacroExprs(e.Cells[1:], scope)
		case "progn":
			return allPureMacroExprs(e.Cells[1:], scope)
		case "let", "let*":
			return pureMacroLet(e, scope)
		default:
			return false
		}
	default:
		return false
	}
}

func allPureMacroExprs(es []*LVal, scope map[string]symKind) bool {
	for _, e := range es {
		if !pureMacroExpr(e, scope) {
			return false
		}
	}
	return true
}

// pureMacroLet admits (let ...) / (let* ...) forms whose binding
// expressions are either the literal call (gensym) — introducing a gensym
// local — or themselves pure.  let evaluates bindings in the outer scope;
// let* extends the scope sequentially.
func pureMacroLet(e *LVal, scope map[string]symKind) bool {
	if len(e.Cells) < 3 {
		return false
	}
	bindings := e.Cells[1]
	if bindings.Type != LSExpr {
		return false
	}
	sequential := kernelOp(e.Cells[0].Str) == "let*"
	inner := make(map[string]symKind, len(scope)+len(bindings.Cells))
	for k, v := range scope {
		inner[k] = v
	}
	bindScope := scope
	if sequential {
		bindScope = inner
	}
	for _, b := range bindings.Cells {
		if b.Type != LSExpr || len(b.Cells) != 2 {
			return false
		}
		sym, val := b.Cells[0], b.Cells[1]
		if sym.Type != LSymbol {
			return false
		}
		if isGensymCall(val) {
			inner[sym.Str] = symGensym
			continue
		}
		if !pureMacroExpr(val, bindScope) {
			return false
		}
		inner[sym.Str] = symFormal
	}
	return allPureMacroExprs(e.Cells[2:], inner)
}

func isGensymCall(v *LVal) bool {
	return v.Type == LSExpr && !v.quoted && len(v.Cells) == 1 &&
		v.Cells[0].Type == LSymbol && !v.Cells[0].quoted &&
		kernelOp(v.Cells[0].Str) == "gensym"
}

// pureMacroTemplate scans quasiquote template content.  quoteDepth counts
// enclosing quote levels WITHIN the template (Quoted flags, LQuote wrappers
// and (quote ...) forms); an unquoted gensym local at quoteDepth > 0 would
// escape as data and rejects the macro.
//
// One family of quotes is EXEMPT from the count: binder syntax.  ELPS
// programs write binding lists with brackets — `[g (gensym)]` parses as a
// quoted list — inside let/let*/labels/flet/lambda forms, where the special
// operator consumes the list as syntax (binding names never become runtime
// values).  When one of those binder forms appears in CODE position
// (quoteDepth 0), the scanner discharges exactly one syntactic quote level
// on the binding list, on each binding, and on a labels/flet/lambda formals
// list, so `(let* ([(unquote g) ...]) ...)` is accepted while any deeper
// quoting — `'(unquote g)` in a binding value, a quoted body form — still
// counts and still rejects a gensym underneath it.
func pureMacroTemplate(t *LVal, scope map[string]symKind, quoteDepth int) bool {
	return pureMacroTemplateQ(t, scope, quoteDepth, false)
}

// templateQuoteLevels mirrors findAndUnquote's quote-level counting and
// returns the unwrapped node.
func templateQuoteLevels(t *LVal) (*LVal, int) {
	levels := 0
	if t.quoted {
		levels++
	}
	for t.Type == LQuote {
		levels++
		t = t.Cells[0]
		if t == nil {
			return nil, levels
		}
	}
	return t, levels
}

func pureMacroTemplateQ(t *LVal, scope map[string]symKind, quoteDepth int, syntaxQuote bool) bool {
	if t == nil || isSingleton(t) {
		return true
	}
	inner, levels := templateQuoteLevels(t)
	if syntaxQuote && levels > 0 {
		levels-- // discharge the binder-syntax bracket quote
	}
	qd := quoteDepth + levels
	t = inner
	if t == nil {
		return true
	}
	if t.Type != LSExpr {
		return true // atoms are inert template content
	}
	if len(t.Cells) == 0 {
		return true
	}
	head := t.Cells[0]
	if head.Type == LSymbol && !head.quoted {
		switch head.Str {
		case "unquote", "unquote-splicing":
			if len(t.Cells) != 2 {
				return false
			}
			arg := t.Cells[1]
			if arg.Type != LSymbol || arg.quoted {
				return false // computed unquote: not provably pure
			}
			kind, ok := scope[arg.Str]
			if !ok {
				return false // free variable in unquote
			}
			if kind == symGensym && qd > 0 {
				return false // gensym escaping as quoted data
			}
			return true
		}
		switch kernelOp(head.Str) {
		case "quasiquote":
			return false // nested quasiquote: rejected for the POC
		case "quote":
			for _, c := range t.Cells[1:] {
				if !pureMacroTemplateQ(c, scope, qd+1, false) {
					return false
				}
			}
			return true
		case "let", "let*":
			if qd == 0 && len(t.Cells) >= 2 {
				return pureTemplateBindings(t.Cells[1], scope, false) &&
					allPureTemplates(t.Cells[2:], scope, qd)
			}
		case "labels", "flet":
			if qd == 0 && len(t.Cells) >= 2 {
				return pureTemplateBindings(t.Cells[1], scope, true) &&
					allPureTemplates(t.Cells[2:], scope, qd)
			}
		case "lambda":
			if qd == 0 && len(t.Cells) >= 2 {
				return pureMacroTemplateQ(t.Cells[1], scope, qd, true) &&
					allPureTemplates(t.Cells[2:], scope, qd)
			}
		}
	}
	for _, c := range t.Cells {
		if !pureMacroTemplateQ(c, scope, qd, false) {
			return false
		}
	}
	return true
}

func allPureTemplates(ts []*LVal, scope map[string]symKind, qd int) bool {
	for _, c := range ts {
		if !pureMacroTemplateQ(c, scope, qd, false) {
			return false
		}
	}
	return true
}

// pureTemplateBindings scans a let/let*/labels/flet binding list inside a
// template, discharging the syntactic bracket quotes: one on the list, one
// on each binding, and (for function binders) one on each binding's formals
// list.  Everything else inside the bindings is scanned normally.
func pureTemplateBindings(list *LVal, scope map[string]symKind, funBinder bool) bool {
	if list == nil || isSingleton(list) {
		return true
	}
	inner, levels := templateQuoteLevels(list)
	if levels > 1 {
		return false // more than binder syntax: quoted data
	}
	if inner == nil || inner.Type != LSExpr {
		return pureMacroTemplateQ(list, scope, 0, true)
	}
	for _, b := range inner.Cells {
		bi, blev := templateQuoteLevels(b)
		if blev > 1 {
			return false
		}
		if bi == nil || bi.Type != LSExpr {
			if !pureMacroTemplateQ(b, scope, 0, true) {
				return false
			}
			continue
		}
		for i, cell := range bi.Cells {
			// A function binder's formals list ([name formals body...])
			// gets one more syntactic-quote discharge at index 1.
			syntax := funBinder && i == 1
			if !pureMacroTemplateQ(cell, scope, 0, syntax) {
				return false
			}
		}
	}
	return true
}
