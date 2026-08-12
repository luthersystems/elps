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
// # Name resolution: the syntactic verdict is NOT the whole admission test
//
// Matching an operator by NAME proves nothing on its own.  `if`, `let`,
// `let*`, `progn`, `quasiquote` and `gensym` are ordinary bindings in ELPS,
// and a program that rebinds one of them — by defun, by defmacro, by set, or
// by inheriting a shadowed binding from a used package — changes what the
// body does without changing a character of its syntax.  Every one of those
// shadows was demonstrated to return a WRONG ANSWER under caching (the
// macro's expansion stopped being a pure rewrite, and the cache froze the
// first one), so the prover does not assume kernel semantics: it RECORDS the
// operator spellings it interpreted and requires them to resolve to the
// kernel bindings before any expansion is cached.
//
// The verdict is therefore split in two:
//
//   - proveUserMacroPure returns a macroPurity whose `pure` bit is purely
//     SYNTACTIC — a function of the macro's (sealed, immutable) formals and
//     body nodes and nothing else.  That is what may be memoized on the
//     formals node.
//   - macroPurity.defRefs / .callRefs list the name→kernel-binding
//     obligations the syntactic verdict rests on.  They are ENVIRONMENT
//     dependent, so they are re-checked on every dispatch (macrocache.go,
//     opsResolveToKernel) and are never memoized.  Re-checking is also what
//     makes a LATER `(set 'if ...)` take effect: a verdict cached against an
//     environment could not see it.
//
// defRefs are resolved in the macro's DEFINING environment (funData.env),
// which is where the body actually evaluates at expansion time; callRefs in
// the CALLING environment, which is where the expansion evaluates.  If any
// obligation fails to resolve to the kernel binding the macro is simply not
// cacheable — refusing to cache is always sound.
//
// Names the prover interprets but does NOT need to resolve, and why:
//
//   - `quote` in template position, and the parser's own quote flags/LQuote
//     wrappers.  Reading a form as a quote only ever INCREASES the quote
//     depth, and quote depth is used in exactly one place: rejecting a
//     gensym that escapes as data.  Mis-reading a shadowed `quote` as the
//     kernel one can therefore only reject a macro that might have been
//     admissible — it can never admit one.  (`quote` in body position is
//     not accepted by the body grammar at all.)
//   - `unquote` / `unquote-splicing`.  These are not bindings: quasiquote
//     consumes them as syntax, matching the name on an LSymbol head
//     (getUnquoteType, lisp/macro.go).  Requiring the enclosing
//     `quasiquote` to BE the kernel quasiquote — which defRefs does — is
//     what makes that reading correct.
//   - `true` / `false`, the only free symbol reads the body grammar admits.
//     They are kernel CONSTANTS: set, defun, defmacro, let, let*, labels and
//     flet all refuse to rebind them, so there is no binding for an
//     obligation to check.  Asserted, not assumed —
//     TestMacroCacheTrueFalseAreKernelConstants.
//
// # Remaining boundary
//
// The obligations cover the operators the prover interprets.  They do not
// cover the CONTENT of a template, which is output code evaluated at the
// callsite: `(quasiquote (if ...))` is inert to the prover and reused
// verbatim, so whatever `if` means at the callsite it means identically with
// and without the cache.  The one place template content is interpreted is
// the binder-syntax discharge (see pureMacroTemplate), which is why those
// binder spellings become callRefs whenever the macro mints a gensym.
//
// One reading is known to be WRONG and is nonetheless harmless, which is
// worth stating rather than leaving to be rediscovered.  defRefs resolve in
// funData.env, while the body evaluates one frame BELOW that, in a call
// environment carrying the macro's own formals.  A macro whose formal is
// named `if` (or `let*`, or `gensym`) is therefore admitted on the strength
// of a binding the body will never consult.  It cannot change an answer,
// because a macro formal is bound to an UNEVALUATED argument node and an
// argument node is never a function: the body's `(if ...)` can only fail to
// call it, deterministically and identically with the cache on or off
// (TestMacroCacheFormalShadowingAnOperatorAgrees).  Should macro formals ever
// carry callable values, the obligations must move to the call environment.

// kernelRef is one name→kernel-binding obligation: the operator SPELLING as
// written in the macro source (so a `lisp:`-qualified spelling is resolved
// as written) and the funData FID the binding must have.
type kernelRef struct {
	spelling string
	fid      string
}

// macroPurity is the memoizable half of the admission test: a syntactic
// verdict plus the name-resolution obligations it rests on.
type macroPurity struct {
	// body is the macro body this verdict was proven from.  The memo is
	// keyed on the formals node, and a macro-generating macro can give two
	// macros one formals node with different bodies — see
	// userMacroPurityMemo (macrocache.go).
	body     []*LVal
	defRefs  []kernelRef // resolved in the macro's defining environment
	callRefs []kernelRef // resolved in the calling environment
	pure     bool
}

// specialOpFID mirrors the FID format AddSpecialOps assigns; builtinFunFID
// mirrors AddBuiltins.  Tests pin both against the registration code.
func specialOpFID(name string) string  { return "<special-op ``" + name + "''>" }
func builtinFunFID(name string) string { return "<builtin-function ``" + name + "''>" }

// proveUserMacroPure reports whether fun (an LFunMacro with lisp cells:
// Cells[0] formals, Cells[1:] body) provably performs a pure structural
// rewrite, making its expansions safe to cache per callsite — SUBJECT to the
// name-resolution obligations in the returned macroPurity.
func proveUserMacroPure(fun *LVal) macroPurity {
	p := &prover{}
	formals := fun.Cells[0]
	if formals.Type != LSExpr {
		return macroPurity{}
	}
	scope := make(map[string]symKind, len(formals.Cells))
	for _, f := range formals.Cells {
		if f.Type != LSymbol {
			return macroPurity{}
		}
		if f.Str == VarArgSymbol || f.Str == OptArgSymbol || f.Str == KeyArgSymbol {
			continue
		}
		scope[f.Str] = symFormal
	}
	body := fun.Cells[1:]
	if len(body) == 0 {
		return macroPurity{}
	}
	for _, e := range body {
		if !p.pureMacroExpr(e, scope) {
			return macroPurity{}
		}
	}
	// The binder-syntax discharge inside templates (pureMacroTemplate) reads
	// let/let*/labels/flet/lambda in the OUTPUT code as syntax rather than
	// data.  That reading matters in exactly one case — it is what admits a
	// gensym written into a binding position — so the callsite obligation is
	// only incurred when the macro actually mints a gensym.
	if p.hasGensym {
		for _, ref := range p.binderRefs {
			p.callRefs = addRef(p.callRefs, ref)
		}
	}
	return macroPurity{pure: true, defRefs: p.defRefs, callRefs: p.callRefs}
}

// prover carries the name-resolution obligations accumulated while walking a
// macro body.  It holds no environment: the walk is purely syntactic.
type prover struct {
	defRefs    []kernelRef
	callRefs   []kernelRef
	binderRefs []kernelRef
	hasGensym  bool
}

func addRef(refs []kernelRef, ref kernelRef) []kernelRef {
	for _, r := range refs {
		if r == ref {
			return refs
		}
	}
	return append(refs, ref)
}

// needSpecialOp records that the walk read `spelling` as the kernel special
// operator `op`, evaluated in the macro's DEFINING environment.
func (p *prover) needSpecialOp(spelling, op string) {
	p.defRefs = addRef(p.defRefs, kernelRef{spelling: spelling, fid: specialOpFID(op)})
}

// needBuiltin records a builtin-function obligation in the defining
// environment (gensym is the only one).
func (p *prover) needBuiltin(spelling, name string) {
	p.defRefs = addRef(p.defRefs, kernelRef{spelling: spelling, fid: builtinFunFID(name)})
}

// needBinder records that the walk read `spelling` in TEMPLATE (output-code)
// position as the kernel binding operator `op`, evaluated in the CALLING
// environment.  Promoted to a real obligation only if the macro mints a
// gensym — see proveUserMacroPure.
func (p *prover) needBinder(spelling, op string) {
	p.binderRefs = addRef(p.binderRefs, kernelRef{spelling: spelling, fid: specialOpFID(op)})
}

type symKind uint8

const (
	symFormal symKind = iota
	symGensym
)

// kernelOp strips a "lisp:" package qualifier so the operator checks accept
// both spellings; any other qualifier is left intact (and thus rejected by
// the operator switch).  The UNSTRIPPED spelling is what gets resolved — see
// kernelRef.
func kernelOp(name string) string {
	const q = "lisp:"
	if len(name) > len(q) && name[:len(q)] == q {
		return name[len(q):]
	}
	return name
}

func (p *prover) pureMacroExpr(e *LVal, scope map[string]symKind) bool {
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
		// Parser-level quoting, not a name lookup — nothing to resolve.
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
		op := kernelOp(head.Str)
		switch op {
		case "quasiquote":
			if len(e.Cells) != 2 {
				return false
			}
			p.needSpecialOp(head.Str, op)
			return p.pureMacroTemplate(e.Cells[1], scope, 0)
		case "if":
			if len(e.Cells) < 3 || len(e.Cells) > 4 {
				return false
			}
			p.needSpecialOp(head.Str, op)
			return p.allPureMacroExprs(e.Cells[1:], scope)
		case "progn":
			p.needSpecialOp(head.Str, op)
			return p.allPureMacroExprs(e.Cells[1:], scope)
		case "let", "let*":
			if !p.pureMacroLet(e, scope) {
				return false
			}
			p.needSpecialOp(head.Str, op)
			return true
		default:
			return false
		}
	default:
		return false
	}
}

func (p *prover) allPureMacroExprs(es []*LVal, scope map[string]symKind) bool {
	for _, e := range es {
		if !p.pureMacroExpr(e, scope) {
			return false
		}
	}
	return true
}

// pureMacroLet admits (let ...) / (let* ...) forms whose binding
// expressions are either the literal call (gensym) — introducing a gensym
// local — or themselves pure.  let evaluates bindings in the outer scope;
// let* extends the scope sequentially.
func (p *prover) pureMacroLet(e *LVal, scope map[string]symKind) bool {
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
		// (gensym) is the ONE expression the body grammar admits without
		// proving it from its own structure, so the proof has to come from
		// somewhere: the head must resolve to the kernel gensym builtin in
		// the defining environment, and the call must take no arguments.
		// Shadowing `gensym` with an impure function is otherwise a clean
		// defeat — the binding value is never examined at all.
		if spelling, ok := gensymCallSpelling(val); ok {
			p.needBuiltin(spelling, "gensym")
			p.hasGensym = true
			inner[sym.Str] = symGensym
			continue
		}
		if !p.pureMacroExpr(val, bindScope) {
			return false
		}
		inner[sym.Str] = symFormal
	}
	return p.allPureMacroExprs(e.Cells[2:], inner)
}

// gensymCallSpelling reports whether v is a zero-argument call whose head
// names gensym, returning the head's spelling for resolution.
func gensymCallSpelling(v *LVal) (string, bool) {
	if v.Type != LSExpr || v.quoted || len(v.Cells) != 1 {
		return "", false
	}
	head := v.Cells[0]
	if head.Type != LSymbol || head.quoted || kernelOp(head.Str) != "gensym" {
		return "", false
	}
	return head.Str, true
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
//
// The discharge is the one place the prover INTERPRETS template content, so
// it is the one place a shadow in the CALLING environment can make the
// reading wrong: a `let*` that treats its binding list as data leaks the
// gensym name.  Those spellings are recorded as callRefs (only when the
// macro mints a gensym, the sole case where the reading matters).
func (p *prover) pureMacroTemplate(t *LVal, scope map[string]symKind, quoteDepth int) bool {
	return p.pureMacroTemplateQ(t, scope, quoteDepth, false)
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

func (p *prover) pureMacroTemplateQ(t *LVal, scope map[string]symKind, quoteDepth int, syntaxQuote bool) bool {
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
	// Unquote recognition MUST mirror getUnquoteType (lisp/macro.go),
	// which matches the operator name on an LSymbol head and IGNORES that
	// symbol's quote flag.  ``('unquote (f))'' is therefore a real unquote
	// to quasiquote — it evaluates (f) at expansion time — even though the
	// head is quoted.  Scanning it as inert template content would admit
	// arbitrary expansion-time computation into a "pure" macro.
	if head.Type == LSymbol {
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
	}
	if head.Type == LSymbol && !head.quoted {
		op := kernelOp(head.Str)
		switch op {
		case "quasiquote":
			return false // nested quasiquote: rejected for the POC
		case "quote":
			// Reading a form as a quote only RAISES the quote depth, which
			// only ever rejects; no resolution obligation (see the file
			// comment).
			for _, c := range t.Cells[1:] {
				if !p.pureMacroTemplateQ(c, scope, qd+1, false) {
					return false
				}
			}
			return true
		case "let", "let*":
			if qd == 0 && len(t.Cells) >= 2 {
				p.needBinder(head.Str, op)
				return p.pureTemplateBindings(t.Cells[1], scope, false) &&
					p.allPureTemplates(t.Cells[2:], scope, qd)
			}
		case "labels", "flet":
			if qd == 0 && len(t.Cells) >= 2 {
				p.needBinder(head.Str, op)
				return p.pureTemplateBindings(t.Cells[1], scope, true) &&
					p.allPureTemplates(t.Cells[2:], scope, qd)
			}
		case "lambda":
			if qd == 0 && len(t.Cells) >= 2 {
				p.needBinder(head.Str, op)
				return p.pureMacroTemplateQ(t.Cells[1], scope, qd, true) &&
					p.allPureTemplates(t.Cells[2:], scope, qd)
			}
		}
	}
	for _, c := range t.Cells {
		if !p.pureMacroTemplateQ(c, scope, qd, false) {
			return false
		}
	}
	return true
}

func (p *prover) allPureTemplates(ts []*LVal, scope map[string]symKind, qd int) bool {
	for _, c := range ts {
		if !p.pureMacroTemplateQ(c, scope, qd, false) {
			return false
		}
	}
	return true
}

// pureTemplateBindings scans a let/let*/labels/flet binding list inside a
// template, discharging the syntactic bracket quotes: one on the list, one
// on each binding, and (for function binders) one on each binding's formals
// list.  Everything else inside the bindings is scanned normally.
func (p *prover) pureTemplateBindings(list *LVal, scope map[string]symKind, funBinder bool) bool {
	if list == nil || isSingleton(list) {
		return true
	}
	inner, levels := templateQuoteLevels(list)
	if levels > 1 {
		return false // more than binder syntax: quoted data
	}
	if inner == nil || inner.Type != LSExpr {
		return p.pureMacroTemplateQ(list, scope, 0, true)
	}
	for _, b := range inner.Cells {
		bi, blev := templateQuoteLevels(b)
		if blev > 1 {
			return false
		}
		if bi == nil || bi.Type != LSExpr {
			if !p.pureMacroTemplateQ(b, scope, 0, true) {
				return false
			}
			continue
		}
		for i, cell := range bi.Cells {
			// A function binder's formals list ([name formals body...])
			// gets one more syntactic-quote discharge at index 1.
			syntax := funBinder && i == 1
			if !p.pureMacroTemplateQ(cell, scope, 0, syntax) {
				return false
			}
		}
	}
	return true
}
