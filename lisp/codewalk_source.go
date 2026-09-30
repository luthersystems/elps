// Copyright © 2026 The ELPS authors

package lisp

import "github.com/luthersystems/elps/internal/codewalk/hook"

type walkNode = hook.Node[LVal, WalkEvent]
type codeBinding = hook.Binding
type sourceWalkOptions = hook.Options[LVal, WalkEvent]

// walkEnd belongs to the source adapter, not the public runtime event stream.
const walkEnd WalkEvent = WalkLeave + 1

func init() {
	hook.Walk = func(w *CodeWalker, opts sourceWalkOptions, form *LVal) *LVal {
		w.sourceAnalysis, w.sourceVisit, w.binding = true, opts.Visit, opts.BindingForm
		w.sourceReference, w.sourceForm = opts.Reference, opts.Form
		w.sourceEnd, w.sourceSkipLiterals = opts.End, opts.SkipLiterals
		w.sourceEndDepth, w.sourceDeclarationsOnly = opts.EndDepth, opts.DeclarationsOnly
		return w.Walk(form)
	}
	hook.Forms = func(w *CodeWalker, form *LVal) *LVal {
		w.formsOnly = true
		defer func() { w.formsOnly = false }()
		return w.Walk(form)
	}
	hook.PackageForms = packageForms
	hook.Occurrences = func(w *CodeWalker, budget int, form *LVal) bool {
		n := 0
		w.noMemo = true
		w.step = func() *LVal {
			if n++; n > budget {
				return Errorf("code walk budget of %d lists exhausted", budget)
			}
			return nil
		}
		defer func() { w.noMemo, w.step = false, nil }()
		w.Walk(form)
		return n <= budget
	}
}

// sourceValue reports source occurrences without the runtime walk's memo,
// nesting limit or rebuild bookkeeping. Both paths use special's grammar.
func (w *CodeWalker) sourceValue(v *LVal, depth int) *LVal {
	if w.err != nil || v == nil {
		return v
	}
	if v.quoted || v.Type == LQuote {
		w.data(v, depth)
		return v
	}
	if v.Type == LSymbol && !isKeyword(v.Str) {
		if w.sourceReference != nil {
			w.sourceReference(v)
		} else {
			w.reference(WalkRef, v, depth, false)
		}
		return v
	}
	if v.Type != LSExpr || len(v.Cells) == 0 {
		if !w.sourceSkipLiterals {
			w.visit(&walkNode{Event: WalkLiteral, Node: v, Depth: depth})
		}
		return v
	}
	return w.sourceCompound(v, depth)
}

func (w *CodeWalker) sourceCompound(v *LVal, depth int) *LVal {
	op, kind := w.sourceSpecialOp(v.Cells[0])
	result := v
	var descend bool
	if w.sourceForm != nil {
		descend = w.sourceForm(v, op, depth)
	} else {
		descend = w.visit(&walkNode{Event: WalkForm, Node: v, Op: op, Depth: depth})
	}
	if descend {
		if kind != kindNone {
			result = w.special(v, op, kind, depth)
		} else if w.binding != nil {
			if binding := w.binding(v); binding != nil {
				result = w.bindingForm(v, binding, depth)
			} else {
				result = w.call(v, depth)
			}
		} else {
			result = w.call(v, depth)
		}
	}
	if w.sourceDeclarationsOnly {
		return result
	}
	if w.sourceEnd != nil {
		if w.sourceEndDepth == nil || depth == *w.sourceEndDepth {
			w.sourceEnd(depth)
		}
	} else {
		w.visit(&walkNode{Event: walkEnd, Node: v, Op: op, Depth: depth})
	}
	return result
}
