// Copyright © 2026 The ELPS authors

package lint

import (
	"fmt"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// The record type checks (luthersystems/elps#895).  libschema record types
// keep each object's fields valid by checking every write: the constructor
// and s:set! run the field validators.  The interpreter cannot refuse every
// way around those checks without a cost on each call, so these checks find
// the usual ones in source.  All three are warnings now; the next major
// release may make the runtime refuse what they report.

// recordPackage is the qualifier of the libschema package.  Its exports
// include set!, which collides with lisp:set!, so a file never uses the
// package unqualified and the checks match only the qualified spellings.
const recordPackage = "s:"

// The names of the record checks.
const (
	recordSetOutsideMethod = "record-set-outside-method"
	recordTypeBinding      = "record-type-binding"
	recordFieldMutation    = "record-field-mutation"
)

// recordFinding is one finding of the record walker.  kind is the name of
// the analyzer that reports it.
type recordFinding struct {
	node *lisp.LVal
	kind string
	msg  string
	note string
}

// recordContext is what the walker knows about the forms that enclose a
// node.
type recordContext struct {
	// topSet is true inside the value of a top-level set, outside any
	// function body.
	topSet bool
	// inFunction is true inside a function body: a lambda, a defun, a
	// defmacro, a flet or labels function, or a method.
	inFunction bool
	// inMethod is true inside the body of a record method, where this is
	// bound to the object.
	inMethod bool
}

// recordFindings walks exprs and returns every finding of the record
// checks, in source order.
func recordFindings(exprs []*lisp.LVal) []recordFinding {
	var out []recordFinding
	walkRecordTop(exprs, &out)
	return out
}

// walkRecordTop walks top-level forms.  The value of a top-level set or
// defconst binds a global, and so does one inside a top-level progn.
func walkRecordTop(exprs []*lisp.LVal, out *[]recordFinding) {
	for _, e := range exprs {
		switch mutationHead(e) {
		case "set", "defconst":
			for i, c := range e.Cells[1:] {
				walkRecord(c, recordContext{topSet: i == 1}, out)
			}
			continue
		case "progn":
			if !e.IsQuoted() {
				walkRecordTop(e.Cells[1:], out)
				continue
			}
		}
		walkRecord(e, recordContext{}, out)
	}
}

// walkRecord collects the record findings of v in ctx.
func walkRecord(v *lisp.LVal, ctx recordContext, out *[]recordFinding) {
	if v == nil || v.Type != lisp.LSExpr || v.IsQuoted() || len(v.Cells) == 0 {
		return
	}
	head := mutationHead(v)
	args := v.Cells[1:]
	walkAll := func(cells []*lisp.LVal, ctx recordContext) {
		for _, c := range cells {
			walkRecord(c, ctx, out)
		}
	}
	fnCtx := ctx
	fnCtx.topSet, fnCtx.inFunction = false, true
	switch head {
	case "quote", "quasiquote":
		return
	case recordPackage + "make-record":
		if !ctx.topSet {
			*out = append(*out, recordFinding{
				node: v, kind: recordTypeBinding,
				msg:  "s:make-record does not reach a top-level set; a durable dump cannot save objects of this type",
				note: "an object saves its type by the name of the global that holds it; write (set 'name (s:make-record ...)) at the top level of the file",
			})
		}
		methodCtx := fnCtx
		methodCtx.inMethod = true
		for _, c := range args {
			if h := astutil.HeadSymbol(c); (h == recordPackage+"method" || h == "method") && len(c.Cells) >= 3 {
				walkAll(c.Cells[3:], methodCtx)
				continue
			}
			walkRecord(c, ctx, out)
		}
		return
	case recordPackage + "defmethod":
		if len(args) >= 3 {
			walkRecord(args[0], ctx, out)
			methodCtx := fnCtx
			methodCtx.inMethod = true
			walkAll(args[3:], methodCtx)
			return
		}
	case recordPackage + "set!":
		if !ctx.inMethod {
			*out = append(*out, recordFinding{
				node: v, kind: recordSetOutsideMethod,
				msg:  "s:set! outside a method of the record type",
				note: "only the type's own methods change an object's fields; add a method with s:method or s:defmethod and call it",
			})
		}
	case "lambda", "defun", "defmacro":
		walkAll(args, fnCtx)
		return
	case "flet", "labels", "macrolet":
		// A binding [name formals body...] is a function body.  The reader
		// marks a bracketed list as quoted, so walk its cells directly.
		if len(args) > 0 {
			for _, b := range args[0].Cells {
				if len(b.Cells) > 2 {
					walkAll(b.Cells[2:], fnCtx)
				}
			}
			walkAll(args[1:], ctx)
			return
		}
	case "let", "let*":
		// A binding is [name value]; walk the value as the let's own code.
		if len(args) > 0 {
			for _, b := range args[0].Cells {
				if len(b.Cells) > 1 {
					walkAll(b.Cells[1:], ctx)
				}
			}
			walkAll(args[1:], ctx)
			return
		}
	}
	if target, ok := mutatingBuiltins[head]; ok && head != "set!" && ctx.inMethod && target < len(v.Cells) {
		if field, ok := thisFieldRead(v.Cells[target]); ok {
			*out = append(*out, recordFinding{
				node: v, kind: recordFieldMutation,
				msg:  fmt.Sprintf("%s changes field %s of this in place, and its validator does not run", head, field),
				note: fmt.Sprintf("build the new value and give it to s:set!: (s:set! this '%s new-value)", field),
			})
		}
	}
	walkAll(v.Cells, ctx)
}

// thisFieldRead returns the field name when v is (this 'field).
func thisFieldRead(v *lisp.LVal) (string, bool) {
	if v == nil || v.Type != lisp.LSExpr || v.IsQuoted() || len(v.Cells) != 2 ||
		astutil.HeadSymbol(v) != "this" {
		return "", false
	}
	arg := v.Cells[1]
	if arg.Type == lisp.LSymbol && arg.IsQuoted() {
		return arg.Str, true
	}
	if mutationHead(arg) == "quote" && len(arg.Cells) == 2 && arg.Cells[1].Type == lisp.LSymbol {
		return arg.Cells[1].Str, true
	}
	return "", false
}

// recordAnalyzerRun reports the findings of one kind.
func recordAnalyzerRun(kind string) func(pass *Pass) error {
	return func(pass *Pass) error {
		sh := pass.shared
		if sh == nil {
			sh = &passShared{}
		}
		if !sh.recordDone {
			sh.record, sh.recordDone = recordFindings(pass.Exprs), true
		}
		for _, f := range sh.record {
			if f.kind != kind {
				continue
			}
			pass.Report(Diagnostic{
				Message: f.msg,
				Pos:     posFromSource(astutil.SourceLoc(SourceOf(f.node))),
				EndPos:  endPosFromNode(f.node),
				Notes:   []string{f.note},
			})
		}
		return nil
	}
}

// AnalyzerRecordSetOutsideMethod reports s:set! outside a method of a
// record type.
var AnalyzerRecordSetOutsideMethod = &Analyzer{
	Name:     recordSetOutsideMethod,
	Severity: SeverityWarning,
	Doc: "Report s:set! outside a method of a record type.\n\n" +
		"A record type's methods are the one place that changes its objects. s:set! " +
		"checks the new value against the field's validator only. The type's :check " +
		"function runs when a method call returns, so a change from outside a " +
		"method skips it. Add a method and call it.",
	Run: recordAnalyzerRun(recordSetOutsideMethod),
}

// AnalyzerRecordTypeBinding reports a record type that does not reach a
// top-level set.
var AnalyzerRecordTypeBinding = &Analyzer{
	Name:     recordTypeBinding,
	Severity: SeverityWarning,
	Doc: "Report s:make-record whose type does not reach a top-level set.\n\n" +
		"A durable dump saves an object's type by the name of the global that holds " +
		"the type. A type built inside a function, or in a value no global holds, " +
		"has no such name, so its objects cannot be saved. A closure around the " +
		"type is allowed: (set 'order (let (...) (s:make-record ...))).",
	Run: recordAnalyzerRun(recordTypeBinding),
}

// AnalyzerRecordFieldMutation reports a mutating builtin that writes
// through (this 'field) in a method.
var AnalyzerRecordFieldMutation = &Analyzer{
	Name:     recordFieldMutation,
	Severity: SeverityWarning,
	Doc: "Report a method that changes a field's value in place.\n\n" +
		"(this 'field) returns the field's value itself, not a copy. A mutating " +
		"builtin such as assoc! or append! on it changes the field without its " +
		"validator. Build the new value and give it to s:set!.",
	Run: recordAnalyzerRun(recordFieldMutation),
}
