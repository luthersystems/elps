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

// recordFindings returns every finding of the record checks in exprs.
// The calls and the function bodies around them come from
// astutil.FindCallSites, which owns the lexical syntax; this file reads
// only the record forms.
func recordFindings(exprs []*lisp.LVal) []recordFinding {
	var out []recordFinding
	inMethod := make(map[*lisp.LVal]bool)
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"make-record", recordPackage+"defmethod") {
			for _, body := range methodBodies(site.Form) {
				recordMethodBody(body, inMethod, &out)
			}
		}
	}
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"set!") {
			if !inMethod[site.Form] {
				out = append(out, recordFinding{
					node: site.Form, kind: recordSetOutsideMethod,
					msg:  "s:set! outside a method of the record type",
					note: "only the type's own methods change an object's fields; add a method with s:method or s:defmethod and call it",
				})
			}
		}
		recordTypeBindings(top, &out)
	}
	return out
}

// callSites returns the calls in form whose head is one of names.  A site
// past the query's budget is dropped; the record checks are warnings, so a
// missed site costs a finding, never a false one.
func callSites(form *lisp.LVal, names ...string) []astutil.CallSite {
	sites, _ := astutil.FindCallSites(form, names...)
	return sites
}

// methodBodies returns the body forms of each method in a s:make-record or
// s:defmethod call.
func methodBodies(call *lisp.LVal) [][]*lisp.LVal {
	if astutil.HeadSymbol(call) == recordPackage+"defmethod" {
		if len(call.Cells) > 4 {
			return [][]*lisp.LVal{call.Cells[4:]}
		}
		return nil
	}
	var bodies [][]*lisp.LVal
	for _, c := range call.Cells[1:] {
		h := astutil.HeadSymbol(c)
		if (h == recordPackage+"method" || h == "method") && !c.IsQuoted() && len(c.Cells) > 3 {
			bodies = append(bodies, c.Cells[3:])
		}
	}
	return bodies
}

// recordMethodBody marks the s:set! calls of a method body, and reports a
// mutating builtin that writes through (this 'field).
func recordMethodBody(body []*lisp.LVal, inMethod map[*lisp.LVal]bool, out *[]recordFinding) {
	names := make([]string, 0, len(mutatingBuiltins))
	for name := range mutatingBuiltins {
		names = append(names, name)
	}
	for _, form := range body {
		for _, site := range callSites(form, recordPackage+"set!") {
			inMethod[site.Form] = true
		}
		for _, site := range callSites(form, names...) {
			head := mutationHead(site.Form)
			target := mutatingBuiltins[head]
			if target >= len(site.Form.Cells) {
				continue
			}
			if field, ok := thisFieldRead(site.Form.Cells[target]); ok {
				*out = append(*out, recordFinding{
					node: site.Form, kind: recordFieldMutation,
					msg:  fmt.Sprintf("%s changes field %s of this in place, and its validator does not run", head, field),
					note: fmt.Sprintf("build the new value and give it to s:set!: (s:set! this '%s new-value)", field),
				})
			}
		}
	}
}

// recordTypeBindings reports each s:make-record in top that does not reach
// a top-level set or defconst: one outside the bound value, or inside a
// function body within it.
func recordTypeBindings(top *lisp.LVal, out *[]recordFinding) {
	report := func(sites []astutil.CallSite, allowValue bool) {
		for _, site := range sites {
			inFunction := false
			for _, e := range site.Enclosing {
				inFunction = inFunction || e.Function
			}
			if allowValue && !inFunction {
				continue
			}
			*out = append(*out, recordFinding{
				node: site.Form, kind: recordTypeBinding,
				msg:  "s:make-record does not reach a top-level set; a durable dump cannot save objects of this type",
				note: "an object saves its type by the name of the global that holds it; write (set 'name (s:make-record ...)) at the top level of the file",
			})
		}
	}
	name := recordPackage + "make-record"
	switch unqualifiedLispName(astutil.HeadSymbol(top)) {
	case "set", "defconst":
		if !top.IsQuoted() && len(top.Cells) > 2 {
			for i, c := range top.Cells[1:] {
				report(callSites(c, name), i == 1)
			}
			return
		}
	}
	report(callSites(top, name), false)
}

// thisFieldRead returns the field name when v is (this 'field).
func thisFieldRead(v *lisp.LVal) (string, bool) {
	if v == nil || v.Type != lisp.LSExpr || v.IsQuoted() || len(v.Cells) != 2 ||
		astutil.HeadSymbol(v) != "this" {
		return "", false
	}
	if arg := v.Cells[1]; arg.Type == lisp.LSymbol && arg.IsQuoted() {
		return arg.Str, true
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
