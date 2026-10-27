// Copyright © 2026 The ELPS authors

package lint

import (
	"fmt"
	"strings"

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
	recordUnknownMember    = "record-unknown-member"
	recordCheckWrites      = "record-check-writes"
)

// recordFinding is one finding of the record walker.  kind is the name of
// the analyzer that reports it.
type recordFinding struct {
	node *lisp.LVal
	kind string
	msg  string
	note string
}

// recordShape is what one s:make-record declares: the names of its
// fields and methods.  closed is false when a field form is not a literal
// s:has-key or s:may-have-key, so the field names are not all known.
type recordShape struct {
	fields  map[string]bool
	methods map[string]bool
	closed  bool
}

// recordFindings returns every finding of the record checks in exprs.
// The calls and the function bodies around them come from
// astutil.FindCallSites, which owns the lexical syntax; this file reads
// only the record forms.
func recordFindings(exprs []*lisp.LVal) []recordFinding {
	exprs = recordForms(exprs)
	if len(exprs) == 0 {
		return nil
	}
	shapes, byName := recordShapes(exprs)
	w := &recordWalk{handled: make(map[*lisp.LVal]bool)}
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"make-record", recordPackage+"defmethod") {
			shape := shapes[site.Form]
			if astutil.HeadSymbol(site.Form) == recordPackage+"defmethod" && len(site.Form.Cells) > 1 {
				shape = byName[site.Form.Cells[1].Str]
			}
			for _, body := range methodBodies(site.Form) {
				w.methodBody(body, "this", shape)
			}
			if check := checkFunction(site.Form); check != nil {
				w.checkBody(check)
			}
		}
	}
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"set!") {
			if !w.handled[site.Form] {
				w.out = append(w.out, recordFinding{
					node: site.Form, kind: recordSetOutsideMethod,
					msg:  "s:set! outside a method of the record type",
					note: "only the type's own methods change an object's fields; add a method with s:method or s:defmethod and call it",
				})
			}
		}
		recordTypeBindings(top, &w.out)
	}
	return w.out
}

// recordWalk holds what the record checks collect: the s:set! calls a
// method or :check function holds, and the findings.
type recordWalk struct {
	handled map[*lisp.LVal]bool
	out     []recordFinding
}

// mutationReport is the text of a finding on a mutating builtin.  msg
// takes the builtin and the field; note may use the field as %[2]s.
type mutationReport struct {
	kind, msg, note string
}

// recordShapes returns the shape of each s:make-record call in exprs, and
// the shape of each type a top-level set or defconst binds.  A
// s:defmethod in exprs adds its method to the shape of the type it names.
func recordShapes(exprs []*lisp.LVal) (map[*lisp.LVal]*recordShape, map[string]*recordShape) {
	shapes := make(map[*lisp.LVal]*recordShape)
	byName := make(map[string]*recordShape)
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"make-record") {
			shapes[site.Form] = newRecordShape(site.Form)
		}
		if name := boundName(top); name != "" {
			if sites := callSites(top.Cells[2], recordPackage+"make-record"); len(sites) == 1 {
				byName[name] = shapes[sites[0].Form]
			}
		}
	}
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"defmethod") {
			c := site.Form.Cells
			if len(c) > 2 && c[1].Type == lisp.LSymbol && c[2].Type == lisp.LSymbol {
				if shape := byName[c[1].Str]; shape != nil {
					shape.methods[c[2].Str] = true
				}
			}
		}
	}
	return shapes, byName
}

// boundName returns NAME when top is (set 'NAME value) or
// (defconst NAME value).
func boundName(top *lisp.LVal) string {
	if top.IsQuoted() || len(top.Cells) != 3 {
		return ""
	}
	name := top.Cells[1]
	switch unqualifiedLispName(astutil.HeadSymbol(top)) {
	case "set":
		if name.Type == lisp.LSymbol && name.IsQuoted() {
			return name.Str
		}
	case "defconst":
		if name.Type == lisp.LSymbol && !name.IsQuoted() {
			return name.Str
		}
	}
	return ""
}

// newRecordShape reads the field and method forms of a s:make-record call.
func newRecordShape(call *lisp.LVal) *recordShape {
	shape := &recordShape{fields: make(map[string]bool), methods: make(map[string]bool), closed: true}
	forms := call.Cells[min(2, len(call.Cells)):]
	for i := 0; i < len(forms); i++ {
		form := forms[i]
		switch h := astutil.HeadSymbol(form); {
		case form.IsSymbol(":check"):
			i++
		case (h == recordPackage+"method" || h == "method") && len(form.Cells) > 1 && form.Cells[1].Type == lisp.LSymbol:
			shape.methods[form.Cells[1].Str] = true
		case (h == recordPackage+"has-key" || h == recordPackage+"may-have-key") && len(form.Cells) > 1 && form.Cells[1].Type == lisp.LString:
			shape.fields[form.Cells[1].Str] = true
		default:
			shape.closed = false
		}
	}
	return shape
}

// checkFunction returns the :check form of a s:make-record call.
func checkFunction(call *lisp.LVal) *lisp.LVal {
	if astutil.HeadSymbol(call) != recordPackage+"make-record" {
		return nil
	}
	for i := 2; i+1 < len(call.Cells); i++ {
		if call.Cells[i].IsSymbol(":check") {
			return call.Cells[i+1]
		}
	}
	return nil
}

// recordForms returns the top-level forms that name a symbol of the
// libschema package.  Every record check starts from such a symbol, so the
// other forms (most of a usual file) skip the call-site queries.
func recordForms(exprs []*lisp.LVal) []*lisp.LVal {
	var out []*lisp.LVal
	for _, top := range exprs {
		found := false
		astutil.Walk([]*lisp.LVal{top}, func(node, _ *lisp.LVal, _ int) {
			found = found || (node.Type == lisp.LSymbol && strings.HasPrefix(node.Str, recordPackage))
		})
		if found {
			out = append(out, top)
		}
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

// mutatingNames returns the names of the mutating builtins.
func mutatingNames() []string {
	names := make([]string, 0, len(mutatingBuiltins))
	for name := range mutatingBuiltins {
		names = append(names, name)
	}
	return names
}

// methodBody marks the s:set! calls of a method body as handled.  It
// reports a mutating builtin that writes through (this 'field), and, when
// the type's shape is known, a member name the type does not declare.
func (w *recordWalk) methodBody(body []*lisp.LVal, this string, shape *recordShape) {
	known := shape != nil && shape.closed
	for _, form := range body {
		for _, site := range callSites(form, recordPackage+"set!") {
			w.handled[site.Form] = true
			c := site.Form.Cells
			if !known || len(c) < 3 || !c[1].IsSymbol(this) {
				continue
			}
			if field, ok := quotedName(c[2]); ok && !shape.fields[field] {
				w.out = append(w.out, unknownMember(site.Form, fmt.Sprintf("s:set! names %s, which is not a field of the type", field)))
			}
		}
		if known {
			for _, site := range callSites(form, this) {
				name, ok := quotedName(site.Form.Cells[min(1, len(site.Form.Cells)-1)])
				if ok && len(site.Form.Cells) > 1 && !shape.fields[name] && !shape.methods[name] {
					w.out = append(w.out, unknownMember(site.Form, name+" is not a field or method of the type"))
				}
			}
		}
		w.fieldMutations(form, this, mutationReport{recordFieldMutation,
			"%s changes field %s of this in place, and its validator does not run",
			"build the new value and give it to s:set!: (s:set! this '%[2]s new-value)"})
	}
}

// unknownMember returns a record-unknown-member finding.
func unknownMember(node *lisp.LVal, msg string) recordFinding {
	return recordFinding{
		node: node, kind: recordUnknownMember, msg: msg,
		note: "a record object has only the fields and methods its type declares; check the spelling",
	}
}

// checkBody reports a write in a :check function: s:set!, or a
// mutating builtin on a field of the object it checks.  It marks the s:set!
// calls as handled, so they are not also reported as outside a method.
func (w *recordWalk) checkBody(check *lisp.LVal) {
	for _, site := range callSites(check, recordPackage+"set!") {
		w.handled[site.Form] = true
		w.out = append(w.out, recordFinding{
			node: site.Form, kind: recordCheckWrites,
			msg:  "s:set! in a :check function",
			note: ":check runs after each change and must only read the object; raise an error to refuse the change",
		})
	}
	// When the :check form is itself a function, its first formal is the
	// object.
	for _, site := range callSites(check, mutatingNames()...) {
		for _, e := range site.Enclosing {
			if e.Function && e.Form == check {
				w.fieldMutations(site.Form, firstFormal(check), mutationReport{recordCheckWrites,
					"%s changes field %s in a :check function",
					":check runs after each change and must only read the object; raise an error to refuse the change"})
				break
			}
		}
	}
}

// fieldMutations reports each mutating builtin in form whose target is
// (receiver 'field).
func (w *recordWalk) fieldMutations(form *lisp.LVal, receiver string, r mutationReport) {
	for _, site := range callSites(form, mutatingNames()...) {
		head := mutationHead(site.Form)
		target := mutatingBuiltins[head]
		if target >= len(site.Form.Cells) {
			continue
		}
		if field, ok := receiverFieldRead(site.Form.Cells[target], receiver); ok {
			w.out = append(w.out, recordFinding{
				node: site.Form, kind: r.kind,
				msg:  fmt.Sprintf(r.msg, head, field),
				note: fmt.Sprintf(r.note, head, field),
			})
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

// firstFormal returns the name of the first formal of a function form.
func firstFormal(fn *lisp.LVal) string {
	if len(fn.Cells) < 2 {
		return ""
	}
	formals := fn.Cells[1]
	if len(formals.Cells) == 0 {
		return ""
	}
	name, _ := symbolName(formals.Cells[0])
	return name
}

// symbolName returns the name of a symbol.
func symbolName(v *lisp.LVal) (string, bool) {
	if v.Type == lisp.LSymbol {
		return v.Str, true
	}
	return "", false
}

// receiverFieldRead returns the field name when v is (receiver 'field).
func receiverFieldRead(v *lisp.LVal, receiver string) (string, bool) {
	if v == nil || v.Type != lisp.LSExpr || v.IsQuoted() || len(v.Cells) != 2 ||
		astutil.HeadSymbol(v) != receiver {
		return "", false
	}
	return quotedName(v.Cells[1])
}

// quotedName returns the name of a quoted symbol.
func quotedName(v *lisp.LVal) (string, bool) {
	if v.Type == lisp.LSymbol && v.IsQuoted() {
		return v.Str, true
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

// AnalyzerRecordUnknownMember reports a member name a record type does not
// declare, read or set through this in a method.
var AnalyzerRecordUnknownMember = &Analyzer{
	Name:     recordUnknownMember,
	Severity: SeverityWarning,
	Doc: "Report a field or method name that the record type does not declare.\n\n" +
		"In a method, (this 'name) must name a field or method of the type, and " +
		"(s:set! this 'name ...) must name a field. A wrong name fails only when the " +
		"method runs. The check knows the type's fields and methods from its " +
		"s:make-record and from each s:defmethod in the same file; it skips a type " +
		"whose field forms are not literal s:has-key or s:may-have-key calls.",
	Run: recordAnalyzerRun(recordUnknownMember),
}

// AnalyzerRecordCheckWrites reports a write in a record type's :check
// function.
var AnalyzerRecordCheckWrites = &Analyzer{
	Name:     recordCheckWrites,
	Severity: SeverityWarning,
	Doc: "Report a :check function that changes the object it checks.\n\n" +
		"A record type's :check function runs after the constructor and after each " +
		"method call. It must only read the object and raise an error to refuse a " +
		"change. A write in it (s:set!, or a mutating builtin such as assoc! on a " +
		"field) changes the object after its validators ran.",
	Run: recordAnalyzerRun(recordCheckWrites),
}
