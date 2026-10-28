// Copyright © 2026 The ELPS authors

package lint

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/codewalk"
	"github.com/luthersystems/elps/lisp"
)

// The record type checks (luthersystems/elps#895).  libschema record types
// keep each object's fields valid by checking every write: the constructor
// and s:set-field! run the field validators.  The interpreter cannot refuse every
// way around those checks without a cost on each call, so these checks find
// the usual ones in source.  All of them are warnings now; the next major
// release may make the runtime refuse what they report.

// recordPackage is the qualifier of the libschema package.  Several of its
// exports (not, when, list, error, sorted-map) hide lisp names after a
// use-package, so a file uses the package qualified, and the checks match
// only the qualified spellings.
const recordPackage = "s:"

// The names of the record checks.
const (
	recordSetOutsideMethod = "record-set-outside-method"
	recordTypeBinding      = "record-type-binding"
	recordFieldMutation    = "record-field-mutation"
	recordUnknownMember    = "record-unknown-member"
	recordCheckWrites      = "record-check-writes"
	recordMethodArity      = "record-method-arity"
	recordDefmethodType    = "record-defmethod-type"
	recordPlainSet         = "record-plain-set"
	recordCapturedSet      = "record-captured-set"
	recordDefmethodScope   = "record-defmethod-not-toplevel"
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
	fields map[string]bool
	// methods maps each method to its signature after the receiver, or
	// to nil when its formals cannot be read.
	methods map[string]*analysis.Signature
	closed  bool
}

// recordFindings returns every finding of the record checks in exprs.
// The calls and the function bodies around them come from
// astutil.FindCallSites, which owns the lexical syntax; this file reads
// only the record forms.
func recordFindings(exprs []*lisp.LVal, semantics *analysis.Result) []recordFinding {
	all := exprs
	exprs = recordForms(exprs)
	if len(exprs) == 0 {
		return nil
	}
	w := &recordWalk{handled: make(map[*lisp.LVal]bool), locals: make(map[*lisp.LVal]map[*lisp.LVal]map[string]bool)}
	var workspace analysis.RecordMethods
	if semantics != nil {
		workspace = semantics.RecordMethods
	}
	types := recordShapes(exprs, all, workspace)
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"make-record", recordPackage+"defmethod") {
			shape := types.shapes[site.Form]
			if astutil.HeadSymbol(site.Form) == recordPackage+"defmethod" && len(site.Form.Cells) > 1 {
				shape = types.byName[site.Form.Cells[1].Str]
				w.defmethodType(site.Form, types, semantics)
				w.defmethodScope(top, site.Form)
			}
			for _, m := range recordMethods(site.Form) {
				w.methodBody(m.body, m.recv, shape)
				w.capturedSets(top, m)
			}
			if check := checkFunction(site.Form); check != nil {
				w.checkBody(check)
			}
		}
	}
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"set-field!") {
			if !w.handled[site.Form] {
				w.out = append(w.out, recordFinding{
					node: site.Form, kind: recordSetOutsideMethod,
					msg:  "s:set-field! outside a method of the record type",
					note: "only the type's own methods change an object's fields; add a method with s:method or s:defmethod and call it",
				})
			}
		}
		recordTypeBindings(top, &w.out)
	}
	return w.out
}

// recordWalk holds what the record checks collect: the s:set-field! calls a
// method or :check function holds, and the findings.
type recordWalk struct {
	handled map[*lisp.LVal]bool
	out     []recordFinding
	// locals maps a top-level form to the lexical names in scope at each
	// form in it (lexicalNames).  It is computed once per top form, on
	// the first method in it.
	locals map[*lisp.LVal]map[*lisp.LVal]map[string]bool
}

// mutationReport is the text of a finding on a mutating builtin.  msg
// takes the builtin and the field; note may use the field as %[2]s.
type mutationReport struct {
	kind, msg, note string
}

// recordTypes is what a file declares about record types.
type recordTypes struct {
	// shapes is the shape of each s:make-record call.
	shapes map[*lisp.LVal]*recordShape
	// byName is the shape of each type a top-level set or defconst binds.
	byName map[string]*recordShape
	// bound maps every name a top-level set or defconst binds to its
	// value form.
	bound map[string]*lisp.LVal
}

// recordShapes returns the record types of exprs, the top forms that use
// the s: package.  A s:defmethod in exprs adds its method to the shape of
// the type it names.  bound comes from all, every top form of the file.
//
// workspace holds the methods the s:defmethod calls of every workspace
// file add, or nil without a workspace scan.  A method that another file
// adds is then a member too, so a call to it is not reported as unknown.
// The file's own declarations win: the workspace data comes from the
// files on disk, which can be older than the text being linted.
func recordShapes(exprs, all []*lisp.LVal, workspace analysis.RecordMethods) recordTypes {
	t := recordTypes{
		shapes: make(map[*lisp.LVal]*recordShape),
		byName: make(map[string]*recordShape),
		bound:  make(map[string]*lisp.LVal),
	}
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"make-record") {
			t.shapes[site.Form] = newRecordShape(site.Form)
		}
		if name := boundName(top); name != "" {
			if sites := callSites(top.Cells[2], recordPackage+"make-record"); len(sites) == 1 {
				t.byName[name] = t.shapes[sites[0].Form]
			}
		}
	}
	for _, top := range all {
		if name := boundName(top); name != "" {
			t.bound[name] = top.Cells[2]
		}
	}
	for _, top := range exprs {
		for _, site := range callSites(top, recordPackage+"defmethod") {
			c := site.Form.Cells
			if len(c) > 3 && c[1].Type == lisp.LSymbol && c[2].Type == lisp.LSymbol {
				if shape := t.byName[c[1].Str]; shape != nil {
					shape.methods[c[2].Str] = methodSignature(c[3])
				}
			}
		}
	}
	for name, shape := range t.byName {
		for method, sig := range workspace[name] {
			if _, ok := shape.methods[method]; !ok && !shape.fields[method] {
				shape.methods[method] = sig
			}
		}
	}
	return t
}

// methodSignature returns the signature of a method's formals after the
// receiver, or nil when they are not a list.  A typed formal, [name type]
// (#905), counts as its name.
func methodSignature(formals *lisp.LVal) *analysis.Signature {
	return analysis.RecordMethodSignature(formals)
}

// methodFormals returns a method's formals, the receiver first, with each
// typed formal replaced by its name.
func methodFormals(formals *lisp.LVal) []*lisp.LVal {
	return analysis.RecordMethodFormals(formals)
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
	shape := &recordShape{fields: make(map[string]bool), methods: make(map[string]*analysis.Signature), closed: true}
	forms := call.Cells[min(2, len(call.Cells)):]
	for i := 0; i < len(forms); i++ {
		form := forms[i]
		switch h := astutil.HeadSymbol(form); {
		case form.IsSymbol(":check"), form.IsSymbol(":implements"):
			i++
		case (h == recordPackage+"method" || h == "method") && len(form.Cells) > 2 && form.Cells[1].Type == lisp.LSymbol:
			shape.methods[form.Cells[1].Str] = methodSignature(form.Cells[2])
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

// recordMethod is one method in a s:make-record or s:defmethod call: the
// name of its receiver, its formals and its body.  form is the s:method
// form or the s:defmethod call.
type recordMethod struct {
	recv    string
	formals *lisp.LVal
	body    []*lisp.LVal
	form    *lisp.LVal
}

// recordMethods returns each method of a s:make-record or s:defmethod call
// that has a receiver.
func recordMethods(call *lisp.LVal) []recordMethod {
	if astutil.HeadSymbol(call) == recordPackage+"defmethod" {
		if len(call.Cells) > 4 {
			if recv := receiverName(call.Cells[3]); recv != "" {
				return []recordMethod{{recv, call.Cells[3], call.Cells[4:], call}}
			}
		}
		return nil
	}
	var methods []recordMethod
	for _, c := range call.Cells[1:] {
		h := astutil.HeadSymbol(c)
		if (h == recordPackage+"method" || h == "method") && !c.IsQuoted() && len(c.Cells) > 3 {
			if recv := receiverName(c.Cells[2]); recv != "" {
				methods = append(methods, recordMethod{recv, c.Cells[2], c.Cells[3:], c})
			}
		}
	}
	return methods
}

// receiverName returns the first formal of a method's formals: its
// receiver.
func receiverName(formals *lisp.LVal) string {
	if len(formals.Cells) == 0 {
		return ""
	}
	name, _ := symbolName(formals.Cells[0])
	return name
}

// mutatingNames returns the names of the mutating builtins.
func mutatingNames() []string {
	names := make([]string, 0, len(mutatingBuiltins))
	for name := range mutatingBuiltins {
		names = append(names, name)
	}
	return names
}

// methodBody marks the s:set-field! calls of a method body as handled.  It
// reports a mutating builtin that writes through (recv 'field), and, when
// the type's shape is known, a member name the type does not declare.
func (w *recordWalk) methodBody(body []*lisp.LVal, recv string, shape *recordShape) {
	known := shape != nil && shape.closed
	for _, form := range body {
		for _, site := range callSites(form, recordPackage+"set-field!") {
			w.handled[site.Form] = true
			c := site.Form.Cells
			if !known || len(c) < 3 || !c[1].IsSymbol(recv) {
				continue
			}
			if field, ok := quotedName(c[2]); ok && !shape.fields[field] {
				w.out = append(w.out, unknownMember(site.Form, fmt.Sprintf("s:set-field! names %s, which is not a field of the type", field)))
			}
		}
		if known {
			for _, site := range callSites(form, recv) {
				w.memberCall(site.Form, shape)
			}
		}
		w.plainSets(form, recv)
		r := strings.ReplaceAll(recv, "%", "%%")
		w.fieldMutations(form, recv, mutationReport{recordFieldMutation,
			"%s changes field %s of " + r + " in place, and its validator does not run",
			"build the new value and give it to s:set-field!: (s:set-field! " + r + " '%[2]s new-value)"})
	}
}

// memberCall checks a call (recv 'name args...) in a method of a type
// whose members are all known: the member must exist, a field takes no
// arguments, and a method takes the arguments its formals accept.
func (w *recordWalk) memberCall(call *lisp.LVal, shape *recordShape) {
	if len(call.Cells) < 2 {
		return
	}
	name, ok := quotedName(call.Cells[1])
	if !ok {
		return
	}
	n := len(call.Cells) - 2
	sig, isMethod := shape.methods[name]
	switch {
	case shape.fields[name]:
		if n > 0 {
			w.out = append(w.out, arityFinding(call, fmt.Sprintf("field %s takes no arguments, got %d", name, n)))
		}
	case !isMethod:
		w.out = append(w.out, unknownMember(call, name+" is not a field or method of the type"))
	case sig == nil:
	case n < sig.MinArity():
		w.out = append(w.out, arityFinding(call, fmt.Sprintf("method %s needs at least %d argument(s), got %d", name, sig.MinArity(), n)))
	case sig.MaxArity() >= 0 && n > sig.MaxArity():
		w.out = append(w.out, arityFinding(call, fmt.Sprintf("method %s accepts at most %d argument(s), got %d", name, sig.MaxArity(), n)))
	}
}

// arityFinding returns a record-method-arity finding.
func arityFinding(node *lisp.LVal, msg string) recordFinding {
	return recordFinding{
		node: node, kind: recordMethodArity, msg: msg,
		note: "the arguments after the member name go to the method's formals after the receiver",
	}
}

// plainSets reports a set! or set in a method body that rebinds the
// receiver.  The receiver is a local name, so the binding changes and the
// object does not.  A set! of a name the body binds itself is not the
// receiver.
func (w *recordWalk) plainSets(form *lisp.LVal, recv string) {
	var current *lisp.LVal
	cw := &lisp.CodeWalker{KeepGoing: true, Visit: func(n *lisp.WalkNode) bool {
		switch n.Event {
		case lisp.WalkForm:
			current = n.Node
		case lisp.WalkSet:
			if !n.Bound && n.Node.Str == recv && current != nil {
				w.plainSet(current, recv)
			}
		case lisp.WalkRef, lisp.WalkBind, lisp.WalkDefine, lisp.WalkLiteral, lisp.WalkData, lisp.WalkEnter, lisp.WalkLeave:
		}
		return true
	}}
	codewalk.Occurrences(cw, maxPlainSetWork, form)
	for _, site := range callSites(form, "set") {
		if name, ok := quotedName(site.Form.Cells[min(1, len(site.Form.Cells)-1)]); ok && name == recv {
			w.plainSet(site.Form, recv)
		}
	}
}

// maxPlainSetWork bounds the lists plainSets walks in one method body.
const maxPlainSetWork = 1 << 16

// capturedSets reports each set! in method m of a name that a closure
// around the type binds: a let, let*, flet, labels or function formal
// outside the method.  Such a name is one variable for every object of
// the type, so a method that changes it changes shared state.  No object
// holds that state, so a durable dump does not save it and the rollback
// of a failed method call does not undo it.
//
// The walk is syntactic, as the other record checks are, so it runs
// without workspace analysis.  A set! of a global is not reported: the
// check is about locals that look private to the type.
func (w *recordWalk) capturedSets(top *lisp.LVal, m recordMethod) {
	names := methodFormals(m.formals)
	if names == nil {
		return
	}
	locals, ok := w.locals[top]
	if !ok {
		locals = lexicalNames(top)
		w.locals[top] = locals
	}
	captured := locals[m.form]
	if len(captured) == 0 {
		return
	}
	// Walk the method as the lambda it runs as.  The walk then binds the
	// receiver and the formals, and a set! it reports as not bound is
	// free in the method.
	fn := lisp.SExpr(lisp.Cells{lisp.Symbol("lambda"), lisp.SExpr(names)}.Append(m.body...))
	var current *lisp.LVal
	cw := &lisp.CodeWalker{KeepGoing: true, Visit: func(n *lisp.WalkNode) bool {
		switch n.Event {
		case lisp.WalkForm:
			current = n.Node
		case lisp.WalkSet:
			if !n.Bound && captured[n.Node.Str] && current != nil {
				name := n.Node.Str
				w.out = append(w.out, recordFinding{
					node: current, kind: recordCapturedSet,
					msg:  "set! changes " + name + ", a local of the closure around the record type; every object of the type shares it",
					note: "a durable dump does not save " + name + ", and a failed method call does not undo it; keep per-object state in a field and change it with s:set-field!",
				})
			}
		case lisp.WalkRef, lisp.WalkBind, lisp.WalkDefine, lisp.WalkLiteral, lisp.WalkData, lisp.WalkEnter, lisp.WalkLeave:
		}
		return true
	}}
	codewalk.Occurrences(cw, maxPlainSetWork, fn)
}

// lexicalNames returns, for each compound form in code position in top,
// the names that the lexical binding forms around it bind.  Globals are
// not in the sets.  Macros are not expanded, so a name a macro binds is
// not seen; the check then misses a finding and never makes a false one.
func lexicalNames(top *lisp.LVal) map[*lisp.LVal]map[string]bool {
	out := make(map[*lisp.LVal]map[string]bool)
	var scopes []map[string]bool
	cw := &lisp.CodeWalker{KeepGoing: true, Visit: func(n *lisp.WalkNode) bool {
		switch n.Event {
		case lisp.WalkEnter:
			scopes = append(scopes, make(map[string]bool))
		case lisp.WalkLeave:
			if len(scopes) > 0 {
				scopes = scopes[:len(scopes)-1]
			}
		case lisp.WalkBind:
			if len(scopes) > 0 && n.Node.Type == lisp.LSymbol {
				scopes[len(scopes)-1][n.Node.Str] = true
			}
		case lisp.WalkForm:
			if len(scopes) == 0 {
				break
			}
			names := make(map[string]bool)
			for _, s := range scopes {
				for name := range s {
					names[name] = true
				}
			}
			out[n.Node] = names
		case lisp.WalkRef, lisp.WalkSet, lisp.WalkDefine, lisp.WalkLiteral, lisp.WalkData:
		}
		return true
	}}
	codewalk.Occurrences(cw, maxPlainSetWork, top)
	return out
}

// plainSet reports one set! or set on the receiver.
func (w *recordWalk) plainSet(call *lisp.LVal, recv string) {
	c := call.Cells
	f := recordFinding{
		node: call, kind: recordPlainSet,
		msg:  c[0].Str + " rebinds the local name " + recv + "; the object does not change",
		note: "to change a field of the object, use (s:set-field! " + recv + " 'field value)",
	}
	if !strings.HasSuffix(c[0].Str, "!") {
		f.msg = c[0].Str + " binds the package name " + recv + ", not the receiver; the object does not change"
	}
	if len(c) == 4 {
		if field, ok := quotedName(c[2]); ok {
			f.msg = c[0].Str + " does not change a record field"
			f.note = "use (s:set-field! " + recv + " '" + field + " value)"
		}
	}
	w.out = append(w.out, f)
}

// defmethodType reports a s:defmethod whose first argument cannot be a
// record type: a literal, a quoted name, or a name the file binds to
// something that is not a type.  A name bound in another file is trusted.
func (w *recordWalk) defmethodType(call *lisp.LVal, types recordTypes, semantics *analysis.Result) {
	if len(call.Cells) < 2 {
		return
	}
	arg := call.Cells[1]
	if msg := notATypeForm(arg); msg != "" {
		w.out = append(w.out, defmethodTypeFinding(arg, msg))
		return
	}
	if arg.Type != lisp.LSymbol || types.byName[arg.Str] != nil {
		return
	}
	if v := types.bound[arg.Str]; v != nil {
		if msg := notATypeForm(v); msg != "" {
			w.out = append(w.out, defmethodTypeFinding(arg, arg.Str+" is bound to "+msg+", not to a record type"))
		}
		return
	}
	if semantics == nil || semantics.RootScope == nil {
		return
	}
	if sym := semantics.RootScope.Lookup(arg.Str); sym != nil {
		switch sym.Kind {
		case analysis.SymFunction, analysis.SymMacro, analysis.SymBuiltin, analysis.SymSpecialOp, analysis.SymType:
			w.out = append(w.out, defmethodTypeFinding(arg, arg.Str+" is a "+sym.Kind.String()+", not a record type"))
		case analysis.SymVariable, analysis.SymParameter:
		}
	}
}

// defmethodScope reports a s:defmethod that does not run at load: one
// in a function body, a macro call or any other form that is not a
// load-scope wrapper.  See AnalyzerRecordDefmethodNotToplevel for why.
func (w *recordWalk) defmethodScope(top, call *lisp.LVal) {
	path := pathTo(top, call, maxPlainSetWork)
	if path == nil {
		return
	}
	if where := notLoadScope(path); where != "" {
		w.out = append(w.out, recordFinding{
			node: call, kind: recordDefmethodScope,
			msg:  "s:defmethod inside " + where + " runs when that code runs, not at load; the method reaches only the VM that runs it",
			note: "move the s:defmethod to the top level of the file, or into a top-level let, let*, flet, labels or progn around the type",
		})
	}
}

// notLoadScope returns "" when each form on path, from the top form to the
// s:defmethod call, keeps it at load scope, or else the head of the first
// form that does not.  The load-scope forms are progn, the body and the
// binding values of let and let*, the body of flet and labels, and the
// value of a top-level set or defconst.  A flet or labels binding is a
// function body, so it is not load scope.
func notLoadScope(path []*lisp.LVal) string {
	for i := 0; i+1 < len(path); i++ {
		parent, child := path[i], path[i+1]
		idx := cellIndex(parent, child)
		head := unqualifiedLispName(astutil.HeadSymbol(parent))
		switch {
		case head == "progn" && idx >= 1:
		case (head == "let" || head == "let*") && idx >= 2:
		case (head == "let" || head == "let*") && idx == 1:
			// The binding list, then one binding, then its value.
			if i+3 >= len(path) || cellIndex(child, path[i+2]) < 0 || cellIndex(path[i+2], path[i+3]) != 1 {
				return head + " bindings"
			}
			i += 2
		case (head == "flet" || head == "labels") && idx >= 2:
		case i == 0 && (head == "set" || head == "defconst") && idx == 2:
		default:
			if head == "" {
				return "a form"
			}
			return head
		}
	}
	return ""
}

// cellIndex returns the index of child in the cells of parent, or -1.
func cellIndex(parent, child *lisp.LVal) int {
	for i, c := range parent.Cells {
		if c == child {
			return i
		}
	}
	return -1
}

// pathTo returns the lists from top down to target, both included, or nil
// when target is not found within budget lists.  target comes from a
// call-site query, which returns only calls in code position, so the
// search also enters quoted lists: a let binding [x v] reads as one.
func pathTo(top, target *lisp.LVal, budget int) []*lisp.LVal {
	var path []*lisp.LVal
	var find func(v *lisp.LVal) bool
	find = func(v *lisp.LVal) bool {
		if budget <= 0 || v.Type != lisp.LSExpr {
			return false
		}
		budget--
		path = append(path, v)
		if v == target {
			return true
		}
		for _, c := range v.Cells {
			if find(c) {
				return true
			}
		}
		path = path[:len(path)-1]
		return false
	}
	if find(top) {
		return path
	}
	return nil
}

// notATypeForm describes a form whose value is never a record type, or
// returns "" when the form may evaluate to one.
func notATypeForm(v *lisp.LVal) string {
	switch {
	case v.IsQuoted():
		return "a quoted value"
	case v.Type == lisp.LString, v.Type == lisp.LInt, v.Type == lisp.LFloat:
		return "a literal"
	}
	return ""
}

// defmethodTypeFinding returns a record-defmethod-type finding.
func defmethodTypeFinding(node *lisp.LVal, msg string) recordFinding {
	return recordFinding{
		node: node, kind: recordDefmethodType, msg: msg,
		note: "the first argument of s:defmethod is the type a s:make-record returns: (s:defmethod TYPE NAME (recv args...) body...)",
	}
}

// unknownMember returns a record-unknown-member finding.
func unknownMember(node *lisp.LVal, msg string) recordFinding {
	return recordFinding{
		node: node, kind: recordUnknownMember, msg: msg,
		note: "a record object has only the fields and methods its type declares; check the spelling",
	}
}

// checkBody reports a write in a :check function: s:set-field!, or a
// mutating builtin on a field of the object it checks.  It marks the s:set-field!
// calls as handled, so they are not also reported as outside a method.
func (w *recordWalk) checkBody(check *lisp.LVal) {
	for _, site := range callSites(check, recordPackage+"set-field!") {
		w.handled[site.Form] = true
		w.out = append(w.out, recordFinding{
			node: site.Form, kind: recordCheckWrites,
			msg:  "s:set-field! in a :check function",
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
	sites := callSites(form, mutatingNames()...)
	if len(sites) == 0 {
		return
	}
	// The receiver's calls in form.  A call to a nested function's formal
	// of the same name is not one of them.
	reads := make(map[*lisp.LVal]bool)
	for _, read := range callSites(form, receiver) {
		reads[read.Form] = true
	}
	for _, site := range sites {
		head := mutationHead(site.Form)
		target := mutatingBuiltins[head]
		if target >= len(site.Form.Cells) || !reads[site.Form.Cells[target]] {
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
	return receiverName(fn.Cells[1])
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
			sh.record, sh.recordDone = recordFindings(pass.Exprs, pass.Semantics), true
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

// AnalyzerRecordSetOutsideMethod reports s:set-field! outside a method of a
// record type.
var AnalyzerRecordSetOutsideMethod = &Analyzer{
	Name:     recordSetOutsideMethod,
	Severity: SeverityWarning,
	Doc: "Report s:set-field! outside a method of a record type.\n\n" +
		"A record type's methods are the one place that changes its objects. s:set-field! " +
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
// through (recv 'field) in a method.
var AnalyzerRecordFieldMutation = &Analyzer{
	Name:     recordFieldMutation,
	Severity: SeverityWarning,
	Doc: "Report a method that changes a field's value in place.\n\n" +
		"(recv 'field), where recv is the method's receiver, returns the field's value itself, not a copy. A mutating " +
		"builtin such as assoc! or append! on it changes the field without its " +
		"validator. Build the new value and give it to s:set-field!.",
	Run: recordAnalyzerRun(recordFieldMutation),
}

// AnalyzerRecordUnknownMember reports a member name a record type does not
// declare, read or set through the receiver in a method.
var AnalyzerRecordUnknownMember = &Analyzer{
	Name:     recordUnknownMember,
	Severity: SeverityWarning,
	Doc: "Report a field or method name that the record type does not declare.\n\n" +
		"In a method, (recv 'name) must name a field or method of the type, and " +
		"(s:set-field! recv 'name ...) must name a field, where recv is the method's receiver. A wrong name fails only when the " +
		"method runs. The check knows the type's fields and methods from its " +
		"s:make-record and from each s:defmethod in the same file. With a workspace " +
		"scan it also knows the methods an s:defmethod in another file adds. It skips " +
		"a type whose field forms are not literal s:has-key or s:may-have-key calls.",
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
		"change. A write in it (s:set-field!, or a mutating builtin such as assoc! on a " +
		"field) changes the object after its validators ran.",
	Run: recordAnalyzerRun(recordCheckWrites),
}

// AnalyzerRecordMethodArity reports a member call in a method with the
// wrong number of arguments.
var AnalyzerRecordMethodArity = &Analyzer{
	Name:     recordMethodArity,
	Severity: SeverityWarning,
	Doc: "Report a call (recv 'name args...) in a method that gives a member the wrong number of arguments.\n\n" +
		"A field read takes no arguments. A method call takes the arguments its formals " +
		"accept after the receiver. The check knows the members of a type made by " +
		"s:make-record in the same file, with the s:defmethod methods of that file " +
		"and, with a workspace scan, of the other workspace files.",
	Run: recordAnalyzerRun(recordMethodArity),
}

// AnalyzerRecordDefmethodType reports a s:defmethod whose type argument
// is not a record type.
var AnalyzerRecordDefmethodType = &Analyzer{
	Name:     recordDefmethodType,
	Severity: SeverityWarning,
	Doc: "Report a s:defmethod whose first argument is not a record type.\n\n" +
		"The first argument is evaluated and must give the type s:make-record returns. " +
		"The check reports a literal, a quoted name, a name the file binds to a literal, " +
		"and a name that is a function, macro or builtin. A name bound in another file " +
		"is not checked.",
	Run: recordAnalyzerRun(recordDefmethodType),
}

// AnalyzerRecordCapturedSet reports a set! in a method of a local that a
// closure around the record type binds.
var AnalyzerRecordCapturedSet = &Analyzer{
	Name:     recordCapturedSet,
	Severity: SeverityWarning,
	Doc: "Report set! in a method of a local of the closure around the record type.\n\n" +
		"A type built in a let, let*, flet or labels can use the closure's locals as " +
		"private helpers. Each local is one variable for every object of the type. " +
		"A method that changes it with set! changes state that all objects share and " +
		"that no object holds: a durable dump does not save it, and a failed method " +
		"call does not undo it. Keep per-object state in a field.",
	Run: recordAnalyzerRun(recordCapturedSet),
}

// AnalyzerRecordDefmethodNotToplevel reports a s:defmethod that runs at
// call time instead of at load.
var AnalyzerRecordDefmethodNotToplevel = &Analyzer{
	Name:     recordDefmethodScope,
	Severity: SeverityWarning,
	Doc: "Report s:defmethod that does not run at load, outside a test file.\n\n" +
		"A template publishes a record type with its method table as it stands at " +
		"publication. A s:defmethod in a function body, such as a defun, lambda or " +
		"endpoint body, runs at call time and adds its method only in the VM that runs " +
		"the call. The method is lost after the call, it is not in the type's saved " +
		"form, and a later call in another VM does not see it. Write s:defmethod at the " +
		"top level, or in a top-level let, let*, flet, labels or progn. A *_test.lisp " +
		"or *_testhelpers.lisp file is skipped: only test runners load it.",
	Run: func(pass *Pass) error {
		// A test may add a method at call time on purpose, to test
		// s:defmethod itself, and a test file is never part of a template.
		base := filepath.Base(pass.Filename)
		if strings.HasSuffix(base, "_test.lisp") || strings.HasSuffix(base, "_testhelpers.lisp") {
			return nil
		}
		return recordAnalyzerRun(recordDefmethodScope)(pass)
	},
}

// AnalyzerRecordPlainSet reports a set! or set in a method that rebinds
// the receiver.
var AnalyzerRecordPlainSet = &Analyzer{
	Name:     recordPlainSet,
	Severity: SeverityWarning,
	Doc: "Report set! or set on a method's receiver.\n\n" +
		"The receiver is a local name. (set! o v) and (set 'o v) change the binding, " +
		"not the object, and (set! o 'field v) is not a field write. Use " +
		"(s:set-field! o 'field v) to change a field.",
	Run: recordAnalyzerRun(recordPlainSet),
}
