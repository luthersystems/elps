// Copyright © 2026 The ELPS authors

// Package idiom is the elpsidiom analyzer.  It reports Go builtin code that
// an elps helper states more plainly, and a few mistakes in code that uses
// the helpers.  It is for code outside elps, such as a port of Lisp code to
// Go: other modules run it with their own vet tool.  elps's own gate (make
// elpsvet) runs it with -fixonly, which reports only the idioms that carry a
// suggested fix.  The hints stay out of elps's gate, because elps's builtins
// keep their error messages.
//
// IDIOMS, reported with category "info".  Each one keeps behaviour
// identical; where the rewrite is mechanical, the diagnostic carries a
// suggested fix, so `-fix` applies it:
//
//	x.Type == lisp.LError              x.IsError()                    (fix)
//	lisp.GoError(x) != nil             x.IsError()                    (fix)
//	lisp.GoError(x) == nil             !x.IsError()                   (fix)
//	lisp.QExpr([]*lisp.LVal{...})      lisp.Cells{...}.List()         (fix)
//	lisp.SExpr([]*lisp.LVal{...})      lisp.Cells{...}.SExpr()        (fix)
//	lisp.Vector([]*lisp.LVal{...})     lisp.Cells{...}.Vector()       (fix)
//	lisp.Array(nil, x)                 lisp.Vector(x)                 (fix)
//	x.Type == lisp.LSymbol && x.Str == name, where x and name read a
//	value with no side effect (an identifier, a selector, a constant
//	index, or a constant name)         x.IsSymbol(name)               (fix)
//	x.Type != lisp.LSymbol || x.Str != name
//	                                   !x.IsSymbol(name)              (fix)
//	if msg := env.Runtime.CheckAlloc(n); msg != "" {
//		return env.Errorf("%s", msg)
//	}                                  env.CheckAlloc(n)              (fix)
//	range m.MapKeys().Cells, after an m.Type == lisp.LSortMap check
//	                                   range m.Keys()                 (hint)
//	range m.MapEntries().Cells, after an m.Type == lisp.LSortMap check,
//	or in case lisp.LSortMap or case lisp.ShapeMap of a switch on m's type
//	                                   for k, v := range m.All()      (hint)
//	m.MapGetString(k) read as .Str after a .Type == lisp.LString check,
//	after an m.Type == lisp.LSortMap check
//	                                   lisp.Field[string](m, k)       (hint)
//	v := args.Cells[i] (or a, b := args.Cells[0], args.Cells[1]) followed
//	by a v.Type check that returns Errorf
//	                                   an ArgReader read              (hint)
//	env.CallBuiltin(sortedMap, lisp.String("k"), v, ...), where sortedMap
//	is lisp.BuiltinFunc("sorted-map")  env.MapOf("k", v, ...)         (fix)
//	env.CallBuiltin(ref, m, k, v), where ref is lisp.BuiltinFunc("assoc!")
//	                                   env.MapPut(m, k, v)            (fix)
//	env.CallBuiltin(ref, m, k) for "get"
//	                                   env.MapLookup(m, k)            (fix)
//	env.CallBuiltin(ref, v) for "to-string"
//	                                   env.ToString(v)                (fix)
//	env.CallBuiltin(ref, lisp.String(f), v, ...) for "format-string"
//	                                   env.FormatString(f, v, ...)    (fix)
//	fn.Cells[1] = doc or fn.Cells[1].Str = doc, where every value of fn
//	is a lisp.FunInPackage result      lisp.FunInPackageDoc(..., doc) (hint)
//	a switch or if-else chain on v.Type with an LSExpr arm that reads
//	v.Cells and an LArray arm that loops over v.ArrayIndex(lisp.Int(i))
//	                                   v.SeqCells() or lisp.SeqOf[T]  (hint)
//	three or more if err != nil { return env.Error(err) } in a function
//	                                   a lisp.FuncE body              (hint)
//	v.Native.(T) where a v.Type == lisp.LNative test guards it
//	                                   lisp.NativeValue[T](v)         (hint)
//
// A ref is a lisp.BuiltinFunc call or a package-level variable that one
// initializes.  MapPut, MapLookup, ToString and FormatString check the
// context, as CallBuiltin does, and then run the builtin's own body, so the
// allocation checks and the errors are the same.  The rule does not report a
// call with a different argument count, a spread argument list or a
// format-string format that is not lisp.String(f): the helper would not make
// the arity check or the check that the format is a string.
//
// A hint has no fix, because the rewrite changes the code's shape.  Keys
// yields a lisp.MapKey, not an *LVal.  SeqCells refuses a multi-dimensional
// array and returns the value's own cells.  NativeValue makes the type test
// that the old code makes apart from the assertion.  No idiom suggests a
// checked helper (MapPut, MapLookup, CheckAlloc) where the old code made no
// check, and no idiom suggests NativeValue where the old code made no type
// test.
//
// MISTAKES, reported with category "error":
//
//   - In a FuncE or Func*E body, and in a function of the same package that
//     such a body returns the error of, fmt.Errorf or errors.New(err.Error())
//     over an error that came from lisp.Result, lisp.ResultAs, lisp.GoError
//     or such a function.  The wrap gives the error condition "error", so
//     Lisp's handler-bind no longer sees the inner condition.  Raise a new
//     error with env.Errorf instead.
//   - A *lisp.ErrorVal in a function's result list.  A nil *ErrorVal
//     returned as an error is a non-nil error.  Return error.
//   - lisp.ResultAs[T] over a call whose result type varies (get, funcall,
//     apply, nth, first, second, aref, and LEnv.MapLookup), for a T other
//     than *lisp.LVal.  A value of another type is then a run-time error.
//   - A Func1E, Func2E or Func3E builtin registered with formals that are not
//     exactly its count of required arguments.  The builtin takes required
//     positional arguments only.
//   - An LEnv.MapOf key that is not a string or an *LVal, or a value that is
//     not one of *LVal, string, int, float64, bool, []byte, []*LVal or Cells.
//     MapOf panics on one at run time.
//
// INVISIBLE: code built through variables the rule does not trace (a
// BuiltinRef held in a local variable or a struct field, a formals list held
// in a local variable, a builtin passed through a slice), a type test that
// a called function makes, helpers in other packages, and reflection.  A clean run is evidence, not proof.
package idiom

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/constant"
	"go/format"
	"go/token"
	"go/types"
	"sort"
	"strings"

	"golang.org/x/tools/go/analysis"
)

const lispPkgPath = "github.com/luthersystems/elps/lisp"

// Categories of the diagnostics.
const (
	// CategoryInfo marks an idiom: code an elps helper states more plainly.
	CategoryInfo = "info"
	// CategoryError marks a mistake.
	CategoryError = "error"
)

// fixOnly is the -fixonly flag: report only the diagnostics that carry a
// suggested fix.
var fixOnly bool

func init() {
	Analyzer.Flags.BoolVar(&fixOnly, "fixonly", false,
		"report only the idioms that carry a suggested fix; a gate uses it to keep a cleaned tree clean")
}

// Analyzer is the elpsidiom analyzer.
var Analyzer = &analysis.Analyzer{
	Name: "elpsidiom",
	Doc: "report Go builtin code that an elps helper states more plainly (IsError, Cells, CheckAlloc, Keys, Field, " +
		"ArgReader, MapOf, MapPut, MapLookup, ToString, FormatString, FunInPackageDoc, SeqCells, NativeValue), and mistakes in code that uses the helpers: an error wrap that hides a Lisp condition, a *ErrorVal " +
		"result, ResultAs over a result of varying type, and a Func*E builtin registered with the wrong formals",
	Run: run,
}

// varyingBuiltins are the builtins whose result type depends on the data.
var varyingBuiltins = map[string]bool{
	"get": true, "funcall": true, "apply": true, "nth": true,
	"first": true, "second": true, "aref": true,
}

// errorBindings are the typed bindings whose body returns an error.
var errorBindings = map[string]int{"FuncE": -1, "Func1E": 1, "Func2E": 2, "Func3E": 3}

type state struct {
	pass  *analysis.Pass
	decls map[*types.Func]*ast.FuncDecl
	// builtinRefs maps a package-level BuiltinRef variable to the builtin
	// name its lisp.BuiltinFunc initializer names.
	builtinRefs map[types.Object]string
	// arities maps a package-level LBuiltin variable to the argument count
	// of the Func*E call that initializes it.
	arities map[types.Object]int
	// funVars maps a variable to true when every value assigned to it is a
	// lisp.FunInPackage call, and to false when another value is assigned.
	funVars map[types.Object]bool
	src     map[string][]byte
}

func run(pass *analysis.Pass) (any, error) {
	if pass.Pkg.Path() == lispPkgPath {
		return nil, nil
	}
	s := &state{
		pass:        pass,
		decls:       make(map[*types.Func]*ast.FuncDecl),
		builtinRefs: make(map[types.Object]string),
		arities:     make(map[types.Object]int),
		funVars:     make(map[types.Object]bool),
		src:         make(map[string][]byte),
	}
	s.collect()
	s.collectFunVars()
	for _, file := range pass.Files {
		s.checkFile(file)
	}
	s.checkErrorWraps()
	return nil, nil
}

// collect records the package's function declarations, BuiltinRef variables
// and Func*E variables.
func (s *state) collect() {
	for _, file := range s.pass.Files {
		for _, decl := range file.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				if fn, ok := s.pass.TypesInfo.Defs[d.Name].(*types.Func); ok {
					s.decls[fn] = d
				}
			case *ast.GenDecl:
				if d.Tok != token.VAR {
					continue
				}
				for _, spec := range d.Specs {
					vs, ok := spec.(*ast.ValueSpec)
					if !ok || len(vs.Names) != len(vs.Values) {
						continue
					}
					for i, name := range vs.Names {
						obj := s.pass.TypesInfo.Defs[name]
						if obj == nil {
							continue
						}
						call, ok := ast.Unparen(vs.Values[i]).(*ast.CallExpr)
						if !ok {
							continue
						}
						switch fn := s.lispFunc(call); {
						case fn == nil:
						case fn.Name() == "BuiltinFunc" && len(call.Args) == 1:
							if name, ok := s.constString(call.Args[0]); ok {
								s.builtinRefs[obj] = name
							}
						case errorBindings[fn.Name()] > 0:
							s.arities[obj] = errorBindings[fn.Name()]
						}
					}
				}
			}
		}
	}
}

// lispFunc returns the package-level lisp function call calls, or nil.
func (s *state) lispFunc(call *ast.CallExpr) *types.Func {
	fn := s.callee(call)
	if fn == nil || fn.Pkg() == nil || fn.Pkg().Path() != lispPkgPath || fn.Signature().Recv() != nil {
		return nil
	}
	return fn
}

// lispMethod returns the lisp method call calls (such as LEnv.CallBuiltin),
// or nil.
func (s *state) lispMethod(call *ast.CallExpr, recv string) *types.Func {
	fn := s.callee(call)
	if fn == nil || fn.Pkg() == nil || fn.Pkg().Path() != lispPkgPath {
		return nil
	}
	r := fn.Signature().Recv()
	if r == nil || !isLispNamed(derefType(r.Type()), recv) {
		return nil
	}
	return fn
}

func (s *state) callee(call *ast.CallExpr) *types.Func {
	fun := ast.Unparen(call.Fun)
	switch idx := fun.(type) {
	case *ast.IndexExpr:
		fun = ast.Unparen(idx.X)
	case *ast.IndexListExpr:
		fun = ast.Unparen(idx.X)
	}
	var id *ast.Ident
	switch f := fun.(type) {
	case *ast.Ident:
		id = f
	case *ast.SelectorExpr:
		id = f.Sel
	default:
		return nil
	}
	fn, _ := s.pass.TypesInfo.Uses[id].(*types.Func)
	return fn
}

// builtinName returns the builtin name that a BuiltinRef expression names:
// a lisp.BuiltinFunc call with a constant name, or a package-level variable
// that such a call initializes.  It returns "" for any other expression.
func (s *state) builtinName(e ast.Expr) string {
	var id *ast.Ident
	switch r := ast.Unparen(e).(type) {
	case *ast.Ident:
		id = r
	case *ast.SelectorExpr:
		id = r.Sel
	case *ast.CallExpr:
		if fn := s.lispFunc(r); fn != nil && fn.Name() == "BuiltinFunc" && len(r.Args) == 1 {
			name, _ := s.constString(r.Args[0])
			return name
		}
	}
	if id == nil {
		return ""
	}
	return s.builtinRefs[s.pass.TypesInfo.Uses[id]]
}

func (s *state) constString(e ast.Expr) (string, bool) {
	tv, ok := s.pass.TypesInfo.Types[e]
	if !ok || tv.Value == nil || tv.Value.Kind() != constant.String {
		return "", false
	}
	return constant.StringVal(tv.Value), true
}

func derefType(t types.Type) types.Type {
	if p, ok := types.Unalias(t).(*types.Pointer); ok {
		return p.Elem()
	}
	return t
}

func isLispNamed(t types.Type, name string) bool {
	named, ok := types.Unalias(t).(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj.Name() == name && obj.Pkg() != nil && obj.Pkg().Path() == lispPkgPath
}

// isLispConst reports whether e names the lisp constant name (lisp.LError).
func (s *state) isLispConst(e ast.Expr, name string) bool {
	var id *ast.Ident
	switch x := ast.Unparen(e).(type) {
	case *ast.Ident:
		id = x
	case *ast.SelectorExpr:
		id = x.Sel
	default:
		return false
	}
	c, ok := s.pass.TypesInfo.Uses[id].(*types.Const)
	return ok && c.Name() == name && c.Pkg() != nil && c.Pkg().Path() == lispPkgPath
}

// typeField returns x when e is x.Type for an LVal x, else nil.
func (s *state) typeField(e ast.Expr) ast.Expr {
	sel, ok := ast.Unparen(e).(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Type" {
		return nil
	}
	selection := s.pass.TypesInfo.Selections[sel]
	if selection == nil || selection.Kind() != types.FieldVal || !isLispNamed(derefType(selection.Recv()), "LVal") {
		return nil
	}
	return sel.X
}

func (s *state) text(n ast.Node) string {
	var buf bytes.Buffer
	if err := format.Node(&buf, s.pass.Fset, n); err != nil {
		return ""
	}
	return buf.String()
}

// source returns the bytes of the file between two positions.
func (s *state) source(from, to token.Pos) (string, bool) {
	file := s.pass.Fset.File(from)
	if file == nil {
		return "", false
	}
	data, ok := s.src[file.Name()]
	if !ok {
		var err error
		read := s.pass.ReadFile
		if read == nil {
			return "", false
		}
		data, err = read(file.Name())
		if err != nil {
			return "", false
		}
		s.src[file.Name()] = data
	}
	start, end := file.Offset(from), file.Offset(to)
	if start < 0 || end > len(data) || start > end {
		return "", false
	}
	return string(data[start:end]), true
}

// operand wraps x in parentheses when a method call on it needs them.
func (s *state) operand(x ast.Expr) string {
	t := s.text(x)
	switch ast.Unparen(x).(type) {
	case *ast.Ident, *ast.SelectorExpr, *ast.CallExpr, *ast.IndexExpr, *ast.ParenExpr:
		return t
	}
	return "(" + t + ")"
}

func (s *state) report(n ast.Node, category, msg string, fixes ...analysis.SuggestedFix) {
	if fixOnly && len(fixes) == 0 {
		return
	}
	s.pass.Report(analysis.Diagnostic{
		Pos:            n.Pos(),
		End:            n.End(),
		Category:       category,
		Message:        msg,
		SuggestedFixes: fixes,
	})
}

func replace(n ast.Node, msg, text string) analysis.SuggestedFix {
	return analysis.SuggestedFix{
		Message:   msg,
		TextEdits: []analysis.TextEdit{{Pos: n.Pos(), End: n.End(), NewText: []byte(text)}},
	}
}

func (s *state) checkFile(file *ast.File) {
	var stack []ast.Node
	ast.Inspect(file, func(n ast.Node) bool {
		if n == nil {
			stack = stack[:len(stack)-1]
			return false
		}
		stack = append(stack, n)
		switch x := n.(type) {
		case *ast.BinaryExpr:
			s.checkIsError(x)
			s.checkGoErrorNil(x)
			if len(stack) < 2 || !sameChain(stack[len(stack)-2], x) {
				s.checkIsSymbol(x)
			}
		case *ast.CallExpr:
			s.checkQExpr(x)
			s.checkResultAs(x)
			s.checkRegistration(x)
			s.checkMapOf(x)
			s.checkSortedMapCall(x)
			s.checkBuiltinHelper(x)
		case *ast.IfStmt:
			s.checkCheckAlloc(x)
			if len(stack) < 2 || !isElseOf(stack[len(stack)-2], x) {
				s.checkSeqBranches(x, ifBranches(x))
			}
		case *ast.SwitchStmt:
			s.checkSeqBranches(x, s.switchBranches(x))
		case *ast.AssignStmt:
			s.checkFunDocWrite(x)
		case *ast.RangeStmt:
			s.checkMapKeysRange(x, stack)
			s.checkMapEntriesRange(x, stack)
		case *ast.TypeAssertExpr:
			s.checkNativeAssert(x, stack)
		case *ast.FuncType:
			s.checkErrorValResult(x)
		case *ast.FuncDecl:
			if x.Body != nil {
				s.checkErrorReturns(x.Name, x.Body)
			}
		case *ast.FuncLit:
			s.checkErrorReturns(x, x.Body)
		case *ast.BlockStmt:
			s.checkArgCells(x)
			s.checkMapGetString(x, stack)
		}
		return true
	})
}

// checkIsError: x.Type == lisp.LError and x.Type != lisp.LError.
func (s *state) checkIsError(b *ast.BinaryExpr) {
	if b.Op != token.EQL && b.Op != token.NEQ {
		return
	}
	x := s.typeField(b.X)
	other := b.Y
	if x == nil {
		x, other = s.typeField(b.Y), b.X
	}
	if x == nil || !s.isLispConst(other, "LError") {
		return
	}
	text := s.operand(x) + ".IsError()"
	if b.Op == token.NEQ {
		text = "!" + text
	}
	s.report(b, CategoryInfo, "use "+text+", which is the same compare", replace(b, "Use IsError", text))
}

// cellsMethods maps a constructor that takes a cell slice to the lisp.Cells
// method that calls it on the receiver.
var cellsMethods = map[string]string{"QExpr": "List", "SExpr": "SExpr", "Vector": "Vector"}

// checkQExpr: lisp.QExpr, lisp.SExpr and lisp.Vector over a slice literal,
// and lisp.Array(nil, x).
func (s *state) checkQExpr(call *ast.CallExpr) {
	fn := s.lispFunc(call)
	if fn != nil && fn.Name() == "Array" && len(call.Args) == 2 && isNilIdent(s, call.Args[0]) {
		// Vector(cells) is Array(nil, cells) (lisp/lisp.go).
		if qual, ok := lispQualifier(call.Fun); ok {
			text := qual + "Vector(" + s.text(call.Args[1]) + ")"
			s.report(call, CategoryInfo, "use "+text+", which is Array(nil, ...)", replace(call, "Use lisp.Vector", text))
		}
		return
	}
	if fn == nil || len(call.Args) != 1 {
		return
	}
	method, ok := cellsMethods[fn.Name()]
	if !ok {
		return
	}
	lit, ok := ast.Unparen(call.Args[0]).(*ast.CompositeLit)
	if !ok {
		return
	}
	at, ok := lit.Type.(*ast.ArrayType)
	if !ok || at.Len != nil {
		return
	}
	qual, ok := lispQualifier(call.Fun)
	if !ok {
		return
	}
	elts, ok := s.source(lit.Lbrace+1, lit.Rbrace)
	if !ok {
		return
	}
	text := qual + "Cells{" + elts + "}." + method + "()"
	s.report(call, CategoryInfo, "use "+qual+"Cells{...}."+method+"(), which builds the same "+cellsNoun[method],
		replace(call, "Use lisp.Cells", text))
}

var cellsNoun = map[string]string{"List": "list", "SExpr": "s-expression", "Vector": "vector"}

// lispQualifier returns "lisp." (or the file's name for the lisp import)
// from a qualified call target.
func lispQualifier(fun ast.Expr) (string, bool) {
	sel, ok := ast.Unparen(fun).(*ast.SelectorExpr)
	if !ok {
		return "", false
	}
	id, ok := sel.X.(*ast.Ident)
	if !ok {
		return "", false
	}
	return id.Name + ".", true
}

// checkCheckAlloc: if msg := env.Runtime.CheckAlloc(n); msg != "" {
// return ..., env.Errorf("%s", msg) }.
func (s *state) checkCheckAlloc(stmt *ast.IfStmt) {
	init, ok := stmt.Init.(*ast.AssignStmt)
	if !ok || init.Tok != token.DEFINE || len(init.Lhs) != 1 || len(init.Rhs) != 1 || stmt.Else != nil {
		return
	}
	msgID, ok := init.Lhs[0].(*ast.Ident)
	if !ok {
		return
	}
	call, ok := ast.Unparen(init.Rhs[0]).(*ast.CallExpr)
	if !ok || s.lispMethod(call, "Runtime") == nil || s.callee(call).Name() != "CheckAlloc" || len(call.Args) != 1 {
		return
	}
	rtSel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
	if !ok {
		return
	}
	runtimeSel, ok := ast.Unparen(rtSel.X).(*ast.SelectorExpr)
	if !ok || runtimeSel.Sel.Name != "Runtime" {
		return
	}
	env := runtimeSel.X
	cond, ok := stmt.Cond.(*ast.BinaryExpr)
	if !ok || cond.Op != token.NEQ {
		return
	}
	if id, isID := ast.Unparen(cond.X).(*ast.Ident); !isID || id.Name != msgID.Name {
		return
	}
	if lit, isLit := ast.Unparen(cond.Y).(*ast.BasicLit); !isLit || lit.Value != `""` {
		return
	}
	if len(stmt.Body.List) != 1 {
		return
	}
	ret, ok := stmt.Body.List[0].(*ast.ReturnStmt)
	if !ok {
		return
	}
	errIdx := -1
	for i, r := range ret.Results {
		rc, ok := ast.Unparen(r).(*ast.CallExpr)
		if !ok || s.lispMethod(rc, "LEnv") == nil || s.callee(rc).Name() != "Errorf" || len(rc.Args) != 2 {
			continue
		}
		if f, isConst := s.constString(rc.Args[0]); !isConst || f != "%s" {
			continue
		}
		if id, isID := ast.Unparen(rc.Args[1]).(*ast.Ident); !isID || id.Name != msgID.Name {
			continue
		}
		recv, ok := ast.Unparen(rc.Fun).(*ast.SelectorExpr)
		if !ok || s.text(recv.X) != s.text(env) {
			continue
		}
		errIdx = i
	}
	if errIdx < 0 {
		return
	}
	results := make([]string, len(ret.Results))
	for i, r := range ret.Results {
		if i == errIdx {
			results[i] = "lerr"
			continue
		}
		results[i] = s.text(r)
	}
	text := fmt.Sprintf("if lerr := %s.CheckAlloc(%s); lerr.IsError() {\n\treturn %s\n}",
		s.operand(env), s.text(call.Args[0]), strings.Join(results, ", "))
	s.report(stmt, CategoryInfo, "use "+s.operand(env)+".CheckAlloc, which returns the same error",
		replace(stmt, "Use LEnv.CheckAlloc", text))
}

// mapChecked reports whether an enclosing if statement in stack tests
// m.Type == lisp.LSortMap for an m printed as name.
func (s *state) mapChecked(stack []ast.Node, name string) bool {
	for i := len(stack) - 1; i >= 0; i-- {
		ifs, ok := stack[i].(*ast.IfStmt)
		if !ok {
			continue
		}
		found := false
		ast.Inspect(ifs.Cond, func(n ast.Node) bool {
			b, ok := n.(*ast.BinaryExpr)
			if !ok || b.Op != token.EQL {
				return true
			}
			x := s.typeField(b.X)
			other := b.Y
			if x == nil {
				x, other = s.typeField(b.Y), b.X
			}
			if x != nil && s.isLispConst(other, "LSortMap") && s.text(x) == name {
				found = true
			}
			return !found
		})
		if found {
			// The check guards the body only.
			if i+1 < len(stack) && stack[i+1] == ifs.Body {
				return true
			}
		}
	}
	return false
}

// checkMapKeysRange: range m.MapKeys().Cells after a map check.
func (s *state) checkMapKeysRange(r *ast.RangeStmt, stack []ast.Node) {
	sel, ok := ast.Unparen(r.X).(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Cells" {
		return
	}
	call, ok := ast.Unparen(sel.X).(*ast.CallExpr)
	if !ok || s.lispMethod(call, "LVal") == nil || s.callee(call).Name() != "MapKeys" {
		return
	}
	fsel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
	if !ok {
		return
	}
	m := fsel.X
	if !s.mapChecked(stack, s.text(m)) {
		return
	}
	s.report(r.X, CategoryInfo, "range "+s.operand(m)+".Keys() walks the keys without building a list; "+
		"the key becomes a lisp.MapKey, so this is a hint, not a fix")
}

// checkMapGetString: v := m.MapGetString(k) (or the if-init form), then
// v.Type == lisp.LString and v.Str, after a map check.
func (s *state) checkMapGetString(block *ast.BlockStmt, stack []ast.Node) {
	check := func(assign *ast.AssignStmt, uses ast.Node) {
		if assign == nil || assign.Tok != token.DEFINE || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 {
			return
		}
		id, ok := assign.Lhs[0].(*ast.Ident)
		if !ok {
			return
		}
		call, ok := ast.Unparen(assign.Rhs[0]).(*ast.CallExpr)
		if !ok || s.lispMethod(call, "LVal") == nil || s.callee(call).Name() != "MapGetString" {
			return
		}
		fsel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
		if !ok {
			return
		}
		m := fsel.X
		if !s.mapChecked(stack, s.text(m)) {
			return
		}
		typed, str := false, false
		ast.Inspect(uses, func(n ast.Node) bool {
			switch x := n.(type) {
			case *ast.BinaryExpr:
				if v := s.typeField(x.X); v != nil && s.text(v) == id.Name && s.isLispConst(x.Y, "LString") {
					typed = true
				}
			case *ast.SelectorExpr:
				if v, ok := x.X.(*ast.Ident); ok && v.Name == id.Name && x.Sel.Name == "Str" {
					str = true
				}
			}
			return true
		})
		if typed && str {
			s.report(call, CategoryInfo, fmt.Sprintf("lisp.Field[string](%s, %s) reads a string field in one call; "+
				"it returns ok=false for a missing key or another type", s.text(m), s.text(call.Args[0])))
		}
	}
	for i, st := range block.List {
		switch x := st.(type) {
		case *ast.IfStmt:
			if a, ok := x.Init.(*ast.AssignStmt); ok {
				check(a, x)
			}
		case *ast.AssignStmt:
			if i+1 < len(block.List) {
				check(x, &ast.BlockStmt{List: block.List[i+1:]})
			}
		}
	}
}

// checkArgCells: v := args.Cells[i], or a tuple a, b := args.Cells[0],
// args.Cells[1], followed by an if statement that tests the Type of one of
// the variables and returns an Errorf call.
func (s *state) checkArgCells(block *ast.BlockStmt) {
	for i, st := range block.List {
		assign, ok := st.(*ast.AssignStmt)
		if !ok || assign.Tok != token.DEFINE || len(assign.Lhs) != len(assign.Rhs) {
			continue
		}
		var names []string
		for j, l := range assign.Lhs {
			id, ok := l.(*ast.Ident)
			if !ok || id.Name == "_" || !s.isArgCell(assign.Rhs[j]) {
				continue
			}
			names = append(names, id.Name)
		}
		if len(names) == 0 {
			continue
		}
	next:
		for _, next := range block.List[i+1:] {
			ifs, ok := next.(*ast.IfStmt)
			if !ok {
				continue
			}
			for _, name := range names {
				if s.testsType(ifs.Cond, name) && returnsErrorf(s, ifs.Body) {
					s.report(assign, CategoryInfo, "an ArgReader read (lisp.ReadArgs, then String, Int, Map, ...) decodes "+
						name+" and records the type error; check a.Err() once")
					break next
				}
			}
		}
	}
}

// isArgCell reports whether e is x.Cells[n] for an LVal x and a literal n.
func (s *state) isArgCell(e ast.Expr) bool {
	idx, ok := ast.Unparen(e).(*ast.IndexExpr)
	if !ok {
		return false
	}
	sel, ok := ast.Unparen(idx.X).(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Cells" || s.typeFieldOwner(sel) == nil {
		return false
	}
	_, ok = ast.Unparen(idx.Index).(*ast.BasicLit)
	return ok
}

// typeFieldOwner returns x when sel is x.Cells for an LVal x.
func (s *state) typeFieldOwner(sel *ast.SelectorExpr) ast.Expr {
	selection := s.pass.TypesInfo.Selections[sel]
	if selection == nil || selection.Kind() != types.FieldVal || !isLispNamed(derefType(selection.Recv()), "LVal") {
		return nil
	}
	return sel.X
}

func (s *state) testsType(cond ast.Expr, name string) bool {
	found := false
	ast.Inspect(cond, func(n ast.Node) bool {
		if b, ok := n.(*ast.BinaryExpr); ok && (b.Op == token.NEQ || b.Op == token.EQL) {
			if v := s.typeField(b.X); v != nil && s.text(v) == name {
				found = true
			}
		}
		return !found
	})
	return found
}

func returnsErrorf(s *state, body *ast.BlockStmt) bool {
	found := false
	ast.Inspect(body, func(n ast.Node) bool {
		if ret, ok := n.(*ast.ReturnStmt); ok {
			for _, r := range ret.Results {
				if c, ok := ast.Unparen(r).(*ast.CallExpr); ok {
					if fn := s.callee(c); fn != nil && (fn.Name() == "Errorf" || fn.Name() == "ErrorConditionf") {
						found = true
					}
				}
			}
		}
		return !found
	})
	return found
}

// checkErrorValResult: a *lisp.ErrorVal in a result list.
func (s *state) checkErrorValResult(ft *ast.FuncType) {
	if ft.Results == nil {
		return
	}
	for _, field := range ft.Results.List {
		t := s.pass.TypesInfo.TypeOf(field.Type)
		if p, ok := types.Unalias(t).(*types.Pointer); ok && isLispNamed(p.Elem(), "ErrorVal") {
			s.report(field.Type, CategoryError, "a *lisp.ErrorVal result is a typed nil when it is nil, which is a non-nil error; "+
				"return error, and use lisp.GoError or lisp.Result to make one")
		}
	}
}

// checkResultAs: lisp.ResultAs[T](env.CallBuiltin(ref, ...)) for a builtin
// whose result type varies, and lisp.ResultAs[T](env.MapLookup(...)).
func (s *state) checkResultAs(call *ast.CallExpr) {
	fn := s.lispFunc(call)
	if fn == nil || fn.Name() != "ResultAs" || len(call.Args) != 1 {
		return
	}
	if t := s.pass.TypesInfo.TypeOf(call); t != nil {
		if tuple, ok := t.(*types.Tuple); ok && tuple.Len() == 2 {
			if p, ok := types.Unalias(tuple.At(0).Type()).(*types.Pointer); ok && isLispNamed(p.Elem(), "LVal") {
				return // ResultAs[*lisp.LVal] accepts every value
			}
		}
	}
	inner, ok := ast.Unparen(call.Args[0]).(*ast.CallExpr)
	if !ok {
		return
	}
	m := s.lispMethod(inner, "LEnv")
	if m == nil {
		return
	}
	name := ""
	switch m.Name() {
	case "MapLookup":
		name = "get"
	case "CallBuiltin":
		if len(inner.Args) == 0 {
			return
		}
		name = s.builtinName(inner.Args[0])
	}
	if !varyingBuiltins[name] {
		return
	}
	s.report(call, CategoryError, fmt.Sprintf("the result type of %s varies with the data, so lisp.ResultAs fails at run time on "+
		"another type; use lisp.Result and test the type", name))
}

// checkRegistration: a call that passes lisp.Formals(...) and a Func*E
// builtin whose argument count the formals do not match.
func (s *state) checkRegistration(call *ast.CallExpr) {
	var formals *ast.CallExpr
	arity := 0
	var builtinArg ast.Expr
	for _, arg := range call.Args {
		c, ok := ast.Unparen(arg).(*ast.CallExpr)
		if ok {
			if fn := s.lispFunc(c); fn != nil && fn.Name() == "Formals" {
				formals = c
				continue
			}
			if fn := s.lispFunc(c); fn != nil && errorBindings[fn.Name()] > 0 {
				arity, builtinArg = errorBindings[fn.Name()], arg
				continue
			}
		}
		var id *ast.Ident
		switch x := ast.Unparen(arg).(type) {
		case *ast.Ident:
			id = x
		case *ast.SelectorExpr:
			id = x.Sel
		}
		if id != nil {
			if n, ok := s.arities[s.pass.TypesInfo.Uses[id]]; ok {
				arity, builtinArg = n, arg
			}
		}
	}
	if formals == nil || arity == 0 {
		return
	}
	required := 0
	for _, a := range formals.Args {
		name, ok := s.constString(a)
		if !ok {
			return // not a constant: unknown
		}
		if strings.HasPrefix(name, "&") {
			s.report(formals, CategoryError, fmt.Sprintf("%s is a Func*E builtin, which takes %d required arguments only; "+
				"its formals declare %s: use Func1, Func2 or Func3 with Opt decoders, or a plain LBuiltin", s.text(builtinArg), arity, name))
			return
		}
		required++
	}
	if required != arity {
		s.report(formals, CategoryError, fmt.Sprintf("%s takes exactly %d arguments, but its formals declare %d", s.text(builtinArg), arity, required))
	}
}

// funcBody is a function literal or declaration the wrap rule reads.
type funcBody struct {
	body *ast.BlockStmt
	fn   *types.Func // nil for a literal
}

// checkErrorWraps reports fmt.Errorf and errors.New(err.Error()) over a Lisp
// error, inside FuncE and Func*E bodies and the functions they return the
// errors of.
func (s *state) checkErrorWraps() {
	var work []funcBody
	scoped := make(map[*types.Func]bool)
	add := func(fn *types.Func) {
		fn = fn.Origin()
		if scoped[fn] {
			return
		}
		fd := s.decls[fn]
		if fd == nil || fd.Body == nil {
			return
		}
		scoped[fn] = true
		work = append(work, funcBody{body: fd.Body, fn: fn})
	}
	for _, file := range s.pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok || len(call.Args) == 0 {
				return true
			}
			fn := s.lispFunc(call)
			if fn == nil {
				return true
			}
			if _, ok := errorBindings[fn.Name()]; !ok {
				return true
			}
			switch b := ast.Unparen(call.Args[len(call.Args)-1]).(type) {
			case *ast.FuncLit:
				work = append(work, funcBody{body: b.Body})
			case *ast.Ident:
				if f, ok := s.pass.TypesInfo.Uses[b].(*types.Func); ok {
					add(f)
				}
			case *ast.SelectorExpr:
				if f, ok := s.pass.TypesInfo.Uses[b.Sel].(*types.Func); ok {
					add(f)
				}
			}
			return true
		})
	}
	// Close the set over the functions whose errors a scoped body returns.
	for i := 0; i < len(work); i++ { //nolint:intrange // add appends to work inside the loop, and a range over len(work) reads the length once
		for _, f := range s.returnedCallees(work[i].body) {
			add(f)
		}
	}
	for _, w := range work {
		s.checkWrapsIn(w.body, scoped)
	}
}

// returnedCallees returns the same-package functions whose results body
// returns: called in a return statement, or assigned to a variable that a
// return statement names.
func (s *state) returnedCallees(body *ast.BlockStmt) []*types.Func {
	returned := make(map[types.Object]bool)
	var out []*types.Func
	ast.Inspect(body, func(n ast.Node) bool {
		ret, ok := n.(*ast.ReturnStmt)
		if !ok {
			return true
		}
		for _, r := range ret.Results {
			switch x := ast.Unparen(r).(type) {
			case *ast.Ident:
				if obj := s.pass.TypesInfo.Uses[x]; obj != nil {
					returned[obj] = true
				}
			case *ast.CallExpr:
				if fn := s.callee(x); fn != nil && fn.Pkg() == s.pass.Pkg {
					out = append(out, fn)
				}
			}
		}
		return true
	})
	ast.Inspect(body, func(n ast.Node) bool {
		assign, ok := n.(*ast.AssignStmt)
		if !ok || len(assign.Rhs) != 1 {
			return true
		}
		call, ok := ast.Unparen(assign.Rhs[0]).(*ast.CallExpr)
		if !ok {
			return true
		}
		fn := s.callee(call)
		if fn == nil || fn.Pkg() != s.pass.Pkg {
			return true
		}
		for _, l := range assign.Lhs {
			if id, ok := l.(*ast.Ident); ok {
				obj := s.pass.TypesInfo.ObjectOf(id)
				if obj != nil && returned[obj] {
					out = append(out, fn)
				}
			}
		}
		return true
	})
	return out
}

// lispErrorCall reports whether call returns a Lisp error as its error
// result: lisp.Result, lisp.ResultAs, lisp.GoError, or a scoped function.
func (s *state) lispErrorCall(call *ast.CallExpr, scoped map[*types.Func]bool) bool {
	fn := s.callee(call)
	if fn == nil {
		return false
	}
	if fn.Pkg() != nil && fn.Pkg().Path() == lispPkgPath && fn.Signature().Recv() == nil {
		switch fn.Name() {
		case "Result", "ResultAs", "GoError":
			return true
		}
	}
	return scoped[fn.Origin()]
}

func (s *state) checkWrapsIn(body *ast.BlockStmt, scoped map[*types.Func]bool) {
	lispErrs := make(map[types.Object]bool)
	ast.Inspect(body, func(n ast.Node) bool {
		assign, ok := n.(*ast.AssignStmt)
		if !ok || len(assign.Rhs) != 1 {
			return true
		}
		call, ok := ast.Unparen(assign.Rhs[0]).(*ast.CallExpr)
		if !ok || !s.lispErrorCall(call, scoped) {
			return true
		}
		for _, l := range assign.Lhs {
			id, ok := l.(*ast.Ident)
			if !ok {
				continue
			}
			obj := s.pass.TypesInfo.ObjectOf(id)
			if obj == nil || !types.Identical(obj.Type(), types.Universe.Lookup("error").Type()) {
				continue
			}
			lispErrs[obj] = true
		}
		return true
	})
	isLispErr := func(e ast.Expr) bool {
		switch x := ast.Unparen(e).(type) {
		case *ast.Ident:
			return lispErrs[s.pass.TypesInfo.Uses[x]]
		case *ast.CallExpr:
			return s.lispErrorCall(x, scoped)
		}
		return false
	}
	ast.Inspect(body, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		fn := s.callee(call)
		if fn == nil || fn.Pkg() == nil {
			return true
		}
		switch {
		case fn.Pkg().Path() == "fmt" && fn.Name() == "Errorf":
			for _, a := range call.Args[1:] {
				if isLispErr(a) {
					s.report(call, CategoryError, "fmt.Errorf over a Lisp error gives the error condition \"error\", so "+
						"handler-bind no longer sees the inner condition; return the error as is, or raise a new one with env.Errorf")
					return true
				}
			}
		case fn.Pkg().Path() == "errors" && fn.Name() == "New" && len(call.Args) == 1:
			inner, ok := ast.Unparen(call.Args[0]).(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := ast.Unparen(inner.Fun).(*ast.SelectorExpr)
			if ok && sel.Sel.Name == "Error" && isLispErr(sel.X) {
				s.report(call, CategoryError, "errors.New(err.Error()) over a Lisp error drops its condition, data and stack; "+
					"return the error as is, or raise a new one with env.Errorf")
			}
		}
		return true
	})
}

// checkMapOf: the static types of LEnv.MapOf's keys and values.
func (s *state) checkMapOf(call *ast.CallExpr) {
	m := s.lispMethod(call, "LEnv")
	if m == nil || m.Name() != "MapOf" || call.Ellipsis.IsValid() {
		return
	}
	for i, arg := range call.Args {
		t := s.pass.TypesInfo.TypeOf(arg)
		if t == nil {
			continue
		}
		if i%2 == 0 {
			if !s.mapOfKeyType(t) {
				s.report(arg, CategoryError, fmt.Sprintf("MapOf key of type %s panics at run time; a key is a string or an *LVal", t))
			}
			continue
		}
		if !s.mapOfValueType(t) {
			s.report(arg, CategoryError, fmt.Sprintf("MapOf value of type %s panics at run time; a value is *LVal, string, int, "+
				"float64, bool, []byte, []*LVal or Cells (use lisp.NativeOf for a native, lisp.StringList for a []string)", t))
		}
	}
}

func (s *state) mapOfKeyType(t types.Type) bool {
	if isBasic(t, types.String, types.UntypedString) {
		return true
	}
	p, ok := types.Unalias(t).(*types.Pointer)
	return ok && isLispNamed(p.Elem(), "LVal")
}

func (s *state) mapOfValueType(t types.Type) bool {
	if isBasic(t, types.String, types.UntypedString, types.Int, types.UntypedInt, types.Float64,
		types.UntypedFloat, types.Bool, types.UntypedBool, types.UntypedNil) {
		return true
	}
	if isLispNamed(t, "Cells") {
		return true
	}
	if p, ok := types.Unalias(t).(*types.Pointer); ok && isLispNamed(p.Elem(), "LVal") {
		return true
	}
	if sl, ok := types.Unalias(t).(*types.Slice); ok {
		if b, ok := sl.Elem().(*types.Basic); ok && b.Kind() == types.Byte {
			return true
		}
		if p, ok := types.Unalias(sl.Elem()).(*types.Pointer); ok && isLispNamed(p.Elem(), "LVal") {
			return true
		}
	}
	return false
}

// isBasic reports whether t is exactly one of the basic kinds; a named type
// over one is not.
func isBasic(t types.Type, kinds ...types.BasicKind) bool {
	b, ok := types.Unalias(t).(*types.Basic)
	if !ok {
		return false
	}
	for _, k := range kinds {
		if b.Kind() == k {
			return true
		}
	}
	return false
}

// checkSortedMapCall: env.CallBuiltin(sortedMap, lisp.String("k"), v, ...)
// with literal string keys.
func (s *state) checkSortedMapCall(call *ast.CallExpr) {
	m := s.lispMethod(call, "LEnv")
	if m == nil || m.Name() != "CallBuiltin" || len(call.Args) < 3 || len(call.Args)%2 == 0 || call.Ellipsis.IsValid() {
		return
	}
	if s.builtinName(call.Args[0]) != "sorted-map" {
		return
	}
	recv, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
	if !ok {
		return
	}
	parts := make([]string, 0, len(call.Args)-1)
	for i, arg := range call.Args[1:] {
		if i%2 == 0 {
			k, ok := ast.Unparen(arg).(*ast.CallExpr)
			if !ok || len(k.Args) != 1 {
				return
			}
			if fn := s.lispFunc(k); fn == nil || fn.Name() != "String" {
				return
			}
			if _, ok := s.constString(k.Args[0]); !ok {
				return
			}
			parts = append(parts, s.text(k.Args[0]))
			continue
		}
		if !s.mapOfValueType(s.pass.TypesInfo.TypeOf(arg)) {
			return
		}
		parts = append(parts, s.text(arg))
	}
	text := s.operand(recv.X) + ".MapOf(" + strings.Join(parts, ", ") + ")"
	s.report(call, CategoryInfo, "use "+s.operand(recv.X)+".MapOf with Go string keys, which returns the same map or error",
		replace(call, "Use LEnv.MapOf", text))
}

// builtinHelpers maps a builtin name to the LEnv helper that returns the
// same value or error from the same checks, and to the argument count that
// CallBuiltin must pass.  A count of -1 is format-string's: one or more.
var builtinHelpers = map[string]struct {
	helper string
	nargs  int
}{
	"assoc!":        {"MapPut", 3},
	"get":           {"MapLookup", 2},
	"to-string":     {"ToString", 1},
	"format-string": {"FormatString", -1},
}

// checkBuiltinHelper: env.CallBuiltin(ref, ...) for assoc!, get, to-string
// and format-string.  Each helper checks the context first, as CallBuiltin
// does, and then runs the builtin's own body, so the result and the error
// are the same (lisp/goport_test.go, TestMapPutParity and its siblings).
func (s *state) checkBuiltinHelper(call *ast.CallExpr) {
	m := s.lispMethod(call, "LEnv")
	if m == nil || m.Name() != "CallBuiltin" || len(call.Args) == 0 || call.Ellipsis.IsValid() {
		return
	}
	h, ok := builtinHelpers[s.builtinName(call.Args[0])]
	if !ok {
		return
	}
	args := call.Args[1:]
	parts := make([]string, 0, len(args))
	switch {
	case h.nargs >= 0:
		if len(args) != h.nargs {
			return // a different count is an arity error that the helper does not make
		}
		for _, a := range args {
			parts = append(parts, s.text(a))
		}
	default:
		// FormatString takes the format as a Go string.  Only a
		// lisp.String(f) format is surely a string; for any other value,
		// the helper would drop format-string's check of the format's type.
		if len(args) == 0 {
			return
		}
		f, ok := ast.Unparen(args[0]).(*ast.CallExpr)
		if !ok || len(f.Args) != 1 || f.Ellipsis.IsValid() {
			return
		}
		if fn := s.lispFunc(f); fn == nil || fn.Name() != "String" {
			return
		}
		parts = append(parts, s.text(f.Args[0]))
		for _, a := range args[1:] {
			parts = append(parts, s.text(a))
		}
	}
	recv, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
	if !ok {
		return
	}
	helper := s.operand(recv.X) + "." + h.helper
	s.report(call, CategoryInfo, "use "+helper+", which returns the same value or error from the same checks",
		replace(call, "Use LEnv."+h.helper, helper+"("+strings.Join(parts, ", ")+")"))
}

// collectFunVars records the variables that hold a lisp.FunInPackage result.
// A variable that is also assigned another value is not recorded.
func (s *state) collectFunVars() {
	note := func(id *ast.Ident, value ast.Expr) {
		obj := s.pass.TypesInfo.ObjectOf(id)
		if obj == nil {
			return
		}
		isFun := false
		if call, ok := ast.Unparen(value).(*ast.CallExpr); ok {
			fn := s.lispFunc(call)
			isFun = fn != nil && fn.Name() == "FunInPackage"
		}
		if prev, seen := s.funVars[obj]; seen && !prev {
			return
		}
		s.funVars[obj] = isFun
	}
	for _, file := range s.pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			switch x := n.(type) {
			case *ast.AssignStmt:
				for i, l := range x.Lhs {
					id, ok := ast.Unparen(l).(*ast.Ident)
					if !ok {
						continue
					}
					if len(x.Lhs) != len(x.Rhs) {
						note(id, nil)
						continue
					}
					note(id, x.Rhs[i])
				}
			case *ast.ValueSpec:
				for i, id := range x.Names {
					if len(x.Names) != len(x.Values) {
						note(id, nil)
						continue
					}
					note(id, x.Values[i])
				}
			}
			return true
		})
	}
}

// checkFunDocWrite: fn.Cells[1] = doc, or fn.Cells[1].Str = doc, for an fn
// that holds a lisp.FunInPackage result.  The second cell of a function
// value is its docstring, which FunInPackageDoc sets.
func (s *state) checkFunDocWrite(assign *ast.AssignStmt) {
	if assign.Tok != token.ASSIGN {
		return
	}
	for _, l := range assign.Lhs {
		target := ast.Unparen(l)
		if sel, ok := target.(*ast.SelectorExpr); ok && sel.Sel.Name == "Str" {
			target = ast.Unparen(sel.X)
		}
		idx, ok := target.(*ast.IndexExpr)
		if !ok {
			continue
		}
		if tv, ok := s.pass.TypesInfo.Types[idx.Index]; !ok || tv.Value == nil || tv.Value.String() != "1" {
			continue
		}
		cells, ok := ast.Unparen(idx.X).(*ast.SelectorExpr)
		if !ok || cells.Sel.Name != "Cells" || s.typeFieldOwner(cells) == nil {
			continue
		}
		id, ok := ast.Unparen(cells.X).(*ast.Ident)
		if !ok || !s.funVars[s.pass.TypesInfo.Uses[id]] {
			continue
		}
		s.report(l, CategoryInfo, "lisp.FunInPackageDoc(pkg, fid, formals, fn, doc) sets the docstring when it builds "+
			id.Name+"; the doc is a Go string, so this is a hint, not a fix")
	}
}

// branch is one arm of a switch statement or an if-else chain: the tests
// that select it and its body.
type branch struct {
	conds []ast.Expr // boolean tests; nil for a tag switch
	types []ast.Expr // the case values of a switch on x.Type
	body  []ast.Stmt
}

func isElseOf(parent ast.Node, ifs *ast.IfStmt) bool {
	p, ok := parent.(*ast.IfStmt)
	return ok && p.Else == ifs
}

// ifBranches returns the arms of an if-else chain.
func ifBranches(ifs *ast.IfStmt) []branch {
	var out []branch
	for ifs != nil {
		out = append(out, branch{conds: []ast.Expr{ifs.Cond}, body: ifs.Body.List})
		switch e := ifs.Else.(type) {
		case *ast.IfStmt:
			ifs = e
		case *ast.BlockStmt:
			out = append(out, branch{body: e.List})
			ifs = nil
		default:
			ifs = nil
		}
	}
	return out
}

// switchBranches returns the arms of a switch statement.  For a switch on
// x.Type it also returns x as the tag.
func (s *state) switchBranches(sw *ast.SwitchStmt) []branch {
	var out []branch
	tag := sw.Tag != nil && s.typeField(sw.Tag) != nil
	for _, st := range sw.Body.List {
		cc, ok := st.(*ast.CaseClause)
		if !ok {
			continue
		}
		if tag {
			out = append(out, branch{types: cc.List, body: cc.Body})
			continue
		}
		out = append(out, branch{conds: cc.List, body: cc.Body})
	}
	return out
}

// typeTests returns, for a test of the form x.Type == lisp.C (alone or as a
// term of an && chain), the text of x and the constant expression C.
func (s *state) typeTests(e ast.Expr, add func(x string, c ast.Expr)) {
	b, ok := ast.Unparen(e).(*ast.BinaryExpr)
	if !ok {
		return
	}
	switch b.Op {
	case token.LAND:
		s.typeTests(b.X, add)
		s.typeTests(b.Y, add)
	case token.EQL:
		x, other := s.typeField(b.X), b.Y
		if x == nil {
			x, other = s.typeField(b.Y), b.X
		}
		if x != nil {
			add(s.text(x), other)
		}
	}
}

// checkSeqBranches: a switch or an if-else chain on v.Type with an LSExpr
// arm that reads v.Cells and an LArray arm that loops over
// v.ArrayIndex(lisp.Int(i)).  v.SeqCells() returns both.
func (s *state) checkSeqBranches(stmt ast.Node, branches []branch) {
	tagX := ""
	if sw, ok := stmt.(*ast.SwitchStmt); ok && sw.Tag != nil {
		if x := s.typeField(sw.Tag); x != nil {
			tagX = s.text(x)
		}
	}
	listArm := make(map[string]bool)
	arrayArm := make(map[string]bool)
	for _, br := range branches {
		tested := make(map[string]map[string]bool)
		add := func(x string, c ast.Expr) {
			for _, name := range []string{"LSExpr", "LArray"} {
				if s.isLispConst(c, name) {
					if tested[x] == nil {
						tested[x] = make(map[string]bool)
					}
					tested[x][name] = true
				}
			}
		}
		for _, c := range br.types {
			add(tagX, c)
		}
		for _, c := range br.conds {
			s.typeTests(c, add)
		}
		for x, names := range tested {
			if x == "" {
				continue
			}
			body := &ast.BlockStmt{List: br.body}
			if names["LSExpr"] && !names["LArray"] && s.readsCells(body, x) {
				listArm[x] = true
			}
			if names["LArray"] && !names["LSExpr"] && s.loopsArrayIndex(body, x) {
				arrayArm[x] = true
			}
		}
	}
	xs := make([]string, 0, len(listArm))
	for x := range listArm {
		xs = append(xs, x)
	}
	sort.Strings(xs)
	for _, x := range xs {
		if arrayArm[x] {
			s.report(stmt, CategoryInfo, fmt.Sprintf("%s.SeqCells() (or lisp.SeqOf[T](%s)) returns the cells of a list or a "+
				"one-dimensional vector; it refuses a multi-dimensional array, and the cells are %s's own storage, so this "+
				"is a hint, not a fix", x, x, x))
			return
		}
	}
}

// readsCells reports whether body reads x.Cells for an LVal x printed as x.
func (s *state) readsCells(body ast.Node, x string) bool {
	found := false
	ast.Inspect(body, func(n ast.Node) bool {
		if sel, ok := n.(*ast.SelectorExpr); ok && sel.Sel.Name == "Cells" {
			if owner := s.typeFieldOwner(sel); owner != nil && s.text(owner) == x {
				found = true
			}
		}
		return !found
	})
	return found
}

// loopsArrayIndex reports whether body holds a loop that calls
// x.ArrayIndex(lisp.Int(...)).
func (s *state) loopsArrayIndex(body ast.Node, x string) bool {
	found := false
	ast.Inspect(body, func(n ast.Node) bool {
		var loop ast.Node
		switch l := n.(type) {
		case *ast.ForStmt:
			loop = l.Body
		case *ast.RangeStmt:
			loop = l.Body
		default:
			return !found
		}
		ast.Inspect(loop, func(m ast.Node) bool {
			call, ok := m.(*ast.CallExpr)
			if !ok || len(call.Args) != 1 {
				return !found
			}
			if fn := s.lispMethod(call, "LVal"); fn == nil || fn.Name() != "ArrayIndex" {
				return !found
			}
			fsel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
			if !ok || s.text(fsel.X) != x {
				return !found
			}
			if arg, ok := ast.Unparen(call.Args[0]).(*ast.CallExpr); ok {
				if fn := s.lispFunc(arg); fn != nil && fn.Name() == "Int" {
					found = true
				}
			}
			return !found
		})
		return !found
	})
	return found
}

// checkNativeAssert: v.Native.(T) where a test of v.Type == lisp.LNative
// guards it.  lisp.NativeValue[T](v) makes both tests.
func (s *state) checkNativeAssert(ta *ast.TypeAssertExpr, stack []ast.Node) {
	if ta.Type == nil {
		return // a type switch
	}
	sel, ok := ast.Unparen(ta.X).(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Native" {
		return
	}
	selection := s.pass.TypesInfo.Selections[sel]
	if selection == nil || selection.Kind() != types.FieldVal || !isLispNamed(derefType(selection.Recv()), "LVal") {
		return
	}
	x := s.text(sel.X)
	if !s.nativeGuarded(stack, x) {
		return
	}
	s.report(ta, CategoryInfo, fmt.Sprintf("lisp.NativeValue[%s](%s) tests %s.Type == lisp.LNative and the payload's type "+
		"in one call, so the separate type test can go; this is a hint, not a fix", s.text(ta.Type), x, x))
}

// nativeGuarded reports whether a test of x.Type against lisp.LNative
// guards the top node of stack: an enclosing if body or case clause that
// selects x.Type == lisp.LNative, an earlier if statement that returns when
// x.Type != lisp.LNative, or such an if statement right after the statement
// that holds the node.
func (s *state) nativeGuarded(stack []ast.Node, x string) bool {
	for i := len(stack) - 2; i >= 0; i-- {
		child := stack[i+1]
		switch p := stack[i].(type) {
		case *ast.FuncLit, *ast.FuncDecl:
			return false // a test outside the function does not guard it when it runs
		case *ast.IfStmt:
			if child == p.Body && s.selectsNative(p.Cond, x) {
				return true
			}
		case *ast.CaseClause:
			if s.caseSelectsNative(stack[:i], p, x) {
				return true
			}
			if s.listGuards(p.Body, child, x) {
				return true
			}
		case *ast.BlockStmt:
			if s.listGuards(p.List, child, x) {
				return true
			}
		}
	}
	return false
}

// selectsNative reports whether cond, alone or as a term of an && chain,
// tests x.Type == lisp.LNative.
func (s *state) selectsNative(cond ast.Expr, x string) bool {
	found := false
	s.typeTests(cond, func(v string, c ast.Expr) {
		if v == x && s.isLispConst(c, "LNative") {
			found = true
		}
	})
	return found
}

// caseSelectsNative reports whether cc is "case lisp.LNative:" of a switch
// on x.Type, or a case of a tagless switch whose one test selects
// x.Type == lisp.LNative.  outer is the stack above cc.
func (s *state) caseSelectsNative(outer []ast.Node, cc *ast.CaseClause, x string) bool {
	if len(cc.List) != 1 {
		return false
	}
	for i := len(outer) - 1; i >= 0; i-- {
		sw, ok := outer[i].(*ast.SwitchStmt)
		if !ok {
			continue
		}
		if sw.Tag == nil {
			return s.selectsNative(cc.List[0], x)
		}
		v := s.typeField(sw.Tag)
		return v != nil && s.text(v) == x && s.isLispConst(cc.List[0], "LNative")
	}
	return false
}

// listGuards reports whether, in the statement list, an if statement before
// child returns when x.Type != lisp.LNative.  It also accepts such an if
// statement right after child when child is an assignment.
func (s *state) listGuards(list []ast.Stmt, child ast.Node, x string) bool {
	at := -1
	for j, st := range list {
		if st == child {
			at = j
			break
		}
	}
	if at < 0 {
		return false
	}
	for j := 0; j < at; j++ {
		if s.returnsUnlessNative(list[j], x) {
			return true
		}
	}
	// A test right after the assertion guards it only when the statement
	// just stores the result.
	switch child.(type) {
	case *ast.AssignStmt, *ast.DeclStmt:
		return at+1 < len(list) && s.returnsUnlessNative(list[at+1], x)
	}
	return false
}

// returnsUnlessNative reports whether st is an if statement with no else
// whose body ends in a return and whose test, alone or as a term of an ||
// chain, is x.Type != lisp.LNative.
func (s *state) returnsUnlessNative(st ast.Stmt, x string) bool {
	ifs, ok := st.(*ast.IfStmt)
	if !ok || ifs.Else != nil || len(ifs.Body.List) == 0 {
		return false
	}
	if _, ok := ifs.Body.List[len(ifs.Body.List)-1].(*ast.ReturnStmt); !ok {
		return false
	}
	var walk func(e ast.Expr) bool
	walk = func(e ast.Expr) bool {
		b, ok := ast.Unparen(e).(*ast.BinaryExpr)
		if !ok {
			return false
		}
		switch b.Op {
		case token.LOR:
			return walk(b.X) || walk(b.Y)
		case token.NEQ:
			v, other := s.typeField(b.X), b.Y
			if v == nil {
				v, other = s.typeField(b.Y), b.X
			}
			return v != nil && s.text(v) == x && s.isLispConst(other, "LNative")
		}
		return false
	}
	return walk(ifs.Cond)
}

// boolChain reports whether n is an && or || expression.
func boolChain(n ast.Node) bool {
	b, ok := n.(*ast.BinaryExpr)
	return ok && (b.Op == token.LAND || b.Op == token.LOR)
}

// chainTerms returns the terms of an op chain (&& or ||) in source order.
// It does not look inside parentheses, so adjacent terms are adjacent in the
// source.
func chainTerms(e ast.Expr, op token.Token) []ast.Expr {
	b, ok := e.(*ast.BinaryExpr)
	if !ok || b.Op != op {
		return []ast.Expr{e}
	}
	return append(chainTerms(b.X, op), chainTerms(b.Y, op)...)
}

// pure reports whether e reads a value with no side effect: an identifier,
// or a selector or a constant index over one.  Reading it twice or once is
// the same.
func (s *state) pure(e ast.Expr) bool {
	switch x := ast.Unparen(e).(type) {
	case *ast.Ident:
		return true
	case *ast.SelectorExpr:
		return s.pure(x.X)
	case *ast.IndexExpr:
		tv, ok := s.pass.TypesInfo.Types[x.Index]
		return ok && tv.Value != nil && s.pure(x.X)
	}
	return false
}

// symbolTypeTest returns x for x.Type cmp lisp.LSymbol.
func (s *state) symbolTypeTest(e ast.Expr, cmp token.Token) ast.Expr {
	b, ok := e.(*ast.BinaryExpr)
	if !ok || b.Op != cmp {
		return nil
	}
	x, other := s.typeField(b.X), b.Y
	if x == nil {
		x, other = s.typeField(b.Y), b.X
	}
	if x == nil || !s.isLispConst(other, "LSymbol") {
		return nil
	}
	return x
}

// symbolNameTest returns x and name for x.Str cmp name, where name is a
// string constant or a pure string expression.
func (s *state) symbolNameTest(e ast.Expr, cmp token.Token) (ast.Expr, ast.Expr) {
	b, ok := e.(*ast.BinaryExpr)
	if !ok || b.Op != cmp {
		return nil, nil
	}
	for _, pair := range [][2]ast.Expr{{b.X, b.Y}, {b.Y, b.X}} {
		sel, ok := ast.Unparen(pair[0]).(*ast.SelectorExpr)
		if !ok || sel.Sel.Name != "Str" || s.typeFieldOwner(sel) == nil {
			continue
		}
		if _, ok := s.constString(pair[1]); ok {
			return sel.X, pair[1]
		}
		if s.pure(pair[1]) && isBasic(s.pass.TypesInfo.TypeOf(pair[1]), types.String) {
			return sel.X, pair[1]
		}
	}
	return nil, nil
}

// checkIsSymbol: x.Type == lisp.LSymbol && x.Str == name, as two adjacent
// terms of an && chain, and x.Type != lisp.LSymbol || x.Str != name, as two
// adjacent terms of an || chain.  x and name must be pure, so that the
// rewrite reads each one once with no change.
func (s *state) checkIsSymbol(b *ast.BinaryExpr) {
	cmp, neg := token.EQL, ""
	switch b.Op {
	case token.LAND:
	case token.LOR:
		cmp, neg = token.NEQ, "!"
	default:
		return
	}
	terms := chainTerms(b, b.Op)
	for i := 0; i+1 < len(terms); i++ {
		t1, t2 := terms[i], terms[i+1]
		x := s.symbolTypeTest(t1, cmp)
		nx, name := s.symbolNameTest(t2, cmp)
		if x == nil || nx == nil {
			nx, name = s.symbolNameTest(t1, cmp)
			x = s.symbolTypeTest(t2, cmp)
		}
		if x == nil || nx == nil || s.text(x) != s.text(nx) || !s.pure(x) {
			continue
		}
		text := neg + s.operand(x) + ".IsSymbol(" + s.text(name) + ")"
		s.pass.Report(analysis.Diagnostic{
			Pos:      t1.Pos(),
			End:      t2.End(),
			Category: CategoryInfo,
			Message:  "use " + text + ", which is the same compare",
			SuggestedFixes: []analysis.SuggestedFix{{
				Message:   "Use IsSymbol",
				TextEdits: []analysis.TextEdit{{Pos: t1.Pos(), End: t2.End(), NewText: []byte(text)}},
			}},
		})
		i++
	}
}

// sameChain reports whether parent continues the && or || chain of b.
func sameChain(parent ast.Node, b *ast.BinaryExpr) bool {
	p, ok := parent.(*ast.BinaryExpr)
	return ok && p.Op == b.Op && boolChain(p)
}

// checkGoErrorNil: lisp.GoError(x) != nil and lisp.GoError(x) == nil.
// GoError returns a non-nil error exactly when x.Type == lisp.LError.
func (s *state) checkGoErrorNil(b *ast.BinaryExpr) {
	if b.Op != token.EQL && b.Op != token.NEQ {
		return
	}
	call, other := ast.Unparen(b.X), b.Y
	if isNilIdent(s, call) {
		call, other = ast.Unparen(b.Y), b.X
	}
	c, ok := call.(*ast.CallExpr)
	if !ok || len(c.Args) != 1 || !isNilIdent(s, other) {
		return
	}
	if fn := s.lispFunc(c); fn == nil || fn.Name() != "GoError" {
		return
	}
	text := s.operand(c.Args[0]) + ".IsError()"
	if b.Op == token.EQL {
		text = "!" + text
	}
	s.report(b, CategoryInfo, "use "+text+", which is the same test: GoError returns nil exactly when the value is not an error",
		replace(b, "Use IsError", text))
}

func isNilIdent(s *state, e ast.Expr) bool {
	id, ok := ast.Unparen(e).(*ast.Ident)
	if !ok {
		return false
	}
	_, isNil := s.pass.TypesInfo.Uses[id].(*types.Nil)
	return isNil
}

// minErrorReturns is the count of env.Error(err) returns that makes a
// function a FuncE candidate.
const minErrorReturns = 3

// checkErrorReturns: a function with minErrorReturns or more statements of
// the form if err != nil { return ..., env.Error(err) }.  A FuncE body
// returns nil, err instead, and FuncE makes the Lisp error.
func (s *state) checkErrorReturns(at ast.Node, body *ast.BlockStmt) {
	n := 0
	ast.Inspect(body, func(node ast.Node) bool {
		switch x := node.(type) {
		case *ast.FuncLit:
			return false // counted on its own
		case *ast.IfStmt:
			if s.isErrorReturn(x) {
				n++
			}
		}
		return true
	})
	if n < minErrorReturns {
		return
	}
	s.report(at, CategoryInfo, fmt.Sprintf("%d returns of env.Error(err): a lisp.FuncE body returns nil, err, and FuncE "+
		"makes the Lisp error; FuncE returns a bare *lisp.ErrorVal as is, with its condition, and does not tell the "+
		"debugger about it a second time, so this is a hint, not a fix", n))
}

// isErrorReturn reports whether ifs is if err != nil { return ...,
// env.Error(err) } for an err of type error.
func (s *state) isErrorReturn(ifs *ast.IfStmt) bool {
	cond, ok := ifs.Cond.(*ast.BinaryExpr)
	if !ok || cond.Op != token.NEQ || !isNilIdent(s, cond.Y) || ifs.Else != nil || len(ifs.Body.List) != 1 {
		return false
	}
	errID, ok := ast.Unparen(cond.X).(*ast.Ident)
	if !ok {
		return false
	}
	errObj := s.pass.TypesInfo.Uses[errID]
	if errObj == nil || !types.Identical(errObj.Type(), types.Universe.Lookup("error").Type()) {
		return false
	}
	ret, ok := ifs.Body.List[0].(*ast.ReturnStmt)
	if !ok {
		return false
	}
	for _, r := range ret.Results {
		c, ok := ast.Unparen(r).(*ast.CallExpr)
		if !ok || len(c.Args) != 1 {
			continue
		}
		if m := s.lispMethod(c, "LEnv"); m == nil || m.Name() != "Error" {
			continue
		}
		if id, ok := ast.Unparen(c.Args[0]).(*ast.Ident); ok && s.pass.TypesInfo.Uses[id] == errObj {
			return true
		}
	}
	return false
}

// checkMapEntriesRange: range m.MapEntries().Cells after a map check.
func (s *state) checkMapEntriesRange(r *ast.RangeStmt, stack []ast.Node) {
	sel, ok := ast.Unparen(r.X).(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Cells" {
		return
	}
	call, ok := ast.Unparen(sel.X).(*ast.CallExpr)
	if !ok || s.lispMethod(call, "LVal") == nil || s.callee(call).Name() != "MapEntries" {
		return
	}
	fsel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
	if !ok {
		return
	}
	m := s.text(fsel.X)
	if !s.mapChecked(stack, m) && !s.mapCase(stack, m) {
		return
	}
	s.report(r.X, CategoryInfo, "for k, v := range "+s.operand(fsel.X)+".All() walks the entries without building a "+
		"list of pairs; the key becomes a lisp.MapKey, so this is a hint, not a fix")
}

// mapCase reports whether the top node of stack is in a case clause that
// selects a map m: case lisp.LSortMap of a switch on m.Type, or case
// lisp.ShapeMap of a switch on lisp.ShapeOf(m.Type).
func (s *state) mapCase(stack []ast.Node, m string) bool {
	for i := len(stack) - 2; i >= 1; i-- {
		switch p := stack[i].(type) {
		case *ast.FuncLit, *ast.FuncDecl:
			return false
		case *ast.CaseClause:
			if len(p.List) != 1 {
				continue
			}
			sw, ok := stack[i-2].(*ast.SwitchStmt) // the clause is in the switch's body
			if !ok || sw.Tag == nil {
				continue
			}
			tag := ast.Unparen(sw.Tag)
			want := "LSortMap"
			if c, ok := tag.(*ast.CallExpr); ok && len(c.Args) == 1 {
				if fn := s.lispFunc(c); fn != nil && fn.Name() == "ShapeOf" {
					tag, want = c.Args[0], "ShapeMap"
				}
			}
			if x := s.typeField(tag); x != nil && s.text(x) == m && s.isLispConst(p.List[0], want) {
				return true
			}
		}
	}
	return false
}
