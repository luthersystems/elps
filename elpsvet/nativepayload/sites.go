// Copyright © 2026 The ELPS authors

package nativepayload

import (
	"go/ast"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/analysis"
)

// nativeSite is one native construction.  Both analyzers read the same
// sites, so they cannot disagree about where a native is built.
type nativeSite struct {
	// payload is the payload's static type, nil when it has none.
	payload types.Type
	// file is the file of the site.
	file *ast.File
	// fn is the enclosing function declaration, or nil at package scope.
	// A site inside a closure reports the closure's declaration.
	fn *ast.FuncDecl
	// what names the spelling for a diagnostic.
	what string
	// also holds more positions whose lines a marker may sit on: a
	// literal's Native key and value.
	also []token.Pos
	// site is what the allowlist tier needs to know about the site.
	site payloadSite
	// pos is where the site is reported.
	pos token.Pos
	// address marks `&v.Native`, which has no payload type.
	address bool
}

// lines returns the positions whose lines a marker can cover.
func (s nativeSite) lines() []token.Pos {
	return append([]token.Pos{s.pos}, s.also...)
}

// walkNatives calls visit for every native construction in the package:
// in function bodies (closures included) and in package-level var and
// const initializers (function literals inside them included).  A native
// built at package scope is shared by every Runtime in the process before a
// template is even involved -- the ownership rule's territory, reached from
// the payload side.
func walkNatives(pass *analysis.Pass, visit func(nativeSite)) {
	inKernel := inKernelPkg(pass)
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			var fn *ast.FuncDecl
			var root ast.Node = decl
			if d, ok := decl.(*ast.FuncDecl); ok {
				if d.Body == nil {
					continue
				}
				fn, root = d, d.Body
			}
			emit := func(s nativeSite) {
				s.file, s.fn = file, fn
				s.site.inKernel = inKernel
				visit(s)
			}
			ast.Inspect(root, func(n ast.Node) bool {
				switch x := n.(type) {
				case *ast.CallExpr:
					nativeCall(pass, x, emit)
				case *ast.CompositeLit:
					nativeLiteral(pass, x, emit)
				case *ast.AssignStmt:
					nativeAssign(pass, x, emit)
				case *ast.UnaryExpr:
					nativeAddress(pass, x, emit)
				}
				return true
			})
		}
	}
}

// nativeCall handles lisp.Native(x), the typed lisp.NativeOf[T](x), and the
// lisp.Value(x) calls the compiler can see falling through to Native.
func nativeCall(pass *analysis.Pass, call *ast.CallExpr, emit func(nativeSite)) {
	if len(call.Args) != 1 {
		return
	}
	fn := calleeFunc(pass, call)
	if fn == nil || fn.Pkg() == nil || fn.Pkg().Path() != lispPkgPath {
		return
	}
	arg := call.Args[0]
	switch fn.Name() {
	case "Native":
	case "NativeOf":
		// Implemented as a call to Native, so it constructs exactly the
		// same value.  A generic instantiation still resolves to the
		// generic *types.Func, whose name is NativeOf and never Native, so
		// it needs its own arm -- without one the typed constructor is a
		// spelling the rule cannot see, and a rule that cannot see a
		// spelling fails open.
	case "Value":
		if directlyRepresentable(pass.TypesInfo.TypeOf(arg)) {
			// Value's type switch handles it without a Native.
			return
		}
	default:
		return
	}
	emit(nativeSite{
		pos:     call.Pos(),
		payload: pass.TypesInfo.TypeOf(arg),
		what:    "lisp." + fn.Name(),
		site:    payloadSite{kind: siteConstructor},
	})
}

// nativeLiteral handles a keyed literal setting the lisp.LVal.Native field
// -- `lisp.LVal{Native: x}`, and equally `lisp.ErrorVal{Native: x}`, since
// ErrorVal is a defined type over LVal and its keys resolve to the same
// field objects.  The literal's TYPE is not consulted; the key's object is.
//
// The SIBLING `Type:` key of the same literal IS consulted, because it is
// the header the kernel is building and the allowlist rows are claims about
// headers (payloadSite.exemptsRow).  The whole element list is read before
// anything is reported, so a literal that spells Native first is treated the
// same as one that spells Type first.
//
// The report sits on the literal's opening line, where a call would be
// reported, so a trailing marker on `&lisp.LVal{` covers a payload two
// lines down; a marker on the `Native:` line itself is honoured as well.
func nativeLiteral(pass *analysis.Pass, lit *ast.CompositeLit, emit func(nativeSite)) {
	native := nativeKeyValues(pass, lit)
	if len(native) == 0 {
		// Much the commonest case, and the reason the header type is
		// resolved only after it: every composite literal in the tree
		// reaches this function.
		return
	}
	header, hasTypeKey := literalHeader(pass, lit)
	for _, kv := range native {
		emit(nativeSite{
			pos:     lit.Pos(),
			also:    []token.Pos{kv.Key.Pos(), kv.Value.Pos()},
			payload: pass.TypesInfo.TypeOf(kv.Value),
			what:    "LVal.Native literal",
			site:    payloadSite{kind: siteHeaderLiteral, headerType: header, hasTypeKey: hasTypeKey},
		})
	}
}

// nativeKeyValues collects the literal's elements that set the
// lisp.LVal.Native FIELD, matched by the key's object rather than by the
// literal's spelled type.
func nativeKeyValues(pass *analysis.Pass, lit *ast.CompositeLit) []*ast.KeyValueExpr {
	var out []*ast.KeyValueExpr
	for _, elt := range lit.Elts {
		kv, ok := elt.(*ast.KeyValueExpr)
		if !ok {
			continue
		}
		key, ok := kv.Key.(*ast.Ident)
		if !ok || !isNativeField(pass.TypesInfo.Uses[key]) {
			continue
		}
		out = append(out, kv)
	}
	return out
}

// nativeAssign handles a write to the lisp.LVal.Native field on an existing
// value -- the bypass that can put a payload into a value the assigning
// function does not own -- however the field is reached.
func nativeAssign(pass *analysis.Pass, stmt *ast.AssignStmt, emit func(nativeSite)) {
	if len(stmt.Lhs) != len(stmt.Rhs) {
		// Multi-value RHS: the payload type is a tuple element, not an
		// expression type.  Documented blind spot (file comment).
		return
	}
	for i, lhs := range stmt.Lhs {
		sel, ok := ast.Unparen(lhs).(*ast.SelectorExpr)
		if !ok || !selectsNativeField(pass, sel) {
			continue
		}
		emit(nativeSite{
			pos:     stmt.Pos(),
			payload: pass.TypesInfo.TypeOf(stmt.Rhs[i]),
			what:    "LVal.Native assignment",
			site:    payloadSite{kind: siteFieldWrite},
		})
	}
}

// nativeAddress handles `&v.Native`: a pointer through which any payload
// can later be stored, with no type at this site to classify.
func nativeAddress(pass *analysis.Pass, expr *ast.UnaryExpr, emit func(nativeSite)) {
	if expr.Op != token.AND {
		return
	}
	sel, ok := ast.Unparen(expr.X).(*ast.SelectorExpr)
	if !ok || !selectsNativeField(pass, sel) {
		return
	}
	emit(nativeSite{pos: expr.Pos(), what: "address of LVal.Native", address: true})
}
