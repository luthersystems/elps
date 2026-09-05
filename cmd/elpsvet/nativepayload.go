// Copyright © 2026 The ELPS authors

// The elpsnativepayload analyzer is the fourth elpsvet rule: nothing
// plausibly MUTABLE may become a native payload unless a human has written
// down why sharing it is safe.
//
// # The invariant
//
// Fork copies the LVal spine and SHARES LVal.Native by reference (lisp/fork.go).
// A template is forked once per request, so a mutable native that reaches a
// template is state shared by every fork in the process.  The fork/isolation
// oracle in elpstest can only recognise a payload as stateful when its type
// declares lisp.NativeCloner; a mutable payload type that does not declare it
// is a leak channel no isolation test can see from the outside.  This rule
// is the static half of that audit, ported from the substrate repository's
// nativepayload analyzer and adapted to the module that DEFINES the
// constructors.
//
// # The rule
//
// A native construction -- `lisp.Native(x)`, the typed `lisp.NativeOf[T](x)`
// (inferred or explicitly instantiated), a `lisp.Value(x)` the compiler can
// see falling through to Native, a `lisp.LVal{Native: x}` literal, or a
// write to a `.Native` field -- is REPORTED unless one of:
//
//  1. the payload's static type has a BASIC underlying type (string, int,
//     bool, a float, a defined type over one of those, ...).  A non-pointer
//     value of basic underlying type inside an interface is immutable by
//     construction: it is not addressable, so every type assertion yields a
//     copy and no fork can reach the shared value.  A soundness decision,
//     not an allowlist one.  unsafe.Pointer is excluded -- a pointer wearing
//     a basic type's clothes;
//
//  2. the payload's type declares lisp.NativeCloner, i.e. its method set
//     carries `CloneNative() interface{}`.  That is the kernel's clone
//     protocol: Fork, detach and the lisp copy builtin all duplicate such a
//     payload instead of sharing it, and it is the same test the isolation
//     oracle applies, so the static rule and the dynamic one agree;
//
//  3. the payload's type is on allowedPayloadTypes below -- the AUDITED
//     inventory, each row carrying the reason a human checked;
//
//  4. an audited `//elpsvet:allow <justification>` comment covers the line
//     (trailing, or standalone on the line above), or the enclosing
//     function's doc comment carries one.  A bare marker with no
//     justification does NOT suppress: an allow that says nothing is a
//     classification nobody made.
//
// # Interface-typed payloads
//
// A payload whose static type is an interface (or a type parameter) has no
// type to classify: whatever it is at runtime was decided elsewhere.  Unlike
// the substrate port, this rule REPORTS those by default rather than hiding
// them behind a flag, because this module is where `interface{}` enters the
// system -- Native, NativeOf and Value are defined here, the fork and detach
// walkers apply the policy here, and the error-condition constructors accept
// arbitrary data here.  Each of those sites is a contract, and the contract
// has to be written down at the site as an `//elpsvet:allow`, never silently
// passed through as "unknowable".
//
// # Why it is deliberately dumb
//
// "Is this payload mutable" is undecidable, so the rule does not guess.  A
// composite type -- pointer, slice, map, chan, func, struct, array -- is
// reported whether or not it happens to be used immutably, and a NEW payload
// type FAILS until somebody classifies it.  That fail-closed property is the
// whole point: the check is a forcing function for the audit, not a
// mutability oracle.  time.Time is a struct holding a *Location and
// *regexp.Regexp is a pointer; no structural rule can call them safe, only
// the audit can, and the audit is what allowedPayloadTypes records.
//
// # What it does not see
//
// The payload type must be visible AT THE CONSTRUCTION SITE.  An indirect
// call (`f := lisp.Native; f(x)`) is not a call the callee resolver can see;
// a multi-value assignment (`v.Native, ok = g()`) has no expression type for
// its right-hand side and is skipped.  A payload constructed in ANOTHER
// module is that module's to audit -- substrate runs the same rule over its
// own tree for exactly that reason.
package main

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
)

const (
	// nativeAllowMarker is the audited suppression: the ownership rule's
	// marker, held to a stricter standard here (see justifiedAllow).
	nativeAllowMarker = "elpsvet:allow"

	// nativeClonerMethod is the one method of lisp.NativeCloner.
	nativeClonerMethod = "CloneNative"
)

var nativePayloadAnalyzer = &analysis.Analyzer{
	Name: "elpsnativepayload",
	Doc: "flag lisp.LVal native payloads whose type is not provably safe to SHARE across forks" +
		" (Fork shares LVal.Native by reference) unless the type declares lisp.NativeCloner," +
		" is on the audited allowlist, or //elpsvet:allow <justification> covers the site",
	Run: runNativePayload,
}

// allowedPayloadTypes is the AUDITED inventory of native payload types that
// are safe to share by reference across every fork of a template, keyed by
// the type as types.TypeString spells it with full import paths (so a
// pointer type is keyed with its `*`), with WHY as the value.  The reasons
// print nowhere; they are here so the next author adding a row can see what
// a real justification looks like, and so a reviewer can check the claim.
//
// Adding a row is a claim that a human checked the value cannot be MUTATED
// through the payload a fork shares -- either because the type has no
// mutating API, because every operation on it allocates, or because the
// kernel's own walkers copy it on every path.  It is not a claim that the
// value is small, or that the current callers behave.
//
// This map may only SHRINK.
var allowedPayloadTypes = map[string]string{
	// Kernel-owned representation slots.  LVal.Native doubles as the backing
	// store for several non-LNative types, and the fork and detach walkers
	// carry an explicit arm for each, so none of them travels by reference.

	"*github.com/luthersystems/elps/lisp.funData": "the LFun payload. The fork walker's LFun arm " +
		"(fork.go) re-mints the funData per fork with the captured env remapped into the fork, so " +
		"a fork never shares the template's; detach refuses LFun values rather than copy them. A " +
		"builtin's Go function pointer travels by reference, which is code, not state",

	"*[]byte": "LBytes backing storage. The *[]byte arms in the fork walker (fork.go) and the " +
		"detach walker (detach.go) each allocate a fresh slice and copy the bytes, so no fork can " +
		"reach the template's buffer",

	"*github.com/luthersystems/elps/lisp.MapData": "LSortMap backing storage. The fork walker's " +
		"mapData arm, detachMapData and copyMapData each rebuild the map structure, so a fork's " +
		"assoc!/dissoc! cannot reach the template's maps",

	"*github.com/luthersystems/elps/lisp.CallStack": "an LError's recorded stack. It is minted by " +
		"CallStack.Copy at the capture point and is a snapshot from then on -- the stack the " +
		"evaluator pushes and pops is Runtime.Stack, never the copy -- and the *CallStack arms of " +
		"both walkers deep-copy it through detachCallStack, frames, locations and GoStack bytes " +
		"included",

	// Standard-library payloads.

	"*regexp.Regexp": "immutable after Compile by documented contract ('A Regexp is safe for " +
		"concurrent use by multiple goroutines'). The one mutating method, Longest, is documented " +
		"as such and nothing in libregexp calls it; every other method allocates its result",

	"time.Time": "value type with no mutating API: every method returns a new Time, the only " +
		"pointer-receiver methods (UnmarshalJSON/UnmarshalBinary/GobDecode) assign through a copy " +
		"taken by type assertion, and the *Location it holds is either a process-wide immutable " +
		"singleton or a Location loaded once and never written",

	"error": "read-only by contract. The kernel stores an error only as an LError's data cell " +
		"(ErrorCondition, LEnv.ErrorCondition) and thereafter only reads it -- Error() for the " +
		"message, GoError to hand it back -- and nothing in the tree asserts it to a concrete type " +
		"and writes through it. The row is a contract, not a proof: an embedder's error value can " +
		"point at anything, and what the kernel promises is never to mutate it, which is all " +
		"sharing needs",

	"*github.com/luthersystems/elps/lisp/lisplib/libschema.validatorTag": "a pointer to a " +
		"zero-size struct, shared process-wide on purpose: the validator credential is the payload's " +
		"TYPE (isValidator asserts *validatorTag), it has no field to read or write through, and the " +
		"type is unexported with no constructor, so nothing outside the package can mint one",

	"*github.com/luthersystems/elps/lisp/lisplib/libjson.ownMessage": "immutable after " +
		"construction: an unexported type with unexported fields, minted on one line " +
		"(DumpMessageBuiltin) from bytes the serializer just wrote, and every method and reader " +
		"(MarshalJSON, jsonMessage) only reads msg and loadable -- nothing assigns to either " +
		"afterwards, and no other package can name the type to try",
}

func runNativePayload(pass *analysis.Pass) (interface{}, error) {
	for _, file := range pass.Files {
		allow := markerLinesMatching(pass.Fset, file, justifiedAllow)
		for _, decl := range file.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				if d.Body == nil || hasJustifiedAllow(d.Doc) {
					continue
				}
				checkNativeConstructions(pass, d.Body, allow)
			case *ast.GenDecl:
				// Package-level var/const initializers, including function
				// literals inside them.  A native built at package scope is
				// shared by every Runtime in the process before Fork is even
				// involved -- the ownership rule's territory, reached from
				// the payload side.
				checkNativeConstructions(pass, d, allow)
			}
		}
	}
	return nil, nil
}

// checkNativeConstructions reports every native construction under n.
func checkNativeConstructions(pass *analysis.Pass, n ast.Node, allow map[int]bool) {
	ast.Inspect(n, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.CallExpr:
			checkNativeCall(pass, x, allow)
		case *ast.CompositeLit:
			checkNativeLiteral(pass, x, allow)
		case *ast.AssignStmt:
			checkNativeAssign(pass, x, allow)
		}
		return true
	})
}

// checkNativeCall handles lisp.Native(x), the typed lisp.NativeOf[T](x), and
// the lisp.Value(x) calls the compiler can see falling through to Native.
func checkNativeCall(pass *analysis.Pass, call *ast.CallExpr, allow map[int]bool) {
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
	reportNativePayload(pass, call.Pos(), pass.TypesInfo.TypeOf(arg), "lisp."+fn.Name(), allow)
}

// checkNativeLiteral handles `lisp.LVal{Native: x}` -- the constructor
// bypass the kernel itself uses for every non-LNative payload slot.
func checkNativeLiteral(pass *analysis.Pass, lit *ast.CompositeLit, allow map[int]bool) {
	if !isLValType(pass.TypesInfo.TypeOf(lit)) {
		return
	}
	for _, elt := range lit.Elts {
		kv, ok := elt.(*ast.KeyValueExpr)
		if !ok {
			continue
		}
		key, ok := kv.Key.(*ast.Ident)
		if !ok || key.Name != "Native" {
			continue
		}
		reportNativePayload(pass, kv.Value.Pos(), pass.TypesInfo.TypeOf(kv.Value), "lisp.LVal literal", allow)
	}
}

// checkNativeAssign handles `v.Native = x` on an existing LVal -- the other
// bypass, and the one that can put a payload into a value the assigning
// function does not own.
func checkNativeAssign(pass *analysis.Pass, stmt *ast.AssignStmt, allow map[int]bool) {
	if len(stmt.Lhs) != len(stmt.Rhs) {
		// Multi-value RHS: the payload type is a tuple element, not an
		// expression type.  Documented blind spot (file comment).
		return
	}
	for i, lhs := range stmt.Lhs {
		sel, ok := lhs.(*ast.SelectorExpr)
		if !ok || sel.Sel.Name != "Native" {
			continue
		}
		if !isLValType(pass.TypesInfo.TypeOf(sel.X)) {
			continue
		}
		reportNativePayload(pass, stmt.Pos(), pass.TypesInfo.TypeOf(stmt.Rhs[i]), "LVal.Native assignment", allow)
	}
}

func reportNativePayload(pass *analysis.Pass, pos token.Pos, payload types.Type, what string, allow map[int]bool) {
	if allow[pass.Fset.Position(pos).Line] {
		return
	}
	switch classifyPayload(payload) {
	case payloadSafe:
		return
	case payloadDynamic:
		pass.Reportf(pos,
			"%s payload type %s is not statically known (an interface or type parameter), so whether it is"+
				" safe to share across forks cannot be checked here; construct the native from a concrete"+
				" type, or annotate //%s with a justification naming the contract the payload is held to",
			what, payloadTypeString(payload), nativeAllowMarker)
	case payloadReport:
		pass.Reportf(pos,
			"%s payload type %s is not a known-safe value type: Fork shares LVal.Native BY REFERENCE, so a"+
				" payload that reaches a template is shared by every fork in the process; implement"+
				" lisp.NativeCloner on the type, add it to allowedPayloadTypes in cmd/elpsvet/nativepayload.go"+
				" with a justification, or annotate //%s with one",
			what, payloadTypeString(payload), nativeAllowMarker)
	}
}

type payloadVerdict int

const (
	// payloadSafe: sharing this payload across forks cannot be observed.
	payloadSafe payloadVerdict = iota
	// payloadReport: plausibly mutable, or simply unclassified.
	payloadReport
	// payloadDynamic: interface-typed or a type parameter, so there is no
	// static payload type to classify.
	payloadDynamic
)

// classifyPayload decides a payload type's verdict.  The ORDER matters: the
// allowlist and the NativeCloner check are consulted on the type itself
// before the underlying type is looked at, since every audited row
// (time.Time, *regexp.Regexp, the kernel's pointer slots) has a composite
// underlying type that would otherwise be reported.
func classifyPayload(t types.Type) payloadVerdict {
	if t == nil {
		return payloadDynamic
	}
	t = types.Unalias(t)
	if _, ok := t.(*types.TypeParam); ok {
		return payloadDynamic
	}
	if _, ok := allowedPayloadTypes[types.TypeString(t, nil)]; ok {
		return payloadSafe
	}
	if declaresNativeCloner(t) {
		return payloadSafe
	}
	switch u := t.Underlying().(type) {
	case *types.Basic:
		if u.Kind() == types.UnsafePointer {
			return payloadReport
		}
		return payloadSafe
	case *types.Interface:
		return payloadDynamic
	}
	return payloadReport
}

// declaresNativeCloner reports whether t's method set carries
// `CloneNative() interface{}` -- structurally, which is what a Go interface
// assertion checks too.  It is t's OWN method set that matters: a value of a
// type whose CloneNative has a pointer receiver does not satisfy
// lisp.NativeCloner at runtime, so Native(v) of such a value is reported
// while Native(&v) is not.
func declaresNativeCloner(t types.Type) bool {
	sel := types.NewMethodSet(t).Lookup(nil, nativeClonerMethod)
	if sel == nil {
		return false
	}
	sig, ok := sel.Type().(*types.Signature)
	if !ok || sig.Params().Len() != 0 || sig.Results().Len() != 1 {
		return false
	}
	iface, ok := types.Unalias(sig.Results().At(0).Type()).Underlying().(*types.Interface)
	return ok && iface.Empty()
}

// payloadTypeString renders a payload type for a diagnostic without the
// full import path, which keeps the messages readable.
func payloadTypeString(t types.Type) string {
	if t == nil {
		return "unknown"
	}
	return types.TypeString(t, func(p *types.Package) string { return p.Name() })
}

// directlyRepresentable reports whether lisp.Value's type switch converts t
// without falling through to Native.  Mirrors the switch in lisp/lisp.go:
// bool, string, []byte, int, float64, []*LVal.  Identity is the right
// comparison -- a Go type switch matches `case []byte` only for the unnamed
// type, so `type Blob []byte` DOES become a native.
func directlyRepresentable(t types.Type) bool {
	if t == nil {
		return false
	}
	switch u := types.Unalias(t).(type) {
	case *types.Basic:
		switch u.Kind() {
		case types.Bool, types.String, types.Int, types.Float64,
			types.UntypedBool, types.UntypedString, types.UntypedInt, types.UntypedFloat:
			return true
		default:
			return false
		}
	case *types.Slice:
		if b, ok := types.Unalias(u.Elem()).(*types.Basic); ok && b.Kind() == types.Uint8 {
			return true // []byte
		}
		ptr, ok := types.Unalias(u.Elem()).(*types.Pointer)
		return ok && isLValType(ptr.Elem()) // []*lisp.LVal
	}
	return false
}

// isLValType reports whether t is lisp.LVal or *lisp.LVal.
func isLValType(t types.Type) bool {
	if t == nil {
		return false
	}
	t = types.Unalias(t)
	if ptr, ok := t.(*types.Pointer); ok {
		t = types.Unalias(ptr.Elem())
	}
	named, ok := t.(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj != nil && obj.Name() == "LVal" && obj.Pkg() != nil && obj.Pkg().Path() == lispPkgPath
}

// calleeFunc resolves a call's callee to its *types.Func, so package aliases
// and dot imports resolve like the compiler resolves them rather than by
// matching the source text "lisp.Native".  An EXPLICITLY instantiated
// generic -- lisp.NativeOf[*Handle](h) -- wraps the callee in an index
// expression (IndexExpr for one type argument, IndexListExpr for several),
// which is unwrapped first; missing that would leave a spelling the rule
// cannot see.
func calleeFunc(pass *analysis.Pass, call *ast.CallExpr) *types.Func {
	fun := ast.Unparen(call.Fun)
	switch idx := fun.(type) {
	case *ast.IndexExpr:
		fun = ast.Unparen(idx.X)
	case *ast.IndexListExpr:
		fun = ast.Unparen(idx.X)
	}
	var id *ast.Ident
	switch fun := fun.(type) {
	case *ast.Ident:
		id = fun
	case *ast.SelectorExpr:
		id = fun.Sel
	default:
		return nil
	}
	fn, _ := pass.TypesInfo.Uses[id].(*types.Func)
	return fn
}

// justifiedAllow reports whether a comment's text is an //elpsvet:allow
// marker THAT CARRIES A JUSTIFICATION: the marker followed by whitespace and
// at least one non-blank character.  A bare marker is not an audit and does
// not suppress.  The rule cannot check that the words are true, only that
// somebody wrote them down where the next reader will see them.
func justifiedAllow(text string) bool {
	text = strings.TrimPrefix(text, "//")
	text = strings.TrimPrefix(text, "/*")
	text = strings.TrimSuffix(text, "*/")
	text = strings.TrimSpace(text)
	rest, ok := strings.CutPrefix(text, nativeAllowMarker)
	if !ok || rest == "" {
		return false
	}
	if rest[0] != ' ' && rest[0] != '\t' {
		return false // a different marker sharing the prefix
	}
	return strings.TrimSpace(rest) != ""
}

func hasJustifiedAllow(cg *ast.CommentGroup) bool {
	if cg == nil {
		return false
	}
	for _, c := range cg.List {
		if justifiedAllow(c.Text) {
			return true
		}
	}
	return false
}
