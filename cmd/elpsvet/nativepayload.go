// Copyright © 2026 The ELPS authors

// The elpsnativepayload analyzer is the fourth elpsvet rule: nothing
// plausibly MUTABLE may become a native payload unless a human has written
// down why sharing it is safe.
//
// # The invariant
//
// A template PUBLISHES one value graph and instantiates a VM from it per
// request.  Instantiation rebuilds every LVal header, cell span, byte buffer
// and map backing, but an approved native payload is NOT rebuilt: the
// instantiate loop stores `p.natives[value.payload]` straight into the fresh
// header (lisp/template_plan.go), so one Go value is shared by every VM the
// template ever mints, for the life of the process.  Publication is where
// that sharing is decided, and (*templateInventory).native (lisp/template.go)
// is the runtime half of the audit: a payload is admitted only if its type
// is a struct VALUE implementing internal/templatepolicy.Immutable, if its
// reflect.Kind is an actual scalar, or if the embedder approved it through
// lisp.TemplateWithNativePolicy.  Anything else fails publication with
// "native %T has no template immutability declaration".
//
// This rule is the static half.  It runs at the construction sites, where
// the payload still has a Go type a reader can see, and it fails a NEW
// payload type before the runtime has to -- so the audit happens in review
// rather than in whatever request first publishes the value.
//
// # The rule
//
// A native construction -- `lisp.Native(x)`, the typed `lisp.NativeOf[T](x)`
// (inferred or explicitly instantiated), a `lisp.Value(x)` the compiler can
// see falling through to Native, a keyed literal setting the lisp.LVal.Native
// FIELD (`lisp.LVal{Native: x}`, or `lisp.ErrorVal{Native: x}` -- ErrorVal is
// `type ErrorVal LVal`, the same struct and the same field object), or a
// write to that field however it is reached (`v.Native`, `e.Native` on an
// ErrorVal, `(*lisp.ErrorVal)(v).Native`, a promoted `w.Native` through an
// embedding struct) -- is REPORTED unless one of:
//
//  1. the payload's static type has a basic underlying type that is on
//     runtimeScalarKinds -- the kind-for-kind mirror of the scalar
//     reflect.Kind arm of templateInventory.native.  A non-pointer value of
//     scalar underlying type inside an interface is immutable by
//     construction: it is not addressable, so every type assertion yields a
//     copy and no VM can reach the shared value.  `uintptr` and
//     `unsafe.Pointer` are NOT in the tier, because they are not in the
//     runtime's either -- both are addresses wearing a basic type's clothes,
//     and the runtime names reflect.Uintptr and reflect.UnsafePointer in the
//     arm it refuses;
//
//  2. the payload's static type is a STRUCT VALUE whose method set carries
//     internal/templatepolicy.Immutable's unexported templateImmutable()
//     -- which only embedding templatepolicy.Marker can supply, since no
//     package outside this module can name that method.  This is the same
//     pair of conditions the runtime applies, and the struct-value half is
//     load-bearing: a pointer's method set inherits the value's marker, but
//     a caller can replace the whole pointee, so `*T` is REPORTED even when
//     `T` embeds Marker.  A pointer form needs TemplateWithNativePolicy
//     approval from the embedder instead;
//
//  3. the payload's type is on allowedPayloadTypes below -- the AUDITED
//     inventory, each row carrying the reason a human checked and the
//     HEADER TYPE whose storage it is -- AND the site shows that header.
//     Every row is a kernel storage slot that templateInventory.val handles
//     by an explicit arm keyed off the header's Type, so it never reaches
//     templateInventory.native; a row is therefore a claim about a HEADER,
//     not about a type in the abstract, and it is only true where the site
//     is building that header.  Three conditions, all required
//     (payloadSite.exemptsRow): (a) the site is IN package
//     github.com/luthersystems/elps/lisp, since the rows describe the
//     kernel's own representation slots and a package outside the kernel has
//     no business building one; (b) the site is not a CONSTRUCTOR, since
//     Native, NativeOf and a falling-through Value always build an LNative
//     whose payload val hands straight to native(), where `*[]byte`,
//     `*MapData` and `*funData` are refused like any other pointer; and (c)
//     for a keyed LITERAL, the same literal's `Type:` key names the row's
//     LType constant -- LBytes for `*[]byte`, LSortMap for `*MapData`, LFun
//     for `*funData` -- resolved through the type checker rather than by
//     source text.  A literal with no Type key shows no header; a literal
//     naming another Type shows the wrong one; and `LVal{Type: LNative,
//     Native: &b}` names exactly the header val routes to native(), which
//     refuses the payload.  All three are reported, with
//     payloadKernelSlotMisuse naming the header the row belongs to.
//
//     A `.Native` FIELD WRITE has no header to show, so (c) cannot apply to
//     it and (a) carries the whole weight: `v.Native = &b` is exempt in
//     package lisp and reported everywhere else.  See "What it does not see"
//     for why that residual is where it is;
//
//  4. an audited `//elpsvet:allow-native <justification>` comment covers the
//     site: trailing on the reported line, standalone on the line above it,
//     or -- for a multi-line literal -- on either the opening line or the
//     `Native:` line; or the enclosing function's doc comment carries one.
//     The justification must be at least three words.  A bare or one-word
//     marker does NOT suppress: an allow that says nothing is a
//     classification nobody made.  One justification covers every
//     construction on its line, which is deliberate -- the line is the unit
//     an author annotates and a reader audits.
//
// Taking the field's ADDRESS (`&v.Native`) is reported unconditionally: what
// is later stored through the pointer has no type at this site, so the only
// honest verdict is that the rule cannot see it.
//
// # The marker is this rule's own
//
// elpsownership's `//elpsvet:allow` is a bare prefix match with no
// justification enforced.  Had this rule shared it, one sentence written for
// the ownership rule ("guarded singleton", "sealed formals") on a
// package-level native would have silenced both rules with a reason that
// addresses neither.  `//elpsvet:allow-native` is therefore separate, and
// the ownership rule's matcher stops at the marker's word boundary so the
// native marker does not satisfy it either (main.go, allowed).
//
// # NativeCloner is not a tier
//
// The rule this was ported from exempted any type declaring
// lisp.NativeCloner, because Fork duplicated such a payload instead of
// sharing it.  Templates replaced Fork and CloneNative is explicitly NOT an
// admission protocol (lisp/native.go): NewTemplate rejects mutable payloads
// INCLUDING NativeCloner implementations, and an approved immutable payload
// is shared without CloneNative ever being called (template_plan.go says so
// at the checkNativeAffinity loop).  A CloneNative method is therefore
// evidence that a payload is mutable enough to need cloning, which is the
// opposite of a reason to exempt it, and this rule reports such a type.
//
// # A retained call stack is banned outright
//
// A `*lisp.CallStack` payload is not merely unclassified: it is refused by
// (*templateInventory).checkDiagnosticPayload before any policy runs, and so
// is a `*[]byte` header aliasing the live Runtime.GoStack (#629).  The rule
// gives that type its own diagnostic naming the runtime check, because
// adding an allowlist row or a native policy for it would not make it
// publishable.
//
// # Interface-typed payloads
//
// A payload whose static type is an interface (or a type parameter) has no
// type to classify: whatever it is at runtime was decided elsewhere.  Unlike
// the substrate port, this rule REPORTS those by default rather than hiding
// them behind a flag, because this module is where `interface{}` enters the
// system -- Native, NativeOf and Value are defined here, the detach walker
// and the template planner apply the policy here, and the error-condition
// constructors accept arbitrary data here.  Each of those sites is a
// contract, and the contract has to be written down at the site as an
// `//elpsvet:allow-native`, never silently passed through as "unknowable".
//
// # Why it is deliberately dumb
//
// "Is this payload mutable" is undecidable, so the rule does not guess.  A
// composite type -- pointer, slice, map, chan, func, struct, array -- is
// reported whether or not it happens to be used immutably, and a NEW payload
// type FAILS until somebody classifies it.  That fail-closed property is the
// whole point: the check is a forcing function for the audit, not a
// mutability oracle.  time.Time is a struct holding a *Location and
// *regexp.Regexp is a pointer; the runtime refuses both by name in
// lisp/lisplib/template_natives_test.go, which is why neither is a row here,
// and libtime and libregexp wrap them in marked owned struct values instead.
//
// # What it does not see
//
// The payload type must be visible AT THE CONSTRUCTION SITE, and the site
// must be one of the spellings above.  Invisible:
//
//   - an indirect call (`f := lisp.Native; f(x)`): the callee resolver sees
//     objects, not function values;
//   - a multi-value assignment (`v.Native, ok = g()`): the right-hand side
//     is a tuple, with no expression type per element;
//   - a positional (unkeyed) `LVal{...}` literal: only possible inside
//     package lisp, where the struct's unexported fields are nameable, and
//     not written anywhere;
//   - anything done through reflect (`reflect.ValueOf(v).Elem().FieldByName
//     ("Native").Set(...)`): a runtime property, not a source one;
//   - a payload constructed in ANOTHER module: that module's to audit --
//     substrate runs the same rule over its own tree for exactly that reason;
//   - which HEADER TYPE a `.Native` FIELD WRITE inside package lisp is
//     writing onto.  A literal shows its header in the same expression, so
//     the allowlist tier reads it (condition 3c above); a field write shows
//     nothing -- `v.Native = payload` says only that SOME header gets some
//     payload.  Seven such writes exist in the kernel today (lisp/copier.go
//     339 and 343, lisp/detach.go 199 and 209, lisp/template_plan.go 501,
//     503 and 505), and each is guarded a few lines up by a check of the
//     header's own Type that this rule does not model -- detach's
//     `if v.Type != LBytes { return ... }`, the copier's `switch v.Type`,
//     the planner's replay of a plan whose kinds publication already fixed.
//     So the tier still trusts a row payload written through the field
//     INSIDE package lisp, and ONLY there: the same write in any other
//     package is reported, which is where the bypass mattered -- an
//     embedder holding an `*LVal` could store a `*[]byte` onto an LNative
//     header and be exempt.  Narrowing the residual further means teaching
//     the rule those guards, not tightening the site test; tightening it
//     alone would report all seven kernel writes and force seven
//     annotations onto code publication already routes correctly.
package main

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
)

const (
	// nativeAllowMarker is this rule's audited suppression.  It is NOT the
	// ownership rule's //elpsvet:allow (see the file comment) and is held to
	// a stricter standard: see justifiedNativeAllow.
	nativeAllowMarker = "elpsvet:allow-native"

	// nativeAllowMinWords is the least a justification may be.  Three words
	// is not an audit standard; it is the line below which a marker is
	// punctuation ("//elpsvet:allow-native .") rather than a sentence.
	nativeAllowMinWords = 3

	// templateImmutableMethod is the one method of
	// internal/templatepolicy.Immutable.  It is UNEXPORTED, which is the
	// point: only a type embedding templatepolicy.Marker can have it, and
	// only this module can embed that.
	templateImmutableMethod = "templateImmutable"

	// nativeFieldName is the lisp.LVal field this rule tracks.  The field
	// OBJECT is what is matched (isNativeField), never the receiver's
	// spelled type, so ErrorVal, conversions and embedding all resolve to
	// the same field.
	nativeFieldName = "Native"

	// lTypeFieldName is the lisp.LVal field naming the HEADER a value is,
	// and the discriminant (*templateInventory).val switches on.  A keyed
	// literal that sets Native alongside it says which of val's arms the
	// payload will take, which is what makes the allowlist tier checkable
	// (payloadSite.exemptsRow).  Matched by object, like Native.
	lTypeFieldName = "Type"
)

// templatePolicyPkgPath is the internal package declaring the Immutable
// marker contract.  The method is matched together with this path so that a
// downstream type with a same-named method of its own cannot claim the tier.
const templatePolicyPkgPath = "github.com/luthersystems/elps/internal/templatepolicy"

// callStackTypeName is the payload type publication refuses outright, by
// name, in (*templateInventory).checkDiagnosticPayload.
const callStackTypeName = "CallStack"

var nativePayloadAnalyzer = &analysis.Analyzer{
	Name: "elpsnativepayload",
	Doc: "flag lisp.LVal native payloads whose type is not provably safe for a template to publish" +
		" (an admitted payload is shared by every VM the template mints) unless the type is a struct" +
		" value embedding internal/templatepolicy.Marker, is on the audited allowlist, or" +
		" //elpsvet:allow-native <justification> covers the site",
	Run: runNativePayload,
}

// payloadRow is one audited allowlist entry.  A row is a claim about a
// HEADER, not about a type in the abstract: LVal.Native doubles as the
// backing store for several non-LNative kernel types, and
// (*templateInventory).val (lisp/template.go) routes each of them by the
// header's Type to an arm that is not native().  So a row carries the LType
// constant whose storage it is, and the tier is true only where the site
// shows that header (payloadSite.exemptsRow).
type payloadRow struct {
	// reason is why a human decided sharing this payload is safe.  It
	// prints nowhere; it is here so the next author adding a row can see
	// what a real justification looks like, and so a reviewer can check the
	// claim.
	reason string
	// headerType is the lisp.LType constant this storage belongs to, by
	// NAME, resolved through the type checker at the site (headerTypeNamed)
	// rather than by source text.  It is the arm templateInventory.val takes
	// for such a header, which is what keeps the payload away from
	// templateInventory.native.
	headerType string
}

// allowedPayloadTypes is the AUDITED inventory of native payload types that
// need no marker and no policy, keyed by the type as types.TypeString spells
// it with full import paths (so a pointer type is keyed with its `*`).
//
// Every row today is a KERNEL REPRESENTATION SLOT, and the planner rebuilds
// the storage per VM instead of sharing it.  A row is therefore a claim
// about a type the RUNTIME already handles by name, not a second admission
// channel beside the marker and the host policy -- an embedder-visible
// payload belongs in tier 1 or 2, or carries a site annotation.
//
// This map may only SHRINK.
var allowedPayloadTypes = map[string]payloadRow{
	"*github.com/luthersystems/elps/lisp.funData": {headerType: "LFun", reason: "the LFun payload. " +
		"templateInventory.val's LFun arm (lisp/template.go) walks it as function data rather than as " +
		"a native -- it never reaches templateInventory.native -- and the planner mints a fresh " +
		"funData per VM (template_plan.go's instantiate stores instance.functions[...], not a shared " +
		"payload). A builtin's Go function pointer travels by reference, which is code, not state"},

	"*[]byte": {headerType: "LBytes", reason: "LBytes backing storage. templateInventory.val's " +
		"*[]byte arm records a byte span instead of calling native (lisp/template.go), and instantiate " +
		"gives each VM its own header over its own copied buffer (template_plan.go), so no VM can " +
		"reach another's bytes. The one header that is NOT admissible -- an alias of the live " +
		"Runtime.GoStack -- is rejected by checkDiagnosticPayload rather than by this row (#629)"},

	"*github.com/luthersystems/elps/lisp.MapData": {headerType: "LSortMap", reason: "LSortMap backing " +
		"storage. templateInventory.val routes it to mapData, which walks the map's values and records " +
		"the backing (lisp/template.go), and instantiate rebuilds every map and its backing per VM " +
		"(template_plan.go), so a VM's assoc!/dissoc! cannot reach the published maps"},
}

// lTypeName is the lisp type whose constants a row's headerType names.  It
// is checked alongside the constant's name and package so that a same-named
// constant of some other type cannot satisfy a row.
const lTypeName = "LType"

// siteKind says WHICH SPELLING built the payload, because the allowlist tier
// is only true for some of them.
//
// lisp.Native, lisp.NativeOf and a lisp.Value that falls through all build an
// LNative header (lisp/lisp.go, lisp/native.go), and templateInventory.val's
// LNative arm hands that payload straight to native() -- which knows nothing
// about the kernel's representation slots and refuses a *[]byte, a *MapData
// and a *funData like any other pointer.  The rows are true only where the
// kernel writes its own storage onto the header val routes to one of its
// explicit *[]byte, mapData and LFun arms instead -- which is a property of
// the SITE, not of the payload type, and is why the site is carried this far.
type siteKind int

const (
	// siteConstructor: lisp.Native(x), lisp.NativeOf[T](x), or a
	// lisp.Value(x) the compiler can see falling through.  Always an
	// LNative header, so the allowlist does not apply.
	siteConstructor siteKind = iota
	// siteHeaderLiteral: a keyed `LVal{Type: ..., Native: x}` literal.  The
	// header is spelled in the SAME expression, so the allowlist tier can
	// read it and require that it matches the row.
	siteHeaderLiteral
	// siteFieldWrite: `v.Native = x`.  The header is whatever v already is,
	// which this rule cannot see.
	siteFieldWrite
)

// payloadSite is everything about a construction site the allowlist tier
// needs: the spelling, whether the site is inside the kernel package whose
// representation slots the rows describe, and -- for a literal -- the LType
// constant the same literal names in its `Type:` key.
type payloadSite struct {
	// headerType is the NAME of the LType constant the literal's `Type:`
	// key resolves to ("LBytes", "LFun", ...), empty when the site is not a
	// literal, when the literal names no Type, or when what it names is not
	// a constant of lisp.LType declared in package lisp.  It comes from the
	// type checker, so `lisp.LBytes`, a dot-imported `LBytes` and an
	// aliased-import `l.LBytes` are one object and resolve alike, while a
	// same-named constant from some other package does not resolve at all.
	headerType string
	kind       siteKind
	// inKernel is pass.Pkg.Path() == lispPkgPath.  The rows are the
	// kernel's own storage slots; a package outside the kernel that builds
	// one is doing something the rows say nothing about.
	inKernel bool
}

// exemptsRow reports whether an allowlist row is true AT THIS SITE.  Three
// conditions, and the file comment says why each one is load-bearing:
//
//  1. inKernel -- the rows describe package lisp's own representation slots;
//  2. not a constructor -- Native, NativeOf and a falling-through Value all
//     build an LNative, and val hands an LNative's payload to native(),
//     which refuses every row type;
//  3. for a LITERAL, the same literal's `Type:` key names the row's header.
//     `LVal{Type: LNative, Native: &b}` is precisely the header val routes
//     to native(), so it is reported; so is a literal with no Type key,
//     which shows no header at all.
//
// A FIELD WRITE shows no header either, and cannot: `v.Native = payload`
// says nothing about what v is.  Inside the kernel the tier still trusts
// one, because every such write in package lisp is guarded a few lines up by
// a check of the header's own Type (lisp/detach.go, lisp/copier.go) or
// replays a plan whose kinds publication already fixed
// (lisp/template_plan.go) -- guards this rule does not model.  Outside the
// kernel a field write is reported, which is where the bypass mattered.
func (s payloadSite) exemptsRow(row payloadRow) bool {
	if !s.inKernel {
		return false
	}
	switch s.kind {
	case siteHeaderLiteral:
		return s.headerType == row.headerType
	case siteFieldWrite:
		return true
	default:
		return false
	}
}

func runNativePayload(pass *analysis.Pass) (interface{}, error) {
	for _, file := range pass.Files {
		allow := markerLinesMatching(pass.Fset, file, justifiedNativeAllow)
		for _, decl := range file.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				if d.Body == nil || hasJustifiedNativeAllow(d.Doc) {
					continue
				}
				checkNativeConstructions(pass, d.Body, allow)
			case *ast.GenDecl:
				// Package-level var/const initializers, including function
				// literals inside them.  A native built at package scope is
				// shared by every Runtime in the process before a template
				// is even involved -- the ownership rule's territory,
				// reached from the payload side.
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
		case *ast.UnaryExpr:
			checkNativeAddress(pass, x, allow)
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
	reportNativePayload(pass, call.Pos(), pass.TypesInfo.TypeOf(arg), "lisp."+fn.Name(),
		payloadSite{kind: siteConstructor, inKernel: inKernelPkg(pass)}, allow)
}

// checkNativeLiteral handles a keyed literal setting the lisp.LVal.Native
// field -- `lisp.LVal{Native: x}`, and equally `lisp.ErrorVal{Native: x}`,
// since ErrorVal is a defined type over LVal and its keys resolve to the
// same field objects.  The literal's TYPE is not consulted; the key's
// object is.
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
func checkNativeLiteral(pass *analysis.Pass, lit *ast.CompositeLit, allow map[int]bool) {
	native := nativeKeyValues(pass, lit)
	if len(native) == 0 {
		// Much the commonest case, and the reason the header type is
		// resolved only after it: every composite literal in the tree
		// reaches this function.
		return
	}
	site := payloadSite{
		kind:       siteHeaderLiteral,
		inKernel:   inKernelPkg(pass),
		headerType: literalHeaderType(pass, lit),
	}
	for _, kv := range native {
		reportNativePayload(pass, lit.Pos(), pass.TypesInfo.TypeOf(kv.Value), "LVal.Native literal", site, allow,
			kv.Key.Pos(), kv.Value.Pos())
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

// literalHeaderType returns the NAME of the lisp.LType constant the
// literal's `Type:` key names, or "" when the literal has no Type key, when
// its value is not a constant of lisp.LType, or when it is a computed
// expression rather than a named constant.
//
// The key is matched by its field OBJECT, like the Native key, so ErrorVal
// and any conversion of LVal resolve to the same field.  The VALUE is
// resolved through the type checker rather than read as source text, so
// `lisp.LBytes`, a dot-imported `LBytes` and an aliased-import
// `l.LBytes` are all the same object and all resolve, while a same-named
// constant declared in some other package is a different object and does
// not.
func literalHeaderType(pass *analysis.Pass, lit *ast.CompositeLit) string {
	for _, elt := range lit.Elts {
		kv, ok := elt.(*ast.KeyValueExpr)
		if !ok {
			continue
		}
		key, ok := kv.Key.(*ast.Ident)
		if !ok || !isLValTypeField(pass.TypesInfo.Uses[key]) {
			continue
		}
		return headerTypeNamed(pass, kv.Value)
	}
	return ""
}

// headerTypeNamed resolves expr to the name of a lisp.LType constant
// DECLARED IN package lisp, or "" if it is anything else.  Both halves
// matter: the declaring package, so no other package's constant can claim a
// row, and the constant's TYPE, so a same-named constant of another type in
// package lisp could not either.
//
// A re-declared alias -- `const h = lisp.LBytes`, whether in another package
// or as a local inside lisp -- is a different constant object with a
// different name, and resolves to "".  That is a false POSITIVE, which is
// the direction this rule fails in everywhere else: the author writes the
// header constant the kernel writes, or annotates.
func headerTypeNamed(pass *analysis.Pass, expr ast.Expr) string {
	var id *ast.Ident
	switch e := ast.Unparen(expr).(type) {
	case *ast.Ident:
		id = e
	case *ast.SelectorExpr:
		id = e.Sel
	default:
		return ""
	}
	konst, ok := pass.TypesInfo.Uses[id].(*types.Const)
	if !ok || konst.Pkg() == nil || konst.Pkg().Path() != lispPkgPath {
		return ""
	}
	named, ok := types.Unalias(konst.Type()).(*types.Named)
	if !ok {
		return ""
	}
	obj := named.Obj()
	if obj == nil || obj.Name() != lTypeName || obj.Pkg() == nil || obj.Pkg().Path() != lispPkgPath {
		return ""
	}
	return konst.Name()
}

// isLValTypeField reports whether obj is the lisp.LVal.Type field object --
// the header discriminant templateInventory.val switches on.  Matched the
// same way as the Native field, by object rather than by the receiver's
// spelled type.
func isLValTypeField(obj types.Object) bool {
	v, ok := obj.(*types.Var)
	return ok && v.IsField() && v.Name() == lTypeFieldName && v.Pkg() != nil && v.Pkg().Path() == lispPkgPath
}

// inKernelPkg reports whether the package under analysis IS package lisp.
// The allowlist rows describe the kernel's own representation slots, so a
// row is only true of a site the kernel wrote.
func inKernelPkg(pass *analysis.Pass) bool {
	return pass.Pkg != nil && pass.Pkg.Path() == lispPkgPath
}

// checkNativeAssign handles a write to the lisp.LVal.Native field on an
// existing value -- the bypass that can put a payload into a value the
// assigning function does not own -- however the field is reached.
func checkNativeAssign(pass *analysis.Pass, stmt *ast.AssignStmt, allow map[int]bool) {
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
		reportNativePayload(pass, stmt.Pos(), pass.TypesInfo.TypeOf(stmt.Rhs[i]), "LVal.Native assignment",
			payloadSite{kind: siteFieldWrite, inKernel: inKernelPkg(pass)}, allow)
	}
}

// checkNativeAddress handles `&v.Native`: a pointer through which any
// payload can later be stored, with no type at this site to classify.
func checkNativeAddress(pass *analysis.Pass, expr *ast.UnaryExpr, allow map[int]bool) {
	if expr.Op != token.AND {
		return
	}
	sel, ok := ast.Unparen(expr.X).(*ast.SelectorExpr)
	if !ok || !selectsNativeField(pass, sel) {
		return
	}
	if allow[pass.Fset.Position(expr.Pos()).Line] {
		return
	}
	pass.Reportf(expr.Pos(),
		"address of LVal.Native taken: whatever is later stored through the pointer is a native payload"+
			" this rule cannot see the type of; store through the field directly so the payload is"+
			" checked at the store, or annotate //%s with a justification",
		nativeAllowMarker)
}

// reportNativePayload classifies payload and reports at pos unless a
// justified marker covers pos's line or any of the also lines (a literal's
// key and value positions).
func reportNativePayload(pass *analysis.Pass, pos token.Pos, payload types.Type, what string, site payloadSite, allow map[int]bool, also ...token.Pos) {
	if allow[pass.Fset.Position(pos).Line] {
		return
	}
	for _, p := range also {
		if allow[pass.Fset.Position(p).Line] {
			return
		}
	}
	switch classifyPayload(payload, site) {
	case payloadSafe:
		return
	case payloadDynamic:
		pass.Reportf(pos,
			"%s payload type %s is not statically known (an interface or type parameter), so whether a"+
				" template may publish it cannot be checked here; construct the native from a concrete"+
				" type, or annotate //%s with a justification naming the contract the payload is held to",
			what, payloadTypeString(payload), nativeAllowMarker)
	case payloadDiagnostic:
		pass.Reportf(pos,
			"%s payload type %s is a retained diagnostic stack, which"+
				" (*templateInventory).checkDiagnosticPayload in lisp/template.go refuses at publication"+
				" before any sharing policy runs, so no marker, allowlist row or native policy can make it"+
				" publishable; keep the stack out of the payload, or annotate //%s with a justification"+
				" that the value never reaches a template",
			what, payloadTypeString(payload), nativeAllowMarker)
	case payloadKernelSlotMisuse:
		row := allowedPayloadTypes[types.TypeString(types.Unalias(payload), nil)]
		pass.Reportf(pos,
			"%s payload type %s is a kernel representation slot, and the allowlist row for it is a claim"+
				" about a HEADER, not about the type: it is true only of package lisp building an %s"+
				" header, which (*templateInventory).val routes to its own arm (lisp/template.go)."+
				" %s Every other header is an LNative, whose payload val hands to native(), and native()"+
				" refuses this type; build the header the kernel builds, or annotate //%s with a"+
				" justification that the value provably never reaches a template",
			what, payloadTypeString(payload), row.headerType, site.misuseReason(row), nativeAllowMarker)
	case payloadReport:
		pass.Reportf(pos,
			"%s payload type %s is not a known-safe value type: a template publishes one value graph and"+
				" shares an admitted native payload with every VM it mints, so publication admits only"+
				" audited immutable payloads; embed internal/templatepolicy.Marker on an immutable STRUCT"+
				" VALUE, have the embedder approve the payload with lisp.TemplateWithNativePolicy, or"+
				" annotate //%s with a justification that the payload provably never reaches a template",
			what, payloadTypeString(payload), nativeAllowMarker)
	}
}

// misuseReason says which of exemptsRow's conditions this site failed, so
// the diagnostic names the actual problem rather than reciting the rule.
// It is only called on a site the row did NOT exempt, so at least one arm
// applies; the fallthrough covers a future site kind added without a
// verdict.
func (s payloadSite) misuseReason(row payloadRow) string {
	if !s.inKernel {
		return "This site is outside package lisp, whose own storage the row describes."
	}
	switch {
	case s.kind == siteConstructor:
		return "A constructor always builds an LNative."
	case s.kind == siteHeaderLiteral && s.headerType == "":
		return "This literal sets no Type key, so it shows no header at all."
	case s.kind == siteHeaderLiteral:
		return "This literal's Type key is " + s.headerType + ", not " + row.headerType + "."
	}
	return "This site does not show the header the row belongs to."
}

type payloadVerdict int

const (
	// payloadSafe: publishing this payload in a template cannot be observed
	// by a VM, on the same grounds the runtime admits it.
	payloadSafe payloadVerdict = iota
	// payloadReport: plausibly mutable, or simply unclassified.
	payloadReport
	// payloadDynamic: interface-typed or a type parameter, so there is no
	// static payload type to classify.
	payloadDynamic
	// payloadDiagnostic: a retained call stack, which publication refuses
	// outright -- no marker, row or policy can make it admissible.
	payloadDiagnostic
	// payloadKernelSlotMisuse: an allowedPayloadTypes row reached at a site
	// that does not show the row's header.  The row is real, but it
	// describes the kernel's own representation storage on a specific
	// non-LNative header: a constructor makes an LNative, a literal naming
	// another Type (or naming none) shows no such header, and a site
	// outside package lisp is not the kernel writing its own storage.
	// templateInventory.val hands an LNative's payload to native(), and
	// native() refuses every row type.
	payloadKernelSlotMisuse
)

// classifyPayload decides a payload type's verdict, mirroring
// (*templateInventory).native (lisp/template.go) plus the diagnostic ban in
// checkDiagnosticPayload.  The ORDER matters: the ban comes first because no
// later tier may override it, then the allowlist and the marker tier are
// consulted on the type ITSELF, before the underlying type is looked at --
// an audited kernel slot is a pointer and a marked payload is a struct, both
// of which would otherwise be reported.
func classifyPayload(t types.Type, site payloadSite) payloadVerdict {
	if t == nil {
		return payloadDynamic
	}
	t = types.Unalias(t)
	if _, ok := t.(*types.TypeParam); ok {
		return payloadDynamic
	}
	if isRetainedCallStack(t) {
		return payloadDiagnostic
	}
	if row, ok := allowedPayloadTypes[types.TypeString(t, nil)]; ok {
		// A row is a claim about a HEADER templateInventory.val handles by
		// an explicit arm, never about a payload it routes to native().  A
		// constructor builds an LNative and so always routes to native(),
		// where every row type is refused; a literal that names some other
		// header -- LNative above all -- routes there too; and a site
		// outside package lisp is not building the kernel's storage at all.
		// Saying that precisely beats reporting the type as an
		// unclassified pointer.
		if site.exemptsRow(row) {
			return payloadSafe
		}
		return payloadKernelSlotMisuse
	}
	if declaresTemplateImmutable(t) {
		return payloadSafe
	}
	switch u := t.Underlying().(type) {
	case *types.Basic:
		if runtimeScalarKinds[u.Kind()] {
			return payloadSafe
		}
		// Uintptr and UnsafePointer: an address wearing a basic type's
		// clothes, and named in the runtime's REJECTED arm.
		return payloadReport
	case *types.Interface:
		return payloadDynamic
	}
	return payloadReport
}

// runtimeScalarKinds mirrors, kind for kind, the scalar arm of
// (*templateInventory).native (lisp/template.go).  That switch also spells
// out the kinds it refuses, so the two lists can be read against each other:
// of the kinds that can reach here at all -- the ones with a *types.Basic
// underlying type -- reflect.Uintptr and reflect.UnsafePointer are on the
// runtime's refused arm and are therefore absent here.  Both are addresses
// with a scalar's manners: immutable inside the interface, but a VM that
// converts one back through unsafe reaches whatever the publisher was
// pointing at.  Every composite kind the runtime refuses has no basic
// underlying type and so never reaches this map at all.
//
// The untyped kinds are defensive rather than load-bearing: an untyped
// constant argument is converted to its DEFAULT type before it lands in an
// interface parameter, so what the type checker records is int, float64,
// bool, string or rune (= int32), each of which is scalar at runtime too.
// An untyped nil argument makes a nil payload, which native() admits on its
// first line.
var runtimeScalarKinds = map[types.BasicKind]bool{
	types.Bool:   true,
	types.String: true,

	types.Int:   true,
	types.Int8:  true,
	types.Int16: true,
	types.Int32: true,
	types.Int64: true,

	types.Uint:   true,
	types.Uint8:  true,
	types.Uint16: true,
	types.Uint32: true,
	types.Uint64: true,

	types.Float32:    true,
	types.Float64:    true,
	types.Complex64:  true,
	types.Complex128: true,

	types.UntypedBool:    true,
	types.UntypedInt:     true,
	types.UntypedRune:    true,
	types.UntypedFloat:   true,
	types.UntypedComplex: true,
	types.UntypedString:  true,
	types.UntypedNil:     true,
}

// declaresTemplateImmutable reports whether t is admitted by the marker tier
// of (*templateInventory).native: a STRUCT VALUE whose method set carries
// internal/templatepolicy.Immutable's unexported templateImmutable().
//
// Both halves are the runtime's.  The method is matched by name AND by
// declaring package, which is how the runtime's type assertion behaves --
// an unexported method is only satisfiable by embedding templatepolicy.Marker,
// which nothing outside this module can import.  The struct-value half is the
// half that is easy to lose: a *T inherits T's method set and so would pass
// an assertion, but the runtime additionally requires
// reflect.TypeOf(payload).Kind() == reflect.Struct, because a caller holding
// the pointer can replace the whole pointee no matter how private its fields
// are.  A pointer form needs TemplateWithNativePolicy approval instead.
func declaresTemplateImmutable(t types.Type) bool {
	if _, ok := t.Underlying().(*types.Struct); !ok {
		return false
	}
	ms := types.NewMethodSet(t)
	for i := range ms.Len() {
		fn, ok := ms.At(i).Obj().(*types.Func)
		if !ok || fn.Name() != templateImmutableMethod {
			continue
		}
		if fn.Pkg() == nil || fn.Pkg().Path() != templatePolicyPkgPath {
			continue
		}
		sig, ok := fn.Type().(*types.Signature)
		if ok && sig.Params().Len() == 0 && sig.Results().Len() == 0 {
			return true
		}
	}
	return false
}

// isRetainedCallStack reports whether t is lisp.CallStack or *lisp.CallStack
// -- the payload (*templateInventory).checkDiagnosticPayload rejects by name,
// typed nils and non-error headers included, before any sharing policy runs.
func isRetainedCallStack(t types.Type) bool {
	if ptr, ok := t.(*types.Pointer); ok {
		t = types.Unalias(ptr.Elem())
	}
	named, ok := t.(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj != nil && obj.Name() == callStackTypeName &&
		obj.Pkg() != nil && obj.Pkg().Path() == lispPkgPath
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
// without falling through to Native.  It mirrors the switch in lisp/lisp.go
// arm for arm -- bool, string, []byte, int, float64, []*LVal -- and the
// mirror has to be EXACT IN BOTH DIRECTIONS.  Matching too little reports a
// call that constructs no native; matching too MUCH silently exempts one
// that does, which is the failure this shape had: an isLValType that
// accepted LVal *or* *LVal stripped one pointer layer too many, so a
// `[]**LVal` -- which Value has no arm for, and which therefore becomes an
// opaque native that publication refuses -- read as Value's `[]*LVal` arm.
//
// Identity is the right comparison throughout: a Go type switch matches
// `case []byte` only for the unnamed type, so `type Blob []byte` DOES become
// a native, and `[]*ErrorVal` is not `[]*LVal` however alike the two look.
func directlyRepresentable(t types.Type) bool {
	if t == nil {
		return false
	}
	switch u := types.Unalias(t).(type) {
	case *types.Basic:
		switch u.Kind() {
		case types.Bool, types.String, types.Int, types.Float64,
			// An untyped constant argument is recorded with its default
			// type, so these are defensive.  The defaults for an untyped
			// rune and an untyped complex are deliberately absent: Value
			// has no int32 or complex128 arm either.
			types.UntypedBool, types.UntypedString, types.UntypedInt, types.UntypedFloat:
			return true
		default:
			return false
		}
	case *types.Slice:
		elem := types.Unalias(u.Elem())
		if b, ok := elem.(*types.Basic); ok && b.Kind() == types.Uint8 {
			return true // []byte -- the unnamed element type only
		}
		ptr, ok := elem.(*types.Pointer)
		if !ok {
			return false
		}
		return isLValNamed(types.Unalias(ptr.Elem())) // []*lisp.LVal, and nothing deeper
	}
	return false
}

// isLValNamed reports whether t is EXACTLY the named type lisp.LVal.  It
// does NOT look through a pointer: its one caller has already accounted for
// the single layer Value's `[]*LVal` arm spells, and looking through a
// second is what let `[]**LVal` pass.  Field access is matched on the field
// object (isNativeField), never on a receiver's spelled type, so nothing
// else needs this.
func isLValNamed(t types.Type) bool {
	named, ok := t.(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj != nil && obj.Name() == "LVal" && obj.Pkg() != nil && obj.Pkg().Path() == lispPkgPath
}

// isNativeField reports whether obj is the lisp.LVal.Native field object --
// the one field of that name declared in package lisp (lisp/lisp.go).  A
// defined type over LVal (ErrorVal) shares the struct and therefore the
// object; a promoted selection through an embedding struct resolves to it;
// a struct in another package that happens to have a field called Native
// does not.
func isNativeField(obj types.Object) bool {
	v, ok := obj.(*types.Var)
	return ok && v.IsField() && v.Name() == nativeFieldName && v.Pkg() != nil && v.Pkg().Path() == lispPkgPath
}

// selectsNativeField reports whether sel is a field selection of
// lisp.LVal.Native, by whatever receiver expression and embedding path.
func selectsNativeField(pass *analysis.Pass, sel *ast.SelectorExpr) bool {
	s, ok := pass.TypesInfo.Selections[sel]
	return ok && s.Kind() == types.FieldVal && isNativeField(s.Obj())
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

// justifiedNativeAllow reports whether a comment's text is an
// //elpsvet:allow-native marker THAT CARRIES A JUSTIFICATION: the marker,
// whitespace, and at least nativeAllowMinWords words.  The rule cannot check
// that the words are true, only that somebody wrote a sentence down where
// the next reader will see it.
func justifiedNativeAllow(text string) bool {
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
	return len(strings.Fields(rest)) >= nativeAllowMinWords
}

func hasJustifiedNativeAllow(cg *ast.CommentGroup) bool {
	if cg == nil {
		return false
	}
	for _, c := range cg.List {
		if justifiedNativeAllow(c.Text) {
			return true
		}
	}
	return false
}
