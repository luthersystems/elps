// Copyright © 2026 The ELPS authors

// The elpsdurablenative analyzer: every native payload type that a module
// builds is durable or transient.  A durable dump (libjson.DumpDurable)
// saves a native only when its registry has a codec for the payload's Go
// type.  Without this rule a missing codec shows only at run time, as a
// "cannot be saved" error on the first dump that holds the value.
//
// # The rule
//
// A native construction (the sites elpsnativepayload reads, see
// walkNatives) is reported unless its payload type is one of:
//
//  1. durable: a package-level value of type libjson.DurableCodec[T], with T
//     identical to the payload type, is declared in the analysed package or
//     exported by a package it imports, in the analysed module.  The
//     module's registry lists only its own codecs, so a codec of another
//     module counts only when the module re-declares it, for example
//     `var TimeCodec = libtime.DurableTimeCodec.WithName("m:time")`;
//  2. transient: the payload type's named type declares a documented
//     TransientNative() method (the libjson.TransientNative interface).  A
//     value receiver marks T and *T; a pointer receiver marks only *T.  A
//     TransientNative promoted from an embedded field does not count, and
//     is reported;
//  3. transient at this site: a `//<TransientMarker> <reason>` comment
//     covers the site (see transientLines).  It applies only to a type
//     declared outside the analysed module (pointers stripped), which
//     cannot have a method.  The marker word must end at a space or at the
//     end of the comment.  One marker covers one construction: a marker
//     that covers none, or more than one, is reported.
//
// A type that is both durable and transient is reported.  A payload that
// is a type parameter (a generic wrapper) is reported: build the native
// where the type is concrete.  An interface-typed payload is not reported:
// its runtime type is not known here.  A kernel representation slot
// (*[]byte, *lisp.MapData, *lisp.funData, the elpsnativepayload allowlist)
// is not a native and is not reported.  In package lisp, a retained call
// stack, a literal whose Type key names a header other than LNative, and a
// type-parameter payload (the generic constructor NativeOf) are not
// reported either: none of them is an LNative payload a dump can meet.
//
// The analyzer also checks the declarations:
//
//   - a TransientNative method needs a doc comment that says why the type
//     is never saved, and a site marker needs a reason;
//   - a call of libjson.NewFrozenDurableRegistry must list every
//     DurableCodec and ForeignCodec value of the analysed module that the
//     calling package declares or that a package it imports exports.  An
//     argument `Codec.WithName("x")` lists Codec.  A call that spreads a
//     slice (codecs...) is not checked;
//   - an unexported codec value is reported, unless its package has a
//     NewFrozenDurableRegistry call that lists codecs one by one, because
//     no registry outside the package can list it;
//   - a pointer codec value is reported.  The analyzer reads only values.
//
// # Limits
//
// The analyzer checks only the packages of its Module: run over another
// module's tree, it reports nothing there.  A native built in another
// module is that module's to check, with its own DurableConfig.  The codecs it
// sees depend on the packages a run loads: under go vet -vettool
// (unitchecker), imports come from export data, which can leave out an
// indirect import.  A ForeignCodec makes no payload type durable, because
// its reflect.Type is a run-time value.  A registry that lists a codec is
// not checked against the codecs a dump meets at run time: a module that
// must save a type keeps a test that dumps one.
package nativepayload

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
)

const (
	// libjsonPkgPath declares DurableCodec, ForeignCodec,
	// NewFrozenDurableRegistry and TransientNative.
	libjsonPkgPath = "github.com/luthersystems/elps/lisp/lisplib/libjson"

	// transientMethod is the method of libjson.TransientNative.
	transientMethod = "TransientNative"

	// registryFunc is the libjson function that registers codec values.
	registryFunc = "NewFrozenDurableRegistry"

	// elpsModule is the module elps's own configuration analyses.
	elpsModule = "github.com/luthersystems/elps"
)

// DurableConfig configures an elpsdurablenative analyzer.  Module is
// required; the other fields have defaults.
type DurableConfig struct {
	// Name is the analyzer's name.  Default "elpsdurablenative".
	Name string
	// Module is the analysed module's path, and it is required.  The
	// analyzer checks only packages of this module.  Only a codec declared
	// in it makes a type durable, the site marker cannot mark a type
	// declared in it, and a NewFrozenDurableRegistry call must list every
	// codec value it declares.
	Module string
	// TransientMarker is the site marker without the leading "//".
	// Default "elpsvet:transient".
	TransientMarker string
	// Registry says where a new codec must be listed, for the diagnostic.
	// Default "pass it to libjson.NewFrozenDurableRegistry".
	Registry string
}

// DurableAnalyzer is elps's own elpsdurablenative analyzer, for the module
// github.com/luthersystems/elps.
var DurableAnalyzer = NewDurable(DurableConfig{Module: elpsModule})

// NewDurable returns an elpsdurablenative analyzer with the given
// configuration.  It panics when cfg.Module is empty.
func NewDurable(cfg DurableConfig) *analysis.Analyzer {
	if cfg.Module == "" {
		panic("nativepayload.NewDurable: DurableConfig.Module is empty; set it to the module path the rule checks")
	}
	if cfg.Name == "" {
		cfg.Name = "elpsdurablenative"
	}
	if cfg.TransientMarker == "" {
		cfg.TransientMarker = "elpsvet:transient"
	}
	if cfg.Registry == "" {
		cfg.Registry = "pass it to libjson." + registryFunc
	}
	d := &durable{cfg}
	return &analysis.Analyzer{
		Name: cfg.Name,
		Doc: "flag a lisp.LVal native payload type that has no libjson.DurableCodec and is not marked transient" +
			" (a TransientNative method or //" + cfg.TransientMarker + "), and a durable codec that" +
			" libjson." + registryFunc + " does not list",
		Run: d.run,
	}
}

// durable is a DurableConfig with its defaults applied.
type durable struct{ DurableConfig }

// codecVar is a package-level durable codec value.  payload is the T of a
// libjson.DurableCodec[T], or nil for a libjson.ForeignCodec.
type codecVar struct {
	obj     *types.Var
	payload types.Type
}

func (d *durable) run(pass *analysis.Pass) (any, error) {
	if !d.inModule(pass.Pkg.Path()) {
		// cmd/elpsvet runs over other modules' trees too; each module
		// checks its own natives with its own configuration.
		return nil, nil
	}
	codecs := visibleCodecs(pass.Pkg)
	if !d.checkRegistryCalls(pass, codecs) {
		for _, c := range codecs {
			if c.obj.Pkg() == pass.Pkg && !c.obj.Exported() {
				pass.Reportf(c.obj.Pos(),
					"durable codec %s is unexported, so no registry can list it; export it, or declare it in the"+
						" package that calls libjson.%s", c.obj.Name(), registryFunc)
			}
		}
	}
	checkPointerCodecs(pass)
	checkTransientMethods(pass)

	transient := map[*ast.File]map[int]*ast.Comment{}
	for _, file := range pass.Files {
		transient[file] = d.transientLines(pass.Fset, file)
		d.checkTransientReasons(pass, file)
	}
	covered := map[*ast.Comment]int{}
	walkNatives(pass, func(s nativeSite) {
		var marker *ast.Comment
		for _, p := range s.lines() {
			if m := transient[s.file][pass.Fset.Position(p).Line]; m != nil {
				marker = m
				break
			}
		}
		if marker != nil {
			covered[marker]++
		}
		d.check(pass, s, marker, codecs)
	})
	for _, file := range pass.Files {
		for _, marker := range transient[file] {
			switch n := covered[marker]; {
			case n == 0:
				pass.Reportf(marker.Pos(), "//%s covers no native construction; move it to the construction's line,"+
					" or delete it", d.TransientMarker)
			case n > 1:
				pass.Reportf(marker.Pos(), "//%s covers %d native constructions; it covers one, so put each"+
					" construction on its own line with its own marker", d.TransientMarker, n)
			}
		}
	}
	return nil, nil
}

// check reports one site whose payload type is neither durable nor
// transient.
func (d *durable) check(pass *analysis.Pass, s nativeSite, marker *ast.Comment, codecs []codecVar) {
	if s.address || s.payload == nil || isRetainedCallStack(types.Unalias(s.payload)) || kernelNonNative(s) {
		return
	}
	if isTypeParam(s.payload) {
		pass.Reportf(s.pos,
			"%s payload type %s is a type parameter, so its type is not known here; build the native where"+
				" the type is concrete", s.what, payloadTypeString(s.payload))
		return
	}
	switch u := s.payload.Underlying().(type) {
	case *types.Interface:
		return
	case *types.Basic:
		if u.Kind() == types.UntypedNil {
			return
		}
	}
	if _, ok := allowedPayloadTypes[types.TypeString(types.Unalias(s.payload), nil)]; ok {
		// A kernel representation slot; elpsnativepayload reports a misuse.
		return
	}
	if marker != nil && d.ownedByModule(s.payload) {
		pass.Reportf(s.pos,
			"%s payload type %s belongs to %s, so //%s cannot mark it; give the type a documented %s"+
				" method or a durable codec", s.what, payloadTypeString(s.payload), d.Module, d.TransientMarker,
			transientMethod)
		return
	}
	isDurable := false
	for _, c := range codecs {
		// Only a codec of this module counts: its registry lists that
		// module's codecs, so a codec of another module is not in it
		// unless the module re-declares it (WithName).
		if c.payload != nil && d.inModule(c.obj.Pkg().Path()) && types.Identical(c.payload, s.payload) {
			isDurable = true
			break
		}
	}
	kind := transientMethodOf(s.payload)
	isTransient := kind == transientDeclared || marker != nil
	switch {
	case kind == transientPromoted && !isTransient && !isDurable:
		pass.Reportf(s.pos,
			"%s payload type %s gets %s from an embedded field, which does not mark it transient;"+
				" declare a documented %s method on the type itself",
			s.what, payloadTypeString(s.payload), transientMethod, transientMethod)
	case isDurable && isTransient:
		pass.Reportf(s.pos, "%s payload type %s has a durable codec and is also marked transient; remove one",
			s.what, payloadTypeString(s.payload))
	case !isDurable && !isTransient:
		pass.Reportf(s.pos,
			"%s payload type %s has no durable codec and is not marked transient; declare a package-level"+
				" libjson.DurableCodec[%s] and %s, give the type a documented %s method"+
				" (libjson.TransientNative), or, for a type declared outside %s, mark the construction"+
				" //%s <reason>",
			s.what, payloadTypeString(s.payload), payloadTypeString(s.payload), d.Registry, transientMethod,
			d.Module, d.TransientMarker)
	}
}

// kernelNonNative reports whether a site in package lisp builds no
// LNative payload a dump could meet: a literal whose Type key names another
// header (an error's stack, the evaluator's terminal marker), or the generic
// constructor NativeOf, whose payload is a type parameter that every call
// site states.
func kernelNonNative(s nativeSite) bool {
	if !s.site.inKernel {
		return false
	}
	if s.site.kind == siteHeaderLiteral && s.site.headerType != "" && s.site.headerType != "LNative" {
		return true
	}
	return isTypeParam(s.payload)
}

// isTypeParam reports whether t, pointers stripped, is a type parameter.
func isTypeParam(t types.Type) bool {
	for {
		switch u := types.Unalias(t).(type) {
		case *types.Pointer:
			t = u.Elem()
		case *types.TypeParam:
			return true
		default:
			return false
		}
	}
}

// inModule reports whether a package path belongs to the analysed module.
func (d *durable) inModule(path string) bool {
	return path == d.Module || strings.HasPrefix(path, d.Module+"/")
}

// ownedByModule reports whether t's named type, pointers stripped, is
// declared in the analysed module.  Such a type can have a TransientNative
// method, so the site marker does not apply to it.
func (d *durable) ownedByModule(t types.Type) bool {
	for {
		p, ok := types.Unalias(t).(*types.Pointer)
		if !ok {
			break
		}
		t = p.Elem()
	}
	named, ok := types.Unalias(t).(*types.Named)
	if !ok || named.Obj().Pkg() == nil {
		return false
	}
	return d.inModule(named.Obj().Pkg().Path())
}

// checkPointerCodecs reports a package-level variable of type
// *libjson.DurableCodec[T] or *libjson.ForeignCodec.
func checkPointerCodecs(pass *analysis.Pass) {
	scope := pass.Pkg.Scope()
	for _, name := range scope.Names() {
		v, ok := scope.Lookup(name).(*types.Var)
		if !ok {
			continue
		}
		if p, ok := types.Unalias(v.Type()).(*types.Pointer); ok {
			if _, ok := codecPayload(p.Elem()); ok {
				pass.Reportf(v.Pos(), "durable codec %s is a pointer; declare it as a libjson.DurableCodec or"+
					" libjson.ForeignCodec value", name)
			}
		}
	}
}

// visibleCodecs returns the durable codec values pkg can see: every one
// pkg declares, and every exported one of the packages it imports,
// directly or through export data.
func visibleCodecs(pkg *types.Package) []codecVar {
	var out []codecVar
	seen := map[*types.Package]bool{}
	var visit func(p *types.Package)
	visit = func(p *types.Package) {
		if p == nil || seen[p] {
			return
		}
		seen[p] = true
		scope := p.Scope()
		for _, name := range scope.Names() {
			v, ok := scope.Lookup(name).(*types.Var)
			if !ok || (p != pkg && !v.Exported()) {
				continue
			}
			if payload, ok := codecPayload(v.Type()); ok {
				out = append(out, codecVar{obj: v, payload: payload})
			}
		}
		for _, imp := range p.Imports() {
			visit(imp)
		}
	}
	visit(pkg)
	return out
}

// codecPayload reports whether t is libjson.DurableCodec[T] (payload T) or
// libjson.ForeignCodec (payload nil).
func codecPayload(t types.Type) (types.Type, bool) {
	named, ok := types.Unalias(t).(*types.Named)
	if !ok {
		return nil, false
	}
	obj := named.Obj()
	if obj.Pkg() == nil || obj.Pkg().Path() != libjsonPkgPath {
		return nil, false
	}
	switch obj.Name() {
	case "DurableCodec":
		if args := named.TypeArgs(); args.Len() == 1 {
			return args.At(0), true
		}
	case "ForeignCodec":
		return nil, true
	}
	return nil, false
}

// checkRegistryCalls reports each codec value of the analysed module that a
// NewFrozenDurableRegistry call does not list.  A call that spreads a slice
// is not checked.  It reports whether the package has a call that lists its
// codecs one by one.
func (d *durable) checkRegistryCalls(pass *analysis.Pass, codecs []codecVar) bool {
	found := false
	for _, file := range pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			fn := calleeFunc(pass, call)
			if fn == nil || fn.Name() != registryFunc || fn.Pkg() == nil || fn.Pkg().Path() != libjsonPkgPath {
				return true
			}
			if call.Ellipsis.IsValid() {
				return true
			}
			found = true
			listed := map[types.Object]bool{}
			for _, arg := range call.Args {
				var id *ast.Ident
				switch a := withNameReceiver(pass, ast.Unparen(arg)).(type) {
				case *ast.Ident:
					id = a
				case *ast.SelectorExpr:
					id = a.Sel
				}
				if id != nil {
					listed[pass.TypesInfo.Uses[id]] = true
				}
			}
			for _, c := range codecs {
				if !listed[c.obj] && d.inModule(c.obj.Pkg().Path()) {
					pass.Reportf(call.Pos(), "libjson.%s does not list the durable codec %s.%s",
						registryFunc, c.obj.Pkg().Name(), c.obj.Name())
				}
			}
			return true
		})
	}
	return found
}

// withNameReceiver returns the codec a registry argument renames,
// `Codec.WithName("x")` listing Codec, or the argument itself.
func withNameReceiver(pass *analysis.Pass, arg ast.Expr) ast.Expr {
	call, ok := arg.(*ast.CallExpr)
	if !ok {
		return arg
	}
	sel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "WithName" {
		return arg
	}
	fn := calleeFunc(pass, call)
	if fn == nil || fn.Pkg() == nil || fn.Pkg().Path() != libjsonPkgPath {
		return arg
	}
	return ast.Unparen(sel.X)
}

// transientKind is how a payload type relates to TransientNative.
type transientKind int

const (
	// transientNone: the type has no TransientNative method.
	transientNone transientKind = iota
	// transientDeclared: the type's named type declares the method, so
	// the type is transient.
	transientDeclared
	// transientPromoted: the type's method set gets the method from an
	// embedded field, which does not mark the type.
	transientPromoted
)

// transientMethodOf reports whether payload type t is transient by its
// TransientNative method.  The method counts only when it is declared on
// t's named type: with a value receiver, it marks T and *T; with a pointer
// receiver, it marks only *T.
func transientMethodOf(t types.Type) transientKind {
	t = types.Unalias(t)
	ptr := false
	base := t
	if p, ok := t.(*types.Pointer); ok {
		ptr, base = true, types.Unalias(p.Elem())
	}
	if named, ok := base.(*types.Named); ok {
		for i := range named.NumMethods() {
			m := named.Method(i)
			if m.Name() != transientMethod || !isTransientSig(m.Type()) {
				continue
			}
			_, ptrRecv := types.Unalias(m.Signature().Recv().Type()).(*types.Pointer)
			if ptr || !ptrRecv {
				return transientDeclared
			}
		}
	}
	if sel := types.NewMethodSet(t).Lookup(nil, transientMethod); sel != nil && isTransientSig(sel.Type()) {
		return transientPromoted
	}
	return transientNone
}

// isTransientSig reports whether a method type has no parameters and no
// results, as libjson.TransientNative's method has.
func isTransientSig(t types.Type) bool {
	sig, ok := t.(*types.Signature)
	return ok && sig.Params().Len() == 0 && sig.Results().Len() == 0
}

// checkTransientMethods reports a TransientNative method without a doc
// comment: the comment is where a reader finds why the type is never saved.
func checkTransientMethods(pass *analysis.Pass) {
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			fd, ok := decl.(*ast.FuncDecl)
			if !ok || fd.Recv == nil || fd.Name.Name != transientMethod {
				continue
			}
			if fd.Doc == nil || strings.TrimSpace(fd.Doc.Text()) == "" {
				pass.Reportf(fd.Pos(), "%s needs a doc comment that says why the type is never saved", transientMethod)
			}
		}
	}
}

// transientLines maps each line a site marker covers to that marker.  A
// trailing marker covers its own line.  A standalone marker covers the
// first line after its comment block, so it can sit above the allow markers
// of the same construction, which cover only the next line.
func (d *durable) transientLines(fset *token.FileSet, file *ast.File) map[int]*ast.Comment {
	code := codeLines(fset, file)
	lines := make(map[int]*ast.Comment)
	for _, cg := range file.Comments {
		for _, c := range cg.List {
			if _, ok := d.transientReason(c.Text); !ok {
				continue
			}
			line := fset.Position(c.Pos()).Line
			if code[line] {
				lines[line] = c
			} else {
				lines[fset.Position(cg.End()).Line+1] = c
			}
		}
	}
	return lines
}

// transientReason reports whether comment text is a site marker, and
// returns the reason after it.  The marker word must end at a space or at
// the end of the comment.
func (d *durable) transientReason(text string) (string, bool) {
	text = strings.TrimPrefix(text, "//")
	if strings.HasPrefix(text, "/*") {
		text = strings.TrimSuffix(strings.TrimPrefix(text, "/*"), "*/")
	}
	rest, ok := strings.CutPrefix(strings.TrimSpace(text), d.TransientMarker)
	if !ok || (rest != "" && rest[0] != ' ' && rest[0] != '\t') {
		return "", false
	}
	return strings.TrimSpace(rest), true
}

// checkTransientReasons reports a site marker with no reason.
func (d *durable) checkTransientReasons(pass *analysis.Pass, file *ast.File) {
	for _, cg := range file.Comments {
		for _, c := range cg.List {
			if reason, ok := d.transientReason(c.Text); ok && reason == "" {
				pass.Reportf(c.Pos(), "//%s needs a reason", d.TransientMarker)
			}
		}
	}
}
