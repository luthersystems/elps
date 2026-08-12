// Package esc exercises the elpsescape analyzer's cross-package shapes:
// SetSource with a tainted location, the parser-style deref-copy cleanse,
// return-escape through a composite literal, field writes on returned
// values and package-level state, method taint sources, and the
// //elps:aliases escape hatch.
//
// Taint sources here are deliberately NOT lisp runtime fields.  After the
// #382 privatization lisp.LEnv.loc is unexported, so no external package can
// read a runtime-owned location off an environment -- the escape route this
// file used to model through env.Loc no longer exists in the real tree.  The
// shape it stood for does: a *token.Location field read off a struct the
// function did not construct, then stored into something that escapes.  That
// is exactly the live-tree metadata-translation shape carrying the audited
// annotations in lsp/definition.go and mcpserver/service.go, so the fixtures
// take their taint from funcInfo.Source, the struct this package owns.
// lisp.LEnv's one remaining external route, the copying Source() accessor,
// is covered by envSourceAccessor at the bottom of the file.
package esc

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// parser mimics rdparser.Parser: a Location method handing out a pointer
// into its own token state.
type parser struct {
	tok *token.Token
}

func (p *parser) Location() *token.Location {
	return p.tok.Source
}

// funcInfo mimics lisp.FunctionInfo: an exported metadata struct with a
// location field, returned to embedders.
type funcInfo struct {
	Source *token.Location
	Name   string
}

// registry mimics registry-reachable state.
var registry = struct {
	LastLoc *token.Location
}{} // package-level: stores into its fields escape process-wide

// setSourceTainted stores a location read off a struct this function did not
// construct through the exported setter — the same field write as the
// in-kernel lerr.source = env.loc.
func setSourceTainted(src *funcInfo, v *lisp.LVal) {
	v.SetSource(src.Source) // want `SetSource call stores a runtime-owned \*token\.Location`
}

// setSourceMethodTainted proves method results taint: a location-returning
// method on a value this function did not construct aliases that value's
// state (the rdparser p.Location() shape).
func setSourceMethodTainted(p *parser, v *lisp.LVal) {
	v.SetSource(p.Location()) // want `SetSource call stores a runtime-owned \*token\.Location`
}

// setSourceDerefCopy is the parser's sanctioned cleanse: copy the pointed-to
// value into a local and hand out the local's address.
func setSourceDerefCopy(p *parser, v *lisp.LVal) {
	loc := p.Location()
	if loc == nil {
		return
	}
	src := *loc
	v.SetSource(&src)
}

// setSourceFreshLiteral hands out a literal the function owns.
func setSourceFreshLiteral(v *lisp.LVal) {
	v.SetSource(&token.Location{File: "builtin"})
}

// setSourceAnnotated is the deliberate producer-side aliasing contract with
// its audited justification.
func setSourceAnnotated(p *parser, v *lisp.LVal) {
	//elps:aliases fixture justification — producer-owned fixup window; see rdparser.tokenLVal
	v.SetSource(p.Location())
}

// returnEscapeComposite captures a tainted location in a composite literal
// that is returned — the InspectFunction shape.
func returnEscapeComposite(src *funcInfo) *funcInfo {
	return &funcInfo{Source: src.Source} // want `returning a value whose composite literal captured a field stores a runtime-owned \*token\.Location`
}

// returnEscapeLocal captures the same shape through a local.
func returnEscapeLocal(src *funcInfo) *funcInfo {
	info := &funcInfo{Source: src.Source}
	info.Name = "f"
	return info // want `returning a value whose composite literal captured a field stores a runtime-owned \*token\.Location`
}

// returnedFieldStore stores a tainted location into a field of a value the
// function returns.
func returnedFieldStore(src *funcInfo) *funcInfo {
	info := &funcInfo{Name: "f"}
	info.Source = src.Source // want `write to field \.Source of a value this function returns stores a runtime-owned \*token\.Location`
	return info
}

// localFieldStoreDoesNotEscape stores a tainted location into a local that
// never leaves the function: out of the rule's escape scope.
func localFieldStoreDoesNotEscape(src *funcInfo) string {
	info := &funcInfo{Name: "f"}
	info.Source = src.Source
	return info.Name
}

// registryStore stores a tainted location into package-level state.
func registryStore(src *funcInfo) {
	registry.LastLoc = src.Source // want `write to field \.LastLoc of package-level state stores a runtime-owned \*token\.Location`
}

// copyCleansed routes the location through the copyLocation cleanser.
func copyCleansed(src *funcInfo) *funcInfo {
	return &funcInfo{Source: copyLocation(src.Source)}
}

// copyLocation mirrors lisp/detach.go's cleanser for the cross-package
// fixtures (the real one is unexported in package lisp).
func copyLocation(loc *token.Location) *token.Location {
	if loc == nil {
		return nil
	}
	cp := *loc
	return &cp
}

// setSourceTrailingAnnotation pins how far a TRAILING justification reaches:
// the line it trails, and no further.  A blanket line+1 suppression silenced
// the next statement's unrelated violation with nothing in the source saying
// it had ever been considered.
func setSourceTrailingAnnotation(p *parser, v, w *lisp.LVal) {
	v.SetSource(p.Location()) //elps:aliases fixture justification for this line only
	w.SetSource(p.Location()) // want `SetSource call stores a runtime-owned \*token\.Location`
}

// setSourceStandaloneAnnotation is the other half: a marker alone on its line
// is a preamble and must still cover the statement below it.
func setSourceStandaloneAnnotation(p *parser, v, w *lisp.LVal) {
	//elps:aliases fixture justification for the statement below
	v.SetSource(p.Location())
	w.SetSource(p.Location()) // want `SetSource call stores a runtime-owned \*token\.Location`
}

// compositeLiteralAnnotation pins where the justification must go for the
// RETURN-ESCAPE shape: the diagnostic is reported at the return, so a marker
// alone on its line inside the literal does not cover it and the annotation
// belongs above the return (the lsp/definition.go and mcpserver/service.go
// placement).  A standalone marker inside a multi-line literal still counts
// as standalone, which is what makes the lisp/env.go field placement work
// for the diagnostics reported at the field itself.
func compositeLiteralAnnotation(src *funcInfo) *funcInfo {
	//elps:aliases fixture justification for the returned literal below
	return &funcInfo{
		Name:   "f",
		Source: src.Source,
	}
}

// envSourceAccessor pins what became of the old env.Loc route after #382.
// lisp.LEnv.loc is unexported now, so the only way out of an environment is
// the Source() accessor — which returns a COPY and is therefore safe to
// store.  The rule cannot see that: its method-taint approximation is
// deliberately intraprocedural, so a location-returning method on a receiver
// the function did not construct is tainted regardless of what the callee
// does.  This is the same known false-positive class the live tree annotates
// at parser/token/scanner.go (LocStart), and an embedder resolves it the
// same way, with an audited annotation.
func envSourceAccessor(env *lisp.LEnv, v *lisp.LVal) {
	v.SetSource(env.Source()) // want `SetSource call stores a runtime-owned \*token\.Location`
}

// envSourceAccessorAnnotated is how that false positive is meant to be
// retired at a real call site.
func envSourceAccessorAnnotated(env *lisp.LEnv, v *lisp.LVal) {
	//elps:aliases fixture justification — LEnv.Source returns a copy (#382); the intraprocedural method-taint approximation cannot see inside the callee
	v.SetSource(env.Source())
}
