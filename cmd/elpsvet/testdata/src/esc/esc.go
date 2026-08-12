// Package esc exercises the elpsescape analyzer's cross-package shapes:
// SetSource with a tainted location, the parser-style deref-copy cleanse,
// return-escape through a composite literal, field writes on returned
// values and package-level state, method taint sources, and the
// //elps:aliases escape hatch.
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

// setSourceTainted stores a location read off runtime state through the
// exported setter — the same field write as lerr.source = env.Loc.
func setSourceTainted(env *lisp.LEnv, v *lisp.LVal) {
	v.SetSource(env.Loc) // want `SetSource call stores a runtime-owned \*token\.Location`
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
func returnEscapeComposite(env *lisp.LEnv) *funcInfo {
	return &funcInfo{Source: env.Loc} // want `returning a value whose composite literal captured a field stores a runtime-owned \*token\.Location`
}

// returnEscapeLocal captures the same shape through a local.
func returnEscapeLocal(env *lisp.LEnv) *funcInfo {
	info := &funcInfo{Source: env.Loc}
	info.Name = "f"
	return info // want `returning a value whose composite literal captured a field stores a runtime-owned \*token\.Location`
}

// returnedFieldStore stores a tainted location into a field of a value the
// function returns.
func returnedFieldStore(env *lisp.LEnv) *funcInfo {
	info := &funcInfo{Name: "f"}
	info.Source = env.Loc // want `write to field \.Source of a value this function returns stores a runtime-owned \*token\.Location`
	return info
}

// localFieldStoreDoesNotEscape stores a tainted location into a local that
// never leaves the function: out of the rule's escape scope.
func localFieldStoreDoesNotEscape(env *lisp.LEnv) string {
	info := &funcInfo{Name: "f"}
	info.Source = env.Loc
	return info.Name
}

// registryStore stores a tainted location into package-level state.
func registryStore(env *lisp.LEnv) {
	registry.LastLoc = env.Loc // want `write to field \.LastLoc of package-level state stores a runtime-owned \*token\.Location`
}

// copyCleansed routes the location through the copyLocation cleanser.
func copyCleansed(env *lisp.LEnv) *funcInfo {
	return &funcInfo{Source: copyLocation(env.Loc)}
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
func compositeLiteralAnnotation(env *lisp.LEnv) *funcInfo {
	//elps:aliases fixture justification for the returned literal below
	return &funcInfo{
		Name:   "f",
		Source: env.Loc,
	}
}
