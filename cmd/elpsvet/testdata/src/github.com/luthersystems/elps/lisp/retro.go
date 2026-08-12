// Retro-catch fixtures for the elpsescape analyzer (issue #375): the
// pre-fix bodies of ErrorCondition/ErrorConditionf (fixed in ac0a326) and
// ErrorAssociate (fixed in d922290), mirrored from the historical
// lisp/env.go.  The analyzer must flag each exactly where the aliased
// env.loc store sat, proving the rule would have caught both bugs before
// review did.  The Fixed* variants mirror the post-fix bodies and must
// stay clean, and the deliberate-alias variants carry the //elps:aliases
// annotation the real tree uses.

package lisp

import "github.com/luthersystems/elps/parser/token"

// ErrorCondition mirrors the pre-ac0a326 body: env.loc aliased into the
// escaping error's source field through the composite literal.
func (env *LEnv) ErrorCondition(condition string, v ...interface{}) *LVal {
	cells := make([]*LVal, 0, len(v))
	for range v {
		cells = append(cells, &LVal{})
	}
	lerr := &LVal{
		Type:   LError,
		source: env.loc, // want `LVal composite literal field \.source stores a runtime-owned \*token\.Location`
		Str:    condition,
		Native: env.Runtime.Stack.Copy(),
		Cells:  cells,
	}
	return lerr
}

// ErrorConditionf mirrors the pre-ac0a326 body of the formatting variant.
func (env *LEnv) ErrorConditionf(condition string, format string, v ...interface{}) *LVal {
	lerr := &LVal{
		source: env.loc, // want `LVal composite literal field \.source stores a runtime-owned \*token\.Location`
		Type:   LError,
		Str:    condition,
		Native: env.Runtime.Stack.Copy(),
		Cells:  []*LVal{String(format)},
	}
	return lerr
}

// ErrorAssociate mirrors the pre-d922290 body: env.loc aliased into an
// in-flight error the caller keeps.  The target LVal is a parameter — the
// freshness rule's fresh/not-fresh axis says nothing here; the escape rule
// flags the tainted store itself.
func (env *LEnv) ErrorAssociate(lerr *LVal) *LVal {
	if lerr.Type != LError {
		return &LVal{Type: LError}
	}
	if lerr.source == nil {
		lerr.source = env.loc // want `write to LVal field \.source stores a runtime-owned \*token\.Location`
	}
	return nil
}

// FixedErrorConditionf mirrors the post-ac0a326 body: the location is
// routed through copyLocation, so the store is clean.
func (env *LEnv) FixedErrorConditionf(condition string, format string, v ...interface{}) *LVal {
	lerr := &LVal{
		source: copyLocation(env.loc),
		Type:   LError,
		Str:    condition,
		Native: env.Runtime.Stack.Copy(),
		Cells:  []*LVal{String(format)},
	}
	return lerr
}

// FixedErrorAssociate mirrors the post-d922290 body.
func (env *LEnv) FixedErrorAssociate(lerr *LVal) *LVal {
	if lerr.source == nil {
		lerr.source = copyLocation(env.loc)
	}
	return nil
}

// TaggedValue mirrors the deliberate in-runtime alias shape (lisp/env.go's
// TaggedValue and Lambda): the annotation with a justification suppresses
// the diagnostic on the aliasing line.
func (env *LEnv) TaggedValue(typ *LVal, val *LVal) *LVal {
	return &LVal{
		//elps:aliases fixture justification — runtime-internal value; see lisp/env.go
		source: env.loc,
		Str:    typ.Str,
		Cells:  []*LVal{val},
	}
}

// taintThroughLocal proves taint follows local assignments: the location
// read off runtime state keeps its taint through an intermediate variable.
func (env *LEnv) taintThroughLocal(lerr *LVal) {
	loc := env.loc
	lerr.source = loc // want `write to LVal field \.source stores a runtime-owned \*token\.Location`
}

// sourceOffLVal proves v.source reads taint like env.loc reads: aliasing
// one value's location into another escaping value is the same bug.
func sourceOffLVal(dst, src *LVal) {
	dst.source = src.source // want `write to LVal field \.source stores a runtime-owned \*token\.Location`
}

// freshSourceIsOwned proves the freshness carve-out: a location read off an
// LVal this function constructed is this function's own memory, so storing
// it elsewhere is not an escape of runtime state.
func freshSourceIsOwned(dst *LVal) {
	v := &LVal{}
	dst.source = v.source
}

// sourceAccessorInPackage proves the freshLocation fact resolves inside the
// DEFINING package as well as across the import graph, and across FILES
// within it: Source is declared in lisp.go, and the summary pass runs over
// every file before any call site is checked.
func (env *LEnv) sourceAccessorInPackage(lerr *LVal) {
	lerr.source = env.Source()
}

// locRefInPackage is the same store through the by-reference accessor and
// must still be flagged.  Source and LocRef have identical signatures and
// identical receivers; only the callee body differs, which is precisely the
// distinction the fact carries and the old name/receiver heuristic could
// not make.
func (env *LEnv) locRefInPackage(lerr *LVal) {
	lerr.source = env.LocRef() // want `write to LVal field \.source stores a runtime-owned \*token\.Location`
}

// mintedCopy proves the cleanser is now recognised by PROOF rather than by
// being spelled "copyLocation": the fact is computed from the body
// (cp := *loc; return &cp), so this identically-bodied function under a
// different name is just as clean.
func mintedCopy(loc *token.Location) *token.Location { // want mintedCopy:"freshLocation"
	if loc == nil {
		return nil
	}
	cp := *loc
	return &cp
}

func mintedCopyStore(dst *LVal, src *LVal) {
	dst.source = mintedCopy(src.source)
}

// cycleA and cycleB pin that the least fixpoint cannot bootstrap a
// recursive cycle into a fact: each one's only return is the other, so
// neither is ever proven and callers keep the conservative treatment.
func (env *LEnv) cycleA() *token.Location { return env.cycleB() }

func (env *LEnv) cycleB() *token.Location { return env.cycleA() }

func cycleStore(env *LEnv, dst *LVal) {
	dst.source = env.cycleA() // want `write to LVal field \.source stores a runtime-owned \*token\.Location`
}

// namedResultBareReturn pins the bare-return refusal: the assignment to the
// named result is not tracked, so nothing is proven and the caller below
// stays flagged even though the body does allocate.  Refusing to prove is
// the safe direction; a vacuous "all returns are fresh" over zero tracked
// returns would be a hole.
func (env *LEnv) namedResultBareReturn() (loc *token.Location) {
	loc = copyLocation(env.loc)
	return
}

func namedResultStore(env *LEnv, dst *LVal) {
	dst.source = env.namedResultBareReturn() // want `write to LVal field \.source stores a runtime-owned \*token\.Location`
}
