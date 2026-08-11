// Retro-catch fixtures for the elpsescape analyzer (issue #375): the
// pre-fix bodies of ErrorCondition/ErrorConditionf (fixed in ac0a326) and
// ErrorAssociate (fixed in d922290), mirrored from the historical
// lisp/env.go.  The analyzer must flag each exactly where the aliased
// env.Loc store sat, proving the rule would have caught both bugs before
// review did.  The Fixed* variants mirror the post-fix bodies and must
// stay clean, and the deliberate-alias variants carry the //elps:aliases
// annotation the real tree uses.

package lisp

// ErrorCondition mirrors the pre-ac0a326 body: env.Loc aliased into the
// escaping error's source field through the composite literal.
func (env *LEnv) ErrorCondition(condition string, v ...interface{}) *LVal {
	cells := make([]*LVal, 0, len(v))
	for range v {
		cells = append(cells, &LVal{})
	}
	lerr := &LVal{
		Type:   LError,
		source: env.Loc, // want `LVal composite literal field \.source stores a runtime-owned \*token\.Location`
		Str:    condition,
		Native: env.Runtime.Stack.Copy(),
		Cells:  cells,
	}
	return lerr
}

// ErrorConditionf mirrors the pre-ac0a326 body of the formatting variant.
func (env *LEnv) ErrorConditionf(condition string, format string, v ...interface{}) *LVal {
	lerr := &LVal{
		source: env.Loc, // want `LVal composite literal field \.source stores a runtime-owned \*token\.Location`
		Type:   LError,
		Str:    condition,
		Native: env.Runtime.Stack.Copy(),
		Cells:  []*LVal{String(format)},
	}
	return lerr
}

// ErrorAssociate mirrors the pre-d922290 body: env.Loc aliased into an
// in-flight error the caller keeps.  The target LVal is a parameter — the
// freshness rule's fresh/not-fresh axis says nothing here; the escape rule
// flags the tainted store itself.
func (env *LEnv) ErrorAssociate(lerr *LVal) *LVal {
	if lerr.Type != LError {
		return &LVal{Type: LError}
	}
	if lerr.source == nil {
		lerr.source = env.Loc // want `write to LVal field \.source stores a runtime-owned \*token\.Location`
	}
	return nil
}

// FixedErrorConditionf mirrors the post-ac0a326 body: the location is
// routed through copyLocation, so the store is clean.
func (env *LEnv) FixedErrorConditionf(condition string, format string, v ...interface{}) *LVal {
	lerr := &LVal{
		source: copyLocation(env.Loc),
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
		lerr.source = copyLocation(env.Loc)
	}
	return nil
}

// TaggedValue mirrors the deliberate in-runtime alias shape (lisp/env.go's
// TaggedValue and Lambda): the annotation with a justification suppresses
// the diagnostic on the aliasing line.
func (env *LEnv) TaggedValue(typ *LVal, val *LVal) *LVal {
	return &LVal{
		//elps:aliases fixture justification — runtime-internal value; see lisp/env.go
		source: env.Loc,
		Str:    typ.Str,
		Cells:  []*LVal{val},
	}
}

// taintThroughLocal proves taint follows local assignments: the location
// read off runtime state keeps its taint through an intermediate variable.
func (env *LEnv) taintThroughLocal(lerr *LVal) {
	loc := env.Loc
	lerr.source = loc // want `write to LVal field \.source stores a runtime-owned \*token\.Location`
}

// sourceOffLVal proves v.source reads taint like env.Loc reads: aliasing
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
