package lisp

// In package lisp the rules match unqualified names, and the fixes write
// the helpers without a qualifier.
func inLisp(env *LEnv, v *LVal, n int) *LVal {
	if v.Type == LError { // want `use v.IsError\(\), which is the same compare`
		return v
	}
	if GoError(v) != nil { // want `use v.IsError\(\), which is the same test`
		return v
	}
	if v != nil && v.Type == LError { // want `use v.IsError\(\), which is the same test: it is false for a nil value`
		return v
	}
	if v.Type == LSymbol && v.Str == TrueSymbol { // want `use v.IsSymbol\(TrueSymbol\), which is the same compare`
		return v
	}
	if msg := env.Runtime.CheckAlloc(n); msg != "" { // want `use env.CheckAlloc, which returns the same error`
		return env.Errorf("%s", msg)
	}
	l := QExpr([]*LVal{v})                          // want `use Cells\{...\}.List\(\), which builds the same list`
	sx := SExpr([]*LVal{v, l})                      // want `use Cells\{...\}.SExpr\(\), which builds the same s-expression`
	vec := Array(nil, sx.Cells)                     // want `use Vector\(sx.Cells\), which is Array\(nil, ...\)`
	out := &LVal{Type: LSExpr, Cells: []*LVal{vec}} // want `use Cells\{...\} for the Cells field`
	out.Cells = []*LVal{vec, l}                     // want `use Cells\{...\} for the Cells field`
	out.Cells = sx.Cells
	m := SortedMap() // want `use MapOf with the 2 keys`
	m.MapSetString("a", v)
	m.MapSetString("b", l)
	return m
}

// A local name that shadows the helper stops the fix.
func shadowed(v *LVal) *LVal {
	Vector := func(c []*LVal) *LVal { return nil }
	_ = Vector
	Cells := 0
	_ = Cells
	return Array(nil, QExpr([]*LVal{v}).Cells)
}

// Hints and mistakes are not reported in package lisp.
func hintsOff(v *LVal) (*ErrorVal, bool) {
	return nil, v.Type == LNative
}

// The Cells fixes write Cells without a qualifier.
func cellsInLisp(v *LVal) *LVal {
	out := make([]*LVal, len(v.Cells)) // want `use Cells\(v.Cells\).Map, which builds the same slice`
	for i, x := range v.Cells {
		out[i] = String(x.Str)
	}
	c := append([]*LVal{}, v.Cells...) // want `use Cells\(v.Cells\).Clone\(\), which makes the same copy`
	return SExpr(append(out, c...))
}
