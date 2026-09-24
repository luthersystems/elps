package lisp

type LVal struct{ Str string }

type Package struct {
	symbols    map[string]*LVal
	baseValues []*LVal
	Name       string
}

type sortedmap struct {
	m  map[string]*LVal
	tm map[string]int
}

type mirror Package

type embedded struct{ *Package }

type unrelated struct {
	symbols map[string]*LVal
	m       map[string]*LVal
}

func bad(p *Package, s sortedmap, mp *mirror, e embedded) *LVal {
	_ = p.symbols["x"]  // want "direct access to lazily materialized table Package.symbols"
	_ = p.baseValues[0] // want "direct access to lazily materialized table Package.baseValues"
	for range s.m {     // want "direct access to lazily materialized table sortedmap.m"
	}
	p.symbols["x"] = nil   // want "direct access to lazily materialized table Package.symbols"
	_ = &s.m               // want "direct access to lazily materialized table sortedmap.m"
	_ = mp.symbols         // want "direct access to lazily materialized table Package.symbols"
	return e.baseValues[1] // want "direct access to lazily materialized table Package.baseValues"
}

func good(u unrelated, s sortedmap, p *Package) {
	_ = u.symbols["x"]
	_ = u.m["x"]
	_ = s.tm["x"]
	_ = p.Name
}

// Allowlisted accessors.
func (pkg *Package) baseValue(i int) *LVal { return pkg.baseValues[i] }
func (m sortedmap) entry(k string) *LVal   { return m.m[k] }
