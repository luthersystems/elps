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

type rawMap struct {
	m  map[string]*LVal
	tm map[string]int
}

type rawPackage struct {
	symbols    map[string]*LVal
	baseValues []*LVal
	Name       string
}

func converted(s sortedmap, p *Package, pv Package) {
	_ = rawMap(s).m["k"]          // want "conversion of lazily materialized type sortedmap"
	_ = (*rawPackage)(p).symbols  // want "conversion of lazily materialized type Package"
	_ = rawPackage(pv).baseValues // want "conversion of lazily materialized type Package"
	_ = sortedmap(rawMap{})       // converting TO the type reads nothing
	_ = (*mirror)(p)              // want "conversion of lazily materialized type Package"
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
