package lisp

import (
	"slices"
	"sort"
)

type Package struct {
	symbols              map[string]int
	funNames, symbolDocs map[string]string
	externals            []string
	base                 *packageBase
	Name                 string
	baseValues           []int
	slotFunNames         map[string]string
}

type packageBase struct {
	index                map[string]int
	funNames, symbolDocs map[string]string
	externals            []string
	fingerprint          uint64
}

type embedded struct{ *Package }

type unrelated struct{ externals []string }

func bad(p *Package, b *packageBase, e embedded) {
	p.symbols = nil                                               // want "package table write outside the write gate"
	p.symbols["x"] = 1                                            // want "package table write outside the write gate"
	p.funNames["f"] = "x"                                         // want "package table write outside the write gate"
	p.symbolDocs["x"] = "doc"                                     // want "package table write outside the write gate"
	p.externals[0] = "x"                                          // want "package table write outside the write gate"
	p.externals = append(p.externals, "x")                        // want "package table write outside the write gate" "package table write outside the write gate"
	delete(p.symbols, "x")                                        // want "package table write outside the write gate"
	clear(p.funNames)                                             // want "package table write outside the write gate"
	copy(p.externals, []string{"x"})                              // want "package table write outside the write gate"
	sort.Strings(p.externals)                                     // want "package table write outside the write gate"
	sort.StringSlice(p.externals).Sort()                          // want "package table write outside the write gate"
	sort.StringSlice(p.externals).Swap(0, 1)                      // want "package table write outside the write gate"
	sort.Slice(p.externals, func(i, j int) bool { return i < j }) // want "package table write outside the write gate"
	slices.Sort(p.externals)                                      // want "package table write outside the write gate"
	slices.Reverse(p.externals)                                   // want "package table write outside the write gate"
	_ = slices.Delete(p.externals, 0, 1)                          // want "package table write outside the write gate"
	_ = &p.symbols                                                // want "package table write outside the write gate"
	_ = &p.externals[0]                                           // want "package table write outside the write gate"
	b.index["x"]++                                                // want "package table write outside the write gate"
	b.externals = nil                                             // want "package table write outside the write gate"
	b.funNames = nil                                              // want "package table write outside the write gate"
	b.symbolDocs = nil                                            // want "package table write outside the write gate"
	b.fingerprint = 0                                             // want "package table write outside the write gate"
	_ = &b.index                                                  // want "package table write outside the write gate"
	p.base = nil                                                  // want "package table write outside the write gate"
	p.baseValues[0] = 1                                           // want "package table write outside the write gate"
	p.slotFunNames["f"] = "x"                                     // want "package table write outside the write gate"
	*p = Package{}                                                // want "package table write outside the write gate"
	*b = packageBase{}                                            // want "package table write outside the write gate"
	e.externals = nil                                             // want "package table write outside the write gate"
	m := p.symbols
	m["x"] = 3     // want "package table write outside the write gate"
	delete(m, "x") // want "package table write outside the write gate"
	var xs = p.externals
	ys := xs[:]
	ys[0] = "x"                     // want "package table write outside the write gate"
	slices.Sort(ys)                 // want "package table write outside the write gate"
	_ = append(ys, "x")             // want "package table write outside the write gate"
	_ = Package{symbols: nil}       // want "package table write outside the write gate"
	_ = packageBase{fingerprint: 0} // want "package table write outside the write gate"
}

func reads(p *Package, b *packageBase) {
	_ = len(p.externals)
	_ = p.symbols["x"]
	_ = b.index["x"]
	x := p.externals[0]
	x = "local"
	_ = x
	out := slices.Clone(p.externals)
	sort.Strings(out)
	out[0] = "private"
	u := unrelated{}
	u.externals = append(u.externals, "x")
	p.Name = "name"
	_ = Package{Name: "name"}
}

func NewPackage() *Package       { return &Package{symbols: make(map[string]int)} }
func (p *Package) putName()      { p.symbols["x"] = 1 }
func (p *Package) putSlot()      { p.baseValues[0] = 1; p.slotFunNames["f"] = "x" }
func (p *Package) setSymbolDoc() { p.symbolDocs["x"] = "doc" }
func (p *Package) Export()       { p.externals = append(p.externals, "x") }
func (p *Package) Exports()      { sort.Strings(p.externals) }
func (p *Package) exportSorted() { slices.Sort(p.externals) }
func admitPackage() *Package     { return &Package{externals: []string{"x"}} }

type templatePlan struct{}

func (templatePlan) instantiate(p *Package) { p.symbols = nil }

type templateCompiler struct{}

func (templateCompiler) packageDescriptor() *packageBase {
	return &packageBase{index: map[string]int{}}
}
func (b *packageBase) publish() { b.fingerprint = 1 }

// A same-named method on another receiver cannot inherit a gate exemption.
func (u unrelated) Export(p *Package) {
	p.externals = nil // want "package table write outside the write gate"
}

// Defined types retain Package's field identities, just as embedded fields do.
type mirror Package

func moreWrites(p *Package, m *mirror) {
	m.symbols["x"] = 1                                     // want "package table write outside the write gate"
	sort.Sort(sort.Reverse(sort.StringSlice(p.externals))) // want "package table write outside the write gate"
	for _, p.externals[0] = range []string{"x"} {          // want "package table write outside the write gate"
	}
}
