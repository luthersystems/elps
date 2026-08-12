// Copyright © 2026 The ELPS authors

// Package synthphylum generates a synthetic, production-scale ELPS corpus
// for benchmarking the evaluator — in particular the per-callsite
// macro-expansion cache POC (issue #381).
//
// The generator's shape parameters were measured from a real
// production-scale phylum (an embedder application corpus); only the
// STATISTICS were taken, no code or identifiers.  The reference corpus
// measures:
//
//	files                 154 (this generator packs the same volume into ~50)
//	total lines           ~71,600
//	(defun ...) forms     ~2,500
//	(defmacro ...) forms  8 (route/definition wrapper macros)
//	wrapper callsites     606, distributed 313/89/79/52/40/19/14 over the
//	                      seven most-used wrappers (def-route-style forms
//	                      that expand to defun + handler-bind layering)
//	handler-bind sites    ~10 (inside the wrapper templates)
//	utility-macro sites   when ~1,556 · default ~930 · unless ~443 ·
//	                      implies ~179 · get-default ~62
//	(if ...) sites        ~719 (for density comparison)
//
// The generated corpus mirrors those densities: a library file defines the
// same two macro families seen in production embedders (pure template
// utilities — when/unless/default/implies equivalents — plus seven
// definition wrappers that expand to defun with handler-bind layering), and
// the remaining files hold helper functions and wrapper-defined routes
// whose bodies consume the utility-macro budgets.  Helpers form a DAG
// (each caller only invokes lower-numbered helpers) so evaluation depth
// stays bounded, and every route returns a sorted-map so a dispatch
// harness can assert results.
//
// Everything is deterministic: the same Params always generate byte-equal
// output.
package synthphylum

import (
	"fmt"
	"strings"
)

// File is one generated source file.  Files must be loaded in order.
type File struct {
	Name    string
	Content string
}

// Params control corpus shape.  DefaultParams reproduces the
// production-scale reference statistics above.
type Params struct {
	Files       int    // total files (first is the macro library)
	Helpers     int    // plain defun helpers
	WrapperUses [7]int // callsites per wrapper macro, most-used first

	// Utility-macro budgets, spread over helper/route bodies.
	WhenSites       int
	UnlessSites     int
	DefaultSites    int
	ImpliesSites    int
	GetDefaultSites int
}

// DefaultParams returns the production-scale reference shape (~70k lines).
func DefaultParams() Params {
	return Params{
		Files:           50,
		Helpers:         1900,
		WrapperUses:     [7]int{313, 89, 79, 52, 40, 19, 14},
		WhenSites:       1556,
		UnlessSites:     443,
		DefaultSites:    930,
		ImpliesSites:    179,
		GetDefaultSites: 62,
	}
}

// SmallParams returns a scaled-down corpus (~1/20th) for quick tests.
func SmallParams() Params {
	return Params{
		Files:           5,
		Helpers:         95,
		WrapperUses:     [7]int{16, 5, 4, 3, 2, 1, 1},
		WhenSites:       78,
		UnlessSites:     22,
		DefaultSites:    47,
		ImpliesSites:    9,
		GetDefaultSites: 4,
	}
}

// lcg is a deterministic pseudo-random source (no seeding from the clock,
// no dependence on math/rand stream compatibility).
type lcg uint64

func (r *lcg) next() uint64 {
	*r = *r*6364136223846793005 + 1442695040888963407
	return uint64(*r >> 17)
}

func (r *lcg) intn(n int) int {
	if n <= 0 {
		return 0
	}
	return int(r.next() % uint64(n)) // #nosec G115 -- result < n, an int
}

// budget hands out remaining macro-site allowances.
type budget struct {
	when, unless, def, implies, getdef int
}

// Generate produces the corpus.
func Generate(p Params) []File {
	rng := lcg(20260812)
	b := &budget{p.WhenSites, p.UnlessSites, p.DefaultSites, p.ImpliesSites, p.GetDefaultSites}

	// Reserve enough per-body macro emissions: helpers plus routes share
	// the budgets; leftovers degrade to plain (if ...) forms, mirroring
	// the reference corpus's if density.
	totalRoutes := 0
	for _, n := range p.WrapperUses {
		totalRoutes += n
	}
	bodies := p.Helpers + totalRoutes

	files := make([]File, 0, p.Files)
	files = append(files, File{Name: "lib.lisp", Content: libFile()})

	contentFiles := p.Files - 1
	if contentFiles < 1 {
		contentFiles = 1
	}
	helperID := 0
	routeID := 0
	// Wrapper callsites round-robin across their per-wrapper budgets.
	wrapperLeft := p.WrapperUses

	for f := range contentFiles {
		var sb strings.Builder
		fmt.Fprintf(&sb, "; synthetic corpus file %d/%d (generated; see internal/synthphylum)\n", f+1, contentFiles)
		nh := p.Helpers / contentFiles
		if f == contentFiles-1 {
			nh = p.Helpers - helperID
		}
		for range nh {
			writeHelper(&sb, helperID, &rng, b, bodies)
			helperID++
		}
		nr := totalRoutes / contentFiles
		if f == contentFiles-1 {
			nr = totalRoutes - routeID
		}
		for range nr {
			w := pickWrapper(&wrapperLeft, &rng)
			writeRoute(&sb, routeID, w, helperID, &rng, b, bodies)
			routeID++
		}
		files = append(files, File{
			Name:    fmt.Sprintf("mod%03d.lisp", f+1),
			Content: sb.String(),
		})
	}
	return files
}

// libFile emits the macro library: four pure template utilities (the
// production embedder's when/unless/default/implies analogues) and seven
// definition wrappers expanding to defun with handler-bind layering, plus
// the state globals the routes read.
func libFile() string {
	var sb strings.Builder
	sb.WriteString(`; synthetic corpus library (generated; see internal/synthphylum)
(defmacro s-when (p &rest body)
  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
(defmacro s-unless (p &rest body)
  (quasiquote (if (not (unquote p)) (progn (unquote-splicing body)) ())))
(defmacro s-default (x d)
  (let* ([g (gensym)])
    (quasiquote
      (let* ([(unquote g) (unquote x)])
        (if (nil? (unquote g)) (unquote d) (unquote g))))))
(defmacro s-implies (a b)
  (quasiquote (if (unquote a) (not (not (unquote b))) true)))
(set 'route-table (sorted-map))
(defun register-route (name fn)
  (set 'route-table (assoc route-table name fn)))
(defun dispatch (name req)
  (handler-bind ((condition (lambda (&rest c) (sorted-map "error" "unhandled"))))
    (funcall (get route-table name) req)))
(set 'db (sorted-map
  "k0" 10 "k1" 11 "k2" 12 "k3" 13
  "k4" 14 "k5" 15 "k6" 16))
`)
	// Seven wrappers; each layers handler-bind in its template (the
	// reference corpus keeps ~10 handler-bind sites inside 7-8 wrapper
	// macros).  Wrappers 0-2 add a second inner handler-bind layer.
	for w := range 7 {
		inner := "(progn (unquote-splicing body))"
		if w < 3 {
			inner = `(handler-bind ((error (lambda (&rest c) (sorted-map "error" "inner"))))
           (progn (unquote-splicing body)))`
		}
		fmt.Fprintf(&sb, `(defmacro def-w%d (name &rest body)
  (quasiquote
    (progn
      (defun (unquote name) (req)
        (handler-bind ((condition (lambda (&rest c) (sorted-map "error" "outer"))))
          %s))
      (register-route (to-string (quote (unquote name))) (unquote name)))))
`, w, inner)
	}
	return sb.String()
}

func pickWrapper(left *[7]int, rng *lcg) int {
	total := 0
	for _, n := range left {
		total += n
	}
	if total == 0 {
		return 6
	}
	k := rng.intn(total)
	for i, n := range left {
		if k < n {
			left[i]--
			return i
		}
		k -= n
	}
	return 6
}

// takeSite consumes one unit from the largest applicable remaining budget,
// weighted by remaining counts, or reports false when all are exhausted.
func (b *budget) takeSite(rng *lcg) (kind string, ok bool) {
	total := b.when + b.unless + b.def + b.implies + b.getdef
	if total == 0 {
		return "", false
	}
	k := rng.intn(total)
	switch {
	case k < b.when:
		b.when--
		return "when", true
	case k < b.when+b.unless:
		b.unless--
		return "unless", true
	case k < b.when+b.unless+b.def:
		b.def--
		return "default", true
	case k < b.when+b.unless+b.def+b.implies:
		b.implies--
		return "implies", true
	default:
		b.getdef--
		return "getdef", true
	}
}

// perBodySites decides how many macro sites this body receives so budgets
// spread across all bodies (~1.3 sites/body for the default shape).
func perBodySites(b *budget, bodies int, rng *lcg) int {
	remaining := b.when + b.unless + b.def + b.implies + b.getdef
	if remaining == 0 {
		return 0
	}
	avg := remaining/bodies + 1
	if avg > 4 {
		avg = 4
	}
	return 1 + rng.intn(avg+1)
}

// writeHelper emits one helper defun.  Helpers only call lower-numbered
// helpers (a DAG) and only every 8th helper calls another at all, so a
// dispatch touches a bounded set.
func writeHelper(sb *strings.Builder, id int, rng *lcg, b *budget, bodies int) {
	fmt.Fprintf(sb, "(defun helper-%d (req)\n", id)
	fmt.Fprintf(sb, "  (let* ([a (get req \"f%d\")]\n", rng.intn(6))
	fmt.Fprintf(sb, "         [acc %d])\n", rng.intn(100))
	nsites := perBodySites(b, bodies, rng)
	for range nsites {
		kind, ok := b.takeSite(rng)
		if !ok {
			break
		}
		writeSite(sb, kind, rng, "    ")
	}
	// Filler forms mirror the reference corpus's plain-if density and
	// bring per-function line volume to the measured average (~28 lines
	// per defun).
	nfill := 7 + rng.intn(3)
	for j := range nfill {
		switch rng.intn(3) {
		case 0:
			fmt.Fprintf(sb, "    (if (nil? a)\n        (set! acc (+ acc %d))\n        (set! acc (+ acc 1)))\n", rng.intn(9)+1)
		case 1:
			fmt.Fprintf(sb, "    (let* ([t%d (get db \"k%d\")])\n      (if (nil? t%d)\n          (set! acc (- acc %d))\n          (set! acc (+ acc t%d))))\n",
				j, rng.intn(10), j, rng.intn(5)+1, j)
		default:
			fmt.Fprintf(sb, "    (if (> acc %d)\n        (set! acc (- acc %d))\n        (set! acc (+ acc %d)))\n",
				rng.intn(200), rng.intn(7)+1, rng.intn(7)+1)
		}
	}
	if id > 0 && id%8 == 0 {
		callee := id - 1 - rng.intn(minInt(id, 40))
		fmt.Fprintf(sb, "    (set! acc (+ acc (helper-%d req)))\n", callee)
	}
	sb.WriteString("    acc))\n")
}

// writeRoute emits one wrapper-defined route that fans out to helpers.
func writeRoute(sb *strings.Builder, id, wrapper, helperCount int, rng *lcg, b *budget, bodies int) {
	fmt.Fprintf(sb, "(def-w%d route-%d\n", wrapper, id)
	fmt.Fprintf(sb, "  (let* ([acc 0])\n")
	nsites := perBodySites(b, bodies, rng)
	for range nsites {
		kind, ok := b.takeSite(rng)
		if !ok {
			break
		}
		writeSite(sb, kind, rng, "    ")
	}
	fanout := 2 + rng.intn(4)
	for i := 0; i < fanout && helperCount > 0; i++ {
		fmt.Fprintf(sb, "    (set! acc (+ acc (helper-%d req)))\n", rng.intn(helperCount))
	}
	nfill := 2 + rng.intn(3)
	for range nfill {
		fmt.Fprintf(sb, "    (if (> acc %d)\n        (set! acc (- acc %d))\n        (set! acc (+ acc %d)))\n",
			rng.intn(500), rng.intn(9)+1, rng.intn(9)+1)
	}
	fmt.Fprintf(sb, "    (sorted-map \"route\" %d \"acc\" acc)))\n", id)
}

// writeSite emits one utility-macro callsite of the given kind.
func writeSite(sb *strings.Builder, kind string, rng *lcg, indent string) {
	k := rng.intn(10)
	switch kind {
	case "when":
		fmt.Fprintf(sb, "%s(s-when (key? req \"f%d\")\n%s  (set! acc (+ acc %d)))\n", indent, k%6, indent, k+1)
	case "unless":
		fmt.Fprintf(sb, "%s(s-unless (key? req \"f%d\")\n%s  (set! acc (- acc %d)))\n", indent, k%6, indent, k+1)
	case "default":
		fmt.Fprintf(sb, "%s(set! acc (+ acc (s-default (get db \"k%d\") %d)))\n", indent, k, k+2)
	case "implies":
		fmt.Fprintf(sb, "%s(s-implies (> acc %d) (key? req \"f%d\"))\n", indent, k, k%6)
	case "getdef":
		fmt.Fprintf(sb, "%s(set! acc (+ acc (get-default db \"k%d\" %d)))\n", indent, k, k+3)
	}
}

func minInt(a, b int) int {
	if a < b {
		return a
	}
	return b
}
