// Copyright © 2026 The ELPS authors

package lisp

import (
	"io"
	"reflect"
	"strings"
	"testing"
)

// In-package tests for the load cache.  Two things are only checkable from
// inside package lisp: what (*LEnv).readCached actually hands the evaluator
// (the alias, at the funnel rather than at the cache), and the opacity of
// CachedSource as a reflected type.

type mapLoadCache struct {
	entries map[string]*CachedSource
}

func (c *mapLoadCache) Load(key string) (*CachedSource, bool) {
	src, ok := c.entries[key]
	return src, ok
}

func (c *mapLoadCache) Store(key string, src *CachedSource) {
	if c.entries == nil {
		c.entries = make(map[string]*CachedSource)
	}
	c.entries[key] = src
}

// TestReadCachedHandsOutTheCachedNodes is the alias proof at the funnel.
// TestLoadCacheServesTheSameNodes (the external battery) shows the cache
// keeps one entry; this shows the EVALUATOR is given that entry's own nodes
// rather than a copy of them, which is the difference between this hook and
// the embedder-side cache it replaces.
func TestReadCachedHandsOutTheCachedNodes(t *testing.T) {
	env := NewEnv(nil)
	cache := &mapLoadCache{}
	env.Runtime.LoadCache = cache

	const src = `(defun f () '(1 2 3)) (f)`
	// A stand-in for the parser: package lisp cannot import the parser (the
	// parser imports lisp), so the test builds a sealed tree the way the
	// rdparser does — construct, then SealAST each completed top-level
	// expression.  What is under test is the funnel's custody decision, not
	// the parse.
	parses := 0
	parse := func(r io.Reader) ([]*LVal, error) {
		if _, err := io.ReadAll(r); err != nil {
			return nil, err
		}
		parses++
		exprs := []*LVal{
			SExpr([]*LVal{Symbol("quote"), SExpr([]*LVal{Int(1), Int(2), Int(3)})}),
			SExpr([]*LVal{Symbol("quote"), Symbol("done")}),
		}
		for _, expr := range exprs {
			expr.SealAST()
		}
		return exprs, nil
	}

	first, err := env.readCached("f.lisp", "f.lisp", strings.NewReader(src), parse)
	if err != nil {
		t.Fatalf("first read failed: %v", err)
	}
	second, err := env.readCached("f.lisp", "f.lisp", strings.NewReader(src), parse)
	if err != nil {
		t.Fatalf("second read failed: %v", err)
	}
	if parses != 1 {
		t.Fatalf("expected exactly one parse, got %d", parses)
	}
	if len(first) == 0 || len(first) != len(second) {
		t.Fatalf("expression counts differ: %d vs %d", len(first), len(second))
	}
	for i := range first {
		if first[i] != second[i] {
			t.Errorf("expression %d was copied on the cache hit; the funnel must alias", i)
		}
		if !first[i].sealed {
			t.Errorf("expression %d is unsealed; aliasing it across environments would be illegal", i)
		}
	}

	// And the entry the cache holds is that same storage again — the miss
	// path must not keep a pristine copy back, or the cache never serves
	// what it stored (that copy is exactly the cost this hook removes).
	entry, ok := cache.Load(loadCacheKey("f.lisp", "f.lisp", []byte(src)))
	if !ok {
		t.Fatal("the miss path did not store an entry under the derived key")
	}
	if len(entry.prog.exprs) != len(first) {
		t.Fatalf("stored %d expressions, handed out %d", len(entry.prog.exprs), len(first))
	}
	for i := range entry.prog.exprs {
		if entry.prog.exprs[i] != first[i] {
			t.Errorf("expression %d handed to the FIRST load is not the stored node", i)
		}
	}
}

// TestReadCachedNilCacheDoesNotInterpose pins the compatibility promise at
// the funnel: with no cache installed, readCached must pass the caller's own
// io.Reader through untouched.
func TestReadCachedNilCacheDoesNotInterpose(t *testing.T) {
	env := NewEnv(nil)
	stream := strings.NewReader("()")
	var got io.Reader
	_, err := env.readCached("n.lisp", "n.lisp", stream, func(r io.Reader) ([]*LVal, error) {
		got = r
		return nil, nil
	})
	if err != nil {
		t.Fatalf("read failed: %v", err)
	}
	if got != io.Reader(stream) {
		t.Errorf("the nil-cache path wrapped the caller's stream (%T); it must pass it through", got)
	}
}

// TestReadCachedNilCacheAllocatesNothing makes the compatibility promise
// mechanical rather than rhetorical.  "byte-identical to today" is easy to
// break by accident — the funnel takes a closure per call, and a closure
// that escaped would put a heap allocation on every load in every embedder
// that installs no cache.  Escape analysis currently keeps all four of them
// on the stack ("func literal does not escape"); this is the test that
// notices if that stops being true.
func TestReadCachedNilCacheAllocatesNothing(t *testing.T) {
	env := NewEnv(nil)
	parse := func(io.Reader) ([]*LVal, error) { return nil, nil }
	stream := strings.NewReader("")
	got := testing.AllocsPerRun(200, func() {
		_, _ = env.readCached("n.lisp", "n.lisp", stream, parse)
	})
	if got != 0 {
		t.Errorf("the nil-cache path allocated %v times per load; it must allocate nothing", got)
	}
}

// TestCachedSourceIsOpaque is CachedSource's compile-time seal, the same
// guard TestProgramSeal applies to Program: the type's entire value is that
// no exported member yields an AST node, so a `func (s *CachedSource)
// Exprs() []*LVal` added in good faith would silently give every embedder
// back the custody this hook exists to take away.
//
// Anti-vacuity: the accessors that are supposed to exist are asserted too,
// so a mass deletion cannot pass as "no leaks found".
func TestCachedSourceIsOpaque(t *testing.T) {
	lvalPtr := reflect.TypeOf((*LVal)(nil))

	seen := map[reflect.Type]bool{}
	var exposes func(typ reflect.Type) bool
	exposes = func(typ reflect.Type) bool {
		if typ == lvalPtr {
			return true
		}
		if seen[typ] {
			return false
		}
		seen[typ] = true
		switch typ.Kind() {
		case reflect.Pointer, reflect.Slice, reflect.Array, reflect.Chan:
			return exposes(typ.Elem())
		case reflect.Map:
			return exposes(typ.Key()) || exposes(typ.Elem())
		case reflect.Struct:
			// EXPORTED fields only, which is where this differs from
			// TestProgramSeal's identical-looking walk.  Program is never
			// itself a Program method's result, so that walk can afford to
			// descend into everything; *CachedSource IS LoadCache.Load's
			// result, and descending into its unexported prog field would
			// report the type as leaking through itself.  An unexported
			// field is unreachable from outside package lisp, which is the
			// boundary under test — the separate check below asserts that
			// CachedSource has no exported fields at all.
			for i := range typ.NumField() {
				if f := typ.Field(i); f.IsExported() && exposes(f.Type) {
					return true
				}
			}
		case reflect.Func:
			for i := range typ.NumOut() {
				if exposes(typ.Out(i)) {
					return true
				}
			}
		case reflect.Interface:
			// Any non-empty interface could dynamically hold a *LVal.
			// CachedSource has no interface-returning method today; adding
			// one needs a conscious exemption here.
			return typ.NumMethod() > 0 || typ == reflect.TypeOf((*any)(nil)).Elem()
		default:
		}
		return false
	}

	typ := reflect.TypeOf((*CachedSource)(nil))
	found := map[string]bool{}
	for i := range typ.NumMethod() {
		m := typ.Method(i)
		found[m.Name] = true
		for j := range m.Type.NumOut() {
			if exposes(m.Type.Out(j)) {
				t.Errorf("CachedSource.%s result %d (%s) can expose *LVal — the cache boundary is broken",
					m.Name, j, m.Type.Out(j))
			}
		}
	}
	for _, name := range []string{"Key", "Name", "Location", "Len", "Fingerprint", "String"} {
		if !found[name] {
			t.Errorf("CachedSource.%s is missing", name)
		}
	}

	st := reflect.TypeOf(CachedSource{})
	if st.NumField() == 0 {
		t.Error("CachedSource has no fields; expected the unexported parse it wraps")
	}
	for i := range st.NumField() {
		if f := st.Field(i); f.IsExported() {
			t.Errorf("CachedSource has exported field %s %s — the cache boundary is broken", f.Name, f.Type)
		}
	}

	// The LoadCache interface must not traffic in *LVal either: an embedder
	// implements it, so every parameter and result it names is a surface
	// the embedder holds.
	it := reflect.TypeOf((*LoadCache)(nil)).Elem()
	for i := range it.NumMethod() {
		m := it.Method(i)
		for j := range m.Type.NumIn() {
			if exposes(m.Type.In(j)) {
				t.Errorf("LoadCache.%s parameter %d (%s) can expose *LVal", m.Name, j, m.Type.In(j))
			}
		}
		for j := range m.Type.NumOut() {
			if exposes(m.Type.Out(j)) {
				t.Errorf("LoadCache.%s result %d (%s) can expose *LVal", m.Name, j, m.Type.Out(j))
			}
		}
	}
}
