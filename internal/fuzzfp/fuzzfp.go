// Copyright © 2026 The ELPS authors

// Package fuzzfp watches the arguments a fuzz target hands to a builtin and
// reports the mutation that no builtin may ever perform: reaching inside an
// LNative to change the Go value a host handed in.
//
// It used to report a second one -- rewriting a value's source location -- and
// that half is gone; see "# Source: removed on this branch" below for why, and
// for what covers it now.
//
// # Why only that one
//
// A blanket "the arguments came back unchanged" assertion is not available
// here, because several callables mutate an argument BY DESIGN -- `append!`
// grows a vector in place, `assoc!` writes a sorted-map, and MacroCall writes
// to the nodes a macro returns. A guard that fired on those would be turned
// off within a week. So this package asserts only properties that hold for
// every callable in the library, and says so in one place rather than leaving
// each target to decide.
//
// # Source: removed on this branch
//
// This package used to watch a second property -- that no builtin rewrites a
// value's source location -- and issue #318's tracker recorded that as its
// headline result. It is gone, and it is not coming back in this form.
//
// The rule read LVal.Source as an exported field, and its whole premise was
// that every value the interpreter builds without a parser behind it points at
// ONE process-wide token.Location (lisp.nativeSource), so an in-place edit
// relocated a large fraction of the process. Both halves of that premise were
// deleted by the sealing work: #362 removed the shared native-source singleton
// and synthesizes locations on demand (9edf260), and LVal.Source became an
// unexported field behind a value-copy accessor (69be094). There is no shared
// Location left to corrupt and no exported pointer left to corrupt it through,
// so the rule cannot be expressed here and would have nothing to catch.
//
// What replaced it is stronger, not weaker. The sealed-AST fingerprint
// (lisp/sealfp.go) hashes source CONTENTS as part of each sealed argument's
// fingerprint, so a location rewrite on any sealed argument is caught by
// assertion 5 in lisp/lisplib/fuzz_test.go -- and caught as a seal violation,
// which names the corruption class directly. The one case the old rule covered
// and the new one does not is a location rewrite on an UNSEALED argument
// (vectors, maps, natives); that is runtime storage the kernel may legitimately
// rework, which is why it is unsealed in the first place.
//
// # LNative
//
// The tracker's stated reason for recording only an LNative's dynamic type was
// that formatting an arbitrary Go value can traverse a randomised map. That is
// true of fmt, and it is a property of fmt rather than of the values: a walk
// that sorts map entries by their own fingerprint is deterministic over
// exactly the same inputs. [fingerprintGo] is that walk. It reads unexported
// fields through reflect without ever calling Value.Interface (which would
// panic on them), so it sees inside time.Time and *regexp.Regexp, and it
// breaks cycles on pointer identity so a self-referential native terminates.
//
// Deterministic here means "twice in one process gives the same answer", which
// is what a before/after comparison needs and is also the strong form: Go
// randomises map iteration per range statement per run, so two ranges in one
// process already disagree. TestFingerprintIsDeterministic exercises that
// directly, and TestGuardIsDeterministicOverGeneratedValues does it over the
// whole generated corpus.
//
// # What is deliberately NOT watched
//
//   - Cells, Str, Int, Float, Quoted, Spliced. Legitimately mutated; see
//     above.
//   - The Native of an LFun (an *LFunData, holding an *LEnv), an LError (a
//     call stack) or an LBytes (a *[]byte). Only nodes whose Type is LNative
//     have their Native fingerprinted, which is what keeps the walk away from
//     an entire environment and from the deliberately-mutable byte slice.
//   - Meta. Only populated in format-preserving mode, which no builtin target
//     runs in.
package fuzzfp

import (
	"fmt"
	"math"
	"reflect"
	"sort"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// maxNodes caps how many LVal nodes one Guard watches. internal/fuzzval's own
// Budget is 512 values, and the only way past that is descending into a
// function value's body, so this is generous. A cap exists at all so that a
// builtin which builds a huge structure in place cannot turn the guard into
// the slowest part of the target.
const maxNodes = 8192

// maxGoNodes caps one native's fingerprint walk. *regexp.Regexp reaches a
// compiled program of a few hundred nodes; a host is free to hand in something
// far larger, and an unbounded walk would be a denial of service on the target
// rather than a check on the builtin.
const maxGoNodes = 4096

// maxGoDepth bounds nesting independently of maxGoNodes: a linked list a
// million long is caught by the node cap, but a deeply nested value would
// still recurse a million frames before reaching it.
const maxGoDepth = 32

// watched is one LVal node and the properties of it that must survive a call.
type watched struct {
	// native is the structural fingerprint of v.Native, recorded only when
	// typ is LNative.
	v      *lisp.LVal
	native string
	path   string
	typ    lisp.LType
}

// Guard is a set of watched nodes. Create one with [Watch] before the call and
// call [Guard.Check] after it.
type Guard struct {
	nodes []watched
}

// Watch records every LVal reachable from v.
//
// Nodes are recorded BY POINTER, not by position, so a builtin that
// legitimately reshapes the tree -- appends a cell, replaces one, sorts in
// place -- does not knock the before and after records out of alignment and
// produce a spurious report. A node that is dropped from the tree entirely is
// still watched: whoever dropped it may still be holding it.
func Watch(v *lisp.LVal) *Guard {
	g := &Guard{}
	seen := make(map[*lisp.LVal]bool)
	g.walk(v, "arg", seen)
	return g
}

func (g *Guard) walk(v *lisp.LVal, path string, seen map[*lisp.LVal]bool) {
	if v == nil || seen[v] || len(g.nodes) >= maxNodes {
		return
	}
	seen[v] = true

	w := watched{v: v, path: path, typ: v.Type}
	if v.Type == lisp.LNative {
		w.native = fingerprintGo(v.Native)
	}
	g.nodes = append(g.nodes, w)

	for i, c := range v.Cells {
		g.walk(c, path+"["+strconv.Itoa(i)+"]", seen)
	}
	if v.Type == lisp.LSortMap {
		m := v.Map()
		if m == nil || m.Map == nil {
			return
		}
		// Keys() is documented sorted, so the traversal order is stable and a
		// report names the same cell on every run.
		keys := m.Keys()
		if keys == nil || keys.Type == lisp.LError {
			return
		}
		for _, k := range keys.Cells {
			g.walk(k, path+".key("+k.Str+")", seen)
			val, ok := m.Get(k)
			if !ok || val == nil {
				continue
			}
			g.walk(val, path+"["+k.Str+"]", seen)
		}
	}
}

// Check re-derives every watched property and reports the first violation, or
// "" when there is none.
//
// One violation rather than all of them: a fuzz failure is read by a human
// looking for the smallest reproducer, and a builtin that relocates one node
// has almost always relocated a thousand.
func (g *Guard) Check() string {
	for i := range g.nodes {
		if msg := g.nodes[i].check(); msg != "" {
			return msg
		}
	}
	return ""
}

func (w *watched) check() string {
	return w.checkNative()
}

func (w *watched) checkNative() string {
	if w.typ != lisp.LNative {
		return ""
	}
	if w.v.Type != lisp.LNative {
		return fmt.Sprintf("%s: an LNative's Type was changed to %s in place", w.path, w.v.Type)
	}
	if now := fingerprintGo(w.v.Native); now != w.native {
		return fmt.Sprintf("%s: the Go value inside an LNative was mutated"+
			"\n  before: %s\n  after:  %s", w.path, w.native, now)
	}
	return ""
}

// Fingerprint returns the structural fingerprint of a Go value. Exported for
// the determinism tests; the guard itself uses it internally.
func Fingerprint(v any) string { return fingerprintGo(v) }

// fingerprintGo renders v deterministically.
//
// Determinism is the whole point, so every source of ordering nondeterminism
// is handled explicitly:
//
//   - map entries are rendered and then SORTED, so Go's per-range randomised
//     iteration order cannot reach the output;
//   - pointer identity is rendered as a small dense index into the set of
//     pointers seen during THIS walk, never as an address, so ASLR and the
//     allocator cannot reach it either;
//   - cycles terminate at the first repeat of a pointer.
//
// Unexported fields are read through kind-specific accessors (Int, String,
// Len, Pointer, MapRange). Value.Interface is never called: it panics on a
// value obtained from an unexported field, and calling it is the usual reason
// a reflective walk cannot see inside a stdlib type.
func fingerprintGo(v any) string {
	var b strings.Builder
	s := &goWalk{seen: map[uintptr]int{}, budget: maxGoNodes}
	s.value(&b, reflect.ValueOf(v), 0)
	return b.String()
}

type goWalk struct {
	// seen maps a pointer to the order in which this walk first met it, so the
	// rendering of shared structure is stable without ever printing an
	// address.
	seen   map[uintptr]int
	budget int
}

func (s *goWalk) value(b *strings.Builder, v reflect.Value, depth int) {
	if s.budget <= 0 {
		b.WriteString("...")
		return
	}
	s.budget--
	if depth > maxGoDepth {
		b.WriteString("<deep>")
		return
	}
	if !v.IsValid() {
		b.WriteString("nil")
		return
	}
	b.WriteString(v.Type().String())
	b.WriteByte('(')
	defer b.WriteByte(')')

	switch v.Kind() {
	case reflect.Bool:
		b.WriteString(strconv.FormatBool(v.Bool()))
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		b.WriteString(strconv.FormatInt(v.Int(), 10))
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		b.WriteString(strconv.FormatUint(v.Uint(), 10))
	case reflect.Float32, reflect.Float64:
		b.WriteString(formatFloat(v.Float()))
	case reflect.Complex64, reflect.Complex128:
		c := v.Complex()
		b.WriteString(formatFloat(real(c)))
		b.WriteByte('+')
		b.WriteString(formatFloat(imag(c)))
	case reflect.String:
		b.WriteString(strconv.Quote(v.String()))
	case reflect.Pointer, reflect.Interface:
		s.indirect(b, v, depth)
	case reflect.Slice:
		if v.IsNil() {
			b.WriteString("nil")
			return
		}
		// A slice header is (ptr, len, cap). Two slices over one array are
		// distinguishable, so the identity of the backing array is part of the
		// fingerprint -- a builtin that reslices in place has changed the
		// value even when the elements read the same.
		id, repeat := s.mark(v.Pointer())
		if repeat {
			fmt.Fprintf(b, "@%d/%d", id, v.Len())
			return
		}
		fmt.Fprintf(b, "@%d len=%d cap=%d", id, v.Len(), v.Cap())
		s.elements(b, v, depth)
	case reflect.Array:
		s.elements(b, v, depth)
	case reflect.Struct:
		for i := range v.NumField() {
			if i > 0 {
				b.WriteByte(' ')
			}
			b.WriteString(v.Type().Field(i).Name)
			b.WriteByte('=')
			s.value(b, v.Field(i), depth+1)
		}
	case reflect.Map:
		s.mapValue(b, v, depth)
	case reflect.Func, reflect.Chan, reflect.UnsafePointer:
		// Not walkable and not comparable by value. The pointer is stable for
		// the lifetime of the process, which is all a before/after comparison
		// needs, and it is rendered through mark() so no address is printed.
		if v.IsNil() {
			b.WriteString("nil")
			return
		}
		id, _ := s.mark(v.Pointer())
		fmt.Fprintf(b, "@%d", id)
	default:
		b.WriteString("<unsupported>")
	}
}

func (s *goWalk) indirect(b *strings.Builder, v reflect.Value, depth int) {
	if v.IsNil() {
		b.WriteString("nil")
		return
	}
	if v.Kind() == reflect.Pointer {
		id, repeat := s.mark(v.Pointer())
		if repeat {
			// A cycle, or ordinary sharing. Either way the target has already
			// been rendered once; rendering it again would not terminate.
			fmt.Fprintf(b, "@%d", id)
			return
		}
		fmt.Fprintf(b, "@%d ", id)
	}
	s.value(b, v.Elem(), depth+1)
}

func (s *goWalk) elements(b *strings.Builder, v reflect.Value, depth int) {
	n := v.Len()
	for i := range n {
		if s.budget <= 0 {
			b.WriteString(" ...")
			return
		}
		b.WriteByte(' ')
		s.value(b, v.Index(i), depth+1)
	}
}

// mapValue renders a map with its entries sorted by their own rendering.
//
// This is the case the tracker recorded as unfixable. It is fixable: Go
// randomises the ORDER a range yields entries, not the entries themselves, so
// rendering each entry independently and sorting the renderings removes the
// randomness completely. The sort key is the whole "k=v" string rather than
// just the key, so two entries whose keys render identically (possible for
// NaN keys, and for keys past the depth cap) still order deterministically.
func (s *goWalk) mapValue(b *strings.Builder, v reflect.Value, depth int) {
	if v.IsNil() {
		b.WriteString("nil")
		return
	}
	fmt.Fprintf(b, "len=%d", v.Len())
	entries := make([]string, 0, v.Len())
	it := v.MapRange()
	for it.Next() {
		if s.budget <= 0 {
			entries = append(entries, "...")
			break
		}
		// Each entry gets a FRESH pointer-identity table. Sharing one across
		// entries would make an entry's rendering depend on which entries were
		// rendered before it -- i.e. on the very iteration order being sorted
		// away.
		sub := &goWalk{seen: map[uintptr]int{}, budget: min(s.budget, maxGoNodes/4)}
		var eb strings.Builder
		sub.value(&eb, it.Key(), depth+1)
		eb.WriteByte('=')
		sub.value(&eb, it.Value(), depth+1)
		s.budget -= maxGoNodes/4 - sub.budget
		entries = append(entries, eb.String())
	}
	sort.Strings(entries)
	for _, e := range entries {
		b.WriteByte(' ')
		b.WriteString(e)
	}
}

// mark returns a stable small index for p and whether it had been seen before.
func (s *goWalk) mark(p uintptr) (int, bool) {
	if id, ok := s.seen[p]; ok {
		return id, true
	}
	id := len(s.seen)
	s.seen[p] = id
	return id, false
}

// formatFloat renders a float so that NaN compares equal to NaN and the two
// zeros compare unequal.
//
// Both directions matter. A fingerprint in which NaN != NaN would report every
// native holding a NaN as mutated on every call, and one in which -0 == +0
// would miss a sign flip -- which is a real mutation, observable from lisp
// through (/ 1 x).
func formatFloat(f float64) string {
	switch {
	case math.IsNaN(f):
		return "NaN"
	case math.Signbit(f) && f == 0:
		return "-0"
	}
	return strconv.FormatFloat(f, 'x', -1, 64)
}
