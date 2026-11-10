// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// hostWrappedMap is the stock map behind a type the detacher does not know,
// as an embedder's Map is.  A copy of it takes the general path (sort the
// entries, detach each key), so it is the reference the fast path of
// detachMapData is compared with (luthersystems/elps#944).
type hostWrappedMap struct{ sortedmap }

// detachFastMaps returns the interpreter-owned maps the fast path takes,
// each built by build so that a test can make equal maps twice.
func detachFastMaps() map[string]func() *LVal {
	stock := func(kv ...*LVal) func() *LVal {
		return func() *LVal {
			m := SortedMap()
			for i := 0; i < len(kv); i += 2 {
				if lerr := m.Map().Set(kv[i], kv[i+1]); lerr.IsError() {
					panic(lerr.String())
				}
			}
			return m
		}
	}
	shared := QExpr([]*LVal{Int(1), String("shared")})
	return map[string]func() *LVal{
		"empty":   stock(),
		"strings": stock(String("b"), Int(2), String("a"), Int(1), String("c"), Int(3)),
		"symbols": stock(Symbol("b"), Int(2), Symbol("a"), Int(1)),
		"ints":    stock(Int(10), String("ten"), Int(-3), String("minus three"), Int(2), String("two")),
		"mixed": stock(String("s"), Int(1), Symbol("q"), Int(2), Int(7), Int(3), String("7"), Int(4),
			Symbol("a"), Int(5), Int(-1), Int(6), String(""), Int(7)),
		"nested": stock(String("list"), QExpr([]*LVal{Int(1), QExpr([]*LVal{String("x")})}),
			String("bytes"), Bytes([]byte("abc")),
			String("map"), stock(String("inner"), QExpr([]*LVal{Int(9)}))()),
		"aliased values": stock(String("x"), shared, String("y"), shared, Symbol("z"), shared),
		"json": func() *LVal {
			return SortedMapFromData(NewMapData(jsonMap(map[string]any{
				"b": Int(2), "a": QExpr([]*LVal{Int(1)}), "": String("empty key"), "10": Int(10), "9": Int(9),
			})))
		},
	}
}

// wrapAsHostMap returns m's entries in a hostWrappedMap, so that a copy of
// the result takes the detacher's general path.
func wrapAsHostMap(t *testing.T, m *LVal) *LVal {
	t.Helper()
	entries := sortedMapEntries(m.Map())
	require.False(t, entries.IsError(), "%v", entries)
	host := hostWrappedMap{newmap()}
	for _, e := range entries.Cells {
		require.False(t, host.Set(e.Cells[0], e.Cells[1]).IsError())
	}
	return SortedMapFromData(NewMapData(host))
}

// describeMapCopy renders everything a program can read from a copied map:
// the printed map, and each key with its type, in key order.
func describeMapCopy(t *testing.T, m *LVal) string {
	t.Helper()
	entries := sortedMapEntries(m.Map())
	require.False(t, entries.IsError(), "%v", entries)
	var out strings.Builder
	out.WriteString(m.String())
	for _, e := range entries.Cells {
		fmt.Fprintf(&out, "\n%v %s = %s", e.Cells[0].Type, e.Cells[0], e.Cells[1])
	}
	return out.String()
}

// TestDetachMapFastPathMatchesGeneralPath copies each interpreter-owned map
// with both walkers of the detacher (copy shares opaque leaves, detach
// refuses them) and compares the copy with the copy of the same entries
// behind a host Map type, which takes the general path.
func TestDetachMapFastPathMatchesGeneralPath(t *testing.T) {
	env := NewEnv(nil)
	for name, build := range detachFastMaps() {
		t.Run(name, func(t *testing.T) {
			src := build()
			before := describeMapCopy(t, src)
			for mode, copyOf := range map[string]func(*LVal) *LVal{
				"copy": func(v *LVal) *LVal { return deepCopyValue(env, v) },
				"detach": func(v *LVal) *LVal {
					cp, err := v.detach()
					require.NoError(t, err)
					return cp
				},
			} {
				fast := copyOf(src)
				general := copyOf(wrapAsHostMap(t, build()))
				require.Equal(t, LSortMap, fast.Type, mode)
				assert.Equal(t, describeMapCopy(t, general), describeMapCopy(t, fast), mode)
				assert.Equal(t, before, describeMapCopy(t, fast), mode)
				// The copy is the stock map whatever the source was.
				_, stock := fast.Map().mapBacking.(sortedmap)
				assert.True(t, stock, mode)
				assert.NotSame(t, src.Map(), fast.Map(), mode)
				// A write to the copy does not reach the source.
				require.False(t, fast.Map().Set(String("added"), Int(0)).IsError())
				assert.Equal(t, before, describeMapCopy(t, src), mode)
			}
		})
	}
}

// TestDetachMapFastPathKeepsValueAliasing checks that one value held under
// several keys is still one value in the copy, and not the source's.
func TestDetachMapFastPathKeepsValueAliasing(t *testing.T) {
	src := detachFastMaps()["aliased values"]()
	cp, err := src.detach()
	require.NoError(t, err)
	x, _ := cp.Map().Get(String("x"))
	y, _ := cp.Map().Get(String("y"))
	z, _ := cp.Map().Get(Symbol("z"))
	orig, _ := src.Map().Get(String("x"))
	assert.Same(t, x, y)
	assert.Same(t, x, z)
	assert.NotSame(t, orig, x)
}

// TestDetachMapFastPathErrors checks that a value the walk refuses fails
// with the same text on the fast path as on the general path: the path to
// the value names the key, and the depth limit reports itself.
func TestDetachMapFastPathErrors(t *testing.T) {
	build := func() *LVal {
		m := SortedMap()
		m.Map().Set(String("a"), Int(1))
		m.Map().Set(Symbol("fn"), QExpr([]*LVal{Fun("f", Formals(), func(*LEnv, *LVal) *LVal { return Nil() })}))
		m.Map().Set(String("z"), Int(2))
		return m
	}
	_, fastErr := build().detach()
	_, generalErr := wrapAsHostMap(t, build()).detach()
	require.Error(t, fastErr)
	require.Error(t, generalErr)
	assert.Equal(t, generalErr.Error(), fastErr.Error())
	assert.Contains(t, fastErr.Error(), "Map[")

	// The depth limit: maps nested deeper than the limit allows.
	env := NewEnv(nil)
	require.False(t, WithMaxValueDepth(1024)(env).IsError())
	nest := func(wrap bool) *LVal {
		v := Int(1)
		for range 1030 {
			m := SortedMap()
			m.Map().Set(String("k"), v)
			v = m
			if wrap {
				v = wrapAsHostMap(t, m)
			}
		}
		return v
	}
	fast, general := deepCopyValue(env, nest(false)), deepCopyValue(env, nest(true))
	require.True(t, fast.IsError(), "%v", fast)
	assert.Equal(t, general.String(), fast.String())
}

// TestDetachMapEntriesArriveSorted pins what lets detachMapData skip the
// sort for the interpreter's own maps: their Entries yields the pairs in
// the order sortMapEntriesByKey would put them in.
func TestDetachMapEntriesArriveSorted(t *testing.T) {
	maps := detachFastMaps()
	maps["many"] = func() *LVal {
		m := SortedMap()
		for i := range 300 {
			m.Map().Set(String(fmt.Sprintf("k%d", (i*7919)%300)), Int(i))
			m.Map().Set(Int((i*31)%300-150), Int(i))
			m.Map().Set(Symbol(fmt.Sprintf("s%d", (i*13)%300)), Int(i))
		}
		return m
	}
	for name, build := range maps {
		entries := sortedMapEntries(build().Map())
		require.False(t, entries.IsError(), "%s: %v", name, entries)
		got := append([]*LVal(nil), entries.Cells...)
		sortMapEntriesByKey(entries.Cells)
		for i := range got {
			if got[i] != entries.Cells[i] {
				t.Errorf("%s: entry %d is %s, the sort puts %s there", name, i, got[i], entries.Cells[i])
				break
			}
		}
	}
}

// TestDetachMapKeysAreFresh pins the other half: Entries of the
// interpreter's own maps builds each key anew, so the copy may keep the key
// it was handed.  A key that the map itself held would be shared storage.
func TestDetachMapKeysAreFresh(t *testing.T) {
	for name, build := range detachFastMaps() {
		m := build()
		first, second := sortedMapEntries(m.Map()), sortedMapEntries(m.Map())
		require.False(t, first.IsError(), "%s: %v", name, first)
		for i := range first.Cells {
			assert.NotSame(t, first.Cells[i].Cells[0], second.Cells[i].Cells[0], "%s: key %d", name, i)
		}
	}
}

// TestDetachMapFastPathUnderTemplate copies a map that a template VM holds.
// A lazy VM materializes the map's entries on first read, and the copy is
// the first read here, so it must see every entry.
func TestDetachMapFastPathUnderTemplate(t *testing.T) {
	env := initSafetyTestEnv(t)
	src := detachFastMaps()["nested"]()
	require.False(t, env.Put(Symbol("held"), src).IsError())
	want := describeMapCopy(t, src)
	tmpl, err := NewTemplate(env, TemplateWithBuiltinPolicy(func(*LVal) bool { return true }))
	require.NoError(t, err)
	for range 2 {
		vm, err := tmpl.NewVM()
		require.NoError(t, err)
		held := vm.Get(Symbol("held"))
		require.Equal(t, LSortMap, held.Type, "%v", held)
		cp := deepCopyValue(vm, held)
		require.Equal(t, LSortMap, cp.Type, "%v", cp)
		assert.Equal(t, want, describeMapCopy(t, cp))
		// The copy is the VM's own: a write does not reach the template.
		require.False(t, cp.Map().Set(String("added"), Int(0)).IsError())
		assert.Equal(t, want, describeMapCopy(t, held))
	}
}
