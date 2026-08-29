// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// copyNestDepth is how deep the values below nest. It is well past anything
// the package sees in practice, and past the cycle guard's own depth, so the
// agreement asserted here holds on both sides of the guard's escalation.
const copyNestDepth = 80

// containerKinds are the three container shapes the copy helpers handle, in
// the rotation nestedValue uses so that every helper is exercised inside
// every other one.
var containerKinds = []lisp.LType{lisp.LSortMap, lisp.LArray, lisp.LSExpr}

// TestCopyHelpersAgreeOnNestingDepth is the drift test issue #395 asks for.
//
// copyList and copyVector always recursed, carrying the comment "lists may
// contain containers, in which case we need to copy those containers".
// copyMap did not: it set the source's own values into the new map, so the
// "copy" shared every nested container with the value it was copied from and
// a write through it reached the original — the package's own redaction
// example leaked the record it was redacting.
//
// Three helpers with one contract between them is a contract that can drift
// again, and the drift is silent: a shallow copy is indistinguishable from a
// deep one until somebody writes through it. So rather than pinning copyMap
// alone, this asserts that all three rebuild the same number of levels, and
// that the number is the whole value.
func TestCopyHelpersAgreeOnNestingDepth(t *testing.T) {
	t.Parallel()

	helpers := map[string]struct {
		root lisp.LType
		copy func(*lisp.LVal) (*lisp.LVal, error)
	}{
		"copyMap":    {lisp.LSortMap, copyMap},
		"copyVector": {lisp.LArray, copyVector},
		"copyList":   {lisp.LSExpr, copyList},
	}

	depths := make(map[string]int, len(helpers))
	for name, h := range helpers {
		src := nestedValue(h.root, copyNestDepth)
		require.Equal(t, copyNestDepth, containerDepth(src),
			"%s: the test value must actually nest %d deep", name, copyNestDepth)

		cp, err := h.copy(src)
		require.NoError(t, err)
		require.Equal(t, src.String(), cp.String(), "%s must preserve the value", name)

		depths[name] = independentDepth(src, cp)
		assert.Equal(t, copyNestDepth, depths[name],
			"%s stopped rebuilding at level %d of %d: everything below is shared with the source",
			name, depths[name], copyNestDepth)
	}

	// The agreement itself, stated separately from the absolute answer: if a
	// future change makes one helper shallower the assertions above catch it,
	// and if it makes all three shallower together this one still reports
	// that they moved as a group.
	for a := range depths {
		for b := range depths {
			assert.Equal(t, depths[a], depths[b],
				"%s rebuilds %d levels and %s rebuilds %d: the copy helpers must agree",
				a, depths[a], b, depths[b])
		}
	}
}

// TestCopyMapIsDeep is the reproduction from issue #395 at the Go level: a
// write through a copied map must not reach the source.
func TestCopyMapIsDeep(t *testing.T) {
	t.Parallel()

	inner := lisp.SortedMap()
	inner.MapSet("city", lisp.String("London"))
	src := lisp.SortedMap()
	src.MapSet("address", inner)

	cp, err := copyMap(src)
	require.NoError(t, err)

	cpInner, ok := cp.Map().Get(lisp.String("address"))
	require.True(t, ok)
	require.Equal(t, lisp.LSortMap, cpInner.Type)
	cpInner.MapSet("city", lisp.String("REDACTED"))

	cpCity, _ := cpInner.Map().Get(lisp.String("city"))
	srcCity, _ := inner.Map().Get(lisp.String("city"))
	assert.Equal(t, `"REDACTED"`, cpCity.String(), "the write must land on the copy")
	assert.Equal(t, `"London"`, srcCity.String(),
		"the write must not reach the source: copyMap shared its nested containers (#395)")
}

// nestedValue builds a value nesting depth container levels below its root,
// rotating through the container kinds so a helper's recursion passes through
// every other helper on the way down. The innermost value is a leaf, and each
// level also carries a leaf sibling so that a copy has something to get wrong
// besides the spine.
func nestedValue(root lisp.LType, depth int) *lisp.LVal {
	kinds := make([]lisp.LType, depth)
	kinds[0] = root
	next := 0
	for i := range containerKinds {
		if containerKinds[i] == root {
			next = i
		}
	}
	for i := 1; i < depth; i++ {
		next = (next + 1) % len(containerKinds)
		kinds[i] = containerKinds[next]
	}

	v := leafContainer(kinds[depth-1])
	for i := depth - 2; i >= 0; i-- {
		v = wrap(kinds[i], v)
	}
	return v
}

func leafContainer(kind lisp.LType) *lisp.LVal {
	return wrap(kind, lisp.String("leaf"))
}

func wrap(kind lisp.LType, child *lisp.LVal) *lisp.LVal {
	switch kind {
	case lisp.LSortMap:
		m := lisp.SortedMap()
		m.MapSet("child", child)
		m.MapSet("sibling", lisp.Int(1))
		return m
	case lisp.LArray:
		return lisp.Vector([]*lisp.LVal{child, lisp.Int(1)})
	default:
		return lisp.QExpr([]*lisp.LVal{child, lisp.Int(1)})
	}
}

// containerDepth is the longest chain of containers reachable from v,
// counting v itself.
func containerDepth(v *lisp.LVal) int {
	children := containerChildren(v)
	if children == nil {
		return 0
	}
	best := 0
	for _, c := range children {
		if d := containerDepth(c); d > best {
			best = d
		}
	}
	return best + 1
}

// independentDepth is how many levels of containers cp rebuilt before it
// started handing back src's own nodes: the depth of the shallowest container
// that is the same pointer in both, or the full depth of src when the copy is
// independent all the way down.
//
// Pointer identity is the whole question. A shallow copy compares equal to a
// deep one by value — that is why #395 was invisible to every layer of the
// test suite — and differs only in what a write through it reaches.
func independentDepth(src, cp *lisp.LVal) int {
	if src == cp {
		return 0
	}
	srcKids, cpKids := containerChildren(src), containerChildren(cp)
	if len(srcKids) != len(cpKids) {
		return 0
	}
	best := -1
	for i := range srcKids {
		d := independentDepth(srcKids[i], cpKids[i])
		if best < 0 || d < best {
			best = d
		}
	}
	if best < 0 {
		// An independent container with no containers below it: one level
		// rebuilt, matching containerDepth's count for the same node.
		best = 0
	}
	return best + 1
}

// containerChildren is the containers v reaches, in a stable order, or nil if
// v is not a container. Leaves are skipped: they are immutable, so sharing
// one is not sharing anything a write can reach.
func containerChildren(v *lisp.LVal) []*lisp.LVal {
	var cells []*lisp.LVal
	switch v.Type {
	case lisp.LSortMap:
		m := v.Map()
		entries := make([]*lisp.LVal, m.Len())
		if lerr := m.Entries(entries); lerr.Type == lisp.LError {
			panic(fmt.Sprintf("map entries: %v", lerr))
		}
		for _, ent := range entries {
			cells = append(cells, ent.Cells[1])
		}
	case lisp.LArray:
		// The storage list is an internal node of the array, not a value the
		// program can name, so the walk steps straight through it.
		cells = v.Cells[1].Cells
	case lisp.LSExpr:
		cells = v.Cells
	default:
		return nil
	}
	out := make([]*lisp.LVal, 0, len(cells))
	for _, c := range cells {
		switch c.Type {
		case lisp.LSortMap, lisp.LArray, lisp.LSExpr:
			out = append(out, c)
		default:
		}
	}
	return out
}

// TestPathDirectedCopySkipsTheSubtreeItReplaces pins the other half of the
// copy: the value under the path is not walked at all, because it is about to
// be replaced or removed.
//
// That is a performance property, and a performance property nothing asserts
// drifts back. It has one deterministic observable: a value that contains
// itself has no finite copy and the copy walk refuses it (#393), so a cycle
// hanging under the on-path key is answered when it is skipped and refused
// when it is walked. Off the path the refusal must still stand, which is the
// second half of the test.
//
// These go through the Path API rather than the builtins because OKSimpleType
// refuses a cyclic value at the gate before any builtin reaches a copy.
func TestPathDirectedCopySkipsTheSubtreeItReplaces(t *testing.T) {
	t.Parallel()

	cyclicMap := func() *lisp.LVal {
		c := lisp.SortedMap()
		c.MapSet("self", c)
		return c
	}
	cyclicVec := func() *lisp.LVal {
		c := lisp.Vector([]*lisp.LVal{lisp.Nil()})
		c.Cells[1].Cells[0] = c
		return c
	}

	t.Run("on the path, a map entry", func(t *testing.T) {
		src := lisp.SortedMap()
		src.MapSet("a", cyclicMap())
		src.MapSet("b", lisp.String("keep"))

		for name, op := range map[string]func(Path) (*lisp.LVal, error){
			"Set":    func(p Path) (*lisp.LVal, error) { return p.Set(src, lisp.String("v")) },
			"Delete": func(p Path) (*lisp.LVal, error) { return p.Delete(src) },
			"Nil":    func(p Path) (*lisp.LVal, error) { return p.Nil(src) },
		} {
			out, err := op(Root(Chain(Dot("a"))))
			require.NoError(t, err, "%s walked the subtree it was about to replace", name)
			require.NotNil(t, out)
		}
	})

	t.Run("on the path, a sequence element", func(t *testing.T) {
		src := lisp.Vector([]*lisp.LVal{cyclicVec(), lisp.String("keep")})

		for name, op := range map[string]func(Path) (*lisp.LVal, error){
			"Set":    func(p Path) (*lisp.LVal, error) { return p.Set(src, lisp.String("v")) },
			"Delete": func(p Path) (*lisp.LVal, error) { return p.Delete(src) },
			"Nil":    func(p Path) (*lisp.LVal, error) { return p.Nil(src) },
		} {
			out, err := op(Root(Chain(Index(0))))
			require.NoError(t, err, "%s walked the element it was about to replace", name)
			require.NotNil(t, out)
		}
		out, err := Root(Chain(Range(0, 1, false))).Set(src, lisp.Vector([]*lisp.LVal{lisp.String("v")}))
		require.NoError(t, err, "the range set walked the range it was about to replace")
		require.NotNil(t, out)
	})

	t.Run("off the path, still refused", func(t *testing.T) {
		src := lisp.SortedMap()
		src.MapSet("a", lisp.String("plain"))
		src.MapSet("b", cyclicMap())

		_, err := Root(Chain(Dot("a"))).Set(src, lisp.String("v"))
		require.ErrorIs(t, err, errCyclicValue,
			"an off-path subtree is copied, so a cycle in one must still be refused (#393)")

		seq := lisp.Vector([]*lisp.LVal{lisp.String("plain"), cyclicVec()})
		_, err = Root(Chain(Index(0))).Delete(seq)
		require.ErrorIs(t, err, errCyclicValue,
			"an off-path element is copied, so a cycle in one must still be refused (#393)")
	})
}

// TestMapSlotRuleMatchesTheMap pins sameMapSlot against the map the copy is
// built into. sameMapSlot decides which entry the path-directed copy leaves
// out; if it disagreed with the backing's own key rule the copy would drop a
// sibling, or keep one the caller is about to overwrite.
func TestMapSlotRuleMatchesTheMap(t *testing.T) {
	t.Parallel()

	keys := []*lisp.LVal{
		lisp.String("a"), lisp.Symbol("a"),
		lisp.String("b"), lisp.Symbol("b"),
		lisp.String(""), lisp.Int(1), lisp.Bool(true),
	}
	marker := lisp.String("marker")
	for _, a := range keys {
		for _, b := range keys {
			m := lisp.SortedMap().Map()
			stored := m.Set(a, marker).Type != lisp.LError
			got, found := m.Get(b)
			want := stored && found && got == marker
			assert.Equal(t, want, sameMapSlot(a, b),
				"sameMapSlot(%s, %s) disagrees with the sorted-map backing", a, b)
		}
	}
}
