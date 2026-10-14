// Copyright © 2026 The ELPS authors

package libjson

// Sharing between two groups of roots (luthersystems/elps#839).
//
// DurableSharing answers which objects of a new group of values are also
// reachable from an old group: the question an embedder asks when it
// migrates saved values and must know whether the new ones still hold old
// closures.  It does not walk values itself.  It runs DumpDurableRoots's
// encoder over the old roots, then the new, and records the graph the
// encoder's counting pass walks: one node per object and per storage cell
// (the cycle check's nodes), and an edge from the open node to each node
// the pass opens or revisits, and from a view to the cells of its range.
// So the walk is exactly the codec's (lists, maps, vectors and their
// storage, tagged values, errors, native payloads, closures, captured
// frames and code), the limits and charges are the dump's, and the query
// succeeds exactly when DumpDurableRoots of the same roots does.
//
// A node is old when the pass first visited it inside an old root: the old
// roots come first, so that is exactly when an old root reaches it.  Which
// new roots reach an old node is found by one backward walk from the old
// nodes, and the kept closures by one forward walk from the new roots in
// order, so the work is linear in the graph.  A view's range is reached
// through a segment tree over its storage's cells, and the saved frames of
// the kept closures are listed once each.  Every order a result shows
// comes from the encoder's walk order, never from addresses or Go maps.
//
// A new closure counts as made by old code only through its frames: one
// that saves no frame (its frames bind nothing) shares nothing with the old
// roots, and is not reported.

import (
	"errors"
	"fmt"
	"slices"
	"strings"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// Sharing is what DurableSharing reports.
type Sharing struct {
	// Roots has one entry per root of the new group, in its order.
	Roots []RootSharing
	// Closures are the kept closures: each closure a new root reaches
	// that the old roots reach too, and each closure only new roots reach
	// whose captured frames continue a frame the old roots reach.  Each
	// is listed once, under the first new root that reaches it; the list
	// is in the order of those roots, and, within one root, in the
	// encoder's walk order.
	Closures []KeptClosure
	// Frames holds the saved frames of the kept closures, each once: a
	// KeptClosure's Frame and a frame's Parent index it.  A frame is listed
	// when the first kept closure that captures it is.
	Frames []CapturedFrame
}

// RootSharing is what one new root shares with the old group.
type RootSharing struct {
	// Name is the root's name.
	Name string
	// Shared reports that the root reaches an object the old roots reach:
	// a list, vector, map, tagged value, error, bytes, native, storage
	// cell, closure, captured frame or closure code.
	Shared bool
	// KeepsClosure reports that the root reaches a closure or a captured
	// frame the old roots reach, directly or through a new closure whose
	// frames continue an old frame.  It implies Shared.
	KeepsClosure bool
}

// KeptClosure is one kept closure.
type KeptClosure struct {
	// Closure is the closure.  It is the caller's own value, so treat it
	// as read-only.  lisp.LEnv.CheckClosure checks its code.
	Closure *lisp.LVal
	// Root is the name of the first new root that reaches the closure.
	Root string
	// Frame is the index in Sharing.Frames of the closure's innermost
	// saved frame, or -1 when it captured none.  The saved frames are those
	// DumpDurable writes: the root environment and frames with no bindings
	// are left out.  Sharing.Lookup finds a captured binding.
	Frame int
	// Old reports that the old roots reach the closure itself.  When it is
	// false the closure is new, and its captured frames continue a frame
	// the old roots reach: code an old closure ran made it.
	Old bool
}

// CapturedFrame is one saved frame of a kept closure.
type CapturedFrame struct {
	// Bindings holds the frame's bindings, sorted by name.
	Bindings []CapturedBinding
	// Parent is the index in Sharing.Frames of the frame's parent, or -1
	// when the parent is the root environment.
	Parent int
	// Old reports that the old roots reach the frame.
	Old bool
}

// CapturedBinding is one binding of a saved frame.
type CapturedBinding struct {
	// Name is the bound name.
	Name string
	// Tag is the user type of a tagged value (lisp.LTaggedVal), and empty
	// for any other value.
	Tag string
	// Type is the bound value's type.
	Type lisp.LType
}

// Lookup returns the binding of name that a lookup from frame (an index in
// s.Frames, such as KeptClosure.Frame) finds: the binding of the innermost
// frame on the parent chain that binds name.  It reports false when no
// saved frame binds name.
func (s *Sharing) Lookup(frame int, name string) (CapturedBinding, bool) {
	for frame >= 0 && frame < len(s.Frames) {
		f := &s.Frames[frame]
		if i, ok := slices.BinarySearchFunc(f.Bindings, name, func(b CapturedBinding, name string) int {
			return strings.Compare(b.Name, name)
		}); ok {
			return f.Bindings[i], true
		}
		frame = f.Parent
	}
	return CapturedBinding{}, false
}

// SharingRoots is the two groups of roots DurableSharing compares.  Each
// group is in the caller's order, which decides the order of the results.
type SharingRoots struct {
	Old, New []DurableRoot
}

// DurableSharing reports what the values of roots.New share with those of
// roots.Old: for each new root, whether it reaches an object, or a closure
// or captured frame, that an old root reaches, and the kept closures (see
// Sharing).  It walks the graph DumpDurableRoots walks for the roots of
// roots.Old followed by those of roots.New, under the same limits, charges
// and refusals, and fails exactly when that call does.  Root names must be
// distinct across both groups.  It evaluates no Lisp code, and the result is the
// same for the same value graphs, whatever their addresses.
func DurableSharing(env *lisp.LEnv, roots SharingRoots, reg *DurableRegistry, opts ...TypedOption) (*Sharing, error) {
	if env == nil {
		return nil, errors.New("durable json: DurableSharing needs an environment")
	}
	if err := reg.checkUsable(); err != nil {
		return nil, err
	}
	cfg := durableConfig(env, opts)
	oldRoots, newRoots := roots.Old, roots.New
	all := slices.Concat(oldRoots, newRoots)
	list, err := rootList(all, cfg)
	if err != nil {
		return nil, err
	}
	e := newDurableEncoder(env, reg, cfg)
	if len(all) > 0 {
		e.share = &shareGraph{
			rootKey: listKey{&list.Cells[0], len(list.Cells)}, cur: -1, top: make([]int, len(all)),
			closures: map[int]*lisp.LVal{}, frames: map[int]*lisp.LEnv{}, link: map[int]int{},
		}
		for i := range e.share.top {
			e.share.top[i] = -1
		}
	}
	if _, err := e.dump(list); err != nil {
		return nil, err
	}
	out := &Sharing{Roots: make([]RootSharing, len(newRoots))}
	for i, r := range newRoots {
		out.Roots[i].Name = r.Name
	}
	if e.share == nil {
		return out, nil
	}
	e.share.analyze(e, len(oldRoots), newRoots, out)
	return out, nil
}

// rootList checks roots and builds the list DumpDurableRoots writes:
// ("name1" value1 "name2" value2 ...), or () for no roots.
func rootList(roots []DurableRoot, cfg typedConfig) (*lisp.LVal, error) {
	// Each root is at least two values: check before allocating for them.
	if len(roots) > cfg.maxValues/2 {
		return nil, fmt.Errorf("%w: more than %d values", ErrTypedLimit, cfg.maxValues)
	}
	cells := make([]*lisp.LVal, 0, 2*len(roots))
	seen := make(map[string]struct{}, len(roots))
	for _, r := range roots {
		if r.Name == "" || !utf8.ValidString(r.Name) {
			return nil, errors.New("durable json: a root name must be a nonempty UTF-8 string")
		}
		if _, dup := seen[r.Name]; dup {
			return nil, fmt.Errorf("durable json: root %q appears twice", r.Name)
		}
		seen[r.Name] = struct{}{}
		cells = append(cells, lisp.String(r.Name), r.Value)
	}
	return lisp.QExpr(cells), nil
}

// shareGraph records the graph the counting pass walks (see the top of
// this file).  Node ids are the pass's node indexes.
type shareGraph struct {
	// rootKey is the identity of the root list.  The cells the pass walks
	// directly under it are the roots' names and values.
	rootKey any
	// closures holds each closure node's value.
	closures map[int]*lisp.LVal
	// frames holds each captured frame node's environment.
	frames map[int]*lisp.LEnv
	// link holds each closure node's frame node and each frame node's
	// parent frame node (-1 for the root environment).
	link map[int]int
	// root holds, by node, the index of the root inside which the pass
	// first visited the node (-1 for the root list).
	root []int
	// stack holds the open nodes.
	stack []int
	// edges holds the edges in the order the pass met them.
	edges []shareEdge
	// cells holds each storage's cell nodes, by storage group and offset.
	cells [][]int
	// top holds, by root, the root value's node, or -1 for a value that
	// is not an object.
	top []int
	// cur is the root the pass is inside, or -1.
	cur int
}

// shareEdge is an edge from node from: to node to, or, when to is -1, to
// the cells [lo,hi) of storage group.
type shareEdge struct {
	from, to, group, lo, hi int
}

// open records a node the pass opened, with an edge from the enclosing open
// node.
func (g *shareGraph) open(i int) {
	g.root = append(g.root, g.cur)
	if len(g.stack) > 0 {
		g.edge(i)
	}
	g.stack = append(g.stack, i)
}

// close records that the innermost open node finished.
func (g *shareGraph) close() {
	g.stack = g.stack[:len(g.stack)-1]
}

// edge records an edge from the innermost open node to node i.  An edge
// from the root list is a root's value.
func (g *shareGraph) edge(i int) {
	from := g.stack[len(g.stack)-1]
	if from == 0 && g.cur >= 0 {
		g.top[g.cur] = i
	}
	g.edges = append(g.edges, shareEdge{from: from, to: i})
}

// rootCell notes that the pass walks cell k of the root list: root k/2's
// name or value.
func (g *shareGraph) rootCell(k int) {
	g.cur = k / 2
}

// cell records that node i is cell k of storage group, of n cells.
func (g *shareGraph) cell(group, n, k, i int) {
	g.storage(group)
	if g.cells[group] == nil {
		g.cells[group] = make([]int, n)
		for j := range g.cells[group] {
			g.cells[group][j] = -1
		}
	}
	g.cells[group][k] = i
}

// storage makes room for storage group's cells.
func (g *shareGraph) storage(group int) {
	for len(g.cells) <= group {
		g.cells = append(g.cells, nil)
	}
}

// view records an edge from the open view to the cells [lo,hi) of storage
// group.
func (g *shareGraph) view(group, lo, hi int) {
	g.storage(group)
	g.edges = append(g.edges, shareEdge{from: g.stack[len(g.stack)-1], to: -1, group: group, lo: lo, hi: hi})
}

// closure records closure node i and its value.
func (g *shareGraph) closure(i int, v *lisp.LVal) {
	g.closures[i] = v
}

// frame records captured frame node i and its environment.
func (g *shareGraph) frame(i int, env *lisp.LEnv) {
	g.frames[i] = env
}

// setLink records closure node i's frame node, or frame node i's parent
// frame node: f, or -1 for the root environment.
func (g *shareGraph) setLink(i, f int) {
	g.link[i] = f
}

// linkOf returns the frame node setLink recorded for node i, or -1.
func (g *shareGraph) linkOf(i int) int {
	if f, ok := g.link[i]; ok {
		return f
	}
	return -1
}

// frameNode returns the node of a captured frame the pass has visited, or
// -1 for the root environment.
func (e *durableEncoder) frameNode(env *lisp.LEnv) int {
	if env == nil {
		return -1
	}
	if i, ok := e.order[frameKey{env}]; ok {
		return i
	}
	return -1
}

// segTree places one storage's cells under a segment tree, so an edge to
// a range of cells is an edge to the O(log n) tree nodes covering it.
type segTree struct {
	cells []int // the cell nodes (-1: never claimed)
	base  int   // the id of tree node 1; node i is base+i-1
	size  int   // a power of two at least len(cells)
}

// id returns the graph node of tree position p, or -1.
func (t *segTree) id(p int) int {
	if p >= t.size {
		if k := p - t.size; k < len(t.cells) {
			return t.cells[k]
		}
		return -1
	}
	return t.base + p - 1
}

// cover appends the graph nodes covering [lo,hi), left to right.
func (t *segTree) cover(out []int, lo, hi int) []int {
	var right []int
	for l, r := lo+t.size, hi+t.size; l < r; l, r = l/2, r/2 {
		if l&1 == 1 {
			out = append(out, t.id(l))
			l++
		}
		if r&1 == 1 {
			r--
			right = append(right, t.id(r))
		}
	}
	for i := len(right) - 1; i >= 0; i-- {
		out = append(out, right[i])
	}
	return out
}

// analyze fills out from the recorded graph.  nOld is the number of old
// roots; newRoots follow them.
func (g *shareGraph) analyze(e *durableEncoder, nOld int, newRoots []DurableRoot, out *Sharing) {
	n := len(g.root)
	old := func(i int) bool { return i < n && g.root[i] >= 0 && g.root[i] < nOld }
	// Segment trees for the storages a view edge names, in group order.
	trees := make([]*segTree, len(g.cells))
	total := n
	for _, ed := range g.edges {
		if ed.to < 0 && trees[ed.group] == nil {
			size := 1
			for size < len(g.cells[ed.group]) {
				size *= 2
			}
			trees[ed.group] = &segTree{cells: g.cells[ed.group], size: size}
		}
	}
	for _, t := range trees {
		if t != nil {
			t.base = total
			total += t.size - 1
		}
	}
	// The adjacency, in the order the pass met each node's edges.
	var from, to []int
	add := func(a, b int) {
		if b >= 0 {
			from, to = append(from, a), append(to, b)
		}
	}
	var scratch []int
	for _, ed := range g.edges {
		if ed.to >= 0 {
			add(ed.from, ed.to)
			continue
		}
		scratch = trees[ed.group].cover(scratch[:0], ed.lo, ed.hi)
		for _, b := range scratch {
			add(ed.from, b)
		}
	}
	for _, t := range trees {
		if t == nil {
			continue
		}
		for p := 1; p < t.size; p++ {
			add(t.id(p), t.id(2*p))
			add(t.id(p), t.id(2*p+1))
		}
	}
	succ := csr(total, from, to)
	pred := csr(total, to, from)
	// Backward from the old nodes, and from the old closures and frames.
	reachesOld := backward(total, pred, func(i int) bool { return old(i) })
	reachesKept := backward(total, pred, func(i int) bool { return old(i) && (g.closures[i] != nil || g.frames[i] != nil) })
	for r := range newRoots {
		if t := g.top[nOld+r]; t >= 0 {
			out.Roots[r].Shared = reachesOld[t]
			out.Roots[r].KeepsClosure = reachesKept[t]
		}
	}
	if len(g.closures) == 0 {
		return
	}
	// continues[f] reports that frame node f or a frame above it is old.
	continues := map[int]bool{}
	var chain []int
	continuesOld := func(f int) bool {
		chain = chain[:0]
		res := false
		for f >= 0 {
			if c, ok := continues[f]; ok {
				res = c
				break
			}
			if old(f) {
				res = true
				break
			}
			chain = append(chain, f)
			f = g.linkOf(f)
		}
		for _, c := range chain {
			continues[c] = res
		}
		return res
	}
	// Forward from the new roots in order: each kept closure is listed
	// under the first root that reaches it.
	seen := make([]bool, total)
	frameIndex := map[int]int{}
	var stack []int
	for r, root := range newRoots {
		t := g.top[nOld+r]
		if t < 0 || seen[t] {
			continue
		}
		seen[t] = true
		stack = append(stack[:0], t)
		for len(stack) > 0 {
			i := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if v := g.closures[i]; v != nil {
				f := g.linkOf(i)
				isOld := old(i)
				if isOld || continuesOld(f) {
					out.Closures = append(out.Closures, KeptClosure{
						Closure:  v,
						Root:     root.Name,
						Old:      isOld,
						Frame:    g.savedFrame(e, out, frameIndex, f, old),
					})
				}
			}
			next := succ.of(i)
			for k := len(next) - 1; k >= 0; k-- {
				if c := next[k]; !seen[c] {
					seen[c] = true
					stack = append(stack, c)
				}
			}
		}
	}
}

// savedFrame returns the index in out.Frames of frame node f, listing it
// and its parents first when they are not listed yet, or -1 for the root
// environment.  frameIndex holds the frames listed so far, by node.
func (g *shareGraph) savedFrame(e *durableEncoder, out *Sharing, frameIndex map[int]int, f int, old func(int) bool) int {
	// The parents not listed yet, innermost first.
	var chain []int
	for ; f >= 0; f = g.linkOf(f) {
		if _, ok := frameIndex[f]; ok {
			break
		}
		chain = append(chain, f)
	}
	parent := -1
	if f >= 0 {
		parent = frameIndex[f]
	}
	for k := len(chain) - 1; k >= 0; k-- {
		node := chain[k]
		bs := e.frameBindings[g.frames[node]]
		frame := CapturedFrame{Parent: parent, Old: old(node), Bindings: make([]CapturedBinding, len(bs))}
		for j, b := range bs {
			frame.Bindings[j] = CapturedBinding{Name: b.name, Type: b.value.Type}
			if b.value.Type == lisp.LTaggedVal {
				frame.Bindings[j].Tag = b.value.Str
			}
		}
		parent = len(out.Frames)
		frameIndex[node] = parent
		out.Frames = append(out.Frames, frame)
	}
	if len(chain) == 0 {
		return parent
	}
	return frameIndex[chain[0]]
}

// adjacency is a compressed adjacency list: the successors of node i are
// to[start[i]:start[i+1]], in the order they were added.
type adjacency struct {
	start, to []int
}

func (a adjacency) of(i int) []int { return a.to[a.start[i]:a.start[i+1]] }

// csr builds the adjacency of n nodes from edges from[k] -> to[k], keeping
// each node's edges in their order.
func csr(n int, from, to []int) adjacency {
	start := make([]int, n+1)
	for _, f := range from {
		start[f+1]++
	}
	for i := range n {
		start[i+1] += start[i]
	}
	next := slices.Clone(start[:n])
	out := make([]int, len(to))
	for k, f := range from {
		out[next[f]] = to[k]
		next[f]++
	}
	return adjacency{start: start, to: out}
}

// backward marks every node from which a node that seed accepts is
// reachable, seeds included.
func backward(n int, pred adjacency, seed func(int) bool) []bool {
	mark := make([]bool, n)
	var stack []int
	for i := range n {
		if seed(i) {
			mark[i] = true
			stack = append(stack, i)
		}
	}
	for len(stack) > 0 {
		i := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		for _, p := range pred.of(i) {
			if !mark[p] {
				mark[p] = true
				stack = append(stack, p)
			}
		}
	}
	return mark
}
