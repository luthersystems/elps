// Copyright © 2026 The ELPS authors

package libjson

// cellClaims tracks which cells of one view storage a view has claimed
// (written), and the cycle-check node of each claimed cell.  Both the
// encoder and the decoder use it, so they make the same decisions.
//
// A view claims the cells of its range no earlier view claimed, and
// reaches the nodes of the cells earlier views claimed.  Walking a range
// cell by cell would cost the range's length per view: a list and all of
// its tails is quadratic.  Instead, free finds the next unclaimed cell
// through skip links (a union-find with path compression), and reach
// finds the claimed cell of smallest node in a range through a minimum
// tree, dropping cells that reach no open node (they never will again).
// Every operation is logarithmic or amortized near-constant, so the work
// of all views of a storage is O((cells + views) log cells).
//
// Reaching only the cell of smallest node that still reaches an open node
// is enough: if a later cell of the range reaches an open node outside
// that one, the earlier cell's open node encloses the later cell, and the
// later cell passed its reach to it when it finished.
type cellClaims struct {
	// next[k] is k when cell k is unclaimed, else a link toward the next
	// unclaimed cell.  next[n] is n.
	next []int
	// node holds each cell's node (noLow when the cell is unclaimed or
	// dropped).  tree is a minimum tree over node: entry i < size is the
	// position of the minimum leaf of its subtree, and leaf size+k is
	// cell k itself.
	tree []int
	node []int
	size int
	// ops counts tree and link steps, for the tests' bound.
	ops int
}

func newCellClaims(n int) *cellClaims {
	size := 1
	for size < n {
		size *= 2
	}
	c := &cellClaims{next: make([]int, n+1), node: make([]int, size), tree: make([]int, size), size: size}
	for k := range c.next {
		c.next[k] = k
	}
	for k := range c.node {
		c.node[k] = noLow
	}
	for i := size - 1; i >= 1; i-- {
		c.tree[i] = c.at(2 * i)
	}
	return c
}

// at is the position of the minimum leaf under tree entry i.
func (c *cellClaims) at(i int) int {
	if i >= c.size {
		return i - c.size
	}
	return c.tree[i]
}

// free returns the first unclaimed cell at or after k, or the number of
// cells when there is none.
func (c *cellClaims) free(k int) int {
	r := k
	for c.next[r] != r {
		c.ops++
		r = c.next[r]
	}
	for c.next[k] != r {
		c.ops++
		k, c.next[k] = c.next[k], r
	}
	return r
}

// newCellLinks returns claims with skip links only, for a pass that needs
// no reach.
func newCellLinks(n int) *cellClaims {
	c := &cellClaims{next: make([]int, n+1)}
	for k := range c.next {
		c.next[k] = k
	}
	return c
}

// claim records that cell k is claimed by node.
func (c *cellClaims) claim(k, node int) {
	c.next[k] = k + 1
	if c.tree != nil {
		c.set(k, node)
	}
}

func (c *cellClaims) set(k, node int) {
	c.node[k] = node
	for i := (c.size + k) / 2; i >= 1; i /= 2 {
		c.ops++
		l, r := c.at(2*i), c.at(2*i+1)
		if c.node[r] < c.node[l] {
			l = r
		}
		c.tree[i] = l
	}
}

// least returns the position of the claimed cell of smallest node in
// [a,b), or -1.
func (c *cellClaims) least(a, b int) int {
	best := -1
	pick := func(p int) {
		if c.node[p] != noLow && (best < 0 || c.node[p] < c.node[best]) {
			best = p
		}
	}
	for l, r := a+c.size, b+c.size; l < r; l, r = l/2, r/2 {
		c.ops++
		if l&1 == 1 {
			pick(c.at(l))
			l++
		}
		if r&1 == 1 {
			r--
			pick(c.at(r))
		}
	}
	return best
}

// reach returns the node of the claimed cell in [a,b) whose reach a view
// over [a,b) takes, or noLow.  A cell whose node resolves to no open node
// is dropped first: a finished node that reaches no open node never will.
func (c *cellClaims) reach(a, b int, open []bool, low []int) int {
	for {
		p := c.least(a, b)
		if p < 0 {
			return noLow
		}
		if resolveLow(c.node[p], open, low) != noLow {
			return c.node[p]
		}
		c.set(p, noLow)
	}
}
