// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"log"
	"strconv"
	"strings"
	"unicode/utf8"
)

// maxRenderDepth bounds nested value rendering independently of cycle
// detection, output budgets, and evaluator limits. Go's stack overflow is
// fatal, so even an acyclic graph must stop before exhausting the stack.
const maxRenderDepth = 1024

const renderDepthMark = "#<depth-limit>"

// boundedString renders at most limit bytes of String's representation.
// A false result means the complete representation does not fit. No partial
// rendering is returned. Zero is a literal zero-byte limit, not unlimited.
func (v *LVal) boundedString(limit int) (string, bool) {
	if limit < 0 {
		return "", false
	}
	// Scalars need no traversal buffer. Numeric text has a fixed maximum
	// size; symbols may be arbitrarily long, so check before adding quotes.
	var scalar string
	switch v.Type {
	case LInt:
		scalar = strconv.Itoa(v.Int)
	case LFloat:
		scalar = strconv.FormatFloat(v.Float, 'g', -1, 64)
	case LSymbol, LQSymbol:
		quotes := 0
		if v.quoted {
			quotes++
		}
		if v.Type == LQSymbol {
			quotes++
		}
		if quotes > limit || len(v.Str) > limit-quotes {
			return "", false
		}
		return "''"[:quotes] + v.Str, true
	default:
		return v.boundedNestedString(limit)
	}
	if len(scalar) > limit {
		return "", false
	}
	return scalar, true
}

func (v *LVal) boundedNestedString(limit int) (string, bool) {
	var st cycleState
	r := valueRenderer{limit: limit}
	r.root(v, cycleGuard{state: &st})
	if !r.full && !st.cyclic {
		return r.out.String(), true
	}
	// A lazy cycle walk can exhaust the budget before discovering that
	// String's eventual strict rendering is small enough. Retry with the
	// same strict visited set String uses, but also track the active path:
	// repeated DAG nodes alone must NEVER justify a truncated rendering.
	// Nor may a long cycle hidden beyond the lazy walk's depth cap: String
	// uses depth truncation, not strict-cycle rendering, in that case.
	r = valueRenderer{limit: limit, active: make(map[*LVal]int)}
	r.root(v, strictCycleGuard())
	if r.full {
		return "", false
	}
	if !st.cyclic && !r.lazyCycle {
		probe, cyclic := probeRenderCycles(v)
		if !cyclic {
			if len(probe.recovered) == 0 {
				return "", false
			}
			// A malformed error message can exceed the budget before its
			// recovery replaces it with a short sentinel. The probe has
			// established exactly which messages recover at each depth.
			// Replay the acyclic rendering with those replacements known.
			r = valueRenderer{limit: limit, recovered: probe.recovered}
			var retry cycleState
			r.root(v, cycleGuard{state: &retry})
			if r.full || retry.cyclic {
				return "", false
			}
		}
	}
	return r.out.String(), true
}

// probeRenderCycles handles shared nodes which the strict retry first reached
// near the depth cap and then skipped on a shallower path. Capture the
// rendered graph with a separate vertex for each value and depth: recovery
// from a malformed error child can expose different edges at different
// depths. Memoization bounds shared DAG traversal without merging those paths.
func probeRenderCycles(v *LVal) (*renderCycleProbe, bool) {
	p := renderCycleProbe{nodes: make(map[renderProbeNode]*renderProbeVisit)}
	r := valueRenderer{limit: -1, probe: &p}
	var st cycleState
	r.root(v, cycleGuard{state: &st})

	lastDepth := make(map[*LVal]int)
	for node := range p.nodes {
		lastDepth[node.value] = max(lastDepth[node.value], node.depth)
	}
	seen := make(map[renderProbeNode]int)
	var queue []renderProbeNode
	search := 0
	for start := range p.nodes {
		// Lazy tracking starts at depth 64. Only a path from a tracked
		// occurrence to another occurrence of that value proves a cycle.
		if start.depth < cycleGuardDepth || lastDepth[start.value] <= start.depth {
			continue
		}
		search++
		queue = append(queue[:0], start)
		seen[start] = search
		for head := 0; head < len(queue); head++ {
			for child := range p.nodes[queue[head]].children {
				if child.value == start.value {
					return &p, true
				}
				if seen[child] != search {
					seen[child] = search
					queue = append(queue, child)
				}
			}
		}
	}
	return &p, false
}

type renderProbeNode struct {
	value *LVal
	depth int
}

type renderProbeVisit struct {
	children map[renderProbeNode]struct{}
	panicVal any
}

type renderCycleProbe struct {
	nodes     map[renderProbeNode]*renderProbeVisit
	parent    *renderProbeVisit
	recovered map[renderProbeNode]bool
}

// valueRenderer is the shared streaming implementation for nested String
// values and boundedString. String's top-level scalar fast paths stay in
// LVal.str; containers no longer assemble a temporary string per child.
// A negative limit is the ordinary, unlimited String path.
type valueRenderer struct {
	active    map[*LVal]int
	probe     *renderCycleProbe
	recovered map[renderProbeNode]bool
	out       strings.Builder
	limit     int
	full      bool
	lazyCycle bool
}

func (r *valueRenderer) text(s string) {
	if r.full || r.probe != nil {
		return
	}
	if r.limit >= 0 && len(s) > r.limit-r.out.Len() {
		r.full = true
		return
	}
	r.out.WriteString(s)
}

func (r *valueRenderer) root(v *LVal, g cycleGuard) {
	if v.Type == LQuote {
		r.text("'")
		r.value(v.Cells[0], true, g)
		return
	}
	r.value(v, false, g)
}

func (r *valueRenderer) value(v *LVal, onTheRecord bool, g cycleGuard) {
	if r.full || g.abandoned() {
		return
	}
	quote := ""
	if onTheRecord {
		quote = "'"
	}
	switch v.Type {
	case LInt:
		r.text(quote)
		r.text(strconv.Itoa(v.Int))
		return
	case LFloat:
		r.text(quote)
		r.text(strconv.FormatFloat(v.Float, 'g', -1, 64))
		return
	case LString:
		r.text(quote)
		r.quotedString(v.Str)
		return
	case LBytes:
		r.text(quote)
		r.text("#<bytes")
		for _, b := range v.Bytes() {
			if r.full {
				return
			}
			r.text(" ")
			r.text(strconv.Itoa(int(b)))
		}
		r.text(">")
		return
	case LSymbol, LQSymbol:
		if v.quoted {
			quote = "'"
		}
		r.text(quote)
		if v.Type == LQSymbol {
			r.text("'")
		}
		r.text(v.Str)
		return
	case LNative:
		r.text(fmt.Sprintf("#<native value: %T>", v.Native))
		return
	default:
		// The remaining types contain nested values and use the guard below.
	}
	if g.depth >= maxRenderDepth {
		r.text(renderDepthMark)
		return
	}
	if p := r.probe; p != nil {
		g.depth++
		node := renderProbeNode{value: v, depth: g.depth}
		parent := p.parent
		if parent != nil {
			if parent.children == nil {
				parent.children = make(map[renderProbeNode]struct{})
			}
			parent.children[node] = struct{}{}
		}
		if previous := p.nodes[node]; previous != nil {
			if previous.panicVal != nil {
				panic(previous.panicVal)
			}
			return
		}
		visit := new(renderProbeVisit)
		p.nodes[node] = visit
		p.parent = visit
		defer func() {
			p.parent = parent
			if recovered := recover(); recovered != nil {
				// Replay malformed descendants on cache hits so their
				// enclosing error still skips its remaining siblings.
				visit.panicVal = recovered
				panic(recovered)
			}
		}()
		r.nested(v, onTheRecord, g)
		return
	}
	if r.active != nil {
		if firstDepth, ok := r.active[v]; ok {
			// Lazy tracking starts at cycleGuardDepth. Following this
			// cycle again must rediscover a tracked node before the cap
			// for String to use the same strict rendering as this retry.
			period := g.depth + 1 - firstDepth
			if max(firstDepth, cycleGuardDepth)+period <= maxRenderDepth {
				r.lazyCycle = true
			}
		}
	}
	next, cyclic := g.descend(v)
	if cyclic {
		r.text(cycleMark)
		return
	}
	if r.active != nil {
		r.active[v] = next.depth
	}
	if r.active != nil || next.tracking() {
		// Error-message rendering contains malformed child panics. Keep
		// the walk's path correct when that recovery resumes a parent.
		defer func() {
			if r.active != nil {
				delete(r.active, v)
			}
			if next.tracking() {
				next.ascend(v)
			}
		}()
	}
	r.nested(v, onTheRecord, next)
}

// quotedString produces strconv.Quote's representation without allocating
// the escaped form of the whole string. Each temporary holds at most one
// UTF-8 rune (or one invalid byte) and its Go string escapes.
func (r *valueRenderer) quotedString(s string) {
	r.text("\"")
	var scratch [16]byte
	for i := 0; i < len(s) && !r.full; {
		j := i
		for j < len(s) && s[j] >= ' ' && s[j] < utf8.RuneSelf && s[j] != '\\' && s[j] != '"' && s[j] != '\x7f' {
			j++
			if r.limit >= 0 && j-i > r.limit-r.out.Len() {
				r.full = true
				return
			}
		}
		if j > i {
			r.text(s[i:j])
			i = j
			continue
		}
		_, width := utf8.DecodeRuneInString(s[i:])
		quoted := strconv.AppendQuote(scratch[:0], s[i:i+width])
		r.text(string(quoted[1 : len(quoted)-1]))
		i += width
	}
	r.text("\"")
}

func (r *valueRenderer) sequence(cells []*LVal, left, right string, g cycleGuard) {
	if r.full || g.abandoned() {
		return
	}
	// Reserve a small amount from the immediate shape, without visiting
	// any children or multiplying an unbounded source length. This avoids
	// growing wide sequences from an eight-byte initial buffer. It is only
	// a hint: small limits and empty symbol contents must still work.
	hint := len(left) + len(right)
	if n := min(len(cells), 32); n > 0 {
		hint += 2*n - 1
	}
	hint = min(hint, 64)
	if r.limit >= 0 {
		hint = min(hint, r.limit-r.out.Len())
	}
	r.out.Grow(hint)
	r.text(left)
	for i, cell := range cells {
		if r.full || g.abandoned() {
			return
		}
		if i > 0 {
			r.text(" ")
		}
		r.value(cell, false, g)
	}
	r.text(right)
}

func (r *valueRenderer) nested(v *LVal, onTheRecord bool, g cycleGuard) {
	quote := ""
	if onTheRecord {
		quote = "'"
	}
	switch v.Type {
	case LError:
		if v.quoted {
			r.text("'(error '")
			r.text(v.Str)
			r.text(" ")
			r.value(v.Cells[0], false, g)
			r.text(")")
		} else {
			r.errorValue((*ErrorVal)(v), g)
		}
	case LSExpr:
		if v.quoted {
			quote = "'"
		}
		r.text(quote)
		r.sequence(v.Cells, "(", ")", g)
	case LFun:
		if v.quoted {
			quote = "'"
		}
		r.text(quote)
		if v.Builtin() != nil {
			r.text("#<builtin>")
			return
		}
		r.text("(lambda ")
		r.sequence(v.Cells[0].Cells, "(", ")", g)
		for _, expr := range v.Cells[1:] {
			if r.full || g.abandoned() {
				return
			}
			r.text(" ")
			r.value(expr, false, g)
		}
		r.text(")")
	case LQuote:
		r.text("'")
		r.value(v.Cells[0], true, g)
	case LSortMap:
		r.text(quote)
		r.text("(sorted-map")
		// Each entry needs at least its two separating spaces. Check
		// before Entries allocates storage proportional to the map size.
		if r.full || (r.limit >= 0 && v.Map().Len() > (r.limit-r.out.Len())/2) {
			r.full = true
			return
		}
		entries := sortedMapEntries(v.Map())
		if entries.Type == LError {
			// Host maps may report an enumeration error. Its Cells are
			// error data, not key/value pairs.
			r.text(" #<map-error ")
			r.value(entries, false, g)
			r.text(">)")
			return
		}
		for _, pair := range entries.Cells {
			if r.full || g.abandoned() {
				return
			}
			r.text(" ")
			r.value(pair.Cells[0], false, g)
			r.text(" ")
			r.value(pair.Cells[1], false, g)
		}
		r.text(")")
	case LArray:
		if v.Cells[0].Len() == 1 {
			r.text(quote)
			if v.Len() > 0 {
				r.sequence(v.Cells[1].Cells, "(vector ", ")", g)
			} else {
				r.text("(vector)")
			}
		} else {
			r.text("#<array dims=")
			r.value(v.Cells[0], false, g)
			r.text(">")
		}
	case LTaggedVal:
		r.text("#{")
		r.text(v.Str)
		r.text(" ")
		r.value(v.Cells[0], false, g)
		r.text("}")
	case LMarkTerminal:
		r.text(quote)
		r.text("#<terminal-expression ")
		r.value(v.Cells[0], false, g)
		r.text(">")
	case LMarkTailRec:
		r.text(quote)
		r.text("#<tail-recursion frames=")
		r.text(strconv.Itoa(v.Cells[0].Int))
		r.text(" (")
		r.value(v.Cells[1], false, g)
		r.text(" ")
		r.value(v.Cells[2], false, g)
		r.text(")>")
	case LMarkMacExpand:
		r.text(quote)
		r.text("#<macro-expansion ")
		r.value(v.Cells[0], false, g)
		r.text(")>")
	default:
		r.text(quote)
		r.text("#<")
		r.text(v.Type.String())
		r.text(">")
	}
}

// errorValue streams the same location, condition/function prefix, and data
// as ErrorVal.errorString. Native error.Error methods remain host code; their
// own allocations cannot be bounded by an interpreter output budget.
func (r *valueRenderer) errorValue(e *ErrorVal, g cycleGuard) {
	loc, _ := e.Source()
	r.text(loc.File)
	if loc.Pos >= 0 {
		if loc.Line == 0 {
			r.text("[")
			r.text(strconv.Itoa(loc.Pos))
			r.text("]")
		} else {
			r.text(":")
			r.text(strconv.Itoa(loc.Line))
			if loc.Col != 0 {
				r.text(":")
				r.text(strconv.Itoa(loc.Col))
			}
		}
	}
	r.text(": ")
	if e.Str != "error" {
		r.text(e.Str)
		r.text(": ")
	} else if stack := (*LVal)(e).CallStack(); stack != nil && stack.Top() != nil {
		top := stack.Top()
		name := top.Name
		if name == "" {
			name = top.FID
		}
		qualified := top.Package != "" && top.Package != DefaultUserPackage
		if qualified {
			r.text(top.Package)
			r.text(":")
		}
		if name != "" || qualified {
			r.text(name)
			r.text(": ")
		}
	}
	if r.full || g.abandoned() {
		return
	}
	r.errorMessage(e, g)
}

func (r *valueRenderer) errorMessage(e *ErrorVal, g cycleGuard) {
	node := renderProbeNode{value: (*LVal)(e), depth: g.depth}
	if r.recovered[node] {
		r.text(corruptedNativeMessage)
		return
	}
	start := r.out.Len()
	defer func() {
		if recovered := recover(); recovered != nil {
			if r.probe != nil {
				if r.probe.recovered == nil {
					r.probe.recovered = make(map[renderProbeNode]bool)
				}
				r.probe.recovered[node] = true
			}
			// Match ErrorVal.errorMessage's containment boundary: a
			// malformed descendant replaces the entire message, including
			// any prefix data already written, but keeps the error location.
			prefix := r.out.String()[:start]
			r.out.Reset()
			r.full = false
			r.text(prefix)
			r.text(corruptedNativeMessage)
			log.Printf("elps: ErrorVal.ErrorMessage recovered panic during Cells[0].Native type switch: %v; returning sentinel %q", recovered, corruptedNativeMessage)
		}
	}()
	if msg, ok := nativeErrorText(e); ok {
		r.text(msg)
		return
	}
	for i, cell := range e.Cells {
		if r.full || g.abandoned() {
			return
		}
		if i > 0 {
			r.text(" ")
		}
		if cell == nil {
			log.Printf("elps: errorCellMessage skipping nil cell at index %d (LError has malformed Cells slice)", i)
			r.text("<nil>")
		} else if cell.Type == LString {
			r.text(cell.Str)
		} else {
			r.value(cell, false, g)
		}
	}
}

func nativeErrorText(e *ErrorVal) (text string, ok bool) {
	defer func() {
		if recovered := recover(); recovered != nil {
			log.Printf("elps: ErrorVal.ErrorMessage recovered panic during Cells[0].Native type switch: %v; returning sentinel %q", recovered, corruptedNativeMessage)
			text, ok = corruptedNativeMessage, true
		}
	}()
	if len(e.Cells) > 0 && e.Cells[0] != nil {
		if err, ok := e.Cells[0].Native.(error); ok && err != nil {
			return err.Error(), true
		}
	}
	return "", false
}
