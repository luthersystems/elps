// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
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
const renderTruncatedMark = "#<truncated>"

// renderBudget bounds all attempts, including cycle probes which emit no text.
// It is shared across retries so neither recovery nor cycle detection resets it.
type renderBudget struct {
	stepFn    func() bool
	ctx       context.Context
	remaining int
}

// A fixed minimum permits cycle discovery for small final representations.
// Above that floor work scales with the byte cap; arithmetic saturates safely.
func newRenderBudget(limit int, ctx context.Context) renderBudget {
	return renderBudget{ctx: ctx, remaining: 4 * max(1<<20, min(limit, int(^uint(0)>>1)/4))}
}

func (b *renderBudget) step() bool {
	if b.stepFn != nil && !b.stepFn() {
		return false
	}
	if b.remaining <= 0 {
		return false
	}
	b.remaining--
	return b.ctx == nil || b.ctx.Err() == nil
}

// liveRenderContext adopts a captured context only while it can still bound
// work.  An error outlives the request that produced it -- a handler returns
// it, the host cancels the request context, and only then is the error logged
// -- and a captured context that has already been cancelled would fail the
// FIRST budget step, blanking the whole message to #<truncated>.  Rendering an
// error must be total, so a dead context is treated as no context and the byte
// limit alone bounds the output.  A context that is still live is kept: it must
// continue to stop a bulk render that is cancelled mid-traversal.
func liveRenderContext(ctx context.Context) context.Context {
	if ctx != nil && ctx.Err() != nil {
		return nil
	}
	return ctx
}

func truncatedRender(s string, limit int) string {
	if limit < len(renderTruncatedMark) {
		return renderTruncatedMark[:max(0, limit)]
	}
	return s[:min(len(s), limit-len(renderTruncatedMark))] + renderTruncatedMark
}

// Render returns diagnostic text bounded by the environment's output limit and
// active evaluation context. After evaluation, use RenderContext to pass the
// request context explicitly. On exhaustion it substitutes #<truncated>; use builtins such as
// format-string when the program must receive an ordinary allocation error.
func (env *LEnv) Render(v *LVal) string {
	return env.RenderContext(env.evalCtx, v)
}

// RenderContext returns diagnostic text bounded by the environment's output
// limit and ctx, including after EvalContext or LoadStringContext returns.
// Cancellation or exhaustion substitutes a fitting #<truncated> marker.
// A nil context disables cancellation checks.
func (env *LEnv) RenderContext(ctx context.Context, v *LVal) string {
	s, ok := v.boundedStringContext(env.Runtime.MaxAllocBytes(), ctx)
	if !ok {
		return truncatedRender(s, env.Runtime.MaxAllocBytes())
	}
	return s
}

// DiagnosticRenderer shares an output and traversal budget across the values
// in one response. It is not safe for concurrent use.
type DiagnosticRenderer struct {
	budget    renderBudget
	remaining int
	done      bool
}

// NewRenderer creates a response renderer using the runtime's output limit.
// Pass the request context explicitly when rendering after evaluation returns.
func (env *LEnv) NewRenderer(ctx context.Context) *DiagnosticRenderer {
	return env.NewRendererWithLimit(ctx, env.Runtime.MaxAllocBytes())
}

// NewRendererWithLimit reserves a smaller output budget for a response whose
// protocol adds framing, escaping or duplicate representations. The supplied
// limit can only tighten the runtime limit; zero permits no output.
func (env *LEnv) NewRendererWithLimit(ctx context.Context, limit int) *DiagnosticRenderer {
	limit = max(0, min(limit, env.Runtime.MaxAllocBytes()))
	return &DiagnosticRenderer{budget: newRenderBudget(limit, ctx), remaining: limit}
}

// Exhausted reports whether the response has exhausted its rendering budget.
func (r *DiagnosticRenderer) Exhausted() bool {
	return r.done || r.remaining == 0
}

// Render appends a value's diagnostic representation to the response budget.
// Exhaustion emits a fitting truncation marker and stops subsequent rendering.
func (r *DiagnosticRenderer) Render(v *LVal) string {
	if r.Exhausted() {
		return ""
	}
	if v == nil {
		return r.Text("<nil>")
	}
	s, ok := v.boundedWithBudget(r.remaining, &r.budget)
	if !ok {
		s = truncatedRender(s, r.remaining)
		r.done = true
	}
	r.remaining -= len(s)
	return s
}

// Text charges already formatted text to the same response budget. Parts are
// checked before copying, so large names need no unbounded intermediate string.
func (r *DiagnosticRenderer) Text(parts ...string) string {
	if r.Exhausted() {
		return ""
	}
	var out strings.Builder
	for _, part := range parts {
		if !r.budget.step() || len(part) > r.remaining-out.Len() {
			r.done = true
			s := truncatedRender(out.String(), r.remaining)
			r.remaining -= len(s)
			return s
		}
		out.WriteString(part)
	}
	r.remaining -= out.Len()
	return out.String()
}

func (v *LVal) boundedStringContext(limit int, ctx context.Context) (string, bool) {
	budget := newRenderBudget(limit, ctx)
	return v.boundedWithBudget(limit, &budget)
}

func (v *LVal) boundedWithBudget(limit int, budget *renderBudget) (string, bool) {
	if limit < 0 || !budget.step() {
		return "", false
	}
	switch v.Type {
	case LInt, LFloat, LSymbol, LQSymbol:
		return v.boundedString(limit)
	default:
		return v.boundedRender(limit, budget, false)
	}
}

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
	case LString:
		// Quote expands each input byte to at most four bytes, plus the
		// delimiters. Small strings that provably fit need no renderer,
		// cycle guard, or work budget. Larger strings still stream under
		// both budgets rather than allocating an unbounded escaped copy.
		if limit >= 2 && len(v.Str) <= min(4096, (limit-2)/4) {
			return fmt.Sprintf("%q", v.Str), true
		}
		return v.boundedNestedString(limit)
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
	b := newRenderBudget(limit, nil)
	return v.boundedRender(limit, &b, false)
}

func (v *LVal) boundedRender(limit int, budget *renderBudget, message bool) (string, bool) {
	var st cycleState
	r := valueRenderer{limit: limit, budget: *budget, message: message}
	r.root(v, cycleGuard{state: &st})
	budget.remaining = r.budget.remaining
	if !r.full && !st.cyclic {
		return r.out.String(), true
	}
	// The first pass is speculative: a cycle it cannot observe it unrolls
	// until something stops it, so what it spent says nothing about what the
	// rendering costs. Every pass below is linear in the size of the graph
	// by construction, so they share a budget of their own and the caller is
	// charged whichever pass spent more -- a response-wide budget still only
	// shrinks, but one value's failed guess cannot bankrupt it.
	work := newRenderBudget(limit, budget.ctx)
	work.stepFn = budget.stepFn
	analysis := 0
	defer func() { budget.remaining = min(budget.remaining, work.remaining-analysis) }()
	// A lazy cycle walk can exhaust the budget before discovering that
	// String's eventual strict rendering is small enough. Retry with the
	// same strict visited set String uses, but also track the active path:
	// repeated DAG nodes alone must NEVER justify a truncated rendering.
	// Nor may a long cycle hidden beyond the lazy walk's depth cap: String
	// uses depth truncation, not strict-cycle rendering, in that case.
	r = valueRenderer{limit: limit, budget: work, message: message, active: make(map[*LVal]int)}
	r.root(v, strictCycleGuard())
	work = r.budget
	if r.full || work.remaining <= 0 || (work.ctx != nil && work.ctx.Err() != nil) {
		return "", false
	}
	if !st.cyclic && !r.lazyCycle {
		probe, cyclic := probeRenderCycles(v, &work, message)
		if !cyclic {
			// Neither the lazy walk nor the probe descends past
			// maxRenderDepth, so neither observes a cycle whose period
			// exceeds the cap: a ring of 1200 nodes is indistinguishable
			// from a 1200-deep tree that was truncated. The strict
			// rendering may stand for such a value only if BOTH remaining
			// questions say so -- the lazy rendering String is defined by
			// cannot be produced at all, and the value really does contain
			// a cycle. Each runs on a fixed allowance of its own so that
			// the answers are properties of the value rather than of this
			// call's byte limit, and so that what they spend is never
			// mistaken for the rendering itself having failed.
			cyclic = strictRenderingStands(v, message, budget.ctx, &analysis)
		}
		if !cyclic {
			if len(probe.recovered) == 0 {
				return "", false
			}
			// A malformed error message can exceed the budget before its
			// recovery replaces it with a short sentinel. The probe has
			// established exactly which messages recover at each depth.
			// Replay the acyclic rendering with those replacements known.
			r = valueRenderer{limit: limit, budget: work, message: message, recovered: probe.recovered}
			var retry cycleState
			r.root(v, cycleGuard{state: &retry})
			work = r.budget
			if r.full || retry.cyclic {
				return "", false
			}
		}
	}
	if work.remaining <= 0 || (work.ctx != nil && work.ctx.Err() != nil) {
		return "", false
	}
	return r.out.String(), true
}

// strictRenderingStands reports whether the strict rendering of v may be
// returned as the value's representation: it may when the lazy rendering
// String is defined by cannot be produced at all AND the value does contain a
// cycle, which is the case strict mode exists for.
//
// Both walks reach parts of the value the rendering itself does not -- past
// the depth cap, past the byte limit -- so either can be the first to touch a
// malformed header or to call a host Map's Entries. A panic there must not
// become a crash in a value the renderer was about to describe perfectly
// well, so it is contained and answered conservatively.
//
// What the two walks spend is charged to the caller through analysis rather
// than to the rendering's own budget: neither produces output, and an
// exhausted rendering budget means something else entirely.
func strictRenderingStands(v *LVal, message bool, ctx context.Context, analysis *int) (stands bool) {
	lazy, search := newRenderBudget(0, ctx), newRenderBudget(0, ctx)
	start := lazy.remaining + search.remaining
	defer func() {
		*analysis += start - lazy.remaining - search.remaining
		if recovered := recover(); recovered != nil {
			log.Printf("elps: render cycle analysis recovered panic: %v; keeping the depth-bounded verdict", recovered)
			stands = false
		}
	}()
	return !lazyRenderTerminates(v, message, &lazy) && containsCycle(v, &search)
}

// lazyRenderTerminates reports whether the representation String is defined
// by -- the lazy walk's, with its depth cap and its lazy cycle guard -- can be
// produced at all.
//
// A byte limit cannot answer that. A rendering that overruns the caller's
// limit by one byte and a rendering of 2^1024 nodes both come back as "full",
// and the difference decides whether the strict rendering may stand in: it is
// the only representation a branching ring has, and it must never replace a
// representation the value really does have, or the same value would read
// differently at different budgets.
//
// So the walk is repeated with no byte cap and nothing kept -- only the work
// budget, sized independently of limit so the verdict is a property of the
// value. A walk that found the cycle for itself reports false as well: its
// rendering IS the strict one.
//
// The allowance is the budget floor, so a value whose lazy rendering needs
// more work than that reads as having none. Such a rendering is megabytes of
// text; between it and the strict one, the caller gets the strict one.
func lazyRenderTerminates(v *LVal, message bool, budget *renderBudget) bool {
	r := valueRenderer{limit: -1, budget: *budget, message: message, counting: true}
	var st cycleState
	r.root(v, cycleGuard{state: &st})
	*budget = r.budget
	return !r.full && !st.cyclic
}

// probeRenderCycles handles shared nodes which the strict retry first reached
// near the depth cap and then skipped on a shallower path. Capture the
// rendered graph with a separate vertex for each value and depth: recovery
// from a malformed error child can expose different edges at different
// depths. Memoization bounds shared DAG traversal without merging those paths.
func probeRenderCycles(v *LVal, budget *renderBudget, message bool) (*renderCycleProbe, bool) {
	p := renderCycleProbe{nodes: make(map[renderProbeNode]*renderProbeVisit)}
	r := valueRenderer{limit: -1, budget: *budget, message: message, probe: &p}
	var st cycleState
	r.root(v, cycleGuard{state: &st})
	budget.remaining = r.budget.remaining

	lastDepth := make(map[*LVal]int)
	for node := range p.nodes {
		if !budget.step() {
			return &p, false
		}
		lastDepth[node.value] = max(lastDepth[node.value], node.depth)
	}
	seen := make(map[renderProbeNode]int)
	var queue []renderProbeNode
	search := 0
	for start := range p.nodes {
		if !budget.step() {
			return &p, false
		}
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
				if !budget.step() {
					return &p, false
				}
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

// valueRenderer streams values under a byte cap and a shared work budget.
type valueRenderer struct {
	budget    renderBudget
	active    map[*LVal]int
	probe     *renderCycleProbe
	recovered map[renderProbeNode]bool
	out       strings.Builder
	limit     int
	full      bool
	lazyCycle bool
	message   bool
	// counting walks for the traversal alone: text is charged to the work
	// budget and dropped, so a rendering too large to keep can still be
	// established to exist. See lazyRenderTerminates.
	counting bool
}

func (r *valueRenderer) text(s string) {
	if r.budget.ctx != nil && r.budget.ctx.Err() != nil {
		r.full = true
	}
	if r.full || r.probe != nil || r.counting {
		return
	}
	if r.limit >= 0 && len(s) > r.limit-r.out.Len() {
		r.full = true
		return
	}
	r.out.WriteString(s)
}

func (r *valueRenderer) root(v *LVal, g cycleGuard) {
	if r.message {
		r.errorMessage((*ErrorVal)(v), g)
		return
	}
	if v.Type == LQuote {
		r.text("'")
		r.value(v.Cells[0], true, g)
		return
	}
	r.value(v, false, g)
}

func (r *valueRenderer) value(v *LVal, onTheRecord bool, g cycleGuard) {
	if !r.budget.step() {
		r.full = true
		return
	}
	if r.full {
		return
	}
	quote := ""
	if onTheRecord {
		quote = "'"
	}
	switch v.Type {
	case LInt:
		if quote != "" {
			r.text(quote)
		}
		r.text(strconv.Itoa(v.Int))
		return
	case LFloat:
		if quote != "" {
			r.text(quote)
		}
		r.text(strconv.FormatFloat(v.Float, 'g', -1, 64))
		return
	case LString:
		if quote != "" {
			r.text(quote)
		}
		r.quotedString(v.Str)
		return
	case LBytes:
		if quote != "" {
			r.text(quote)
		}
		r.text("#<bytes")
		for _, b := range v.Bytes() {
			if !r.budget.step() {
				r.full = true
				return
			}
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
		if quote != "" {
			r.text(quote)
		}
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
	r.container(v, onTheRecord, g)
}

// Keep guard, probe and recovery frames out of scalar rendering. In particular,
// a scalar must not execute the deferred cleanup belonging to a container.
func (r *valueRenderer) container(v *LVal, onTheRecord bool, g cycleGuard) {
	if g.abandoned() {
		return
	}
	if r.probe != nil && g.depth > maxRenderDepth {
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
			if max(firstDepth, cycleGuardDepth)+period <= maxRenderDepth+1 {
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
	if g.depth >= maxRenderDepth {
		r.text(renderDepthMark)
		return
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
			if j%4096 == 0 && !r.budget.step() {
				r.full = true
				return
			}
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
		if r.probe != nil && v.Map().Len() > r.budget.remaining {
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
