// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"log"
	"strconv"
	"strings"
	"unicode/utf8"
)

// boundedString renders at most limit bytes of String's representation.
// A false result means the complete representation does not fit. No partial
// rendering is returned. Zero is a literal zero-byte limit, not unlimited.
func (v *LVal) boundedString(limit int) (string, bool) {
	if limit < 0 {
		return "", false
	}
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
	r = valueRenderer{limit: limit, active: make(map[*LVal]struct{})}
	r.root(v, strictCycleGuard())
	if r.full || !r.actualCycle {
		return "", false
	}
	return r.out.String(), true
}

// valueRenderer is the shared streaming implementation for nested String
// values and boundedString. String's top-level scalar fast paths stay in
// LVal.str; containers no longer assemble a temporary string per child.
// A negative limit is the ordinary, unlimited String path.
type valueRenderer struct {
	out         strings.Builder
	active      map[*LVal]struct{}
	limit       int
	full        bool
	actualCycle bool
}

func (r *valueRenderer) text(s string) {
	if r.full {
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
	}
	if r.active != nil {
		if _, ok := r.active[v]; ok {
			r.actualCycle = true
		}
	}
	next, cyclic := g.descend(v)
	if cyclic {
		r.text(cycleMark)
		return
	}
	if r.active != nil {
		r.active[v] = struct{}{}
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
	start := r.out.Len()
	defer func() {
		if recovered := recover(); recovered != nil {
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
