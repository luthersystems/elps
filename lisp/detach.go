// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"strings"

	"github.com/luthersystems/elps/internal/fmtmeta"
	"github.com/luthersystems/elps/parser/token"
)

// detach returns a copy of v that shares no memory with v.
//
// detach is the sanctioned way to move a value between Runtimes (issues #362
// and #363; substrate's parse cache is the motivating consumer).  It is
// deliberately unexported: it has no production consumers today, and the
// kernel philosophy is not to expose machinery until a real embedder
// consumer — debugger workflows, cross-runtime transfer — materializes.
// Re-exporting later is additive and easy; the machinery also backs the
// planned lisp-level copy builtin (elps#378).  Copy is a within-runtime
// tool — it deliberately shares an LArray's backing storage
// and a sorted-map's value pointers — so a Copy handed to another Runtime
// still aliases the original.  detach copies everything:
//
//   - Cells, recursively.
//   - LArray backing storage (the storage Copy deliberately shares).
//   - LBytes backing bytes.
//   - Sorted-map data: a fresh MapData with both keys and values detached.
//     The detached map always uses the stock sortedmap implementation,
//     mirroring Copy, even when the source map was built on a custom Map via
//     SortedMapFromData.
//   - An LError's recorded call stack (fresh frames, locations and GoStack).
//   - The Source location and any format-preserving Meta (fresh tokens and
//     locations).
//
// Str, Int and Float travel with the struct copy.  MacroExpansion metadata is
// dropped (nil in the copy): it exists only while a debugger is attached and
// its context holds unevaluated argument values inside the source runtime.
//
// Two shapes cannot be hermetically copied, and detach rejects them rather
// than silently sharing state while claiming isolation:
//
//   - LNative values wrap arbitrary Go data that detach cannot clone.
//   - LFun values.  A builtin holds a Go function; a lambda captures its
//     defining LEnv and, through it, the whole source runtime.  Either way a
//     by-value copy would smuggle the source runtime across the transfer.
//
// The returned error names the path from v to the offending cell:
//
//	Cells[3].Cells[0]: native value (*time.Time) cannot be detached
//
// Internal aliasing within v — the same *LVal reachable along two paths,
// including cycles — is preserved as the same aliasing within the copy.  Only
// sharing between v and the copy is eliminated.
func (v *LVal) detach() (*LVal, error) {
	if v == nil {
		return nil, nil
	}
	d := &detacher{seen: make(map[*LVal]*LVal)}
	return d.detach(v)
}

// detacher tracks original→copy correspondences for one detach call so that
// values reachable along multiple paths (or cyclically) are copied exactly
// once and the copy reproduces the original's internal aliasing.
type detacher struct {
	seen map[*LVal]*LVal
}

func (d *detacher) detach(v *LVal) (*LVal, error) {
	if v == nil {
		return nil, nil
	}
	if cp, ok := d.seen[v]; ok {
		return cp, nil
	}
	switch v.Type {
	case LNative:
		return nil, &detachError{msg: fmt.Sprintf("native value (%T) cannot be detached", v.Native)}
	case LFun:
		return nil, funDetachError(v)
	case LInvalid, LMarkTerminal, LMarkTailRec, LMarkMacExpand, LTypeMax:
		// Not values an application can hold; refuse loudly instead of
		// guessing at a copy.
		return nil, &detachError{msg: fmt.Sprintf("internal %v value cannot be detached", v.Type)}
	case LInt, LFloat, LError, LSymbol, LQSymbol, LSExpr, LQuote, LString,
		LBytes, LSortMap, LArray, LTaggedVal:
		// Detachable; handled below.
	}
	cp := &LVal{}
	*cp = *v
	// A detached copy shares no storage with the original, so a sealed
	// original's copy is freely mutable (see lisp/seal.go).
	cp.sealed = false
	// Register the copy before descending so a value reachable twice maps to
	// one copy and a cycle in v becomes the same cycle in the copy instead of
	// infinite recursion.
	d.seen[v] = cp

	// Under the unexported-source API (issue #362) a value constructed by Go
	// code carries a nil location; copyLocation preserves nil, so a detached
	// native-constructed value stays nil-source rather than materializing a
	// synthetic location.
	cp.source = copyLocation(v.source)
	cp.meta = detachMeta(v.meta)
	// Debugger-only metadata; its context aliases unevaluated argument
	// values inside the source runtime, so a detached value carries none.
	cp.macroExpansion = nil

	// The struct copy above aliased v.Native.  Every payload a detachable
	// type is documented to carry is replaced with a hermetic copy; anything
	// unrecognized is rejected rather than smuggled through.
	switch native := v.Native.(type) {
	case nil:
	case *[]byte:
		if v.Type != LBytes {
			return nil, unexpectedNativeError(v)
		}
		if native != nil {
			b := make([]byte, len(*native))
			copy(b, *native)
			cp.Native = &b
		}
	case *MapData:
		if v.Type != LSortMap {
			return nil, unexpectedNativeError(v)
		}
		mdata, err := d.detachMapData(native)
		if err != nil {
			return nil, err
		}
		cp.Native = mdata
	case *CallStack:
		if v.Type != LError {
			return nil, unexpectedNativeError(v)
		}
		cp.Native = detachCallStack(native)
	default:
		return nil, unexpectedNativeError(v)
	}

	cells, err := d.detachCells(v.Cells)
	if err != nil {
		return nil, err
	}
	cp.Cells = cells
	return cp, nil
}

func (d *detacher) detachCells(cells []*LVal) ([]*LVal, error) {
	if len(cells) == 0 {
		return nil, nil
	}
	out := make([]*LVal, len(cells))
	for i := range cells {
		cp, err := d.detach(cells[i])
		if err != nil {
			return nil, prependPath(err, fmt.Sprintf("Cells[%d]", i))
		}
		out[i] = cp
	}
	return out, nil
}

// detachMapData rebuilds md as a fresh stock sortedmap whose keys and values
// are both detached.
func (d *detacher) detachMapData(md *MapData) (*MapData, error) {
	if md == nil {
		return nil, nil
	}
	if md.mapBacking == nil {
		// Degenerate MapData with no implementation (possible via
		// SortedMapFromData(NewMapData(nil))).  Return a fresh struct rather
		// than md itself so the detached value shares no memory with the
		// original — the detach contract — while preserving the nil Map.
		return &MapData{}, nil
	}
	entries := sortedMapEntries(md)
	if entries.Type == LError {
		return nil, &detachError{msg: fmt.Sprintf("sorted-map entries cannot be enumerated: %v", entries)}
	}
	m := &MapData{newmap()}
	for _, pair := range entries.Cells {
		key, err := d.detach(pair.Cells[0])
		if err != nil {
			return nil, prependPath(err, fmt.Sprintf("MapKey[%s]", pair.Cells[0]))
		}
		val, err := d.detach(pair.Cells[1])
		if err != nil {
			return nil, prependPath(err, fmt.Sprintf("Map[%s]", pair.Cells[0]))
		}
		if lerr := m.Set(key, val); lerr.Type == LError {
			return nil, &detachError{msg: fmt.Sprintf("sorted-map key %s cannot be stored: %v", pair.Cells[0], lerr)}
		}
	}
	return m, nil
}

// detachCallStack deep-copies an LError's recorded stack.  CallStack.Copy
// only freshens the Frames slice header; the frames' Source locations and the
// GoStack bytes stay shared, so both are copied here as well.
func detachCallStack(s *CallStack) *CallStack {
	if s == nil {
		return nil
	}
	cp := s.Copy()
	if s.GoStack != nil {
		cp.GoStack = append([]byte(nil), s.GoStack...)
	}
	for i := range cp.Frames {
		cp.Frames[i].Source = copyLocation(cp.Frames[i].Source)
	}
	return cp
}

// detachMeta deep-copies format-preserving metadata, including the comment
// tokens and their locations.
func detachMeta(m *fmtmeta.Meta) *fmtmeta.Meta {
	if m == nil {
		return nil
	}
	cp := *m
	cp.TrailingComment = copyToken(m.TrailingComment)
	cp.LeadingComments = copyTokens(m.LeadingComments)
	cp.InnerTrailingComments = copyTokens(m.InnerTrailingComments)
	return &cp
}

func copyTokens(toks []*token.Token) []*token.Token {
	if toks == nil {
		return nil
	}
	out := make([]*token.Token, len(toks))
	for i := range toks {
		out[i] = copyToken(toks[i])
	}
	return out
}

func copyToken(t *token.Token) *token.Token {
	if t == nil {
		return nil
	}
	cp := *t
	cp.Source = copyLocation(t.Source)
	return &cp
}

func copyLocation(loc *token.Location) *token.Location {
	if loc == nil {
		return nil
	}
	cp := *loc
	return &cp
}

func funDetachError(v *LVal) error {
	if fd, ok := v.Native.(*funData); ok && fd != nil && fd.builtin != nil {
		return &detachError{msg: "builtin function cannot be detached: builtins hold Go code and a reference to the defining environment"}
	}
	return &detachError{msg: "function cannot be detached: closures capture the defining environment and through it the source runtime"}
}

func unexpectedNativeError(v *LVal) error {
	return &detachError{msg: fmt.Sprintf("unexpected native payload (%T) on %v value cannot be detached", v.Native, v.Type)}
}

// detachError is the error returned by detach.  path holds the segments from
// the value passed to detach down to the offending cell; segments are
// prepended as the recursion unwinds, so the slice is stored innermost-first
// and rendered outermost-first.
type detachError struct {
	msg  string
	path []string
}

func (e *detachError) Error() string {
	if len(e.path) == 0 {
		return e.msg
	}
	var sb strings.Builder
	for i := len(e.path) - 1; i >= 0; i-- {
		sb.WriteString(e.path[i])
		sb.WriteByte('.')
	}
	s := sb.String()
	return s[:len(s)-1] + ": " + e.msg
}

// prependPath records one more path segment on a detachError as the recursion
// unwinds.  Segment construction only happens on the error path, so the happy
// path pays nothing for it.
func prependPath(err error, segment string) error {
	de, ok := err.(*detachError) //nolint:errorlint // detachError is never wrapped; created only in this file.
	if !ok {
		return err
	}
	de.path = append(de.path, segment)
	return de
}
