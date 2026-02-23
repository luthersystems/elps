// Copyright © 2018 The ELPS authors

package debugger

import (
	"fmt"
	"strings"
	"sync"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// Breakpoint represents a location where execution should pause.
type Breakpoint struct {
	ID        int
	File      string
	Line      int
	Condition string // optional: Lisp expression to evaluate
	Enabled   bool
}

// key returns the file:line key used for map indexing.
func (bp *Breakpoint) key() string {
	return breakpointKey(bp.File, bp.Line)
}

func breakpointKey(file string, line int) string {
	return fmt.Sprintf("%s:%d", file, line)
}

// ExceptionBreakMode controls when the debugger pauses on exceptions.
type ExceptionBreakMode int

const (
	// ExceptionBreakNever disables exception breakpoints.
	ExceptionBreakNever ExceptionBreakMode = iota
	// ExceptionBreakAll pauses on all exceptions.
	ExceptionBreakAll
	// ExceptionBreakUncaught pauses only on uncaught exceptions (not yet implemented).
	ExceptionBreakUncaught
)

// BreakpointStore manages breakpoints indexed by file:line for O(1) lookup.
// All methods are safe for concurrent use from the DAP server goroutine
// while the eval goroutine reads via Match.
type BreakpointStore struct {
	mu             sync.RWMutex
	byKey          map[string]*Breakpoint
	nextID         int
	exceptionBreak ExceptionBreakMode
}

// NewBreakpointStore returns an empty breakpoint store.
func NewBreakpointStore() *BreakpointStore {
	return &BreakpointStore{
		byKey: make(map[string]*Breakpoint),
	}
}

// Set adds or replaces a breakpoint at the given file:line. Returns the
// breakpoint with its assigned ID.
func (s *BreakpointStore) Set(file string, line int, condition string) *Breakpoint {
	s.mu.Lock()
	defer s.mu.Unlock()
	key := breakpointKey(file, line)
	bp, exists := s.byKey[key]
	if exists {
		bp.Condition = condition
		bp.Enabled = true
		return bp
	}
	s.nextID++
	bp = &Breakpoint{
		ID:        s.nextID,
		File:      file,
		Line:      line,
		Condition: condition,
		Enabled:   true,
	}
	s.byKey[key] = bp
	return bp
}

// Remove removes the breakpoint at file:line. Returns true if it existed.
func (s *BreakpointStore) Remove(file string, line int) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	key := breakpointKey(file, line)
	_, ok := s.byKey[key]
	if ok {
		delete(s.byKey, key)
	}
	return ok
}

// ClearFile removes all breakpoints for the given file.
func (s *BreakpointStore) ClearFile(file string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	prefix := file + ":"
	for key := range s.byKey {
		if strings.HasPrefix(key, prefix) {
			delete(s.byKey, key)
		}
	}
}

// SetForFile replaces all breakpoints in a file. This implements the DAP
// setBreakpoints semantics (full replacement, not incremental).
func (s *BreakpointStore) SetForFile(file string, lines []int, conditions []string) []*Breakpoint {
	s.mu.Lock()
	defer s.mu.Unlock()
	// Remove old breakpoints for this file.
	prefix := file + ":"
	for key := range s.byKey {
		if strings.HasPrefix(key, prefix) {
			delete(s.byKey, key)
		}
	}
	// Add new ones.
	result := make([]*Breakpoint, len(lines))
	for i, line := range lines {
		s.nextID++
		cond := ""
		if i < len(conditions) {
			cond = conditions[i]
		}
		bp := &Breakpoint{
			ID:        s.nextID,
			File:      file,
			Line:      line,
			Condition: cond,
			Enabled:   true,
		}
		s.byKey[bp.key()] = bp
		result[i] = bp
	}
	return result
}

// Match returns the breakpoint at the given source location, or nil.
func (s *BreakpointStore) Match(src *token.Location) *Breakpoint {
	if src == nil {
		return nil
	}
	s.mu.RLock()
	defer s.mu.RUnlock()
	key := breakpointKey(src.File, src.Line)
	bp, ok := s.byKey[key]
	if !ok || !bp.Enabled {
		return nil
	}
	return bp
}

// SetExceptionBreak sets the exception breakpoint mode.
func (s *BreakpointStore) SetExceptionBreak(mode ExceptionBreakMode) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.exceptionBreak = mode
}

// ExceptionBreak returns the current exception breakpoint mode.
func (s *BreakpointStore) ExceptionBreak() ExceptionBreakMode {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.exceptionBreak
}

// EvalCondition evaluates a breakpoint's condition expression in the
// given environment. Returns true if the condition is satisfied (or if
// there is no condition). The evaluatingCondition flag on the engine
// must be set before calling this to prevent re-entrancy.
func EvalCondition(env *lisp.LEnv, condition string) bool {
	if condition == "" {
		return true
	}
	if env.Runtime.Reader == nil {
		return true
	}
	exprs, err := env.Runtime.Reader.Read("bp-condition", strings.NewReader(condition))
	if err != nil || len(exprs) == 0 {
		return true // condition failed to parse — treat as unconditional
	}
	result := env.Eval(exprs[0])
	// ELPS falsiness: nil, errors, and the false symbol are all falsey.
	if result.IsNil() || result.Type == lisp.LError {
		return false
	}
	if result.Type == lisp.LSymbol && result.Str == lisp.FalseSymbol {
		return false
	}
	return true
}

// All returns all breakpoints in the store.
func (s *BreakpointStore) All() []*Breakpoint {
	s.mu.RLock()
	defer s.mu.RUnlock()
	result := make([]*Breakpoint, 0, len(s.byKey))
	for _, bp := range s.byKey {
		result = append(result, bp)
	}
	return result
}
