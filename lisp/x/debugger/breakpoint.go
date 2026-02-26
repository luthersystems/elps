// Copyright © 2018 The ELPS authors

package debugger

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
	"sync"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// hitOp represents a hit count comparison operator.
type hitOp int

const (
	hitOpNone hitOp = iota
	hitOpEQ         // == or bare number
	hitOpGT         // >
	hitOpGTE        // >=
	hitOpMod        // %
)

// BreakpointSpec describes a breakpoint to set (used by SetForFileSpecs).
type BreakpointSpec struct {
	Line         int
	Condition    string
	HitCondition string
	LogMessage   string
}

// Breakpoint represents a location where execution should pause.
type Breakpoint struct {
	ID           int
	File         string
	Line         int
	Condition    string // optional: Lisp expression to evaluate
	HitCondition string // optional: hit count expression (e.g., ">5", "==3", "%2")
	LogMessage   string // optional: log point message template with {expr} interpolation
	Enabled      bool

	hitCount     int    // number of times this breakpoint has been reached
	parsedHitOp  hitOp
	parsedHitVal int
}

// key returns the file:line key used for map indexing.
func (bp *Breakpoint) key() string {
	return breakpointKey(bp.File, bp.Line)
}

func breakpointKey(file string, line int) string {
	return fmt.Sprintf("%s:%d", normalizePath(file), line)
}

// normalizePath reduces a file path to its basename for breakpoint matching.
// IDE clients send absolute paths (e.g., /Users/.../phylum/organisation.lisp)
// while the ELPS runtime uses relative paths or basenames. Normalizing to
// basename allows breakpoints to match regardless of path format.
func normalizePath(file string) string {
	if file == "" {
		return file
	}
	return filepath.Base(file)
}

// parseHitCondition parses a DAP hit condition string into an operator and value.
// Supported formats: ">5", ">=3", "==10", "%2", or bare "5" (treated as ==5).
func parseHitCondition(s string) (hitOp, int) {
	s = strings.TrimSpace(s)
	if s == "" {
		return hitOpNone, 0
	}
	var op hitOp
	var numStr string
	switch {
	case strings.HasPrefix(s, ">="):
		op = hitOpGTE
		numStr = strings.TrimSpace(s[2:])
	case strings.HasPrefix(s, "=="):
		op = hitOpEQ
		numStr = strings.TrimSpace(s[2:])
	case strings.HasPrefix(s, ">"):
		op = hitOpGT
		numStr = strings.TrimSpace(s[1:])
	case strings.HasPrefix(s, "%"):
		op = hitOpMod
		numStr = strings.TrimSpace(s[1:])
	default:
		op = hitOpEQ
		numStr = s
	}
	val, err := strconv.Atoi(numStr)
	if err != nil {
		return hitOpNone, 0
	}
	return op, val
}

// checkHitCondition tests whether the breakpoint's current hit count
// satisfies the hit condition. Returns true if there is no hit condition
// or if the condition is met.
func (bp *Breakpoint) checkHitCondition() bool {
	if bp.parsedHitOp == hitOpNone {
		return true
	}
	switch bp.parsedHitOp {
	case hitOpEQ:
		return bp.hitCount == bp.parsedHitVal
	case hitOpGT:
		return bp.hitCount > bp.parsedHitVal
	case hitOpGTE:
		return bp.hitCount >= bp.parsedHitVal
	case hitOpMod:
		if bp.parsedHitVal <= 0 {
			return true
		}
		return bp.hitCount%bp.parsedHitVal == 0
	}
	return true
}

// IncrementHitCount increments the breakpoint's hit count and returns
// whether the hit condition is satisfied.
func (bp *Breakpoint) IncrementHitCount() bool {
	bp.hitCount++
	return bp.checkHitCondition()
}

// InterpolateLogMessage evaluates a log point message template, replacing
// {expr} placeholders with the result of evaluating each expression.
func InterpolateLogMessage(env *lisp.LEnv, template string) string {
	if template == "" {
		return ""
	}
	var buf strings.Builder
	for {
		open := strings.Index(template, "{")
		if open < 0 {
			buf.WriteString(template)
			break
		}
		buf.WriteString(template[:open])
		rest := template[open+1:]
		close := strings.Index(rest, "}")
		if close < 0 {
			// Unterminated brace — emit remainder literally.
			buf.WriteByte('{')
			buf.WriteString(rest)
			break
		}
		expr := rest[:close]
		result := EvalInContext(env, expr)
		buf.WriteString(FormatValue(result))
		template = rest[close+1:]
	}
	return buf.String()
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
		bp.HitCondition = ""
		bp.LogMessage = ""
		bp.hitCount = 0
		bp.parsedHitOp = hitOpNone
		bp.parsedHitVal = 0
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
	prefix := normalizePath(file) + ":"
	for key := range s.byKey {
		if strings.HasPrefix(key, prefix) {
			delete(s.byKey, key)
		}
	}
}

// SetForFile replaces all breakpoints in a file. This implements the DAP
// setBreakpoints semantics (full replacement, not incremental).
func (s *BreakpointStore) SetForFile(file string, lines []int, conditions []string) []*Breakpoint {
	specs := make([]BreakpointSpec, len(lines))
	for i, line := range lines {
		cond := ""
		if i < len(conditions) {
			cond = conditions[i]
		}
		specs[i] = BreakpointSpec{Line: line, Condition: cond}
	}
	return s.SetForFileSpecs(file, specs)
}

// SetForFileSpecs replaces all breakpoints in a file using full specs.
// This supports hit conditions and log messages in addition to conditions.
func (s *BreakpointStore) SetForFileSpecs(file string, specs []BreakpointSpec) []*Breakpoint {
	s.mu.Lock()
	defer s.mu.Unlock()
	// Remove old breakpoints for this file (using normalized path).
	prefix := normalizePath(file) + ":"
	for key := range s.byKey {
		if strings.HasPrefix(key, prefix) {
			delete(s.byKey, key)
		}
	}
	// Add new ones.
	result := make([]*Breakpoint, len(specs))
	for i, spec := range specs {
		s.nextID++
		hitOp, hitVal := parseHitCondition(spec.HitCondition)
		bp := &Breakpoint{
			ID:           s.nextID,
			File:         file,
			Line:         spec.Line,
			Condition:    spec.Condition,
			HitCondition: spec.HitCondition,
			LogMessage:   spec.LogMessage,
			Enabled:      true,
			parsedHitOp:  hitOp,
			parsedHitVal: hitVal,
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

// RemoveByID removes the breakpoint with the given ID. Returns true if it
// existed. This is used by the debug REPL's "delete N" command.
func (s *BreakpointStore) RemoveByID(id int) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	for key, bp := range s.byKey {
		if bp.ID == id {
			delete(s.byKey, key)
			return true
		}
	}
	return false
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
