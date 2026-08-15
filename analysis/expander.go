// Copyright © 2026 The ELPS authors

package analysis

import (
	"fmt"
	"runtime"
	"sync"
	"sync/atomic"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// MacroExpander expands a macro call form at analysis time.
// The form includes the macro name as Cells[0] and arguments as Cells[1:].
// pkg is the current package context from the analyzer — the expander
// should resolve the macro name relative to this package, matching the
// runtime's package-scoped symbol resolution.
// Returns the expanded AST, or nil if expansion is not possible (macro not
// found, expansion error, wrong arity, etc.). Returning nil causes the
// analyzer to fall back to treating the call as an opaque macro invocation.
type MacroExpander interface {
	ExpandMacro(form *lisp.LVal, pkg string) *lisp.LVal
}

// PanicReporter is implemented by a MacroExpander that recovers Go panics
// internally and can report having done so.
//
// The problem it solves is that MacroExpander's contract makes a swallowed
// panic INDISTINGUISHABLE from ordinary failure: an implementation that
// recovers can only report that recovery by returning nil, and nil is also the
// answer for "not a macro", "wrong arity" and "expansion errored" — the
// overwhelmingly common cases. A caller that wants to know whether analysis-time
// macro expansion crashed cannot learn it from the return value, ever.
//
// So the signal is carried out of band. Callers that need it (tests, fuzz
// targets, an embedder that wants to log host-code bugs) should type-assert:
//
//	if pr, ok := cfg.MacroExpander.(analysis.PanicReporter); ok {
//	        if n := pr.ExpansionPanics(); n > 0 { ... }
//	}
//
// ExpansionPanics must be monotonic and must never be resettable through this
// interface: a count that can go back down is a count that can be cleared by
// the very code whose bugs it is recording.
type PanicReporter interface {
	// ExpansionPanics returns the number of ExpandMacro calls that did not
	// complete normally over the lifetime of the expander.
	ExpansionPanics() uint64
}

// ExpansionPanic describes one ExpandMacro call that did not complete.
type ExpansionPanic struct {
	// Value is what recover() returned at the aborted call. It is nil in the
	// one case that is not a panic at all: runtime.Goexit unwinding through
	// ExpandMacro (a macro body that reached t.Fatal, say). The abort is
	// counted either way — see EnvMacroExpander.ExpansionPanics.
	Value any

	// GoStack is the Go stack captured inside the recover handler. That
	// handler runs before the panic unwind completes, so the stack points at
	// the panic site rather than at the handler — the same property
	// lisp.(*LEnv).eval relies on when it fills CallStack.GoStack.
	GoStack []byte

	// Macro is the head symbol of the form being expanded and Package the
	// package context it was expanded in, when they could be read. Both are
	// best-effort diagnostics: the form itself may be what was malformed.
	Macro   string
	Package string
}

// EnvMacroExpander uses a live LEnv to expand user-defined macros.
// Expansion errors and panics are caught — the analyzer falls back to
// opaque analysis when ExpandMacro returns nil.
//
// Thread-safe: a mutex serializes expansion calls since MacroCall mutates
// shared Runtime state (call stack, package pointer). The mutex is only
// contended when multiple LSP handlers trigger concurrent analyses.
//
// A recovered panic is COUNTED as well as swallowed; see ExpansionPanics for
// why the count exists and why nothing outside this file can fake it.
type EnvMacroExpander struct {
	Env *lisp.LEnv

	mu       sync.Mutex
	notMacro map[string]bool // cache: symbols that are not macros in the env
	// NOTE: notMacro assumes the env's macro set is stable for this
	// expander's lifetime. If macros are added to the env after creation
	// (e.g. LoadWorkspaceMacros), create a new EnvMacroExpander or call
	// Reset() to clear the cache.

	// aborted counts ExpandMacro calls that left the function without
	// reaching the statement after expand() returned. Monotonic, and there
	// is deliberately no way to decrement or clear it.
	aborted atomic.Uint64

	lastMu    sync.Mutex
	lastPanic *ExpansionPanic
}

// Reset clears the not-a-macro cache. Call this after loading new macros
// into the env (e.g. via LoadWorkspaceMacros) if the expander is reused.
//
// Reset does NOT clear the abort count or the last recorded panic. Evidence
// that host code crashed during analysis outlives a cache invalidation, and a
// reset that wiped it would give the caller a way to lose the only record of a
// swallowed panic.
func (e *EnvMacroExpander) Reset() {
	e.mu.Lock()
	e.notMacro = nil
	e.mu.Unlock()
}

// ExpansionPanics returns how many ExpandMacro calls on this expander did not
// complete normally — a Go panic that the blanket recover swallowed, or a
// runtime.Goexit that unwound through it.
//
// # Why this is here at all
//
// ExpandMacro's recover sets result = nil, and nil is ALSO the ordinary answer
// for "that head symbol is not a macro" — by far the most common outcome, since
// the analyzer consults the expander for every unknown head symbol in the file.
// A panic in analysis-time macro expansion was therefore not merely unreported,
// it was unreportABLE: no caller, and no test or fuzz target, could tell the
// crash apart from the routine case. Merely opening a document runs this code
// over attacker-chosen source (lsp/), and luthersystems/substrate reaches it
// through lint/ as well, so the silent class is a real one.
//
// # Why the marker cannot be forged
//
// This is the same trick lisp.IsInternalPanic uses, and the same one the LSP
// fuzz harness uses on buildWorkspaceIndex: key off something only the
// non-panicking path can produce, rather than off a value the code under test
// chose.
//
// The marker is `completed`, a bool local to ExpandMacro's own stack frame. It
// is set by exactly one statement, placed immediately after the call to
// expand() returns, so it runs if and only if control came back from expand
// normally. A panic anywhere inside expand — including inside a macro BODY that
// env.MacroCall evaluates — skips that statement, and the deferred handler
// observes completed == false and counts the abort.
//
// Nothing reachable from lisp can touch a Go local. A macro cannot expand to a
// value that sets it, an expansion result cannot be shaped to imitate it, and
// an embedder cannot set it either: it does not exist outside the frame. The
// counter it feeds is unexported, is only ever incremented, and the package
// exports no way to lower it — deliberately, so that the code whose bugs it
// records cannot clear its own record. That is the difference between this and
// a `Panicked bool` field, which any holder of the expander could set to
// either value at any time.
//
// The count is monotonic over the expander's lifetime, so callers that want a
// per-operation answer should take a snapshot before and compare after.
func (e *EnvMacroExpander) ExpansionPanics() uint64 {
	return e.aborted.Load()
}

// LastExpansionPanic returns the most recently recorded abort, or nil if there
// has been none. The returned value is a copy; the GoStack slice is shared and
// must not be mutated.
func (e *EnvMacroExpander) LastExpansionPanic() *ExpansionPanic {
	e.lastMu.Lock()
	defer e.lastMu.Unlock()
	if e.lastPanic == nil {
		return nil
	}
	cp := *e.lastPanic
	return &cp
}

// recordAbort is called only from ExpandMacro's deferred handler, only when
// that call failed to complete.
func (e *EnvMacroExpander) recordAbort(r any, form *lisp.LVal, pkg string) {
	e.aborted.Add(1)

	// Captured here, in the deferred handler, so runtime.Stack still reflects
	// the panic site: the unwind is not finished until this function returns.
	buf := make([]byte, 16*1024)
	n := runtime.Stack(buf, false)

	rec := &ExpansionPanic{
		Value:   r,
		GoStack: buf[:n],
		Macro:   safeHeadSymbol(form),
		Package: pkg,
	}
	e.lastMu.Lock()
	e.lastPanic = rec
	e.lastMu.Unlock()
}

// safeHeadSymbol reads the head symbol of form without assuming form is
// well-formed. It runs while recovering from a panic that a malformed form may
// itself have caused, so a second panic here would replace the original with a
// far less useful one.
func safeHeadSymbol(form *lisp.LVal) (name string) {
	defer func() {
		if recover() != nil {
			name = "<unreadable>"
		}
	}()
	if form == nil || len(form.Cells) == 0 || form.Cells[0] == nil ||
		form.Cells[0].Type != lisp.LSymbol {
		return "<unknown>"
	}
	return form.Cells[0].Str
}

// ExpandMacro looks up the head symbol in the environment relative to
// the given package, verifies it is a macro, and calls MacroCall to
// expand it. The env is temporarily switched to pkg so that unqualified
// symbol resolution matches the file's package context — the same way
// the runtime resolves symbols during eval.
//
// A recovered panic still yields nil — the analyzer's fallback to opaque macro
// handling is the right behaviour and does not change — but it is now also
// COUNTED, so the crash is observable even though the return value cannot carry
// it. See ExpansionPanics.
func (e *EnvMacroExpander) ExpandMacro(form *lisp.LVal, pkg string) (result *lisp.LVal) {
	// completed is the non-forgeable marker. It lives in this frame and is set
	// by exactly one statement, below, that only a normal return from expand
	// can reach. Read ExpansionPanics before changing anything here: moving
	// this assignment, or adding a return between expand() and it, breaks the
	// only detector this package has for a swallowed panic.
	completed := false
	defer func() {
		r := recover()
		if completed {
			return
		}
		// Control is leaving ExpandMacro without expand having returned. Either
		// r != nil (a Go panic, which this recover has just swallowed) or
		// r == nil (runtime.Goexit unwinding through here). Both are aborts and
		// both are recorded; only the first is a panic.
		e.recordAbort(r, form, pkg)
		result = nil
	}()

	result = e.expand(form, pkg)
	completed = true
	return result
}

// expand is ExpandMacro's body. It is split out so that ExpandMacro's deferred
// handler can distinguish "expand returned" from "control escaped expand",
// which a single function with several return statements cannot do without a
// flag at every one of them.
func (e *EnvMacroExpander) expand(form *lisp.LVal, pkg string) *lisp.LVal {
	if e.Env == nil || len(form.Cells) == 0 || form.Cells[0].Type != lisp.LSymbol {
		return nil
	}

	name := form.Cells[0].Str

	e.mu.Lock()
	defer e.mu.Unlock()

	// Switch to the file's package for correct symbol resolution.
	// Save and restore — MacroCall may also switch packages internally.
	prevPkg := e.Env.Runtime.Package
	if pkg != "" {
		e.Env.InPackage(lisp.String(pkg))
	}
	defer func() { e.Env.Runtime.Package = prevPkg }()

	// Fast path: already know this symbol is not a macro in this package.
	cacheKey := pkg + "\x00" + name
	if e.notMacro != nil && e.notMacro[cacheKey] {
		return nil
	}

	mac := e.Env.Get(form.Cells[0])
	if mac.Type != lisp.LFun || !mac.IsMacro() {
		// Cache the negative result to avoid future lookups.
		if e.notMacro == nil {
			e.notMacro = make(map[string]bool)
		}
		e.notMacro[cacheKey] = true
		return nil
	}

	// Copy the argument cells into storage this call owns, rather than slicing
	// form's backing array (elps#396).
	//
	// lisp.SExpr does not copy; it wraps the slice it is handed. Macro
	// arguments are not evaluated, so a header over form.Cells[1:] reaches the
	// macro's parameters unchanged — LEnv.bindFormalNext binds a variadic
	// parameter to QExpr(args.Rest()), and argParser.Rest returns p.args[p.i:],
	// yet another window onto the same array. Any destructive builtin in the
	// macro body (stable-sort, sort, append!) then writes through into `form`,
	// which is the analyzer's parse tree and not a scratch copy.
	//
	// This is the runtime's own discipline, which the analyzer was missing.
	// LEnv.evalSExprCells is the only other path that reaches a macro, and on
	// the IsSpecialFun branch it builds the argument list as
	// `newCells := make([]*LVal, 1, len(s.Cells))` followed by
	// `append(newCells, cells...)` — a FRESH array holding the caller's
	// element pointers. The two lines below reproduce that exactly, so
	// expanding a form here now perturbs it neither more nor less than
	// evaluating it does.
	//
	// The copy is shallow for the same reason the runtime's is. The ELEMENTS
	// must stay the caller's nodes: the expansion splices them into its result
	// and the analyzer reads position information off exactly those nodes, so
	// deep-copying would both diverge from eval and change what the language
	// means. What must not be shared is the ARRAY — the only thing an in-place
	// mutator applied to the &rest list itself can reach.
	margs := make([]*lisp.LVal, len(form.Cells)-1)
	copy(margs, form.Cells[1:])
	args := lisp.SExpr(margs)
	mark := e.Env.MacroCall(mac, args)
	if mark.Type == lisp.LError || mark.Type != lisp.LMarkMacExpand {
		return nil
	}

	return mark.Cells[0]
}

// LoadWorkspaceMacros replays workspace preamble forms (in-package,
// use-package, export, defmacro, defun, set) into the given environment.
// This mirrors what the runtime's (load) does — forms are eval'd in source
// order so package context, imports, and definitions build up naturally.
//
// Workspace packages that don't exist in the boot env are auto-created.
// The env's active package is saved and restored after loading.
//
// Malformed forms are skipped — the returned errors slice contains one
// entry per failure. Callers should log these for visibility.
func LoadWorkspaceMacros(env *lisp.LEnv, preamble []*lisp.LVal) []error {
	// Save and restore the env's active package, same as env.load().
	currPkg := env.Runtime.Package
	defer func() { env.Runtime.Package = currPkg }()

	var errs []error
	for _, form := range preamble {
		if err := evalPreambleForm(env, form); err != nil {
			errs = append(errs, err)
		}
	}
	return errs
}

func evalPreambleForm(env *lisp.LEnv, form *lisp.LVal) (retErr error) {
	defer func() {
		if r := recover(); r != nil {
			retErr = fmt.Errorf("panic in preamble form: %v", r)
		}
	}()
	// Auto-create workspace packages so in-package doesn't fail.
	// Import the lang package so builtins (defmacro, use-package, etc.) work.
	head := astutil.HeadSymbol(form)
	if head == "in-package" && len(form.Cells) > 1 {
		if name := preamblePkgName(form.Cells[1]); name != "" {
			env.Runtime.Registry.DefinePackage(name)
			// Temporarily switch to the new package to import lang builtins,
			// then let the actual (in-package ...) eval do the real switch.
			env.InPackage(lisp.String(name))
			env.UsePackage(lisp.Symbol(env.Runtime.Registry.Lang))
		}
	}
	result := env.Eval(form)
	if result.Type == lisp.LError {
		if head == "defmacro" {
			return fmt.Errorf("error loading macro %s: %v", preambleDefName(form), result)
		}
		return fmt.Errorf("error in preamble (%s): %v", head, result)
	}
	return nil
}

// preamblePkgName extracts a package name from a quoted or bare symbol.
func preamblePkgName(v *lisp.LVal) string {
	if v.Type == lisp.LSymbol {
		return v.Str
	}
	// (quote sym) or 'sym
	if v.Type == lisp.LSExpr && v.Quoted && len(v.Cells) > 0 && v.Cells[0].Type == lisp.LSymbol {
		return v.Cells[0].Str
	}
	return ""
}

// preambleDefName extracts the name from a (defmacro name ...) AST node.
func preambleDefName(def *lisp.LVal) string {
	if def != nil && len(def.Cells) > 1 && def.Cells[1].Type == lisp.LSymbol {
		return def.Cells[1].Str
	}
	return "<unknown>"
}
