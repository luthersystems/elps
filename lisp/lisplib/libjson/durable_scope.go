// Copyright © 2026 The ELPS authors

package libjson

// Which captured names a closure reads, decided without evaluating
// anything, the same way when saving and when loading.
//
// The names a closure reads are every unqualified, non-keyword symbol in
// its code, quoted or not, nested lambdas included: a lexical
// over-approximation of its free variables.  Each is read from the
// innermost captured frame that binds it.
//
// A closure is dynamic when its code may read a captured name the code does
// not spell: it calls eval (by any name that resolves to the eval builtin,
// an alias in a frame or a global included), or calls a macro other than
// the audited stdlib macros, whose expansion could name anything.  A
// dynamic closure keeps all of its frames whole.
//
// A name resolves as the evaluator resolves it: in the captured frames,
// innermost first, then as a global of the closure's package (or of the
// package a qualified name names).  The work is bounded: each frame looked
// at is one step, and a dump or load takes at most closureWorkFactor steps
// per value it may hold.

import (
	"fmt"
	"slices"
	"strings"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
)

// closureWorkFactor bounds the name-resolution steps of one dump or load:
// at most closureWorkFactor per value of the value limit.
const closureWorkFactor = 16

// auditedMacros are the stdlib macros a closure may call without being
// dynamic: Go macros whose expansions name only what the call site spells.
var auditedMacros = map[string][]string{
	"lisp":    {"benchmark-simple", "curry-function", "defconst", "defmacro", "deftype", "defun", "get-default", "test-let", "test-let*", "trace"},
	"testing": {"assert-equal", "assert-nil", "assert-not", "assert-not-nil", "assert-string=", "assert=", "benchmark-simple", "test-let", "test-let*"},
}

// closureScope resolves the names closures read in one dump or load.
type closureScope struct {
	reg     *lisp.PackageRegistry
	evalFn  any
	audited map[any]bool
	names   map[codeKey][]string
	work    int
	limit   int
}

func newClosureScope(env *lisp.LEnv, cfg typedConfig) *closureScope {
	reg := env.Runtime.Registry
	s := &closureScope{
		reg:     reg,
		audited: map[any]bool{},
		names:   map[codeKey][]string{},
		limit:   closureWorkFactor*cfg.maxValues + 1024,
	}
	if lang := reg.Package(reg.Lang); lang != nil {
		if f, ok := lang.Symbol("eval"); ok && f.Type == lisp.LFun {
			s.evalFn = f.Native
		}
	}
	for pkgName, names := range auditedMacros {
		pkg := reg.Package(pkgName)
		if pkg == nil {
			continue
		}
		for _, name := range names {
			if f, ok := pkg.Symbol(name); ok && f.Type == lisp.LFun && f.IsMacro() && funraw.Env(f) == nil {
				s.audited[f.Native] = true
			}
		}
	}
	return s
}

// step takes one step of name-resolution work.
func (s *closureScope) step() error {
	s.work++
	if s.work > s.limit {
		return fmt.Errorf("%w: closure name resolution exceeds %d steps", ErrTypedLimit, s.limit)
	}
	return nil
}

// codeNames returns the names a code reads, sorted, computed once per key.
func (s *closureScope) codeNames(key codeKey, cells []*lisp.LVal) []string {
	if names, ok := s.names[key]; ok {
		return names
	}
	seen := map[string]bool{}
	var names []string
	var walk func(v *lisp.LVal)
	walk = func(v *lisp.LVal) {
		switch v.Type {
		case lisp.LSymbol:
			if name := v.Str; name != "" && !strings.Contains(name, ":") && !seen[name] {
				seen[name] = true
				names = append(names, name)
			}
		case lisp.LSExpr, lisp.LQuote:
			for _, c := range v.Cells {
				walk(c)
			}
		case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LBytes, lisp.LArray, lisp.LSortMap, lisp.LTaggedVal,
			lisp.LNative, lisp.LFun, lisp.LError, lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand,
			lisp.LInvalid, lisp.LTypeMax:
		}
	}
	for _, c := range cells {
		walk(c)
	}
	slices.Sort(names)
	s.names[key] = names
	return names
}

// resolve returns what name means to a closure over chain in package pkg,
// or nil when it is unbound or a keyword.
func (s *closureScope) resolve(name string, chain []*lisp.LEnv, pkg string) (*lisp.LVal, error) {
	if name == "" || name[0] == ':' {
		return nil, nil
	}
	if ns, local, ok := strings.Cut(name, ":"); ok {
		if err := s.step(); err != nil {
			return nil, err
		}
		return s.global(ns, local), nil
	}
	for _, env := range chain {
		if err := s.step(); err != nil {
			return nil, err
		}
		if v, ok := funraw.Lookup(env, name); ok {
			return v, nil
		}
	}
	if err := s.step(); err != nil {
		return nil, err
	}
	return s.global(pkg, name), nil
}

// global returns the global name in package pkg, or nil.
func (s *closureScope) global(pkg, name string) *lisp.LVal {
	p := s.reg.Package(pkg)
	if p == nil {
		return nil
	}
	v := p.Get(lisp.Symbol(name))
	if v == nil || v.Type == lisp.LError {
		return nil
	}
	return v
}

// dynamicReason returns why a closure's code may read captured names it
// does not spell ("calls eval via run", "calls macro m"), or "" when it
// cannot.  The first reason in code order is reported.
func (s *closureScope) dynamicReason(cells []*lisp.LVal, chain []*lisp.LEnv, pkg string) (string, error) {
	memo := map[string]*lisp.LVal{}
	lookup := func(name string) (*lisp.LVal, error) {
		if v, ok := memo[name]; ok {
			return v, nil
		}
		v, err := s.resolve(name, chain, pkg)
		memo[name] = v
		return v, err
	}
	var walk func(v *lisp.LVal) (string, error)
	walk = func(v *lisp.LVal) (string, error) {
		switch v.Type {
		case lisp.LSymbol:
			f, err := lookup(v.Str)
			if err != nil {
				return "", err
			}
			if f != nil && f.Type == lisp.LFun && s.evalFn != nil && f.Native == s.evalFn {
				if v.Str == "eval" || strings.HasSuffix(v.Str, ":eval") {
					return "calls eval", nil
				}
				return "calls eval via " + v.Str, nil
			}
		case lisp.LSExpr:
			if !v.IsQuoted() && len(v.Cells) > 0 && v.Cells[0].Type == lisp.LSymbol {
				f, err := lookup(v.Cells[0].Str)
				if err != nil {
					return "", err
				}
				if f != nil && f.Type == lisp.LFun && f.IsMacro() && !s.audited[f.Native] {
					return "calls macro " + v.Cells[0].Str, nil
				}
			}
			for _, c := range v.Cells {
				if r, err := walk(c); r != "" || err != nil {
					return r, err
				}
			}
		case lisp.LQuote:
			for _, c := range v.Cells {
				if r, err := walk(c); r != "" || err != nil {
					return r, err
				}
			}
		case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LBytes, lisp.LArray, lisp.LSortMap, lisp.LTaggedVal,
			lisp.LNative, lisp.LFun, lisp.LError, lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand,
			lisp.LInvalid, lisp.LTypeMax:
		}
		return "", nil
	}
	for _, c := range cells {
		if r, err := walk(c); r != "" || err != nil {
			return r, err
		}
	}
	return "", nil
}
