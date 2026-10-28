// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"

	"github.com/luthersystems/elps/internal/templatepolicy"
)

// match (luthersystems/elps#904, #905): a special operator that tests a
// value against patterns in order and runs the body of the first clause
// that matches, with the names the pattern binds.
//
//	(match expr
//	  (pattern [:when guard] body...)
//	  ...
//	  [(:else body...)])
//
// Patterns:
//
//	_                    anything; binds nothing
//	name                 anything; binds name
//	1, 1.5, "s", :kw     a value equal? to it
//	true, false, ()
//	'sym, (quote datum)  a value equal? to the symbol or datum
//	(list p... [&rest r]) a list of that length (at least, with &rest)
//	(sorted-map k p ...) a sorted map that has each key k
//	[name T]             a value of type T; binds name (_ binds nothing)
//	(T :field p ...)     a value of type T whose fields match
//
// A quoted list in a pattern is always [name T]: the reader gives [x T]
// and '(x T) the same value.  Match a literal list with (list 'x 'y).
//
// Types come from a type library (package s), which registers itself with
// SetMatchTypes.  T is evaluated each time its pattern is tried, as
// (s:is? v T) would evaluate it.
//
// Each pattern node tried costs one step.  A guard, a type expression and
// the body cost what their evaluation costs.  A guard runs after the
// pattern's names are bound; when it is false the next clause is tried,
// and any change the guard made stays.

// MatchTypeFunc reports whether v is of type typ: Bool(true or false), or
// an error, which stops the match.
type MatchTypeFunc func(env *LEnv, v, typ *LVal) *LVal

// MatchFieldFunc reads the field name of v, a value that passed a type
// test.  ok is false when the pattern must not match, for a field the
// library treats as absent; package s returns ok and () for a field that
// is not set, as (v 'name) reads it.  err is an error when v has no field
// name that a pattern may read; it stops the match.
type MatchFieldFunc func(env *LEnv, v *LVal, name string) (field *LVal, ok bool, err *LVal)

// matchTypesSetting is the value setting (Runtime.SetSettingValue) that
// holds the type library's functions, so each VM of a template has them.
const matchTypesSetting = "lisp:match-types"

// matchTypes is the value of matchTypesSetting.
type matchTypes struct {
	templatepolicy.Marker
	is    MatchTypeFunc  //elpsvet:allow-marker a type library's package-level function, which keeps no state
	field MatchFieldFunc //elpsvet:allow-marker a type library's package-level function, which keeps no state
}

// SetMatchTypes registers the functions match uses for type and record
// patterns in rt.  A template publishes them to each VM it makes.  Both
// functions must keep no state of their own: every VM shares them.
func SetMatchTypes(rt *Runtime, is MatchTypeFunc, field MatchFieldFunc) error {
	return rt.SetSettingValue(matchTypesSetting, matchTypes{is: is, field: field})
}

// matchTypesOf returns the functions SetMatchTypes registered in env's
// runtime.
func matchTypesOf(env *LEnv) (matchTypes, bool) {
	v, ok := env.Runtime.SettingValue(matchTypesSetting)
	if !ok {
		return matchTypes{}, false
	}
	m, ok := v.(matchTypes)
	return m, ok
}

// matchBind is one name a pattern binds.
type matchBind struct{ name, val *LVal }

// opMatch is the special operator match.
func opMatch(env *LEnv, args *LVal) *LVal {
	v := env.Eval(args.Cells[0])
	if v.IsError() {
		return v
	}
	clauses := args.Cells[1:]
	var binds []matchBind
	for i, c := range clauses {
		if c.Type != LSExpr || len(c.Cells) == 0 {
			return env.Errorf("match: a clause is (pattern body...): %v", c)
		}
		pat, body := c.Cells[0], c.Cells[1:]
		if isMatchElse(pat) {
			if i != len(clauses)-1 {
				return env.Errorf("match: :else must be the last clause")
			}
			return opProgn(env, SExpr(body))
		}
		var guard *LVal
		if len(body) > 0 && body[0].IsSymbol(":when") {
			if len(body) < 2 {
				return env.Errorf("match: :when needs a guard: %v", c)
			}
			guard, body = body[1], body[2:]
		}
		if lerr := checkMatchPattern(env, pat, nil); lerr != nil {
			return lerr
		}
		m := matcher{env: env, binds: binds[:0]}
		ok, lerr := m.match(pat, v)
		binds = m.binds
		if lerr != nil {
			return lerr
		}
		if !ok {
			continue
		}
		benv := env
		if len(binds) > 0 {
			benv = newEnvN(env, len(binds))
			for _, b := range binds {
				if lerr := benv.Put(b.name, b.val); lerr.IsError() {
					return lerr
				}
			}
		}
		if guard != nil {
			g := benv.Eval(guard)
			if g.IsError() {
				return g
			}
			if Not(g) {
				continue
			}
		}
		return opProgn(benv, SExpr(body))
	}
	return Nil()
}

// isMatchElse reports whether pat is the :else of a final clause.
func isMatchElse(pat *LVal) bool {
	return pat.IsSymbol(":else")
}

// matchPatternKind is the shape of one pattern.
type matchPatternKind uint8

const (
	matchBad     matchPatternKind = iota
	matchAny                      // _
	matchName                     // a name to bind
	matchLiteral                  // a self-evaluating value or 'sym
	matchQuote                    // (quote datum)
	matchTyped                    // [name T]
	matchList                     // (list p...)
	matchMap                      // (sorted-map k p ...)
	matchRecord                   // (T :field p ...)
)

// classifyPattern returns the shape of p.  It reads only p's own cells.
func classifyPattern(p *LVal) matchPatternKind {
	switch p.Type {
	case LInt, LFloat, LString, LBytes:
		return matchLiteral
	case LSymbol:
		switch {
		case p.quoted:
			return matchLiteral
		case p.Str == "_":
			return matchAny
		case p.Str == TrueSymbol || p.Str == FalseSymbol || isKeyword(p.Str):
			return matchLiteral
		case p.Str == "" || p.Str[0] == '&' || strings.Contains(p.Str, ":"):
			// A qualified name is not a local name: Put would bind it,
			// but a reference reads the package.
			return matchBad
		}
		return matchName
	case LSExpr:
		if len(p.Cells) == 0 {
			return matchLiteral // () or '()
		}
		if p.quoted {
			if len(p.Cells) == 2 {
				if k := classifyPattern(p.Cells[0]); k == matchName || k == matchAny {
					return matchTyped
				}
			}
			return matchBad
		}
		head := p.Cells[0]
		if head.Type != LSymbol || head.quoted {
			return matchBad
		}
		switch head.Str {
		case "quote", DefaultLangPackage + ":quote":
			return matchQuote
		case "list", DefaultLangPackage + ":list":
			return matchList
		case "sorted-map", DefaultLangPackage + ":sorted-map":
			return matchMap
		}
		return matchRecord
	case LInvalid, LError, LQuote, LFun, LSortMap, LArray, LNative, LTaggedVal,
		LMarkTerminal, LMarkTailRec, LMarkMacExpand, LTypeMax:
	}
	return matchBad
}

// matchPatternCode calls fn with each type expression in the pattern p:
// the code a match evaluates while it tries p.
func matchPatternCode(p *LVal, fn func(code *LVal)) {
	switch classifyPattern(p) {
	case matchTyped:
		fn(p.Cells[1])
	case matchList:
		for _, q := range p.Cells[1:] {
			matchPatternCode(q, fn)
		}
	case matchMap, matchRecord:
		if classifyPattern(p) == matchRecord {
			fn(p.Cells[0])
		}
		for i := 2; i < len(p.Cells); i += 2 {
			matchPatternCode(p.Cells[i], fn)
		}
	case matchBad, matchAny, matchName, matchLiteral, matchQuote:
	}
}

// matchListParts splits the parts of a list pattern into the element
// patterns and the &rest pattern, or nil.
func matchListParts(parts []*LVal) ([]*LVal, *LVal) {
	n := len(parts)
	if n >= 2 && parts[n-2].IsSymbol(VarArgSymbol) {
		return parts[:n-2], parts[n-1]
	}
	return parts, nil
}

// checkMatchPattern checks the shape of p and that it binds no name twice.
// seen holds the names bound so far.  It returns nil when p is good.
func checkMatchPattern(env *LEnv, p *LVal, seen map[string]bool) *LVal {
	switch classifyPattern(p) {
	case matchAny, matchLiteral:
		return nil // the common leaf patterns need no walk
	case matchBad, matchName, matchQuote, matchTyped, matchList, matchMap, matchRecord:
	}
	type item struct{ p *LVal }
	stack := []item{{p}}
	for len(stack) > 0 {
		p := stack[len(stack)-1].p
		stack = stack[:len(stack)-1]
		bind := func(name *LVal) *LVal {
			if name.Str == "_" {
				return nil
			}
			if seen == nil {
				seen = make(map[string]bool)
			}
			if seen[name.Str] {
				return env.Errorf("match: pattern binds %s twice", name.Str)
			}
			seen[name.Str] = true
			return nil
		}
		switch classifyPattern(p) {
		case matchBad:
			return env.Errorf("match: bad pattern: %v; a quoted list is [name type], and (list 'a 'b) matches a literal list", p)
		case matchAny, matchLiteral:
		case matchName:
			if lerr := bind(p); lerr != nil {
				return lerr
			}
		case matchQuote:
			if len(p.Cells) != 2 {
				return env.Errorf("match: (quote datum) takes one datum: %v", p)
			}
		case matchTyped:
			if lerr := bind(p.Cells[0]); lerr != nil {
				return lerr
			}
		case matchList:
			elems, rest := matchListParts(p.Cells[1:])
			for _, q := range elems {
				if q.IsSymbol(VarArgSymbol) {
					return env.Errorf("match: &rest must come before the last pattern of a list: %v", p)
				}
				stack = append(stack, item{q})
			}
			if rest != nil {
				stack = append(stack, item{rest})
			}
		case matchMap, matchRecord:
			parts := p.Cells[1:]
			if len(parts)%2 != 0 {
				return env.Errorf("match: %v needs key and pattern pairs", p)
			}
			for i := 0; i < len(parts); i += 2 {
				k := parts[i]
				if classifyPattern(p) == matchRecord {
					if k.Type != LSymbol || k.quoted || !isKeyword(k.Str) || len(k.Str) < 2 {
						return env.Errorf("match: a record pattern is (type :field pattern ...): %v", p)
					}
				} else if k.Type != LString && k.Type != LInt && !(k.Type == LSymbol && isKeyword(k.Str)) {
					return env.Errorf("match: a map key in a pattern is a string, an int or a keyword: %v", k)
				}
				stack = append(stack, item{parts[i+1]})
			}
		}
	}
	return nil
}

// matcher matches one clause's pattern and collects its bindings.
type matcher struct {
	env   *LEnv
	binds []matchBind
}

// match reports whether v matches p, whose shape checkMatchPattern has
// checked.
func (m *matcher) match(p, v *LVal) (bool, *LVal) {
	env := m.env
	if lerr := env.step(); lerr.IsError() {
		return false, lerr
	}
	switch classifyPattern(p) {
	case matchAny:
		return true, nil
	case matchName:
		m.binds = append(m.binds, matchBind{p, v})
		return true, nil
	case matchLiteral:
		lit := p
		if p.quoted && p.Type == LSymbol {
			lit = Symbol(p.Str)
		}
		return m.equal(lit, v)
	case matchQuote:
		return m.equal(p.Cells[1], v)
	case matchTyped:
		ok, lerr := m.isType(v, p.Cells[1])
		if !ok || lerr != nil {
			return false, lerr
		}
		if p.Cells[0].Str != "_" {
			m.binds = append(m.binds, matchBind{p.Cells[0], v})
		}
		return true, nil
	case matchList:
		if v.Type != LSExpr {
			return false, nil
		}
		elems, rest := matchListParts(p.Cells[1:])
		n := len(elems)
		if len(v.Cells) < n || (rest == nil && len(v.Cells) != n) {
			return false, nil
		}
		for i, q := range elems {
			if ok, lerr := m.match(q, v.Cells[i]); !ok || lerr != nil {
				return false, lerr
			}
		}
		if rest != nil {
			return m.match(rest, QExpr(Cells(v.Cells[n:]).Clone()))
		}
		return true, nil
	case matchMap:
		if v.Type != LSortMap {
			return false, nil
		}
		parts := p.Cells[1:]
		for i := 0; i < len(parts); i += 2 {
			f, ok := v.Map().Get(parts[i])
			if !ok {
				return false, nil
			}
			if ok, lerr := m.match(parts[i+1], f); !ok || lerr != nil {
				return false, lerr
			}
		}
		return true, nil
	case matchRecord:
		ok, lerr := m.isType(v, p.Cells[0])
		if !ok || lerr != nil {
			return false, lerr
		}
		types, _ := matchTypesOf(env)
		parts := p.Cells[1:]
		for i := 0; i < len(parts); i += 2 {
			f, ok, lerr := types.field(env, v, parts[i].Str[1:])
			if lerr != nil {
				return false, lerr
			}
			if !ok {
				return false, nil
			}
			if ok, lerr := m.match(parts[i+1], f); !ok || lerr != nil {
				return false, lerr
			}
		}
		return true, nil
	case matchBad:
	}
	return false, env.Errorf("match: bad pattern: %v", p)
}

// equal reports whether v is equal? to lit.
func (m *matcher) equal(lit, v *LVal) (bool, *LVal) {
	r := lit.EqualWithEnv(v, m.env)
	if r.IsError() {
		return false, r
	}
	return !Not(r), nil
}

// isType evaluates the type expression typ and tests v against it.
func (m *matcher) isType(v, typ *LVal) (bool, *LVal) {
	env := m.env
	types, ok := matchTypesOf(env)
	if !ok {
		return false, env.Errorf("match: a type pattern needs a type library; load package s")
	}
	t := env.Eval(typ)
	if t.IsError() {
		return false, t
	}
	r := types.is(env, v, t)
	if r.IsError() {
		return false, r
	}
	return !Not(r), nil
}
